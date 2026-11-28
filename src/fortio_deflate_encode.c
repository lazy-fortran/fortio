// SPDX-License-Identifier: MIT
// Deflate encoder (RFC 1951) with zlib framing (RFC 1950).
//
// LZ77 with hash chains feeds blocks of at most SYMBOLS symbols. Each block is
// written as dynamic Huffman, fixed Huffman or stored, whichever is shortest, so
// the output never exceeds fortio_zlib_bound(n) bytes.
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

enum {
    WINDOW = 32768,
    HASH_BITS = 15,
    HASH_SIZE = 1 << HASH_BITS,
    MIN_MATCH = 3,
    MAX_MATCH = 258,
    SYMBOLS = 16384,
    LITLEN = 286,
    DIST = 30,
    CODELEN = 19,
    MAX_BITS = 15,
    MAX_CL_BITS = 7
};

static const uint16_t length_base[29] = {
    3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
    35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258};
static const uint8_t length_extra[29] = {
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
    3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0};
static const uint16_t dist_base[30] = {
    1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
    257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577};
static const uint8_t dist_extra[30] = {
    0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
    7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13};
static const uint8_t cl_order[CODELEN] = {
    16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15};

typedef struct {
    uint8_t *out;
    size_t capacity, position;
    uint64_t bits;
    unsigned count;
    int overflow;
} writer_t;

static void put_bits(writer_t *w, uint32_t value, unsigned width)
{
    w->bits |= (uint64_t)value << w->count;
    w->count += width;
    while (w->count >= 8) {
        if (w->position < w->capacity) {
            w->out[w->position++] = (uint8_t)w->bits;
        } else {
            w->overflow = 1;
        }
        w->bits >>= 8;
        w->count -= 8;
    }
}

static void align_byte(writer_t *w)
{
    if (w->count > 0) {
        put_bits(w, 0, 8 - w->count);
    }
}

static void put_bytes(writer_t *w, const uint8_t *bytes, size_t n)
{
    if (w->position + n > w->capacity) {
        w->overflow = 1;
        return;
    }
    memcpy(w->out + w->position, bytes, n);
    w->position += n;
}

static uint32_t reverse(uint32_t code, unsigned width)
{
    uint32_t r = 0;
    while (width-- > 0) {
        r = (r << 1) | (code & 1u);
        code >>= 1;
    }
    return r;
}

// Huffman code lengths of at most `limit` bits for `n` symbols with frequencies
// `freq`. Unused symbols get length 0. With fewer than two used symbols, two
// symbols get length 1 so that the code is complete.
static void code_lengths(const uint32_t *freq, int n, unsigned limit, uint8_t *len)
{
    int order[LITLEN], used = 0, i, j;
    uint32_t weight[2 * LITLEN];
    int parent[2 * LITLEN];
    unsigned depth[2 * LITLEN], count[MAX_BITS + 2] = {0};
    int nodes, lo, hi;

    memset(len, 0, (size_t)n);
    for (i = 0; i < n; ++i) {
        if (freq[i] > 0) {
            order[used++] = i;
        }
    }
    if (used < 2) {
        int s = used == 1 ? order[0] : 0;
        len[s] = 1;
        len[s == 0 ? 1 : 0] = 1;
        return;
    }
    // leaves by ascending frequency, ties by symbol
    for (i = 1; i < used; ++i) {
        int s = order[i];
        for (j = i - 1; j >= 0 && freq[order[j]] > freq[s]; --j) {
            order[j + 1] = order[j];
        }
        order[j + 1] = s;
    }
    // two-queue Huffman construction: leaves 0..used-1, internal nodes after
    for (i = 0; i < used; ++i) {
        weight[i] = freq[order[i]];
    }
    nodes = used;
    lo = 0;
    hi = used;
    for (i = 0; i < used - 1; ++i) {
        int pick[2], k;
        for (k = 0; k < 2; ++k) {
            if (lo < used && (hi >= nodes || weight[lo] <= weight[hi])) {
                pick[k] = lo++;
            } else {
                pick[k] = hi++;
            }
        }
        weight[nodes] = weight[pick[0]] + weight[pick[1]];
        parent[pick[0]] = nodes;
        parent[pick[1]] = nodes;
        ++nodes;
    }
    depth[nodes - 1] = 0;
    for (i = nodes - 2; i >= 0; --i) {
        depth[i] = depth[parent[i]] + 1;
    }
    for (i = 0; i < used; ++i) {
        count[depth[i] > limit ? limit : depth[i]]++;
    }
    // Kraft repair after clamping to `limit` (as in miniz)
    {
        uint32_t total = 0;
        for (i = (int)limit; i > 0; --i) {
            total += count[i] << (limit - (unsigned)i);
        }
        while (total > (1u << limit)) {
            count[limit]--;
            for (i = (int)limit - 1; i > 0; --i) {
                if (count[i] != 0) {
                    count[i]--;
                    count[i + 1] += 2;
                    break;
                }
            }
            total--;
        }
    }
    // the least frequent symbols get the longest codes
    j = 0;
    for (i = (int)limit; i > 0; --i) {
        unsigned c;
        for (c = 0; c < count[i]; ++c) {
            len[order[j++]] = (uint8_t)i;
        }
    }
}

// Canonical codes (RFC 1951, 3.2.2), bit-reversed for output.
static void canonical(const uint8_t *len, int n, uint16_t *code)
{
    unsigned count[MAX_BITS + 1] = {0}, next[MAX_BITS + 1];
    unsigned c = 0, b;
    int i;

    for (i = 0; i < n; ++i) {
        count[len[i]]++;
    }
    count[0] = 0;
    for (b = 1; b <= MAX_BITS; ++b) {
        c = (c + count[b - 1]) << 1;
        next[b] = c;
    }
    for (i = 0; i < n; ++i) {
        code[i] = len[i] ? (uint16_t)reverse(next[len[i]]++, len[i]) : 0;
    }
}

static int length_symbol(unsigned length)
{
    int i = 0;
    while (i < 28 && length >= length_base[i + 1]) {
        ++i;
    }
    return i;
}

static int dist_symbol(unsigned distance)
{
    int i = 0;
    while (i < 29 && distance >= dist_base[i + 1]) {
        ++i;
    }
    return i;
}

typedef struct {
    uint16_t litlen[SYMBOLS];  // literal byte, or match length + 256
    uint16_t dist[SYMBOLS];    // 0 for a literal
    int n;
    size_t start;              // input position of the block's first byte
} block_t;

// Run-length code of the code-length sequence (symbols 16, 17, 18).
static int rle_lengths(const uint8_t *lens, int n, uint8_t *sym, uint8_t *extra)
{
    int i = 0, m = 0;
    while (i < n) {
        int run = 1;
        while (i + run < n && lens[i + run] == lens[i]) {
            ++run;
        }
        if (lens[i] == 0 && run >= 3) {
            int r = run > 138 ? 138 : run;
            if (r >= 11) {
                sym[m] = 18;
                extra[m++] = (uint8_t)(r - 11);
            } else {
                sym[m] = 17;
                extra[m++] = (uint8_t)(r - 3);
            }
            i += r;
        } else if (lens[i] != 0 && run >= 4) {
            int r = run - 1 > 6 ? 6 : run - 1;
            sym[m] = lens[i];
            extra[m++] = 0;
            sym[m] = 16;
            extra[m++] = (uint8_t)(r - 3);
            i += r + 1;
        } else {
            sym[m] = lens[i];
            extra[m++] = 0;
            ++i;
        }
    }
    return m;
}

static void put_stored(writer_t *w, const uint8_t *data, size_t n, int final)
{
    size_t done = 0;
    do {
        size_t part = n - done > 65535 ? 65535 : n - done;
        int last = final && done + part == n;
        uint8_t header[4];
        put_bits(w, (uint32_t)last, 3);
        align_byte(w);
        header[0] = (uint8_t)part;
        header[1] = (uint8_t)(part >> 8);
        header[2] = (uint8_t)~part;
        header[3] = (uint8_t)(~part >> 8);
        put_bytes(w, header, 4);
        put_bytes(w, data + done, part);
        done += part;
    } while (done < n);
}

static void put_symbols(writer_t *w, const block_t *b, const uint16_t *lcode,
                        const uint8_t *llen, const uint16_t *dcode, const uint8_t *dlen)
{
    int i;
    for (i = 0; i < b->n; ++i) {
        unsigned s = b->litlen[i];
        if (b->dist[i] == 0) {
            put_bits(w, lcode[s], llen[s]);
        } else {
            int ls = length_symbol(s - 256), ds = dist_symbol(b->dist[i]);
            put_bits(w, lcode[257 + ls], llen[257 + ls]);
            put_bits(w, (uint32_t)(s - 256 - length_base[ls]), length_extra[ls]);
            put_bits(w, dcode[ds], dlen[ds]);
            put_bits(w, (uint32_t)(b->dist[i] - dist_base[ds]), dist_extra[ds]);
        }
    }
    put_bits(w, lcode[256], llen[256]);
}

static void flush_block(writer_t *w, block_t *b, const uint8_t *input, size_t end, int final)
{
    uint32_t lfreq[LITLEN] = {0}, dfreq[DIST] = {0}, cfreq[CODELEN] = {0};
    uint8_t llen[LITLEN], dlen[DIST], clen[CODELEN], fixed_llen[288], fixed_dlen[DIST];
    uint16_t lcode[288], dcode[DIST], ccode[CODELEN];
    uint8_t lens[LITLEN + DIST], rsym[LITLEN + DIST], rextra[LITLEN + DIST];
    uint64_t extra_bits = 0, dyn_bits, fixed_bits, stored_bits;
    int i, hlit, hdist, hclen, nr;
    size_t span = end - b->start;

    for (i = 0; i < b->n; ++i) {
        if (b->dist[i] == 0) {
            lfreq[b->litlen[i]]++;
        } else {
            int ls = length_symbol(b->litlen[i] - 256u), ds = dist_symbol(b->dist[i]);
            lfreq[257 + ls]++;
            dfreq[ds]++;
            extra_bits += length_extra[ls] + dist_extra[ds];
        }
    }
    lfreq[256] = 1;
    code_lengths(lfreq, LITLEN, MAX_BITS, llen);
    code_lengths(dfreq, DIST, MAX_BITS, dlen);
    for (hlit = LITLEN; hlit > 257 && llen[hlit - 1] == 0; --hlit) {
    }
    for (hdist = DIST; hdist > 1 && dlen[hdist - 1] == 0; --hdist) {
    }
    memcpy(lens, llen, (size_t)hlit);
    memcpy(lens + hlit, dlen, (size_t)hdist);
    nr = rle_lengths(lens, hlit + hdist, rsym, rextra);
    for (i = 0; i < nr; ++i) {
        cfreq[rsym[i]]++;
    }
    code_lengths(cfreq, CODELEN, MAX_CL_BITS, clen);
    for (hclen = CODELEN; hclen > 4 && clen[cl_order[hclen - 1]] == 0; --hclen) {
    }

    // the fixed code covers 288 symbols, 286 and 287 included (RFC 1951, 3.2.6)
    for (i = 0; i < 288; ++i) {
        fixed_llen[i] = i < 144 ? 8 : i < 256 ? 9 : i < 280 ? 7 : 8;
    }
    for (i = 0; i < DIST; ++i) {
        fixed_dlen[i] = 5;
    }
    dyn_bits = 3 + 14 + 3 * (uint64_t)hclen + extra_bits;
    fixed_bits = 3 + extra_bits;
    for (i = 0; i < LITLEN; ++i) {
        dyn_bits += (uint64_t)lfreq[i] * llen[i];
        fixed_bits += (uint64_t)lfreq[i] * fixed_llen[i];
    }
    for (i = 0; i < DIST; ++i) {
        dyn_bits += (uint64_t)dfreq[i] * dlen[i];
        fixed_bits += (uint64_t)dfreq[i] * 5;
    }
    for (i = 0; i < nr; ++i) {
        dyn_bits += clen[rsym[i]] + (rsym[i] == 16 ? 2 : rsym[i] == 17 ? 3 : rsym[i] == 18 ? 7 : 0);
    }
    stored_bits = 8 * ((uint64_t)span + 5 * (span / 65535 + 1)) + 8;

    if (stored_bits <= dyn_bits && stored_bits <= fixed_bits) {
        put_stored(w, input + b->start, span, final);
    } else if (fixed_bits <= dyn_bits) {
        canonical(fixed_llen, 288, lcode);
        canonical(fixed_dlen, DIST, dcode);
        put_bits(w, (uint32_t)(final | 2), 3);
        put_symbols(w, b, lcode, fixed_llen, dcode, fixed_dlen);
    } else {
        canonical(llen, LITLEN, lcode);
        canonical(dlen, DIST, dcode);
        canonical(clen, CODELEN, ccode);
        put_bits(w, (uint32_t)(final | 4), 3);
        put_bits(w, (uint32_t)(hlit - 257), 5);
        put_bits(w, (uint32_t)(hdist - 1), 5);
        put_bits(w, (uint32_t)(hclen - 4), 4);
        for (i = 0; i < hclen; ++i) {
            put_bits(w, clen[cl_order[i]], 3);
        }
        for (i = 0; i < nr; ++i) {
            put_bits(w, ccode[rsym[i]], clen[rsym[i]]);
            if (rsym[i] == 16) {
                put_bits(w, rextra[i], 2);
            } else if (rsym[i] == 17) {
                put_bits(w, rextra[i], 3);
            } else if (rsym[i] == 18) {
                put_bits(w, rextra[i], 7);
            }
        }
        put_symbols(w, b, lcode, llen, dcode, dlen);
    }
    b->n = 0;
    b->start = end;
}

static uint32_t hash3(const uint8_t *p)
{
    uint32_t v = ((uint32_t)p[0] << 16) | ((uint32_t)p[1] << 8) | p[2];
    return (v * UINT32_C(2654435761)) >> (32 - HASH_BITS);
}

static uint32_t adler32(const uint8_t *bytes, size_t size)
{
    uint32_t a = 1, b = 0;
    while (size > 0) {
        size_t block = size > 5552 ? 5552 : size;
        size -= block;
        while (block-- > 0) {
            a += *bytes++;
            b += a;
        }
        a %= 65521;
        b %= 65521;
    }
    return (b << 16) | a;
}

size_t fortio_zlib_bound(size_t n)
{
    return n + 6 * (n / SYMBOLS + 2) + 5 * (n / 65535 + 1) + 16;
}

// Raw deflate stream of `n` bytes into `out`; returns its length, 0 on overflow.
// `level` 1 to 9 sets the hash-chain search depth.
static size_t deflate_into(const uint8_t *in, size_t n, writer_t *w, int level)
{
    static const int chain_depth[10] = {0, 4, 8, 16, 32, 64, 128, 256, 1024, 4096};
    int32_t *head = malloc(sizeof(int32_t) * HASH_SIZE);
    int32_t *prev = malloc(sizeof(int32_t) * WINDOW);
    block_t *b = malloc(sizeof(block_t));
    int depth = chain_depth[level < 1 ? 1 : level > 9 ? 9 : level];
    size_t pos = 0;

    if (head == NULL || prev == NULL || b == NULL) {
        free(head);
        free(prev);
        free(b);
        w->overflow = 1;
        return 0;
    }
    memset(head, 0xff, sizeof(int32_t) * HASH_SIZE);
    b->n = 0;
    b->start = 0;
    while (pos < n) {
        unsigned best = 0, best_dist = 0;
        if (pos + MIN_MATCH <= n) {
            uint32_t h = hash3(in + pos);
            int32_t cand = head[h];
            size_t limit = n - pos > MAX_MATCH ? MAX_MATCH : n - pos;
            int chain = depth;
            while (cand >= 0 && pos - (size_t)cand <= WINDOW && chain-- > 0) {
                const uint8_t *a = in + cand, *p = in + pos;
                if (a[best] == p[best]) {
                    unsigned l = 0;
                    while (l < limit && a[l] == p[l]) {
                        ++l;
                    }
                    if (l > best) {
                        best = l;
                        best_dist = (unsigned)(pos - (size_t)cand);
                        if (l == limit) {
                            break;
                        }
                    }
                }
                cand = prev[cand & (WINDOW - 1)];
            }
            prev[pos & (WINDOW - 1)] = head[h];
            head[h] = (int32_t)pos;
        }
        if (best >= MIN_MATCH) {
            size_t k;
            b->litlen[b->n] = (uint16_t)(256 + best);
            b->dist[b->n++] = (uint16_t)best_dist;
            // insert the positions inside short matches; long ones are skipped for speed
            if (best <= 32) {
                for (k = 1; k < best && pos + k + MIN_MATCH <= n; ++k) {
                    uint32_t h = hash3(in + pos + k);
                    prev[(pos + k) & (WINDOW - 1)] = head[h];
                    head[h] = (int32_t)(pos + k);
                }
            }
            pos += best;
        } else {
            b->litlen[b->n] = in[pos];
            b->dist[b->n++] = 0;
            ++pos;
        }
        if (b->n == SYMBOLS) {
            flush_block(w, b, in, pos, pos == n);
        }
    }
    if (b->n > 0 || n == 0) {
        flush_block(w, b, in, pos, 1);
    }
    align_byte(w);
    free(head);
    free(prev);
    free(b);
    return w->overflow ? 0 : w->position;
}

size_t fortio_deflate_raw(const int8_t *input, size_t n, int8_t *output, size_t capacity,
                          int level)
{
    writer_t w = {(uint8_t *)output, capacity, 0, 0, 0, 0};
    return deflate_into((const uint8_t *)input, n, &w, level);
}

// zlib stream (header, deflate data, Adler-32); returns its length, 0 on overflow.
size_t fortio_zlib_compress(const int8_t *input, size_t n, int8_t *output, size_t capacity,
                            int level)
{
    const uint8_t *in = (const uint8_t *)input;
    writer_t w = {(uint8_t *)output, capacity, 0, 0, 0, 0};
    uint8_t header[2] = {0x78, level <= 1 ? 0x01 : level < 6 ? 0x5e : level == 6 ? 0x9c : 0xda};
    uint8_t trailer[4];
    uint32_t check;

    put_bytes(&w, header, 2);
    if (w.overflow || (n > 0 && deflate_into(in, n, &w, level) == 0)) {
        return 0;
    }
    if (n == 0) {
        put_bits(&w, 3, 3);  // final fixed block holding only end-of-block (7 zero bits)
        put_bits(&w, 0, 7);
        align_byte(&w);
    }
    check = adler32(in, n);
    trailer[0] = (uint8_t)(check >> 24);
    trailer[1] = (uint8_t)(check >> 16);
    trailer[2] = (uint8_t)(check >> 8);
    trailer[3] = (uint8_t)check;
    put_bytes(&w, trailer, 4);
    return w.overflow ? 0 : w.position;
}
