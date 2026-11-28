// SPDX-License-Identifier: MIT
// Deflate decoder (RFC 1951) for raw and zlib (RFC 1950) streams.
//
// Huffman codes up to FAST_BITS long are decoded with one table lookup, longer
// ones canonically bit by bit (as in zlib's puff). The output buffer is given by
// the caller; when it is too small the decoder stops with FORTIO_INFLATE_FULL and
// the caller retries with a larger one.
#include <stddef.h>
#include <stdint.h>
#include <string.h>

enum {
    FAST_BITS = 10,
    MAX_BITS = 15,
    FORTIO_INFLATE_OK = 0,
    FORTIO_INFLATE_DATA = -1,    // malformed stream
    FORTIO_INFLATE_TRUNC = -2,   // input ends inside the stream
    FORTIO_INFLATE_CHECK = -3,   // Adler-32 differs
    FORTIO_INFLATE_FULL = 1      // output buffer too small
};

typedef struct {
    const uint8_t *in;
    size_t n, pos;
    uint64_t bits;
    unsigned count;
} reader_t;

typedef struct {
    uint16_t fast[1 << FAST_BITS];  // symbol << 4 | length, 0 if longer than FAST_BITS
    uint16_t count[MAX_BITS + 1];
    uint16_t symbol[288];
} huffman_t;

static inline void refill(reader_t *r)
{
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    if (r->pos + 8 <= r->n) {
        // one unaligned load fills the buffer to at least 56 bits
        uint64_t word;
        memcpy(&word, r->in + r->pos, 8);
        r->bits |= word << r->count;
        r->pos += (63 - r->count) >> 3;
        r->count |= 56;
        return;
    }
#endif
    while (r->count <= 56 && r->pos < r->n) {
        r->bits |= (uint64_t)r->in[r->pos++] << r->count;
        r->count += 8;
    }
}

static inline int bits(reader_t *r, unsigned k, unsigned *value)
{
    if (r->count < k) {
        refill(r);
        if (r->count < k) {
            return 0;
        }
    }
    *value = (unsigned)(r->bits & ((UINT64_C(1) << k) - 1));
    r->bits >>= k;
    r->count -= k;
    return 1;
}

// Canonical code from code lengths; 0 on an over-subscribed code.
static int build(huffman_t *h, const uint8_t *len, int n)
{
    uint16_t offs[MAX_BITS + 2];
    int left = 1, i;
    unsigned b;

    memset(h->count, 0, sizeof(h->count));
    memset(h->fast, 0, sizeof(h->fast));
    for (i = 0; i < n; ++i) {
        h->count[len[i]]++;
    }
    for (b = 1; b <= MAX_BITS; ++b) {
        left = (left << 1) - h->count[b];
        if (left < 0) {
            return 0;
        }
    }
    offs[1] = 0;
    for (b = 1; b < MAX_BITS; ++b) {
        offs[b + 1] = (uint16_t)(offs[b] + h->count[b]);
    }
    for (i = 0; i < n; ++i) {
        if (len[i] != 0) {
            h->symbol[offs[len[i]]++] = (uint16_t)i;
        }
    }
    // fast table: walk the canonical codes in order
    {
        unsigned code = 0, k = 0;
        for (b = 1; b <= FAST_BITS; ++b) {
            unsigned c;
            for (c = 0; c < h->count[b]; ++c, ++code, ++k) {
                unsigned rev = 0, x = code, j, step;
                for (j = 0; j < b; ++j) {
                    rev = (rev << 1) | (x & 1u);
                    x >>= 1;
                }
                for (step = rev; step < (1u << FAST_BITS); step += 1u << b) {
                    h->fast[step] = (uint16_t)(h->symbol[k] << 4 | b);
                }
            }
            code <<= 1;
        }
    }
    return 1;
}

static inline int decode(reader_t *r, const huffman_t *h)
{
    unsigned e, len;
    int code = 0, first = 0, index = 0;

    if (r->count < MAX_BITS) {
        refill(r);
    }
    e = h->fast[r->bits & ((1u << FAST_BITS) - 1)];
    if (e != 0 && (e & 15u) <= r->count) {
        r->bits >>= e & 15u;
        r->count -= e & 15u;
        return (int)(e >> 4);
    }
    for (len = 1; len <= MAX_BITS && len <= r->count; ++len) {
        int count;
        code |= (int)((r->bits >> (len - 1)) & 1u);
        count = h->count[len];
        if (code - count < first) {
            r->bits >>= len;
            r->count -= len;
            return h->symbol[index + (code - first)];
        }
        index += count;
        first += count;
        first <<= 1;
        code <<= 1;
    }
    return len > r->count ? -2 : -1;
}

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

static inline int codes_local(reader_t *r, const huffman_t *lit, const huffman_t *dist,
                              uint8_t *out, size_t cap, size_t *pos);

// The reader state is copied into a local so that the compiler can keep it in
// registers: stores into the output bytes could otherwise alias it.
static int codes(reader_t *r, const huffman_t *lit, const huffman_t *dist,
                 uint8_t *out, size_t cap, size_t *pos)
{
    reader_t local = *r;
    size_t at = *pos;
    int status = codes_local(&local, lit, dist, out, cap, &at);
    *r = local;
    *pos = at;
    return status;
}

static inline int codes_local(reader_t *r, const huffman_t *lit, const huffman_t *dist,
                              uint8_t *out, size_t cap, size_t *pos)
{
    for (;;) {
        int s = decode(r, lit);
        if (s < 0) {
            return s == -2 ? FORTIO_INFLATE_TRUNC : FORTIO_INFLATE_DATA;
        }
        if (s < 256) {
            if (*pos >= cap) {
                return FORTIO_INFLATE_FULL;
            }
            out[(*pos)++] = (uint8_t)s;
        } else if (s == 256) {
            return FORTIO_INFLATE_OK;
        } else {
            unsigned extra, length, distance;
            int d;
            s -= 257;
            if (s >= 29 || !bits(r, length_extra[s], &extra)) {
                return s >= 29 ? FORTIO_INFLATE_DATA : FORTIO_INFLATE_TRUNC;
            }
            length = length_base[s] + extra;
            d = decode(r, dist);
            if (d < 0 || d >= 30) {
                return d == -2 ? FORTIO_INFLATE_TRUNC : FORTIO_INFLATE_DATA;
            }
            if (!bits(r, dist_extra[d], &extra)) {
                return FORTIO_INFLATE_TRUNC;
            }
            distance = dist_base[d] + extra;
            if (distance > *pos) {
                return FORTIO_INFLATE_DATA;
            }
            if (*pos + length > cap) {
                return FORTIO_INFLATE_FULL;
            }
            {
                uint8_t *dst = out + *pos;
                const uint8_t *src = dst - distance;
                if (distance >= length) {
                    memcpy(dst, src, length);
                } else {
                    unsigned i;
                    for (i = 0; i < length; ++i) {
                        dst[i] = src[i];
                    }
                }
            }
            *pos += length;
        }
    }
}

static int fixed_tables(huffman_t *lit, huffman_t *dist)
{
    uint8_t len[288];
    int i;
    for (i = 0; i < 288; ++i) {
        len[i] = i < 144 ? 8 : i < 256 ? 9 : i < 280 ? 7 : 8;
    }
    if (!build(lit, len, 288)) {
        return 0;
    }
    for (i = 0; i < 30; ++i) {
        len[i] = 5;
    }
    return build(dist, len, 30);
}

static int dynamic_tables(reader_t *r, huffman_t *lit, huffman_t *dist)
{
    static const uint8_t order[19] = {16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15};
    uint8_t len[320];
    unsigned nlen, ndist, ncode, i, v;
    int index = 0;
    huffman_t lencode;

    if (!bits(r, 5, &nlen) || !bits(r, 5, &ndist) || !bits(r, 4, &ncode)) {
        return FORTIO_INFLATE_TRUNC;
    }
    nlen += 257;
    ndist += 1;
    ncode += 4;
    if (nlen > 286 || ndist > 30) {
        return FORTIO_INFLATE_DATA;
    }
    memset(len, 0, 19);
    for (i = 0; i < ncode; ++i) {
        if (!bits(r, 3, &v)) {
            return FORTIO_INFLATE_TRUNC;
        }
        len[order[i]] = (uint8_t)v;
    }
    if (!build(&lencode, len, 19)) {
        return FORTIO_INFLATE_DATA;
    }
    while ((unsigned)index < nlen + ndist) {
        int s = decode(r, &lencode);
        unsigned repeat, value = 0;
        if (s < 0) {
            return s == -2 ? FORTIO_INFLATE_TRUNC : FORTIO_INFLATE_DATA;
        }
        if (s < 16) {
            len[index++] = (uint8_t)s;
            continue;
        }
        if (s == 16) {
            if (index == 0) {
                return FORTIO_INFLATE_DATA;
            }
            value = len[index - 1];
            if (!bits(r, 2, &repeat)) {
                return FORTIO_INFLATE_TRUNC;
            }
            repeat += 3;
        } else if (s == 17) {
            if (!bits(r, 3, &repeat)) {
                return FORTIO_INFLATE_TRUNC;
            }
            repeat += 3;
        } else {
            if (!bits(r, 7, &repeat)) {
                return FORTIO_INFLATE_TRUNC;
            }
            repeat += 11;
        }
        if (index + repeat > nlen + ndist) {
            return FORTIO_INFLATE_DATA;
        }
        while (repeat-- > 0) {
            len[index++] = (uint8_t)value;
        }
    }
    if (len[256] == 0) {
        return FORTIO_INFLATE_DATA;
    }
    if (!build(lit, len, (int)nlen) || !build(dist, len + nlen, (int)ndist)) {
        return FORTIO_INFLATE_DATA;
    }
    return FORTIO_INFLATE_OK;
}

static int stored(reader_t *r, uint8_t *out, size_t cap, size_t *pos)
{
    unsigned length, check;
    // drop to the byte boundary, then give back whole bytes still in the bit buffer
    r->bits >>= r->count & 7u;
    r->count -= r->count & 7u;
    r->pos -= r->count / 8;
    r->bits = 0;
    r->count = 0;
    if (r->pos + 4 > r->n) {
        return FORTIO_INFLATE_TRUNC;
    }
    length = r->in[r->pos] | (unsigned)r->in[r->pos + 1] << 8;
    check = r->in[r->pos + 2] | (unsigned)r->in[r->pos + 3] << 8;
    r->pos += 4;
    if (length != (~check & 0xffffu)) {
        return FORTIO_INFLATE_DATA;
    }
    if (r->pos + length > r->n) {
        return FORTIO_INFLATE_TRUNC;
    }
    if (*pos + length > cap) {
        return FORTIO_INFLATE_FULL;
    }
    memcpy(out + *pos, r->in + r->pos, length);
    r->pos += length;
    *pos += length;
    return FORTIO_INFLATE_OK;
}

static int inflate_blocks(reader_t *r, uint8_t *out, size_t cap, size_t *pos)
{
    huffman_t lit, dist;
    unsigned last, type;
    int status;

    do {
        if (!bits(r, 1, &last) || !bits(r, 2, &type)) {
            return FORTIO_INFLATE_TRUNC;
        }
        if (type == 0) {
            status = stored(r, out, cap, pos);
        } else if (type == 1) {
            status = fixed_tables(&lit, &dist) ? codes(r, &lit, &dist, out, cap, pos)
                                               : FORTIO_INFLATE_DATA;
        } else if (type == 2) {
            status = dynamic_tables(r, &lit, &dist);
            if (status == FORTIO_INFLATE_OK) {
                status = codes(r, &lit, &dist, out, cap, pos);
            }
        } else {
            status = FORTIO_INFLATE_DATA;
        }
        if (status != FORTIO_INFLATE_OK) {
            return status;
        }
    } while (!last);
    // whole bytes left in the bit buffer belong to what follows the stream
    r->pos -= r->count / 8;
    r->bits = 0;
    r->count = 0;
    return FORTIO_INFLATE_OK;
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

// Raw deflate stream into out[0:cap]; *produced is the number of bytes written.
int fortio_inflate_raw(const int8_t *input, size_t n, int8_t *output, size_t cap,
                       size_t *produced)
{
    reader_t r = {(const uint8_t *)input, n, 0, 0, 0};
    *produced = 0;
    return inflate_blocks(&r, (uint8_t *)output, cap, produced);
}

// zlib stream (header, deflate data, Adler-32) into out[0:cap].
int fortio_inflate_zlib(const int8_t *input, size_t n, int8_t *output, size_t cap,
                        size_t *produced)
{
    const uint8_t *in = (const uint8_t *)input;
    reader_t r = {in, n, 2, 0, 0};
    uint32_t expected;
    int status;

    *produced = 0;
    if (n < 6) {
        return FORTIO_INFLATE_TRUNC;
    }
    if ((in[0] & 15) != 8 || (in[0] >> 4) > 7 || (in[0] * 256u + in[1]) % 31 != 0 || (in[1] & 32)) {
        return FORTIO_INFLATE_DATA;
    }
    status = inflate_blocks(&r, (uint8_t *)output, cap, produced);
    if (status != FORTIO_INFLATE_OK) {
        return status;
    }
    if (r.pos + 4 > n) {
        return FORTIO_INFLATE_TRUNC;
    }
    expected = (uint32_t)in[r.pos] << 24 | (uint32_t)in[r.pos + 1] << 16 |
               (uint32_t)in[r.pos + 2] << 8 | in[r.pos + 3];
    return adler32((const uint8_t *)output, *produced) == expected ? FORTIO_INFLATE_OK
                                                                   : FORTIO_INFLATE_CHECK;
}
