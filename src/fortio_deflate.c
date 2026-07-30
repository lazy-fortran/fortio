// SPDX-License-Identifier: MIT
#include <stddef.h>
#include <stdint.h>
#include <string.h>

enum {
    HASH_BITS = 15,
    HASH_SIZE = 1 << HASH_BITS,
    WINDOW_SIZE = 32768,
    MIN_MATCH = 3,
    MAX_MATCH = 258
};

typedef struct {
    uint8_t *output;
    size_t capacity;
    size_t position;
    uint64_t bits;
    unsigned count;
} bit_writer;

static unsigned reverse_bits(unsigned value, unsigned width)
{
    unsigned result = 0;
    while (width-- != 0) {
        result = (result << 1) | (value & 1u);
        value >>= 1;
    }
    return result;
}

static int write_bits(bit_writer *writer, unsigned value, unsigned width)
{
    writer->bits |= (uint64_t)value << writer->count;
    writer->count += width;
    while (writer->count >= 8) {
        if (writer->position >= writer->capacity) {
            return 0;
        }
        writer->output[writer->position++] = (uint8_t)writer->bits;
        writer->bits >>= 8;
        writer->count -= 8;
    }
    return 1;
}

static int flush_bits(bit_writer *writer)
{
    if (writer->count == 0) {
        return 1;
    }
    if (writer->position >= writer->capacity) {
        return 0;
    }
    writer->output[writer->position++] = (uint8_t)writer->bits;
    writer->bits = 0;
    writer->count = 0;
    return 1;
}

static int write_fixed_symbol(bit_writer *writer, unsigned symbol)
{
    unsigned code;
    unsigned width;

    if (symbol <= 143) {
        code = symbol + 48;
        width = 8;
    } else if (symbol <= 255) {
        code = symbol - 144 + 400;
        width = 9;
    } else if (symbol <= 279) {
        code = symbol - 256;
        width = 7;
    } else {
        code = symbol - 280 + 192;
        width = 8;
    }
    return write_bits(writer, reverse_bits(code, width), width);
}

static unsigned hash3(const uint8_t *input, size_t position)
{
    uint32_t value = ((uint32_t)input[position] << 16)
                   | ((uint32_t)input[position + 1] << 8)
                   | input[position + 2];
    value ^= value >> 9;
    value *= UINT32_C(0x9e3779b1);
    return value >> (32 - HASH_BITS);
}

static void length_code(unsigned length, unsigned *symbol, unsigned *extra,
                        unsigned *extra_width)
{
    static const uint16_t base[] = {
        3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35,
        43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258
    };
    static const uint8_t width[] = {
        0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3,
        3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0
    };
    unsigned index;

    for (index = 0; index < 28 && length >= base[index + 1]; ++index) {
    }
    *symbol = 257 + index;
    *extra_width = width[index];
    *extra = length - base[index];
}

static void distance_code(unsigned distance, unsigned *symbol, unsigned *extra,
                          unsigned *extra_width)
{
    static const uint16_t base[] = {
        1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129,
        193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097,
        6145, 8193, 12289, 16385, 24577
    };
    static const uint8_t width[] = {
        0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6,
        6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13
    };
    unsigned index;

    for (index = 0; index < 29 && distance >= base[index + 1]; ++index) {
    }
    *symbol = index;
    *extra_width = width[index];
    *extra = distance - base[index];
}

size_t fortio_deflate_fixed(const int8_t *input_bytes, size_t input_size,
                            int8_t *output_bytes, size_t output_capacity)
{
    const uint8_t *input = (const uint8_t *)input_bytes;
    uint8_t *output = (uint8_t *)output_bytes;
    int32_t latest[HASH_SIZE];
    bit_writer writer = {output, output_capacity, 0, 0, 0};
    size_t position = 0;

    memset(latest, 0xff, sizeof(latest));
    if (!write_bits(&writer, 3, 3)) {
        return 0;
    }
    while (position < input_size) {
        size_t match_length = 0;
        size_t candidate = 0;
        unsigned hash = 0;

        if (position + MIN_MATCH <= input_size) {
            int32_t stored;
            hash = hash3(input, position);
            stored = latest[hash];
            if (stored >= 0 && position - (size_t)stored <= WINDOW_SIZE) {
                size_t limit = input_size - position;
                candidate = (size_t)stored;
                if (limit > MAX_MATCH) {
                    limit = MAX_MATCH;
                }
                while (match_length < limit
                        && input[candidate + match_length]
                            == input[position + match_length]) {
                    ++match_length;
                }
            }
        }
        if (match_length >= MIN_MATCH) {
            unsigned symbol, extra, extra_width;
            size_t offset;

            length_code((unsigned)match_length, &symbol, &extra, &extra_width);
            if (!write_fixed_symbol(&writer, symbol)
                    || !write_bits(&writer, extra, extra_width)) {
                return 0;
            }
            distance_code((unsigned)(position - candidate), &symbol, &extra,
                          &extra_width);
            if (!write_bits(&writer, reverse_bits(symbol, 5), 5)
                    || !write_bits(&writer, extra, extra_width)) {
                return 0;
            }
            for (offset = 0; offset < match_length; offset += 8) {
                if (position + offset + MIN_MATCH <= input_size) {
                    latest[hash3(input, position + offset)]
                        = (int32_t)(position + offset);
                }
            }
            if (match_length > 3) {
                size_t tail = match_length - 3;
                for (offset = tail; offset < match_length; ++offset) {
                    if (position + offset + MIN_MATCH <= input_size) {
                        latest[hash3(input, position + offset)]
                            = (int32_t)(position + offset);
                    }
                }
            }
            position += match_length;
        } else {
            if (!write_fixed_symbol(&writer, input[position])) {
                return 0;
            }
            if (position + MIN_MATCH <= input_size) {
                latest[hash] = (int32_t)position;
            }
            ++position;
        }
    }
    if (!write_fixed_symbol(&writer, 256) || !flush_bits(&writer)) {
        return 0;
    }
    return writer.position;
}

typedef struct {
    const uint8_t *input;
    size_t size;
    size_t position;
    uint64_t bits;
    unsigned count;
} bit_reader;

static int read_bits(bit_reader *reader, unsigned width, unsigned *value)
{
    while (reader->count < width) {
        if (reader->position >= reader->size) {
            return 0;
        }
        reader->bits |= (uint64_t)reader->input[reader->position++]
                      << reader->count;
        reader->count += 8;
    }
    *value = (unsigned)(reader->bits & ((UINT64_C(1) << width) - 1));
    reader->bits >>= width;
    reader->count -= width;
    return 1;
}

static void fixed_decode_tables(uint16_t symbols[512], uint8_t widths[512])
{
    unsigned symbol;
    memset(widths, 0, 512 * sizeof(widths[0]));
    for (symbol = 0; symbol <= 287; ++symbol) {
        unsigned code;
        unsigned width;
        unsigned reversed;
        unsigned suffix;

        if (symbol <= 143) {
            code = symbol + 48;
            width = 8;
        } else if (symbol <= 255) {
            code = symbol - 144 + 400;
            width = 9;
        } else if (symbol <= 279) {
            code = symbol - 256;
            width = 7;
        } else {
            code = symbol - 280 + 192;
            width = 8;
        }
        reversed = reverse_bits(code, width);
        for (suffix = 0; suffix < (1u << (9 - width)); ++suffix) {
            unsigned index = reversed | (suffix << width);
            symbols[index] = (uint16_t)symbol;
            widths[index] = (uint8_t)width;
        }
    }
}

static uint32_t adler32_bytes(const uint8_t *bytes, size_t size)
{
    uint32_t a = 1;
    uint32_t b = 0;
    size_t position = 0;

    while (position < size) {
        size_t block = size - position;
        size_t i;
        if (block > 5552) {
            block = 5552;
        }
        for (i = 0; i < block; ++i) {
            a += bytes[position + i];
            b += a;
        }
        a %= 65521;
        b %= 65521;
        position += block;
    }
    return (b << 16) | a;
}

int fortio_inflate_fixed_zlib(const int8_t *input_bytes, size_t input_size,
                              int8_t *output_bytes, size_t output_size)
{
    static const uint16_t length_base[] = {
        3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35,
        43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258
    };
    static const uint8_t length_width[] = {
        0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3,
        3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0
    };
    static const uint16_t distance_base[] = {
        1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129,
        193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097,
        6145, 8193, 12289, 16385, 24577
    };
    static const uint8_t distance_width[] = {
        0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6,
        6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13
    };
    const uint8_t *input = (const uint8_t *)input_bytes;
    uint8_t *output = (uint8_t *)output_bytes;
    uint16_t symbols[512];
    uint8_t widths[512];
    bit_reader reader;
    size_t produced = 0;
    unsigned header;

    if (input_size < 8 || (input[0] & 15u) != 8
            || (((unsigned)input[0] << 8) + input[1]) % 31 != 0) {
        return -1;
    }
    reader.input = input + 2;
    reader.size = input_size - 6;
    reader.position = 0;
    reader.bits = 0;
    reader.count = 0;
    if (!read_bits(&reader, 3, &header)) {
        return -1;
    }
    if ((header & 1u) == 0 || ((header >> 1) & 3u) != 1) {
        return 1;
    }
    fixed_decode_tables(symbols, widths);
    for (;;) {
        unsigned look;
        unsigned width;
        unsigned symbol;

        while (reader.count < 9 && reader.position < reader.size) {
            reader.bits |= (uint64_t)reader.input[reader.position++]
                          << reader.count;
            reader.count += 8;
        }
        look = (unsigned)(reader.bits & 511u);
        width = widths[look];
        if (width == 0 || reader.count < width) {
            return -1;
        }
        symbol = symbols[look];
        reader.bits >>= width;
        reader.count -= width;
        if (symbol < 256) {
            if (produced >= output_size) {
                return -1;
            }
            output[produced++] = (uint8_t)symbol;
        } else if (symbol == 256) {
            break;
        } else if (symbol <= 285) {
            unsigned index = symbol - 257;
            unsigned extra = 0;
            unsigned distance_bits;
            unsigned distance_symbol;
            unsigned length = length_base[index];
            unsigned distance;
            size_t i;

            if (!read_bits(&reader, length_width[index], &extra)) {
                return -1;
            }
            length += extra;
            if (!read_bits(&reader, 5, &distance_bits)) {
                return -1;
            }
            distance_symbol = reverse_bits(distance_bits, 5);
            if (distance_symbol >= 30) {
                return -1;
            }
            if (!read_bits(&reader, distance_width[distance_symbol], &extra)) {
                return -1;
            }
            distance = distance_base[distance_symbol] + extra;
            if (distance > produced || produced + length > output_size) {
                return -1;
            }
            if (distance >= length) {
                memcpy(output + produced, output + produced - distance, length);
                produced += length;
            } else if (distance == 1) {
                memset(output + produced, output[produced - 1], length);
                produced += length;
            } else {
                for (i = 0; i < length; ++i) {
                    output[produced] = output[produced - distance];
                    ++produced;
                }
            }
        } else {
            return -1;
        }
    }
    if (produced != output_size) {
        return -1;
    }
    {
        uint32_t expected = ((uint32_t)input[input_size - 4] << 24)
                          | ((uint32_t)input[input_size - 3] << 16)
                          | ((uint32_t)input[input_size - 2] << 8)
                          | input[input_size - 1];
        if (adler32_bytes(output, output_size) != expected) {
            return -1;
        }
    }
    return 0;
}

void fortio_shuffle(const int8_t *input, int8_t *output, size_t count,
                    size_t element_size) {
    size_t byte_index;
    size_t element_index;

    for (byte_index = 0; byte_index < element_size; ++byte_index) {
        for (element_index = 0; element_index < count; ++element_index) {
            output[byte_index * count + element_index] =
                input[element_index * element_size + byte_index];
        }
    }
}

void fortio_unshuffle(const int8_t *input, int8_t *output, size_t count,
                      size_t element_size) {
    size_t byte_index;
    size_t element_index;

    for (element_index = 0; element_index < count; ++element_index) {
        for (byte_index = 0; byte_index < element_size; ++byte_index) {
            output[element_index * element_size + byte_index] =
                input[byte_index * count + element_index];
        }
    }
}

void fortio_unshuffle_r64(const int8_t *input, double *output, size_t count) {
    uint8_t *bytes = (uint8_t *)output;
    size_t element_index;

    for (element_index = 0; element_index < count; ++element_index) {
        bytes[8 * element_index] = (uint8_t)input[element_index];
        bytes[8 * element_index + 1] = (uint8_t)input[count + element_index];
        bytes[8 * element_index + 2] = (uint8_t)input[2 * count + element_index];
        bytes[8 * element_index + 3] = (uint8_t)input[3 * count + element_index];
        bytes[8 * element_index + 4] = (uint8_t)input[4 * count + element_index];
        bytes[8 * element_index + 5] = (uint8_t)input[5 * count + element_index];
        bytes[8 * element_index + 6] = (uint8_t)input[6 * count + element_index];
        bytes[8 * element_index + 7] = (uint8_t)input[7 * count + element_index];
    }
}
