// SPDX-License-Identifier: MIT
#include <stddef.h>
#include <stdint.h>
#include <string.h>

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
