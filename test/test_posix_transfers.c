#ifdef FORTIO_POSIX_TRANSFER_WRAP_TEST
#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int64_t fortio_posix_pread(int, void *, size_t, int64_t);
int64_t fortio_posix_pwrite(int, const void *, size_t, int64_t);
ssize_t __real_pread(int, void *, size_t, off_t);
ssize_t __real_pwrite(int, const void *, size_t, off_t);

static int inject, calls, stop_after;

static int interrupt(void)
{
    ++calls;
    if (calls == 1) {
        errno = EINTR;
        return 1;
    }
    return 0;
}

ssize_t __wrap_pread(int fd, void *data, size_t count, off_t offset)
{
    if (inject) {
        if (interrupt()) return -1;
        if (stop_after && calls > stop_after) return 0;
        if (count > 13) count = 13;
    }
    return __real_pread(fd, data, count, offset);
}

ssize_t __wrap_pwrite(int fd, const void *data, size_t count, off_t offset)
{
    if (inject) {
        if (interrupt()) return -1;
        if (stop_after && calls > stop_after) return 0;
        if (count > 13) count = 13;
    }
    return __real_pwrite(fd, data, count, offset);
}

static void check(int condition, const char *message)
{
    if (!condition) {
        fprintf(stderr, "%s\n", message);
        exit(1);
    }
}

int main(void)
{
    char path[] = "fortio-transfers-XXXXXX";
    unsigned char expected[257], actual[257];
    int fd = mkstemp(path);
    check(fd >= 0, "mkstemp failed");
    unlink(path);
    for (size_t k = 0; k < sizeof(expected); ++k)
        expected[k] = (unsigned char)(k * 17U);

    inject = 1;
    check(fortio_posix_pwrite(fd, expected, sizeof(expected), 29) == 257,
        "short/interrupted writes must complete");
    inject = 0;
    check(__real_pread(fd, actual, sizeof(actual), 29) == 257 &&
        memcmp(actual, expected, sizeof(actual)) == 0, "written data/offset changed");
    inject = 1;
    calls = 0;
    memset(actual, 0, sizeof(actual));
    check(fortio_posix_pread(fd, actual, sizeof(actual), 29) == 257 &&
        memcmp(actual, expected, sizeof(actual)) == 0,
        "short/interrupted reads must preserve data");
    inject = 0;
    check(fortio_posix_pread(fd, actual, sizeof(actual), 280) == 6,
        "EOF must return the available bytes");
    inject = 1;
    stop_after = 2;
    calls = 0;
    check(fortio_posix_pwrite(fd, expected, sizeof(expected), 0) == 13,
        "zero-progress write must return without hanging");
    inject = 0;
    check(fortio_posix_pwrite(-1, expected, 1, 0) == -1,
        "invalid descriptor must fail");
    check(fortio_posix_pread(fd, actual, 2, INT64_MAX) == -1,
        "offset overflow must fail");
    close(fd);

    /* /dev/null does not touch this virtually allocated >2 GiB buffer. */
    size_t large = (size_t)INT32_MAX + 65537U;
    void *buffer = malloc(large);
    check(buffer != NULL, "large virtual buffer allocation failed");
    fd = open("/dev/null", O_WRONLY);
    check(fd >= 0, "open /dev/null failed");
    check(fortio_posix_pwrite(fd, buffer, large, 0) == (int64_t)large,
        "Linux >2 GiB write must complete");
    close(fd);
    free(buffer);
    puts("short transfers, EINTR, EOF, no-progress and >2 GiB write: PASS");
    return 0;
}
#endif
