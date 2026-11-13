#define _POSIX_C_SOURCE 200809L
#include <stdint.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef FORTIO_PWRITE_COUNTER_TEST
ssize_t __real_pwrite(int, const void *, size_t, off_t);

static int64_t pwrite_calls;

ssize_t __wrap_pwrite(int fd, const void *data, size_t count, off_t offset)
{
    ++pwrite_calls;
    return __real_pwrite(fd, data, count, offset);
}

int64_t fortio_test_pwrite_calls(void)
{
    return pwrite_calls;
}
#else
/* Builds without the GNU linker wrapper (FPM, other platforms) cannot count. */
int64_t fortio_test_pwrite_calls(void)
{
    return -1;
}
#endif
