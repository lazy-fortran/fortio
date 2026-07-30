#define _POSIX_C_SOURCE 200809L

#include <fcntl.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

struct fortio_mapping {
    void *address;
    size_t length;
};

static pthread_mutex_t handle_table_mutex;
static pthread_once_t handle_table_once = PTHREAD_ONCE_INIT;

static void initialize_handle_table_mutex(void)
{
    pthread_mutexattr_t attributes;

    pthread_mutexattr_init(&attributes);
    pthread_mutexattr_settype(&attributes, PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init(&handle_table_mutex, &attributes);
    pthread_mutexattr_destroy(&attributes);
}

void fortio_handle_table_lock(void)
{
    pthread_once(&handle_table_once, initialize_handle_table_mutex);
    pthread_mutex_lock(&handle_table_mutex);
}

void fortio_handle_table_unlock(void)
{
    pthread_mutex_unlock(&handle_table_mutex);
}

int fortio_posix_open_read(const char *path)
{
    return open(path, O_RDONLY);
}

int fortio_posix_open_write(const char *path)
{
    return open(path, O_WRONLY);
}

int fortio_posix_close(int descriptor)
{
    return close(descriptor);
}

int64_t fortio_posix_pread(
    int descriptor, void *buffer, size_t count, int64_t offset)
{
    return (int64_t)pread(descriptor, buffer, count, (off_t)offset);
}

int64_t fortio_posix_pwrite(
    int descriptor, const void *buffer, size_t count, int64_t offset)
{
    return (int64_t)pwrite(descriptor, buffer, count, (off_t)offset);
}

void *fortio_mapped_open(int descriptor)
{
    struct stat information;
    struct fortio_mapping *mapping;

    if (fstat(descriptor, &information) != 0 || information.st_size <= 0)
        return NULL;
    mapping = malloc(sizeof(*mapping));
    if (mapping == NULL)
        return NULL;
    mapping->length = (size_t)information.st_size;
    mapping->address = mmap(
        NULL, mapping->length, PROT_READ, MAP_PRIVATE, descriptor, 0);
    if (mapping->address == MAP_FAILED) {
        free(mapping);
        return NULL;
    }
    return mapping;
}

int64_t fortio_mapped_copy(
    const void *opaque_mapping, void *buffer, size_t count, int64_t offset)
{
    const struct fortio_mapping *mapping = opaque_mapping;

    if (mapping == NULL || offset < 0 ||
        (uint64_t)offset + count > mapping->length)
        return -1;
    memcpy(buffer, (const char *)mapping->address + offset, count);
    return (int64_t)count;
}

int64_t fortio_mapped_copy_swap64(
    const void *opaque_mapping, void *buffer, size_t count, int64_t offset)
{
    const struct fortio_mapping *mapping = opaque_mapping;
    const uint64_t *source;
    uint64_t *destination = buffer;
    size_t index;

    if (mapping == NULL || offset < 0 || count % sizeof(uint64_t) != 0 ||
        (uint64_t)offset + count > mapping->length)
        return -1;
    source = (const uint64_t *)((const char *)mapping->address + offset);
    for (index = 0; index < count / sizeof(uint64_t); ++index)
        destination[index] = __builtin_bswap64(source[index]);
    return (int64_t)count;
}

int fortio_mapped_close(void *opaque_mapping)
{
    struct fortio_mapping *mapping = opaque_mapping;
    int code;

    if (mapping == NULL)
        return 0;
    code = munmap(mapping->address, mapping->length);
    free(mapping);
    return code;
}
