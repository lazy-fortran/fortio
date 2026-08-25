#define _POSIX_C_SOURCE 200809L

#include <fcntl.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#ifdef _WIN32
#include <io.h>
#include <windows.h>
#else
#include <pthread.h>
#include <sys/mman.h>
#include <unistd.h>
#endif

struct fortio_mapping {
    void *address;
    size_t length;
#ifdef _WIN32
    HANDLE file_mapping;
#endif
};

#ifdef _WIN32
static CRITICAL_SECTION handle_table_mutex;
static INIT_ONCE handle_table_once = INIT_ONCE_STATIC_INIT;
#define WRITE_LOCK_COUNT 256
static CRITICAL_SECTION write_session_mutexes[WRITE_LOCK_COUNT];
static INIT_ONCE write_session_once = INIT_ONCE_STATIC_INIT;

static BOOL CALLBACK initialize_handle_table_mutex(
    PINIT_ONCE once, PVOID parameter, PVOID *context)
{
    (void)once;
    (void)parameter;
    (void)context;
    InitializeCriticalSection(&handle_table_mutex);
    return TRUE;
}

static BOOL CALLBACK initialize_write_session_mutex(
    PINIT_ONCE once, PVOID parameter, PVOID *context)
{
    size_t index;

    (void)once;
    (void)parameter;
    (void)context;
    for (index = 0; index < WRITE_LOCK_COUNT; ++index)
        InitializeCriticalSection(&write_session_mutexes[index]);
    return TRUE;
}
#else
static pthread_mutex_t handle_table_mutex;
static pthread_once_t handle_table_once = PTHREAD_ONCE_INIT;
#define WRITE_LOCK_COUNT 256
static pthread_mutex_t write_session_mutexes[WRITE_LOCK_COUNT];
static pthread_once_t write_session_once = PTHREAD_ONCE_INIT;

static void initialize_handle_table_mutex(void)
{
    pthread_mutexattr_t attributes;

    pthread_mutexattr_init(&attributes);
    pthread_mutexattr_settype(&attributes, PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init(&handle_table_mutex, &attributes);
    pthread_mutexattr_destroy(&attributes);
}

static void initialize_write_session_mutex(void)
{
    size_t index;

    for (index = 0; index < WRITE_LOCK_COUNT; ++index)
        pthread_mutex_init(&write_session_mutexes[index], NULL);
}
#endif

void fortio_handle_table_lock(void)
{
#ifdef _WIN32
    InitOnceExecuteOnce(
        &handle_table_once, initialize_handle_table_mutex, NULL, NULL);
    EnterCriticalSection(&handle_table_mutex);
#else
    pthread_once(&handle_table_once, initialize_handle_table_mutex);
    pthread_mutex_lock(&handle_table_mutex);
#endif
}

void fortio_handle_table_unlock(void)
{
#ifdef _WIN32
    LeaveCriticalSection(&handle_table_mutex);
#else
    pthread_mutex_unlock(&handle_table_mutex);
#endif
}

int fortio_write_session_lock(const char *path)
{
    const unsigned char *cursor = (const unsigned char *)path;
    uint64_t hash = UINT64_C(1469598103934665603);
    int token;

    while (*cursor != '\0') {
        hash ^= *cursor++;
        hash *= UINT64_C(1099511628211);
    }
    token = (int)(hash % WRITE_LOCK_COUNT);
#ifdef _WIN32
    InitOnceExecuteOnce(
        &write_session_once, initialize_write_session_mutex, NULL, NULL);
    EnterCriticalSection(&write_session_mutexes[token]);
#else
    pthread_once(&write_session_once, initialize_write_session_mutex);
    pthread_mutex_lock(&write_session_mutexes[token]);
#endif
    return token;
}

void fortio_write_session_unlock(int token)
{
#ifdef _WIN32
    LeaveCriticalSection(&write_session_mutexes[token]);
#else
    pthread_mutex_unlock(&write_session_mutexes[token]);
#endif
}

int fortio_posix_open_read(const char *path)
{
#ifdef _WIN32
    HANDLE handle = CreateFileA(
        path, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
        NULL, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL | FILE_FLAG_RANDOM_ACCESS | FILE_FLAG_OVERLAPPED, NULL);
    int descriptor;

    if (handle == INVALID_HANDLE_VALUE)
        return -1;
    descriptor = _open_osfhandle((intptr_t)handle, _O_RDONLY | _O_BINARY);
    if (descriptor < 0)
        CloseHandle(handle);
    return descriptor;
#else
    return open(path, O_RDONLY);
#endif
}

int fortio_posix_open_write(const char *path)
{
#ifdef _WIN32
    HANDLE handle = CreateFileA(
        path, GENERIC_WRITE, FILE_SHARE_READ, NULL, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL | FILE_FLAG_RANDOM_ACCESS | FILE_FLAG_OVERLAPPED, NULL);
    int descriptor;

    if (handle == INVALID_HANDLE_VALUE)
        return -1;
    descriptor = _open_osfhandle((intptr_t)handle, _O_WRONLY | _O_BINARY);
    if (descriptor < 0)
        CloseHandle(handle);
    return descriptor;
#else
    return open(path, O_WRONLY);
#endif
}

int fortio_posix_create_write(const char *path)
{
#ifdef _WIN32
    HANDLE handle = CreateFileA(
        path, GENERIC_WRITE, FILE_SHARE_READ, NULL, CREATE_ALWAYS,
        FILE_ATTRIBUTE_NORMAL | FILE_FLAG_RANDOM_ACCESS | FILE_FLAG_OVERLAPPED, NULL);
    int descriptor;

    if (handle == INVALID_HANDLE_VALUE)
        return -1;
    descriptor = _open_osfhandle((intptr_t)handle, _O_WRONLY | _O_BINARY);
    if (descriptor < 0)
        CloseHandle(handle);
    return descriptor;
#else
    return open(path, O_WRONLY | O_CREAT | O_TRUNC, 0666);
#endif
}

int fortio_posix_path_exists(const char *path)
{
#ifdef _WIN32
    return _access(path, 0) == 0;
#else
    return access(path, F_OK) == 0;
#endif
}

int fortio_posix_close(int descriptor)
{
#ifdef _WIN32
    return _close(descriptor);
#else
    return close(descriptor);
#endif
}

int fortio_posix_sync(int descriptor)
{
#ifdef _WIN32
    return _commit(descriptor) == 0 ? 0 : -1;
#else
    return fsync(descriptor);
#endif
}

int fortio_posix_truncate(int descriptor, int64_t length)
{
    if (length < 0)
        return -1;
#ifdef _WIN32
    return _chsize_s(descriptor, (__int64)length) == 0 ? 0 : -1;
#else
    return ftruncate(descriptor, (off_t)length);
#endif
}

int64_t fortio_posix_pread(
    int descriptor, void *buffer, size_t count, int64_t offset)
{
#ifdef _WIN32
    HANDLE handle = (HANDLE)_get_osfhandle(descriptor);
    OVERLAPPED overlapped = {0};
    DWORD transferred;

    if (handle == INVALID_HANDLE_VALUE || count > UINT32_MAX || offset < 0)
        return -1;
    overlapped.Offset = (DWORD)(uint64_t)offset;
    overlapped.OffsetHigh = (DWORD)((uint64_t)offset >> 32);
    if (!ReadFile(handle, buffer, (DWORD)count, &transferred, &overlapped)) {
        if (GetLastError() != ERROR_IO_PENDING ||
            !GetOverlappedResult(handle, &overlapped, &transferred, TRUE))
            return -1;
    }
    return (int64_t)transferred;
#else
    return (int64_t)pread(descriptor, buffer, count, (off_t)offset);
#endif
}

int64_t fortio_posix_pwrite(
    int descriptor, const void *buffer, size_t count, int64_t offset)
{
#ifdef _WIN32
    HANDLE handle = (HANDLE)_get_osfhandle(descriptor);
    OVERLAPPED overlapped = {0};
    DWORD transferred;

    if (handle == INVALID_HANDLE_VALUE || count > UINT32_MAX || offset < 0)
        return -1;
    overlapped.Offset = (DWORD)(uint64_t)offset;
    overlapped.OffsetHigh = (DWORD)((uint64_t)offset >> 32);
    if (!WriteFile(handle, buffer, (DWORD)count, &transferred, &overlapped)) {
        if (GetLastError() != ERROR_IO_PENDING ||
            !GetOverlappedResult(handle, &overlapped, &transferred, TRUE))
            return -1;
    }
    return (int64_t)transferred;
#else
    return (int64_t)pwrite(descriptor, buffer, count, (off_t)offset);
#endif
}

static uint64_t swap_uint64(uint64_t value)
{
    return ((value & UINT64_C(0x00000000000000ff)) << 56) |
           ((value & UINT64_C(0x000000000000ff00)) << 40) |
           ((value & UINT64_C(0x0000000000ff0000)) << 24) |
           ((value & UINT64_C(0x00000000ff000000)) << 8) |
           ((value & UINT64_C(0x000000ff00000000)) >> 8) |
           ((value & UINT64_C(0x0000ff0000000000)) >> 24) |
           ((value & UINT64_C(0x00ff000000000000)) >> 40) |
           ((value & UINT64_C(0xff00000000000000)) >> 56);
}

int64_t fortio_posix_pwrite_swap64(
    int descriptor, const void *buffer, size_t count, int64_t offset)
{
    const uint64_t *source = buffer;
    uint64_t *converted;
    size_t index;
    int64_t written;

    if (count % sizeof(uint64_t) != 0)
        return -1;
    converted = malloc(count);
    if (converted == NULL)
        return -1;
    for (index = 0; index < count / sizeof(uint64_t); ++index)
        converted[index] = swap_uint64(source[index]);
    written = fortio_posix_pwrite(descriptor, converted, count, offset);
    free(converted);
    return (int64_t)written;
}

void *fortio_mapped_open(int descriptor)
{
#ifdef _WIN32
    HANDLE file = (HANDLE)_get_osfhandle(descriptor);
    LARGE_INTEGER information;
    struct fortio_mapping *mapping;

    if (file == INVALID_HANDLE_VALUE ||
        !GetFileSizeEx(file, &information) || information.QuadPart <= 0)
        return NULL;
    if ((uint64_t)information.QuadPart > SIZE_MAX)
        return NULL;
    mapping = malloc(sizeof(*mapping));
    if (mapping == NULL)
        return NULL;
    mapping->length = (size_t)information.QuadPart;
    mapping->file_mapping =
        CreateFileMapping(file, NULL, PAGE_READONLY, 0, 0, NULL);
    if (mapping->file_mapping == NULL) {
        free(mapping);
        return NULL;
    }
    mapping->address =
        MapViewOfFile(mapping->file_mapping, FILE_MAP_READ, 0, 0, 0);
    if (mapping->address == NULL) {
        CloseHandle(mapping->file_mapping);
        free(mapping);
        return NULL;
    }
    return mapping;
#else
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
#endif
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
        destination[index] = swap_uint64(source[index]);
    return (int64_t)count;
}

int fortio_mapped_close(void *opaque_mapping)
{
    struct fortio_mapping *mapping = opaque_mapping;
    int code;

    if (mapping == NULL)
        return 0;
#ifdef _WIN32
    code = UnmapViewOfFile(mapping->address) ? 0 : -1;
    if (!CloseHandle(mapping->file_mapping))
        code = -1;
#else
    code = munmap(mapping->address, mapping->length);
#endif
    free(mapping);
    return code;
}
