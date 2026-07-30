module fortio_posix
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, c_ptr, c_size_t
    implicit none
    private

    public :: posix_open_read, posix_open_write, posix_create_write, posix_close
    public :: posix_truncate
    public :: posix_path_exists, posix_pread, posix_pwrite, posix_pwrite_swap64
    public :: mapped_open, mapped_copy, mapped_copy_swap64, mapped_close
    public :: handle_table_lock, handle_table_unlock
    public :: write_session_lock, write_session_unlock

    interface
        function posix_open_read(path) bind(C, name="fortio_posix_open_read") result(descriptor)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int) :: descriptor
        end function posix_open_read

        function posix_open_write(path) bind(C, name="fortio_posix_open_write") result(descriptor)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int) :: descriptor
        end function posix_open_write

        function posix_create_write(path) &
                bind(C, name="fortio_posix_create_write") result(descriptor)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int) :: descriptor
        end function posix_create_write

        function posix_path_exists(path) &
                bind(C, name="fortio_posix_path_exists") result(exists)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int) :: exists
        end function posix_path_exists

        function posix_close(descriptor) bind(C, name="fortio_posix_close") result(code)
            import :: c_int
            integer(c_int), value :: descriptor
            integer(c_int) :: code
        end function posix_close

        function posix_truncate(descriptor, length) &
                bind(C, name="fortio_posix_truncate") result(code)
            import :: c_int, c_int64_t
            integer(c_int), value :: descriptor
            integer(c_int64_t), value :: length
            integer(c_int) :: code
        end function posix_truncate

        function posix_pread(descriptor, buffer, count, offset) &
                bind(C, name="fortio_posix_pread") result(bytes_read)
            import :: c_int, c_int64_t, c_ptr, c_size_t
            integer(c_int), value :: descriptor
            type(c_ptr), value :: buffer
            integer(c_size_t), value :: count
            integer(c_int64_t), value :: offset
            integer(c_int64_t) :: bytes_read
        end function posix_pread

        function posix_pwrite(descriptor, buffer, count, offset) &
                bind(C, name="fortio_posix_pwrite") result(bytes_written)
            import :: c_int, c_int64_t, c_ptr, c_size_t
            integer(c_int), value :: descriptor
            type(c_ptr), value :: buffer
            integer(c_size_t), value :: count
            integer(c_int64_t), value :: offset
            integer(c_int64_t) :: bytes_written
        end function posix_pwrite

        function posix_pwrite_swap64(descriptor, buffer, count, offset) &
                bind(C, name="fortio_posix_pwrite_swap64") result(bytes_written)
            import :: c_int, c_int64_t, c_ptr, c_size_t
            integer(c_int), value :: descriptor
            type(c_ptr), value :: buffer
            integer(c_size_t), value :: count
            integer(c_int64_t), value :: offset
            integer(c_int64_t) :: bytes_written
        end function posix_pwrite_swap64

        function mapped_open(descriptor) bind(C, name="fortio_mapped_open") result(mapping)
            import :: c_int, c_ptr
            integer(c_int), value :: descriptor
            type(c_ptr) :: mapping
        end function mapped_open

        function mapped_copy(mapping, buffer, count, offset) &
                bind(C, name="fortio_mapped_copy") result(bytes_copied)
            import :: c_int64_t, c_ptr, c_size_t
            type(c_ptr), value :: mapping, buffer
            integer(c_size_t), value :: count
            integer(c_int64_t), value :: offset
            integer(c_int64_t) :: bytes_copied
        end function mapped_copy

        function mapped_copy_swap64(mapping, buffer, count, offset) &
                bind(C, name="fortio_mapped_copy_swap64") result(bytes_copied)
            import :: c_int64_t, c_ptr, c_size_t
            type(c_ptr), value :: mapping, buffer
            integer(c_size_t), value :: count
            integer(c_int64_t), value :: offset
            integer(c_int64_t) :: bytes_copied
        end function mapped_copy_swap64

        function mapped_close(mapping) bind(C, name="fortio_mapped_close") result(code)
            import :: c_int, c_ptr
            type(c_ptr), value :: mapping
            integer(c_int) :: code
        end function mapped_close

        subroutine handle_table_lock() bind(C, name="fortio_handle_table_lock")
        end subroutine handle_table_lock

        subroutine handle_table_unlock() bind(C, name="fortio_handle_table_unlock")
        end subroutine handle_table_unlock

        function write_session_lock(path) &
                bind(C, name="fortio_write_session_lock") result(token)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int) :: token
        end function write_session_lock

        subroutine write_session_unlock(token) bind(C, name="fortio_write_session_unlock")
            import :: c_int
            integer(c_int), value :: token
        end subroutine write_session_unlock
    end interface
end module fortio_posix
