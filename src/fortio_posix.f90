module fortio_posix
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, c_ptr, c_size_t
    implicit none
    private

    public :: posix_open_read, posix_open_write, posix_close, posix_pread, posix_pwrite
    public :: mapped_open, mapped_copy, mapped_copy_swap64, mapped_close
    public :: handle_table_lock, handle_table_unlock

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

        function posix_close(descriptor) bind(C, name="fortio_posix_close") result(code)
            import :: c_int
            integer(c_int), value :: descriptor
            integer(c_int) :: code
        end function posix_close

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
    end interface
end module fortio_posix
