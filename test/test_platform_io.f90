program test_platform_io
    use, intrinsic :: iso_c_binding, only: c_associated, c_int, c_loc, c_null_char, &
        c_ptr, c_size_t
    use, intrinsic :: iso_fortran_env, only: int8, int64
    use fortio_posix, only: mapped_close, mapped_copy, mapped_open, posix_close, &
        posix_create_write, posix_open_read, posix_pwrite
    implicit none

    character(len=512) :: path
    integer :: unit
    integer(c_int) :: descriptor, code
    integer(int64) :: transferred
    integer(int8), target :: expected(8), mapped_values(8)
    integer(int8) :: stream_values(8)
    type(c_ptr) :: mapping

    call get_command_argument(1, path)
    if (len_trim(path) == 0) path = "build/platform-io.bin"
    expected = [1_int8, 2_int8, 3_int8, 4_int8, 5_int8, 6_int8, 7_int8, 8_int8]

    descriptor = posix_create_write(trim(path)//c_null_char)
    if (descriptor < 0) error stop "platform create failed"
    transferred = posix_pwrite(descriptor, c_loc(expected), int(size(expected), c_size_t), &
        0_int64)
    if (transferred /= size(expected)) error stop "platform positional write failed"
    code = posix_close(descriptor)
    if (code /= 0) error stop "platform write close failed"

    descriptor = posix_open_read(trim(path)//c_null_char)
    if (descriptor < 0) error stop "platform read open failed"
    mapping = mapped_open(descriptor)
    if (.not. c_associated(mapping)) error stop "platform mapping failed"
    transferred = mapped_copy(mapping, c_loc(mapped_values), &
        int(size(mapped_values), c_size_t), 0_int64)
    if (transferred /= size(mapped_values)) error stop "platform mapped copy failed"
    if (any(mapped_values /= expected)) error stop "platform mapped values differ"
    code = mapped_close(mapping)
    if (code /= 0) error stop "platform unmapping failed"
    code = posix_close(descriptor)
    if (code /= 0) error stop "platform read close failed"

    open(newunit=unit, file=trim(path), access="stream", status="old", action="read")
    read(unit) stream_values
    close(unit)
    if (any(stream_values /= expected)) error stop "Fortran stream oracle differs"
end program test_platform_io
