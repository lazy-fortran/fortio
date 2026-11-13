program test_hdf5_close_writes
    ! Closing a file with large 32-bit datasets must not issue one write per value.
    use, intrinsic :: iso_c_binding, only: c_int64_t
    use, intrinsic :: iso_fortran_env, only: int64, real32, real64
    use hdf5_tools, only: HID_T, h5_add, h5_add_float_1, h5_close, h5_create, h5_get, &
        h5_init, h5_deinit, h5_open
    implicit none

    interface
        integer(c_int64_t) function pwrite_calls() bind(C, name="fortio_test_pwrite_calls")
            import :: c_int64_t
        end function pwrite_calls
    end interface

    integer, parameter :: n = 4096
    integer(HID_T) :: file_id
    integer :: i, ints(n), ints_read(n)
    real(real32) :: floats(n)
    real(real64) :: floats_read(n)
    integer(int64) :: before, used
    character(len=512) :: path

    call get_command_argument(1, path)
    if (len_trim(path) == 0) path = "hdf5-close-writes.h5"
    ints = [(3*i - 7, i = 1, n)]
    floats = [(real(i, real32)/8.0_real32, i = 1, n)]

    call h5_init()
    call h5_create(trim(path), file_id)
    call h5_add(file_id, "ints", ints, [1], [n])
    call h5_add_float_1(file_id, "floats", floats, [1], [n])
    before = pwrite_calls()
    call h5_close(file_id)
    if (before >= 0) then
        used = pwrite_calls() - before
        print '(a,i0)', "pwrite calls during close: ", used
        if (used >= n) error stop "h5_close wrote 32-bit datasets one value at a time"
    else
        print '(a)', "pwrite count unavailable without the linker wrapper"
    end if

    call h5_open(trim(path), file_id)
    call h5_get(file_id, "ints", ints_read)
    call h5_get(file_id, "floats", floats_read)
    call h5_close(file_id)
    call h5_deinit()
    if (any(ints_read /= ints)) error stop "int32 dataset changed on disk"
    if (any(floats_read /= real(floats, real64))) error stop "float32 dataset changed on disk"
end program test_hdf5_close_writes
