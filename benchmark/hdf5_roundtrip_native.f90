program hdf5_roundtrip_native
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use hdf5
    implicit none

    integer, parameter :: nx = 2048, ny = 2048, repetitions = 100
    character(len=512) :: path
    integer(HID_T) :: file_id, dataspace_id, dataset_id
    integer(HSIZE_T) :: dimensions(2)
    integer(int64) :: tick_begin, tick_end, tick_rate
    integer :: error, index, repetition
    real(real64) :: elapsed, checksum
    real(real64), allocatable :: values(:, :), received(:, :)

    call get_command_argument(1, path)
    if (len_trim(path) == 0) path = "hdf5-roundtrip-native.h5"
    allocate(values(nx, ny), received(nx, ny))
    values = reshape([(real(index, real64), index = 1, size(values))], shape(values))
    dimensions = [nx, ny]

    call system_clock(tick_begin, tick_rate)
    call h5open_f(error)
    call require_success(error, "initialize")
    call h5fcreate_f(trim(path), H5F_ACC_TRUNC_F, file_id, error)
    call require_success(error, "create")
    call h5screate_simple_f(2, dimensions, dataspace_id, error)
    call require_success(error, "create dataspace")
    call h5dcreate_f(file_id, "values", H5T_NATIVE_DOUBLE, dataspace_id, dataset_id, error)
    call require_success(error, "create dataset")
    call h5dwrite_f(dataset_id, H5T_NATIVE_DOUBLE, values, dimensions, error)
    call require_success(error, "write values")
    call h5dclose_f(dataset_id, error)
    call h5sclose_f(dataspace_id, error)
    call h5fclose_f(file_id, error)

    call h5fopen_f(trim(path), H5F_ACC_RDONLY_F, file_id, error)
    call require_success(error, "open")
    call h5dopen_f(file_id, "values", dataset_id, error)
    call require_success(error, "open dataset")
    do repetition = 1, repetitions
        call h5dread_f(dataset_id, H5T_NATIVE_DOUBLE, received, dimensions, error)
        call require_success(error, "read values")
    end do
    call h5dclose_f(dataset_id, error)
    call h5fclose_f(file_id, error)
    call h5close_f(error)
    call system_clock(tick_end)

    checksum = sum(received)
    if (checksum /= real(size(values), real64)*real(size(values) + 1, real64)/2.0_real64) &
        error stop "round-trip values differ"
    elapsed = real(tick_end - tick_begin, real64)/real(tick_rate, real64)
    write (*, '(a,es24.16,1x,a,es24.16)') "seconds=", elapsed, "checksum=", checksum

contains

    subroutine require_success(code, operation)
        integer, intent(in) :: code
        character(len=*), intent(in) :: operation

        if (code < 0) then
            write (*, '(a,1x,i0)') trim(operation)//" failed:", code
            error stop
        end if
    end subroutine require_success

end program hdf5_roundtrip_native
