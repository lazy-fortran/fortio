program hdf5_append_native
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use hdf5
    implicit none

    integer, parameter :: fields = 6, steps = 1024
    character(len=512) :: path
    integer(HID_T) :: dataset_id, file_id, file_space_id, memory_space_id
    integer(HID_T) :: property_id
    integer(HSIZE_T) :: chunk(2), count(2), dimensions(2), maximum(2), start(2)
    integer(int64) :: tick_begin, tick_end, tick_rate
    integer :: error, field, step
    real(real64) :: elapsed, checksum, record(fields, 1), received(fields, steps)

    call get_command_argument(1, path)
    if (len_trim(path) == 0) path = "hdf5-append-native.h5"

    call system_clock(tick_begin, tick_rate)
    do step = 1, steps
        call h5open_f(error)
        call require_success(error, "initialize")
        if (step == 1) then
            call h5fcreate_f(trim(path), H5F_ACC_TRUNC_F, file_id, error)
            call require_success(error, "create file")
            dimensions = [int(fields, HSIZE_T), 0_HSIZE_T]
            maximum = [int(fields, HSIZE_T), H5S_UNLIMITED_F]
            call h5screate_simple_f(2, dimensions, file_space_id, error, maximum)
            call require_success(error, "create unlimited dataspace")
            call h5pcreate_f(H5P_DATASET_CREATE_F, property_id, error)
            call require_success(error, "create dataset properties")
            chunk = [int(fields, HSIZE_T), 1_HSIZE_T]
            call h5pset_chunk_f(property_id, 2, chunk, error)
            call require_success(error, "set chunk shape")
            call h5dcreate_f(file_id, "timstep_evol.dat", H5T_NATIVE_DOUBLE, &
                file_space_id, dataset_id, error, property_id)
            call require_success(error, "create dataset")
            call h5pclose_f(property_id, error)
            call h5sclose_f(file_space_id, error)
        else
            call h5fopen_f(trim(path), H5F_ACC_RDWR_F, file_id, error)
            call require_success(error, "open file")
            call h5dopen_f(file_id, "timstep_evol.dat", dataset_id, error)
            call require_success(error, "open dataset")
        end if

        dimensions = [int(fields, HSIZE_T), int(step, HSIZE_T)]
        call h5dset_extent_f(dataset_id, dimensions, error)
        call require_success(error, "extend dataset")
        call h5dget_space_f(dataset_id, file_space_id, error)
        call require_success(error, "get file dataspace")
        start = [0_HSIZE_T, int(step - 1, HSIZE_T)]
        count = [int(fields, HSIZE_T), 1_HSIZE_T]
        call h5sselect_hyperslab_f(file_space_id, H5S_SELECT_SET_F, start, count, error)
        call require_success(error, "select appended record")
        call h5screate_simple_f(2, count, memory_space_id, error)
        call require_success(error, "create memory dataspace")
        do field = 1, fields
            record(field, 1) = real(1000*step + field, real64)
        end do
        call h5dwrite_f(dataset_id, H5T_NATIVE_DOUBLE, record, count, error, &
            memory_space_id, file_space_id)
        call require_success(error, "write appended record")
        call h5sclose_f(memory_space_id, error)
        call h5sclose_f(file_space_id, error)
        call h5dclose_f(dataset_id, error)
        call h5fclose_f(file_id, error)
        call h5close_f(error)
    end do

    call h5open_f(error)
    call h5fopen_f(trim(path), H5F_ACC_RDONLY_F, file_id, error)
    call require_success(error, "open completed file")
    call h5dopen_f(file_id, "timstep_evol.dat", dataset_id, error)
    call require_success(error, "open completed dataset")
    dimensions = [int(fields, HSIZE_T), int(steps, HSIZE_T)]
    call h5dread_f(dataset_id, H5T_NATIVE_DOUBLE, received, dimensions, error)
    call require_success(error, "read completed dataset")
    call h5dclose_f(dataset_id, error)
    call h5fclose_f(file_id, error)
    call h5close_f(error)
    call system_clock(tick_end)

    checksum = sum(received)
    if (checksum /= expected_checksum()) error stop "appended values differ"
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

    pure real(real64) function expected_checksum()
        expected_checksum = real(int(fields, int64)*1000_int64*steps*(steps + 1)/2 + &
            int(steps, int64)*fields*(fields + 1)/2, real64)
    end function expected_checksum

end program hdf5_append_native
