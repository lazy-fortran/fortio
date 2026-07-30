program test_thread_safety
    use, intrinsic :: iso_fortran_env, only: real64
    use hdf5_tools, only: HID_T, h5_close, h5_get, h5_open
    use netcdf, only: nf90_close, nf90_get_var, nf90_inq_varid, nf90_noerr, &
        nf90_nowrite, nf90_open, nf90_strerror
    implicit none

    integer, parameter :: repetitions = 200
    character(len=1024) :: hdf5_path, hdf5_generator, netcdf_path, netcdf_cdl
    character(len=4096) :: command
    integer :: command_status, failure_count, iteration

    call get_command_argument(1, hdf5_path)
    call get_command_argument(2, hdf5_generator)
    call get_command_argument(3, netcdf_path)
    call get_command_argument(4, netcdf_cdl)
    if (len_trim(hdf5_path) == 0) hdf5_path = "build/thread-oracle.h5"
    if (len_trim(hdf5_generator) == 0) &
        hdf5_generator = "test/fixtures/make_hdf5_oracle.py"
    if (len_trim(netcdf_path) == 0) netcdf_path = "build/thread-oracle.nc"
    if (len_trim(netcdf_cdl) == 0) netcdf_cdl = "test/fixtures/oracle.cdl"
    command = "python3 "//trim(hdf5_generator)//" "//trim(hdf5_path)
    call execute_command_line(trim(command), exitstat=command_status)
    if (command_status /= 0) error stop "HDF5 oracle generation failed"
    command = "ncgen -o "//trim(netcdf_path)//" "//trim(netcdf_cdl)
    call execute_command_line(trim(command), exitstat=command_status)
    if (command_status /= 0) error stop "NetCDF oracle generation failed"

    failure_count = 0
    !$omp parallel do default(none) shared(failure_count, hdf5_path, netcdf_path)
    do iteration = 1, repetitions
        call verify_hdf5(trim(hdf5_path), failure_count)
        call verify_netcdf(trim(netcdf_path), failure_count)
        call verify_netcdf_error(failure_count)
    end do
    !$omp end parallel do
    if (failure_count /= 0) error stop "concurrent oracle reads failed"

contains

    subroutine verify_hdf5(path, failures)
        character(len=*), intent(in) :: path
        integer, intent(inout) :: failures
        integer(HID_T) :: file_id
        real(real64) :: matrix(3, 2)

        call h5_open(path, file_id)
        call h5_get(file_id, "grid/matrix", matrix)
        call h5_close(file_id)
        if (any(matrix /= reshape([1, 2, 3, 4, 5, 6], shape(matrix)))) then
            !$omp atomic update
            failures = failures + 1
        end if
    end subroutine verify_hdf5

    subroutine verify_netcdf(path, failures)
        character(len=*), intent(in) :: path
        integer, intent(inout) :: failures
        integer :: file_id, status, variable_id
        real(real64) :: matrix(3, 2)

        status = nf90_open(path, nf90_nowrite, file_id)
        if (status == nf90_noerr) status = nf90_inq_varid(file_id, "matrix", variable_id)
        if (status == nf90_noerr) status = nf90_get_var(file_id, variable_id, matrix)
        if (status == nf90_noerr) status = nf90_close(file_id)
        if (status /= nf90_noerr) then
            !$omp atomic update
            failures = failures + 1
            return
        end if
        if (any(matrix /= reshape([1, 2, 3, 4, 5, 6], shape(matrix)))) then
            !$omp atomic update
            failures = failures + 1
        end if
    end subroutine verify_netcdf

    subroutine verify_netcdf_error(failures)
        integer, intent(inout) :: failures
        character(len=512) :: message
        integer :: file_id, status

        status = nf90_open("fortio-intentionally-missing.nc", nf90_nowrite, file_id)
        message = nf90_strerror(status)
        if (status == nf90_noerr) then
            !$omp atomic update
            failures = failures + 1
            return
        end if
        if (len_trim(message) == 0) then
            !$omp atomic update
            failures = failures + 1
        end if
    end subroutine verify_netcdf_error

end program test_thread_safety
