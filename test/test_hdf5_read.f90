program test_hdf5_read
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use fortio, only: fortio_file_t, fortio_status_t
    implicit none

    type(fortio_file_t) :: file
    type(fortio_status_t) :: status
    character(len=1024) :: fixture, generator, command
    integer(int32) :: scalar
    real(real64), allocatable :: x(:), matrix(:, :)
    integer :: command_status
    logical :: fixture_exists

    call get_command_argument(1, fixture)
    if (len_trim(fixture) == 0) fixture = "build/oracle.h5"
    call get_command_argument(2, generator)
    if (len_trim(generator) == 0) generator = "test/fixtures/make_hdf5_oracle.py"
    inquire (file=trim(fixture), exist=fixture_exists)
    if (.not. fixture_exists) then
        command = "python3 "//trim(generator)//" "//trim(fixture)
        call execute_command_line(trim(command), exitstat=command_status)
        if (command_status /= 0) error stop "HDF5 oracle generation failed"
    end if

    call file%open(trim(fixture), status)
    if (.not. status%ok()) error stop status%message

    call file%read("/grid/Nt", scalar, status)
    if (.not. status%ok()) error stop status%message
    if (scalar /= 42) error stop "HDF5 scalar differs from oracle"

    call file%read("/grid/x_values", x, status)
    if (.not. status%ok()) error stop status%message
    if (size(x) /= 3) error stop "HDF5 vector shape differs from oracle"
    if (any(abs(x - [1.25_real64, -2.5_real64, 4.75_real64]) > 1.0e-6_real64)) &
        error stop "HDF5 vector values differ from oracle"

    call file%read("/grid/matrix", matrix, status)
    if (.not. status%ok()) error stop status%message
    if (any(shape(matrix) /= [3, 2])) error stop "HDF5 matrix shape differs from oracle"
    if (any(abs(matrix - reshape([1, 2, 3, 4, 5, 6], [3, 2])) > 1.0e-12_real64)) &
        error stop "HDF5 matrix values differ from oracle"

    call file%close(status)
    if (.not. status%ok()) error stop status%message
end program test_hdf5_read
