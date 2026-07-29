program test_classic_read
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use fortio, only: fortio_file_t, fortio_status_t
    implicit none

    type(fortio_file_t) :: file
    type(fortio_status_t) :: status
    character(len=1024) :: fixture, cdl, command
    integer(int32) :: scalar
    real(real64), allocatable :: x(:), matrix(:, :)
    integer :: command_status
    logical :: fixture_exists

    call get_command_argument(1, fixture)
    if (len_trim(fixture) == 0) then
        fixture = "build/oracle.nc"
    end if
    call get_command_argument(2, cdl)
    if (len_trim(cdl) == 0) cdl = "test/fixtures/oracle.cdl"
    inquire (file=trim(fixture), exist=fixture_exists)
    if (.not. fixture_exists) then
        command = "ncgen -k classic -o "//trim(fixture)//" "//trim(cdl)
        call execute_command_line(trim(command), exitstat=command_status)
        if (command_status /= 0) error stop "ncgen oracle generation failed"
    end if

    call file%open(trim(fixture), status)
    if (.not. status%ok()) error stop status%message

    call file%read("scalar", scalar, status)
    if (.not. status%ok()) error stop status%message
    if (scalar /= 42) error stop "scalar differs from oracle"

    call file%read("x_values", x, status)
    if (.not. status%ok()) error stop status%message
    if (size(x) /= 3) error stop "x shape differs from oracle"
    if (any(abs(x - [1.25_real64, -2.5_real64, 4.75_real64]) > 1.0e-12_real64)) &
        error stop "x values differ from oracle"

    call file%read("matrix", matrix, status)
    if (.not. status%ok()) error stop status%message
    if (any(shape(matrix) /= [3, 2])) error stop "matrix shape differs from oracle"
    if (any(abs(matrix - reshape([1, 2, 3, 4, 5, 6], [3, 2])) > 1.0e-12_real64)) &
        error stop "matrix values differ from oracle"

    call file%close(status)
    if (.not. status%ok()) error stop status%message
end program test_classic_read
