program test_hdf5_write_oracle
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use fortio, only: fortio_file_t, fortio_status_t
    use fortio_hdf5_writer, only: hdf5_writer_t
    implicit none

    type(hdf5_writer_t) :: writer
    type(fortio_file_t) :: reader
    type(fortio_status_t) :: status
    character(len=1024) :: path, dump_path, command, line
    integer(int32) :: scalar
    real(real64), allocatable :: vector(:), matrix(:, :)
    real(real64) :: matrix_input(3, 2)
    integer :: command_status, unit, io_status
    logical :: found_scalar, found_vector, found_matrix

    call get_command_argument(1, path)
    if (len_trim(path) == 0) path = "build/fortio-written.h5"
    dump_path = trim(path)//".dump"
    matrix_input = reshape([1, 2, 3, 4, 5, 6], [3, 2])

    call writer%create(trim(path), status)
    if (.not. status%ok()) error stop status%message
    call writer%add_i32_scalar("scalar", 42_int32, status)
    if (.not. status%ok()) error stop status%message
    call writer%add_r64_1("vector", [1.25_real64, -2.5_real64, 4.75_real64], status)
    if (.not. status%ok()) error stop status%message
    call writer%add_r64_2("matrix", matrix_input, status)
    if (.not. status%ok()) error stop status%message
    call writer%close(status)
    if (.not. status%ok()) error stop status%message

    command = "h5dump -d scalar -d vector -d matrix "//trim(path)//" > "//trim(dump_path)
    call execute_command_line(trim(command), exitstat=command_status)
    if (command_status /= 0) error stop "system HDF5 rejected fortio output"
    open(newunit=unit, file=trim(dump_path), status="old", action="read")
    found_scalar = .false.
    found_vector = .false.
    found_matrix = .false.
    do
        read(unit, '(A)', iostat=io_status) line
        if (io_status /= 0) exit
        if (index(line, "(0): 42") > 0) found_scalar = .true.
        if (index(line, "1.25, -2.5, 4.75") > 0) found_vector = .true.
        if (index(line, "(0,0): 1, 2, 3") > 0) found_matrix = .true.
    end do
    close(unit)
    if (.not. found_scalar) error stop "h5dump scalar differs"
    if (.not. found_vector) error stop "h5dump vector differs"
    if (.not. found_matrix) error stop "h5dump matrix differs"

    call reader%open(trim(path), status)
    if (.not. status%ok()) error stop status%message
    call reader%read("scalar", scalar, status)
    if (.not. status%ok() .or. scalar /= 42) error stop "fortio scalar round trip failed"
    call reader%read("vector", vector, status)
    if (.not. status%ok()) error stop status%message
    if (any(abs(vector - [1.25_real64, -2.5_real64, 4.75_real64]) > 1.0e-12_real64)) &
        error stop "fortio vector round trip failed"
    call reader%read("matrix", matrix, status)
    if (.not. status%ok()) error stop status%message
    if (any(abs(matrix - matrix_input) > 1.0e-12_real64)) &
        error stop "fortio matrix round trip failed"
    call reader%close(status)
end program test_hdf5_write_oracle
