program test_hdf5_tools_read
    use, intrinsic :: iso_fortran_env, only: real64
    use hdf5_tools, only: HID_T, h5_init, h5_deinit, h5_open, h5_close, h5_get, &
                          h5_get_bounds
    implicit none

    integer(HID_T) :: file_id
    character(len=1024) :: fixture, generator, command
    character(len=32) :: label
    integer :: scalar, command_status, lb1, lb2, ub1, ub2
    real(real64) :: x(3), matrix(3, 2)

    call get_command_argument(1, fixture)
    if (len_trim(fixture) == 0) fixture = "build/hdf5-tools-oracle.h5"
    call get_command_argument(2, generator)
    if (len_trim(generator) == 0) generator = "test/fixtures/make_hdf5_oracle.py"
    command = "python3 "//trim(generator)//" "//trim(fixture)
    call execute_command_line(trim(command), exitstat=command_status)
    if (command_status /= 0) error stop "HDF5 oracle generation failed"

    call h5_init()
    call h5_open(trim(fixture), file_id)
    call h5_get(file_id, "grid/Nt", scalar)
    call h5_get(file_id, "grid/x_values", x)
    call h5_get(file_id, "grid/matrix", matrix)
    call h5_get(file_id, "grid/label", label)
    call h5_get_bounds(file_id, "grid/matrix", lb1, lb2, ub1, ub2)
    if (any([lb1, lb2, ub1, ub2] /= [-2, 5, 0, 6])) &
        error stop "hdf5_tools bounds differ from h5py oracle"
    call h5_get_bounds(file_id, "grid/Nt", lb1, ub1)
    if (lb1 /= 0 .or. ub1 /= 0) error stop "missing bounds do not default to zero"
    call h5_close(file_id)
    call h5_deinit()

    if (scalar /= 42) error stop "hdf5_tools integer scalar differs from oracle"
    if (any(abs(x - [1.25_real64, -2.5_real64, 4.75_real64]) > 1.0e-6_real64)) &
        error stop "hdf5_tools vector differs from oracle"
    if (any(abs(matrix - reshape([1, 2, 3, 4, 5, 6], [3, 2])) > 1.0e-12_real64)) &
        error stop "hdf5_tools matrix differs from oracle"
    if (trim(label) /= "stellarator") error stop "hdf5_tools string differs from oracle"
end program test_hdf5_tools_read
