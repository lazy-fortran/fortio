program test_hdf5_tools_read
    use, intrinsic :: iso_fortran_env, only: real64
    use hdf5_tools, only: HID_T, h5_init, h5_deinit, h5_open, h5_close, h5_get, &
                          h5_get_bounds, h5_exists, h5_obj_exists
    implicit none

    integer(HID_T) :: file_id
    character(len=1024) :: fixture, generator, command
    character(len=32) :: label
    integer :: scalar, continued_value, command_status, lb1, lb2, ub1, ub2
    integer :: int_matrix(3, 2), int_cube(3, 2, 2)
    real(real64) :: x(3), matrix(3, 2)
    real(real64) :: real_cube(3, 2, 2)
    real(real64) :: rank4(3, 2, 1, 2), rank5(2, 1, 2, 1, 2)
    complex(real64) :: complex_vector(3)
    logical :: object_exists

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
    call h5_get(file_id, "integer_ranks/int_matrix", int_matrix)
    call h5_get(file_id, "integer_ranks/int_cube", int_cube)
    call h5_get(file_id, "real_ranks/rank4", rank4)
    call h5_get(file_id, "real_ranks/rank5", rank5)
    call h5_get(file_id, "real_ranks/real_cube", real_cube)
    call h5_get(file_id, "real_ranks/complex_vector", complex_vector)
    call h5_get(file_id, "continued/value_0", continued_value)
    if (continued_value /= 10) error stop "first continued link differs from oracle"
    call h5_get(file_id, "continued/value_4", continued_value)
    if (continued_value /= 14) error stop "last continued link differs from oracle"
    call h5_get(file_id, "dense/value_00", continued_value)
    if (continued_value /= 20) error stop "first dense link differs from oracle"
    call h5_get(file_id, "dense/value_11", continued_value)
    if (continued_value /= 31) error stop "last dense link differs from oracle"
    call h5_get_bounds(file_id, "grid/matrix", lb1, lb2, ub1, ub2)
    if (any([lb1, lb2, ub1, ub2] /= [-2, 5, 0, 6])) &
        error stop "hdf5_tools bounds differ from h5py oracle"
    call h5_get_bounds(file_id, "grid/Nt", lb1, ub1)
    if (lb1 /= 0 .or. ub1 /= 0) error stop "missing bounds do not default to zero"
    if (.not. h5_exists(file_id, "grid/matrix")) error stop "existing dataset not found"
    if (.not. h5_exists(file_id, "dense")) error stop "existing dense group not found"
    if (h5_exists(file_id, "grid/missing")) error stop "missing dataset reported present"
    call h5_obj_exists(file_id, "dense/value_11", object_exists)
    if (.not. object_exists) error stop "h5_obj_exists missed dense dataset"
    call h5_close(file_id)
    call h5_deinit()

    if (scalar /= 42) error stop "hdf5_tools integer scalar differs from oracle"
    if (any(abs(x - [1.25_real64, -2.5_real64, 4.75_real64]) > 1.0e-6_real64)) &
        error stop "hdf5_tools vector differs from oracle"
    if (any(abs(matrix - reshape([1, 2, 3, 4, 5, 6], [3, 2])) > 1.0e-12_real64)) &
        error stop "hdf5_tools matrix differs from oracle"
    if (trim(label) /= "stellarator") error stop "hdf5_tools string differs from oracle"
    if (any(int_matrix /= reshape([(scalar, scalar=1, 6)], shape(int_matrix)))) &
        error stop "hdf5_tools integer matrix differs from oracle"
    if (any(int_cube /= reshape([(scalar, scalar=1, 12)], shape(int_cube)))) &
        error stop "hdf5_tools integer cube differs from oracle"
    if (any(abs(rank4 - reshape([(real(scalar, real64), scalar=1, 12)], &
                                shape(rank4))) > 1.0e-12_real64)) &
        error stop "hdf5_tools rank-4 real differs from oracle"
    if (any(abs(real_cube - reshape([(real(scalar, real64), scalar=1, 12)], &
                                    shape(real_cube))) > 1.0e-12_real64)) &
        error stop "hdf5_tools rank-3 real differs from oracle"
    if (any(abs(rank5 - reshape([(real(scalar, real64), scalar=1, 8)], &
                                shape(rank5))) > 1.0e-12_real64)) &
        error stop "hdf5_tools rank-5 real differs from oracle"
    if (any(abs(complex_vector - [cmplx(1.0, 4.0, real64), &
                                  cmplx(-2.0, 5.25, real64), &
                                  cmplx(3.5, -6.0, real64)]) > 1.0e-12_real64)) &
        error stop "hdf5_tools complex vector differs from h5py oracle"
end program test_hdf5_tools_read
