program test_hdf5_tools_read
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use hdf5_tools, only: HID_T, HSIZE_T, SIZE_T, h5_init, h5_deinit, h5_open, &
        h5_open_rw, h5_close, h5_get, h5_add, h5_get_bounds, h5_exists, h5_obj_exists, &
        h5_create, h5_copy
    use h5lt, only: h5ltget_dataset_info_f
    implicit none

    integer(HID_T) :: file_id, copy_id
    integer(HSIZE_T) :: dimensions(2)
    integer(SIZE_T) :: element_count
    character(len=1024) :: fixture, generator, verifier, command, executable, mode
    character(len=32) :: label
    integer :: scalar, rw_value, continued_value, command_status, hdferr, lb1, lb2, &
        type_class, ub1, ub2
    integer :: int_matrix(3, 2), int_cube(3, 2, 2)
    integer :: real_as_integer(3)
    integer(int64) :: big_endian_i64(4), filtered_i64(3), copied_i64(4)
    real(real64) :: x(3), matrix(3, 2)
    real(real64) :: int_scalar_as_real, int_matrix_as_real(3, 2)
    real(real64) :: signed_i64_as_real(4)
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

    call get_command_argument(4, mode)
    if (trim(mode) == "overflow") then
        call h5_init()
        call h5_open(trim(fixture), file_id)
        call h5_get(file_id, "real_ranks/huge_for_integer", scalar)
        call h5_close(file_id)
        call h5_deinit()
        stop 0
    end if

    call h5_init()
    call h5_open(trim(fixture), file_id)
    call h5_get(file_id, "grid/Nt", scalar)
    call h5_get(file_id, "grid/Nt_vector", scalar)
    if (scalar /= 42) error stop "hdf5_tools legacy one-element scalar differs"
    call h5_get(file_id, "grid/x_values", x)
    call h5_get(file_id, "grid/x_values", real_as_integer)
    call h5_get(file_id, "grid/matrix", matrix)
    call h5ltget_dataset_info_f(file_id, "grid/matrix", dimensions, type_class, &
        element_count, hdferr)
    if (hdferr /= 0) error stop "HDF5 dataset-info query failed"
    call h5_get(file_id, "grid/label", label)
    call h5_get(file_id, "integer_ranks/int_matrix", int_matrix)
    call h5_get(file_id, "integer_ranks/int_cube", int_cube)
    call h5_get(file_id, "integer_ranks/int_scalar", int_scalar_as_real)
    call h5_get(file_id, "integer_ranks/int_matrix_as_real", int_matrix_as_real)
    call h5_get(file_id, "integer_ranks/int64_big_endian", big_endian_i64)
    call h5_get(file_id, "integer_ranks/int64_big_endian", signed_i64_as_real)
    call h5_get(file_id, "integer_ranks/int64_filtered", filtered_i64)
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
    call h5_get(file_id, "dense/value_63", continued_value)
    if (continued_value /= 83) error stop "last dense link differs from oracle"
    call h5_get_bounds(file_id, "grid/matrix", lb1, lb2, ub1, ub2)
    if (any([lb1, lb2, ub1, ub2] /= [-2, 5, 0, 6])) &
        error stop "hdf5_tools bounds differ from h5py oracle"
    call h5_get_bounds(file_id, "grid/Nt", lb1, ub1)
    if (lb1 /= 0 .or. ub1 /= 0) error stop "missing bounds do not default to zero"
    if (.not. h5_exists(file_id, "grid/matrix")) error stop "existing dataset not found"
    if (.not. h5_exists(file_id, "dense")) error stop "existing dense group not found"
    if (h5_exists(file_id, "grid/missing")) error stop "missing dataset reported present"
    call h5_obj_exists(file_id, "dense/value_63", object_exists)
    if (.not. object_exists) error stop "h5_obj_exists missed dense dataset"
    call h5_close(file_id)

    call h5_open_rw(trim(fixture), file_id)
    call h5_get(file_id, "grid/Nt", scalar)
    if (scalar /= 42) error stop "read/write handle differs from h5py oracle"
    call h5_add(file_id, "rw_added", 73)
    call h5_get(file_id, "rw_added", rw_value)
    if (rw_value /= 73) error stop "read/write handle does not see its own write"
    call h5_create(trim(fixture)//".copy", copy_id)
    call h5_copy(file_id, "grid", copy_id, "copied_grid")
    call h5_copy(file_id, "integer_ranks", copy_id, "copied_integers")
    call h5_close(copy_id)
    call h5_close(file_id)
    call h5_deinit()
    call h5_init()
    call h5_open(trim(fixture)//".copy", copy_id)
    call h5_get(copy_id, "copied_grid/Nt", rw_value)
    if (rw_value /= 42) error stop "copy from read/write handle differs"
    call h5_get(copy_id, "copied_integers/int64_big_endian", copied_i64)
    if (any(copied_i64 /= big_endian_i64)) &
        error stop "copy of big-endian int64 dataset differs"
    call h5_close(copy_id)
    call h5_deinit()

    call get_command_argument(3, verifier)
    if (len_trim(verifier) == 0) &
        verifier = "test/fixtures/verify_hdf5_open_rw.py"
    command = "python3 "//trim(verifier)//" "//trim(fixture)
    call execute_command_line(trim(command), exitstat=command_status)
    if (command_status /= 0) error stop "HDF5 read/write oracle verification failed"
    call get_command_argument(0, executable)
    command = trim(executable)//" "//trim(fixture)//" "//trim(generator)//" "// &
        trim(verifier)//" overflow"
    call execute_command_line(trim(command), exitstat=command_status)
    if (command_status == 0) &
        error stop "out-of-range HDF5 real-to-integer conversion was accepted"

    if (scalar /= 42) error stop "hdf5_tools integer scalar differs from oracle"
    if (any(abs(x - [1.25_real64, -2.5_real64, 4.75_real64]) > 1.0e-6_real64)) &
        error stop "hdf5_tools vector differs from oracle"
    if (any(real_as_integer /= [1, -2, 4])) &
        error stop "hdf5_tools real-to-integer conversion differs from native HDF5"
    if (any(abs(matrix - reshape([1, 2, 3, 4, 5, 6], [3, 2])) > 1.0e-12_real64)) &
        error stop "hdf5_tools matrix differs from oracle"
    if (any(dimensions /= [3_HSIZE_T, 2_HSIZE_T])) &
        error stop "HDF5 dataset dimensions differ from oracle"
    if (type_class /= 1) error stop "HDF5 dataset class differs from oracle"
    if (element_count /= 8_SIZE_T) error stop "HDF5 element size differs from oracle"
    if (trim(label) /= "stellarator") error stop "hdf5_tools string differs from oracle"
    if (any(int_matrix /= reshape([(scalar, scalar=1, 6)], shape(int_matrix)))) &
        error stop "hdf5_tools integer matrix differs from oracle"
    if (any(int_cube /= reshape([(scalar, scalar=1, 12)], shape(int_cube)))) &
        error stop "hdf5_tools integer cube differs from oracle"
    if (int_scalar_as_real /= -17.0_real64) &
        error stop "hdf5_tools integer scalar conversion differs from native HDF5"
    if (any(int_matrix_as_real /= reshape([(-3.0_real64 + real(scalar, real64), &
        scalar=0, 5)], shape(int_matrix_as_real)))) &
        error stop "hdf5_tools integer matrix conversion differs from native HDF5"
    if (any(big_endian_i64 /= [-1_int64, -17_int64, 1_int64, huge(0_int64)])) &
        error stop "big-endian int64 read differs from h5py oracle"
    if (any(filtered_i64 /= [-9_int64, 0_int64, 23_int64])) &
        error stop "filtered int64 read differs from h5py oracle"
    if (any(signed_i64_as_real(:3) /= [-1.0_real64, -17.0_real64, 1.0_real64])) &
        error stop "signed int64-to-real conversion differs from native HDF5"
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
