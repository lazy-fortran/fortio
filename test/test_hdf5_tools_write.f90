program test_hdf5_tools_write
    use, intrinsic :: iso_fortran_env, only: real64
    use hdf5_tools, only: HID_T, h5_add, h5_close, h5_close_group, h5_create, &
                          h5_create_parent_groups, h5_define_group, h5_get, h5_isvalid, &
                          h5_open, h5_open_group, h5overwrite
    implicit none

    integer(HID_T) :: file_id, group_id
    integer :: scalar, exit_status, unit
    integer :: matrix(2, 3)
    integer, allocatable :: absent(:)
    real(real64) :: cube(2, 2, 2, 2, 2)
    complex(real64) :: complex_vector(2)
    character(len=512) :: command, dump_path, line, path
    character(len=32) :: label
    logical :: found_group, found_matrix, found_matrix_shape, found_cube
    logical :: found_comment, found_comment_value, found_bounds, found_accuracy
    logical :: found_accuracy_value
    logical :: found_string, found_complex

    call get_command_argument(1, path)
    if (len_trim(path) == 0) path = "build/hdf5-tools-written.h5"
    matrix = reshape([1, 2, 3, 4, 5, 6], shape(matrix))
    cube = reshape([(real(scalar, real64), scalar = 1, size(cube))], shape(cube))
    complex_vector = [cmplx(1.5, -2.0, real64), cmplx(3.0, 4.25, real64)]

    call h5_create(trim(path), file_id)
    if (.not. h5_isvalid(file_id)) error stop "new HDF5 identifier is invalid"
    call h5_add(file_id, "answer", 42, "legacy comment", "1")
    h5overwrite = .true.
    call h5_add(file_id, "answer", 43, "replacement comment", "1")
    h5overwrite = .false.
    call h5_add(file_id, "tolerance", 0.25_real64, accuracy=1.0e-8_real64)
    call h5_add(file_id, "label", "stellarator", "configuration label")
    call h5_add(file_id, "enabled", .true.)
    call h5_create_parent_groups(file_id, "prepared/nested/")
    call h5_add(file_id, "prepared/nested/value", 9)
    call h5_add(file_id, "absent", absent, default=7)
    call h5_define_group(file_id, "results", group_id)
    call h5_add(group_id, "matrix", matrix, [1, 1], [2, 3])
    call h5_add(group_id, "cube", cube, [1, 1, 1, 1, 1], [2, 2, 2, 2, 2])
    call h5_add(group_id, "complex_vector", complex_vector, [1], [2])
    call h5_close_group(group_id)
    call h5_close(file_id)
    if (h5_isvalid(file_id)) error stop "closed HDF5 identifier remains valid"
    call h5_create(trim(path)//".second", file_id)
    call h5_add(file_id, "second_write", 1)
    call h5_close(file_id)

    call h5_open(trim(path), file_id)
    call h5_get(file_id, "answer", scalar)
    if (scalar /= 43) error stop "hdf5_tools overwrite differs"
    call h5_get(file_id, "enabled", found_group)
    if (.not. found_group) error stop "hdf5_tools logical round trip differs"
    call h5_get(file_id, "prepared/nested/value", scalar)
    if (scalar /= 9) error stop "prepared parent-group value differs"
    call h5_get(file_id, "absent", scalar)
    if (scalar /= 7) error stop "hdf5_tools unallocated default differs"
    call h5_get(file_id, "label", label)
    if (trim(label) /= "stellarator") error stop "hdf5_tools string round trip differs"
    call h5_open_group(file_id, "results", group_id)
    call h5_close_group(group_id)
    call h5_close(file_id)

    dump_path = trim(path)//".dump"
    command = "h5dump "//trim(path)//" > "//trim(dump_path)
    call execute_command_line(trim(command), exitstat=exit_status)
    if (exit_status /= 0) error stop "system h5dump rejected hdf5_tools output"

    found_group = .false.
    found_matrix = .false.
    found_matrix_shape = .false.
    found_cube = .false.
    found_comment = .false.
    found_comment_value = .false.
    found_bounds = .false.
    found_accuracy = .false.
    found_accuracy_value = .false.
    found_string = .false.
    found_complex = .false.
    open(newunit=unit, file=trim(dump_path), status="old", action="read")
    do
        read(unit, "(a)", iostat=exit_status) line
        if (exit_status /= 0) exit
        if (index(line, 'GROUP "results"') > 0) found_group = .true.
        if (index(line, 'DATASET "matrix"') > 0) found_matrix = .true.
        if (index(line, 'DATASPACE  SIMPLE { ( 3, 2 )') > 0) found_matrix_shape = .true.
        if (index(line, 'DATASPACE  SIMPLE { ( 2, 2, 2, 2, 2 )') > 0) found_cube = .true.
        if (index(line, 'ATTRIBUTE "comment"') > 0) found_comment = .true.
        if (index(line, '"replacement comment"') > 0) found_comment_value = .true.
        if (index(line, 'ATTRIBUTE "lbounds"') > 0) found_bounds = .true.
        if (index(line, 'ATTRIBUTE "accuracy"') > 0) found_accuracy = .true.
        if (index(line, '(0): 1e-08') > 0) found_accuracy_value = .true.
        if (index(line, '(0): "stellarator"') > 0) found_string = .true.
        if (index(line, 'H5T_IEEE_F64LE "real"') > 0) found_complex = .true.
    end do
    close(unit)
    if (.not. found_group) error stop "system h5dump did not find results group"
    if (.not. found_matrix) error stop "system h5dump did not find matrix"
    if (.not. found_matrix_shape) error stop "system h5dump found wrong matrix shape"
    if (.not. found_cube) error stop "system h5dump did not find rank-5 cube"
    if (.not. found_comment) error stop "system h5dump did not find comment attribute"
    if (.not. found_comment_value) error stop "system h5dump found wrong comment value"
    if (.not. found_bounds) error stop "system h5dump did not find bounds attributes"
    if (.not. found_accuracy) error stop "system h5dump did not find accuracy attribute"
    if (.not. found_accuracy_value) error stop "system h5dump found wrong accuracy value"
    if (.not. found_string) error stop "system h5dump found wrong string value"
    if (.not. found_complex) error stop "system h5dump rejected complex compound type"

    command = "h5dump "//trim(path)//".second > /dev/null"
    call execute_command_line(trim(command), exitstat=exit_status)
    if (exit_status /= 0) error stop "system h5dump rejected reused writer output"

end program test_hdf5_tools_write
