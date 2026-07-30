program test_hdf5_tools_write
    use, intrinsic :: iso_fortran_env, only: real64
    use hdf5_tools, only: HID_T, h5_add, h5_close, h5_close_group, h5_create, &
                          h5_define_group, h5_get, h5_open, h5_open_group
    implicit none

    integer(HID_T) :: file_id, group_id
    integer :: scalar, exit_status, unit
    integer :: matrix(2, 3)
    real(real64) :: cube(2, 2, 2, 2, 2)
    character(len=512) :: command, dump_path, line, path
    logical :: found_group, found_matrix, found_matrix_shape, found_cube

    call get_command_argument(1, path)
    if (len_trim(path) == 0) path = "build/hdf5-tools-written.h5"
    matrix = reshape([1, 2, 3, 4, 5, 6], shape(matrix))
    cube = reshape([(real(scalar, real64), scalar = 1, size(cube))], shape(cube))

    call h5_create(trim(path), file_id)
    call h5_add(file_id, "answer", 42, "ignored until attribute support", "1")
    call h5_define_group(file_id, "results", group_id)
    call h5_add(group_id, "matrix", matrix, [1, 1], [2, 3])
    call h5_add(group_id, "cube", cube, [1, 1, 1, 1, 1], [2, 2, 2, 2, 2])
    call h5_close_group(group_id)
    call h5_close(file_id)

    call h5_open(trim(path), file_id)
    call h5_get(file_id, "answer", scalar)
    if (scalar /= 42) error stop "hdf5_tools scalar round trip differs"
    call h5_open_group(file_id, "results", group_id)
    call h5_close_group(group_id)
    call h5_close(file_id)

    dump_path = trim(path)//".dump"
    command = "h5dump -H "//trim(path)//" > "//trim(dump_path)
    call execute_command_line(trim(command), exitstat=exit_status)
    if (exit_status /= 0) error stop "system h5dump rejected hdf5_tools output"

    found_group = .false.
    found_matrix = .false.
    found_matrix_shape = .false.
    found_cube = .false.
    open(newunit=unit, file=trim(dump_path), status="old", action="read")
    do
        read(unit, "(a)", iostat=exit_status) line
        if (exit_status /= 0) exit
        if (index(line, 'GROUP "results"') > 0) found_group = .true.
        if (index(line, 'DATASET "matrix"') > 0) found_matrix = .true.
        if (index(line, 'DATASPACE  SIMPLE { ( 3, 2 )') > 0) found_matrix_shape = .true.
        if (index(line, 'DATASPACE  SIMPLE { ( 2, 2, 2, 2, 2 )') > 0) found_cube = .true.
    end do
    close(unit)
    if (.not. found_group) error stop "system h5dump did not find results group"
    if (.not. found_matrix) error stop "system h5dump did not find matrix"
    if (.not. found_matrix_shape) error stop "system h5dump found wrong matrix shape"
    if (.not. found_cube) error stop "system h5dump did not find rank-5 cube"

end program test_hdf5_tools_write
