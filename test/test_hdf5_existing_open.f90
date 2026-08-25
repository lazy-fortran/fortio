program test_hdf5_existing_open
    use, intrinsic :: iso_fortran_env, only: output_unit
    use hdf5_tools, only: HID_T, h5_add, h5_close, h5_create, h5_defer_close, h5_deinit, &
        h5_exists, h5_get, h5_init, h5_open, h5_open_rw, h5_truncate_existing
    use hdf5_tools, only: h5_stream_write
    implicit none

    integer(HID_T) :: file_id
    character(len=1024) :: path, mode, command
    integer :: value, exit_status

    call get_command_argument(1, path)
    call get_command_argument(2, mode)
    if (len_trim(path) == 0 .or. len_trim(mode) == 0) &
        error stop "an output path and mode are required"

    select case (trim(mode))
    case ("create")
        call h5_init()
        call h5_create(trim(path), file_id)
        call h5_add(file_id, "stale", 1)
        call h5_close(file_id)
        call h5_deinit()
    case ("recreate")
        call h5_init()
        h5_defer_close = .true.
        h5_truncate_existing = .true.
        h5_stream_write = .true.
        call h5_open_rw(trim(path), file_id)
        call h5_add(file_id, "fresh", 2)
        call h5_close(file_id)
        call h5_deinit()
        h5_defer_close = .false.
        h5_truncate_existing = .false.
        h5_stream_write = .false.
        call h5_open(trim(path), file_id)
        if (h5_exists(file_id, "stale")) error stop "existing dataset was retained"
        call h5_get(file_id, "version", value)
        if (value /= 1) error stop "file format version differs"
        call h5_get(file_id, "fresh", value)
        if (value /= 2) error stop "new dataset differs"
        call h5_close(file_id)
        command = "h5dump -d fresh " // trim(path) // " | grep -q '(0): 2'"
        call execute_command_line(trim(command), exitstat=exit_status)
        if (exit_status /= 0) error stop "system h5dump disagrees with writer"
        write (output_unit, '(a)') "existing image recreated"
    case default
        error stop "unknown mode"
    end select
end program test_hdf5_existing_open
