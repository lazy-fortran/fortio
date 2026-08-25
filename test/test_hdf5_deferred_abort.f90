program test_hdf5_deferred_abort
    use hdf5_tools, only: HID_T, h5_add, h5_create, h5_defer_close, h5_init, &
        h5_stream_write, h5_truncate_existing
    implicit none

    integer(HID_T) :: file_id
    character(len=1024) :: path

    call get_command_argument(1, path)
    if (len_trim(path) == 0) error stop "an output path is required"

    call h5_init()
    h5_defer_close = .true.
    h5_stream_write = .true.
    h5_truncate_existing = .true.
    call h5_create(trim(path), file_id)
    call h5_add(file_id, "uncommitted", 7)
    error stop "intentional abort before h5_close"
end program test_hdf5_deferred_abort
