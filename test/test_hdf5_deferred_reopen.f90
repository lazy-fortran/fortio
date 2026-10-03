program test_hdf5_deferred_reopen
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use hdf5_tools, only: HID_T, h5_add, h5_close, h5_create, h5_defer_close, &
        h5_deinit, h5_get, h5_init, h5_open, h5_open_rw, h5_stream_write, &
        h5_truncate_existing, h5overwrite
    implicit none

    integer(HID_T) :: file_id
    character(len=1024) :: prefix, path
    real(dp) :: matrix(2, 3), restored(2, 3)
    integer :: mode, session, value

    call get_command_argument(1, prefix)
    if (len_trim(prefix) == 0) then
        write (*, '(a)') 'deferred-reopen fixture requires an output prefix'
        stop 0
    end if
    matrix = reshape([1.25_dp, 2.25_dp, 3.25_dp, 4.25_dp, 5.25_dp, 6.25_dp], &
        shape(matrix))

    do mode = 1, 2
        if (mode == 1) then
            path = trim(prefix)//'.existing.h5'
        else
            path = trim(prefix)//'.fresh.h5'
        end if
        call h5_init()
        h5_defer_close = mode == 2
        h5_stream_write = .false.
        h5_truncate_existing = .false.
        h5overwrite = .true.
        call h5_create(trim(path), file_id)
        call h5_add(file_id, 'old', 19)
        call h5_add(file_id, 'seed', -7, 'seed-retained', 'keV')
        call h5_add(file_id, 'matrix', matrix, [1, 1], [2, 3])
        call h5_close(file_id)
        if (mode == 1) then
            call h5_deinit()
            call h5_init()
        end if
        h5_defer_close = .true.
        do session = 1, 3
            call h5_open_rw(trim(path), file_id)
            call h5_add(file_id, 'updated', 10*session+1)
            call h5_close(file_id)
        end do
        call h5_deinit()
        h5_defer_close = .false.
        call h5_init()
        call h5_open(trim(path), file_id)
        call h5_get(file_id, 'old', value)
        if (value /= 19) error stop 'existing scalar changed'
        call h5_get(file_id, 'seed', value)
        if (value /= -7) error stop 'retained seed changed'
        call h5_get(file_id, 'updated', value)
        if (value /= 31) error stop 'last deferred update was lost'
        call h5_get(file_id, 'matrix', restored)
        if (any(restored /= matrix)) error stop 'existing matrix changed'
        call h5_close(file_id)
        call h5_deinit()
    end do
    h5overwrite = .false.
end program test_hdf5_deferred_reopen
