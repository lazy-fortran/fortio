program hdf5_append_fortio
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use hdf5_tools, only: HID_T, h5_append_double_1, h5_close, h5_create, &
        h5_define_unlimited_matrix, h5_deinit, h5_get, h5_init, h5_open, h5_open_rw
    use hdf5_tools_f2003, only: H5T_NATIVE_DOUBLE
    implicit none

    integer, parameter :: fields = 6, steps = 1024
    character(len=512) :: path
    integer(HID_T) :: dataset_id, file_id
    integer(int64) :: tick_begin, tick_end, tick_rate
    integer :: field, step
    real(real64) :: elapsed, checksum, record(fields), received(fields, steps)

    call get_command_argument(1, path)
    if (len_trim(path) == 0) path = "hdf5-append-fortio.h5"

    call system_clock(tick_begin, tick_rate)
    do step = 1, steps
        call h5_init()
        if (step == 1) then
            call h5_create(trim(path), file_id)
            call h5_define_unlimited_matrix(file_id, "timstep_evol.dat", &
                H5T_NATIVE_DOUBLE, [fields, -1], dataset_id)
        else
            call h5_open_rw(trim(path), file_id)
        end if
        do field = 1, fields
            record(field) = real(1000*step + field, real64)
        end do
        call h5_append_double_1(dataset_id, record, step)
        call h5_close(file_id)
        call h5_deinit()
    end do

    call h5_open(trim(path), file_id)
    call h5_get(file_id, "timstep_evol.dat", received)
    call h5_close(file_id)
    call system_clock(tick_end)

    checksum = sum(received)
    if (checksum /= expected_checksum()) error stop "appended values differ"
    elapsed = real(tick_end - tick_begin, real64)/real(tick_rate, real64)
    write (*, '(a,es24.16,1x,a,es24.16)') "seconds=", elapsed, "checksum=", checksum

contains

    pure real(real64) function expected_checksum()
        expected_checksum = real(int(fields, int64)*1000_int64*steps*(steps + 1)/2 + &
            int(steps, int64)*fields*(fields + 1)/2, real64)
    end function expected_checksum

end program hdf5_append_fortio
