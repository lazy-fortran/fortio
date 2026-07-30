program hdf5_roundtrip_fortio
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use hdf5_tools, only: HID_T, h5_add, h5_close, h5_create, h5_get, h5_open
    implicit none

    integer, parameter :: nx = 2048, ny = 2048, repetitions = 100
    character(len=512) :: path
    integer(HID_T) :: file_id
    integer(int64) :: tick_begin, tick_end, tick_rate
    integer :: index, repetition
    real(real64) :: elapsed, checksum
    real(real64), allocatable :: values(:, :), received(:, :)

    call get_command_argument(1, path)
    if (len_trim(path) == 0) path = "hdf5-roundtrip-fortio.h5"
    allocate(values(nx, ny), received(nx, ny))
    values = reshape([(real(index, real64), index = 1, size(values))], shape(values))

    call system_clock(tick_begin, tick_rate)
    call h5_create(trim(path), file_id)
    call h5_add(file_id, "values", values, [1, 1], [nx, ny])
    call h5_close(file_id)
    call h5_open(trim(path), file_id)
    do repetition = 1, repetitions
        call h5_get(file_id, "values", received)
    end do
    call h5_close(file_id)
    call system_clock(tick_end)

    checksum = sum(received)
    if (checksum /= real(size(values), real64)*real(size(values) + 1, real64)/2.0_real64) &
        error stop "round-trip values differ"
    elapsed = real(tick_end - tick_begin, real64)/real(tick_rate, real64)
    write (*, '(a,es24.16,1x,a,es24.16)') "seconds=", elapsed, "checksum=", checksum
end program hdf5_roundtrip_fortio
