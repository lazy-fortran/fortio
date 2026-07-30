program netcdf_roundtrip
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use netcdf, only: nf90_close, nf90_create, nf90_def_dim, nf90_def_var, nf90_enddef, &
        nf90_get_var, nf90_noerr, nf90_open, nf90_put_var, NF90_CLOBBER, NF90_DOUBLE, &
        NF90_NOWRITE, nf90_inq_varid
    implicit none

    integer, parameter :: nx = 2048, ny = 2048, repetitions = 5
    character(len=512) :: path
    integer :: ncid, x_dim, y_dim, variable, status, repetition
    integer(int64) :: tick_begin, tick_end, tick_rate
    real(real64) :: elapsed, checksum
    real(real64), allocatable :: values(:, :), received(:, :)

    call get_command_argument(1, path)
    if (len_trim(path) == 0) path = "netcdf-roundtrip.nc"

    allocate(values(nx, ny), received(nx, ny))
    values = reshape([(real(repetition, real64), repetition = 1, size(values))], shape(values))

    call system_clock(tick_begin, tick_rate)
    status = nf90_create(trim(path), NF90_CLOBBER, ncid)
    call require_success(status, "create")
    status = nf90_def_dim(ncid, "x", nx, x_dim)
    call require_success(status, "define x")
    status = nf90_def_dim(ncid, "y", ny, y_dim)
    call require_success(status, "define y")
    status = nf90_def_var(ncid, "values", NF90_DOUBLE, [x_dim, y_dim], variable)
    call require_success(status, "define values")
    status = nf90_enddef(ncid)
    call require_success(status, "end definition")
    status = nf90_put_var(ncid, variable, values)
    call require_success(status, "write values")
    status = nf90_close(ncid)
    call require_success(status, "close output")

    do repetition = 1, repetitions
        status = nf90_open(trim(path), NF90_NOWRITE, ncid)
        call require_success(status, "open input")
        status = nf90_inq_varid(ncid, "values", variable)
        call require_success(status, "inquire values")
        status = nf90_get_var(ncid, variable, received)
        call require_success(status, "read values")
        status = nf90_close(ncid)
        call require_success(status, "close input")
    end do
    call system_clock(tick_end)

    checksum = sum(received)
    if (checksum /= real(size(values), real64)*real(size(values) + 1, real64)/2.0_real64) &
        error stop "round-trip values differ"
    elapsed = real(tick_end - tick_begin, real64)/real(tick_rate, real64)
    write (*, '(a,es24.16,1x,a,es24.16)') "seconds=", elapsed, "checksum=", checksum

contains

    subroutine require_success(code, operation)
        integer, intent(in) :: code
        character(len=*), intent(in) :: operation

        if (code /= nf90_noerr) then
            write (*, '(a,1x,i0)') trim(operation)//" failed:", code
            error stop
        end if
    end subroutine require_success

end program netcdf_roundtrip
