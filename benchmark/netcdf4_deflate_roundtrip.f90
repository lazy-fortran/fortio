program netcdf4_deflate_roundtrip
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use netcdf, only: NF90_CLOBBER, NF90_DOUBLE, NF90_NETCDF4, NF90_NOERR, &
        NF90_NOWRITE, nf90_close, nf90_create, nf90_def_dim, nf90_def_var, &
        nf90_def_var_deflate, nf90_enddef, nf90_get_var, nf90_inq_varid, &
        nf90_open, nf90_put_var
    implicit none

    integer, parameter :: particles = 256, timesteps = 2048, fields = 6
    character(len=512) :: path
    integer :: dim_particle, dim_timestep, field, i, j, ncid, status
    integer :: variables(fields)
    integer(int64) :: tick_begin, tick_end, tick_rate
    real(real64) :: checksum, elapsed
    real(real64), allocatable :: row(:), received(:, :)

    call get_command_argument(1, path)
    if (len_trim(path) == 0) path = "netcdf4-deflate-roundtrip.nc"
    allocate(row(timesteps), received(particles, timesteps))

    call system_clock(tick_begin, tick_rate)
    status = nf90_create(trim(path), ior(NF90_CLOBBER, NF90_NETCDF4), ncid)
    call require_success(status, "create")
    status = nf90_def_dim(ncid, "particle", particles, dim_particle)
    call require_success(status, "define particle")
    status = nf90_def_dim(ncid, "timestep", timesteps, dim_timestep)
    call require_success(status, "define timestep")
    do field = 1, fields
        status = nf90_def_var(ncid, field_name(field), NF90_DOUBLE, &
            [dim_particle, dim_timestep], variables(field))
        call require_success(status, "define field")
        status = nf90_def_var_deflate(ncid, variables(field), 1, 1, 4)
        call require_success(status, "configure field compression")
    end do
    status = nf90_enddef(ncid)
    call require_success(status, "end definition")

    do i = 1, particles
        do j = 1, timesteps
            row(j) = real(i + j, real64)
        end do
        do field = 1, fields
            row = row + 1.0_real64
            status = nf90_put_var(ncid, variables(field), row, &
                start=[i, 1], count=[1, timesteps])
            call require_success(status, "write orbit row")
        end do
    end do
    status = nf90_close(ncid)
    call require_success(status, "close output")

    status = nf90_open(trim(path), NF90_NOWRITE, ncid)
    call require_success(status, "open input")
    checksum = 0.0_real64
    do field = 1, fields
        status = nf90_inq_varid(ncid, field_name(field), variables(field))
        call require_success(status, "find field")
        status = nf90_get_var(ncid, variables(field), received)
        call require_success(status, "read field")
        checksum = checksum + sum(received)
    end do
    status = nf90_close(ncid)
    call require_success(status, "close input")
    call system_clock(tick_end)

    if (checksum /= expected_checksum()) error stop "round-trip values differ"
    elapsed = real(tick_end - tick_begin, real64)/real(tick_rate, real64)
    write (*, '(a,es24.16,1x,a,es24.16)') "seconds=", elapsed, "checksum=", checksum

contains

    pure function field_name(field) result(name)
        integer, intent(in) :: field
        character(len=8) :: name

        write (name, '("orbit_",i1)') field
    end function field_name

    pure real(real64) function expected_checksum()
        integer :: field
        real(real64) :: base

        base = real(timesteps, real64)*real(particles*(particles + 1)/2, real64)
        base = base + real(particles, real64)* &
            real(timesteps*(timesteps + 1)/2, real64)
        expected_checksum = 0.0_real64
        do field = 1, fields
            expected_checksum = expected_checksum + base + &
                real(field*particles*timesteps, real64)
        end do
    end function expected_checksum

    subroutine require_success(code, operation)
        integer, intent(in) :: code
        character(len=*), intent(in) :: operation

        if (code /= NF90_NOERR) then
            write (*, '(a,1x,i0)') trim(operation)//" failed:", code
            error stop
        end if
    end subroutine require_success

end program netcdf4_deflate_roundtrip
