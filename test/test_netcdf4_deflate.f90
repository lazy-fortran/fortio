program test_netcdf4_deflate
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use netcdf, only: NF90_CLOBBER, NF90_DOUBLE, NF90_INT, NF90_NETCDF4, NF90_NOWRITE, &
        NF90_NOERR, nf90_close, nf90_create, nf90_def_dim, nf90_def_var, &
        nf90_def_var_deflate, nf90_enddef, nf90_get_var, nf90_inq_varid, nf90_open, &
        nf90_put_var
    implicit none

    real(real64) :: values(64, 32)
    real(real64) :: readback(64, 32)
    integer(int32) :: particle(64), timestep(32)
    character(len=512) :: path, verifier
    integer :: command_status, dim_particle, dim_timestep, i, j, ncid
    integer :: status, var_field, var_particle, var_timestep

    call get_command_argument(1, path)
    call get_command_argument(2, verifier)
    if (len_trim(path) == 0) path = "fortio-netcdf4-deflate.nc"
    if (len_trim(verifier) == 0) verifier = "test/fixtures/verify_netcdf4_deflate.py"
    do j = 1, size(values, 2)
        do i = 1, size(values, 1)
            values(i, j) = real(i + 10*j, real64)
        end do
    end do
    particle = [(int(i - 1, int32), i=1, size(particle))]
    timestep = [(int(i - 1, int32), i=1, size(timestep))]

    status = nf90_create(trim(path), ior(NF90_CLOBBER, NF90_NETCDF4), ncid)
    if (status /= NF90_NOERR) error stop "create"
    status = nf90_def_dim(ncid, "particle", size(particle), dim_particle)
    if (status /= NF90_NOERR) error stop "define particle"
    status = nf90_def_dim(ncid, "timestep", size(timestep), dim_timestep)
    if (status /= NF90_NOERR) error stop "define timestep"
    status = nf90_def_var(ncid, "particle", NF90_INT, dim_particle, var_particle)
    if (status /= NF90_NOERR) error stop "define particle coordinate"
    status = nf90_def_var(ncid, "timestep", NF90_INT, dim_timestep, var_timestep)
    if (status /= NF90_NOERR) error stop "define timestep coordinate"
    status = nf90_def_var(ncid, "field", NF90_DOUBLE, &
        [dim_particle, dim_timestep], var_field)
    if (status /= NF90_NOERR) error stop "define field"
    status = nf90_def_var_deflate(ncid, var_field, 1, 1, 4)
    if (status /= NF90_NOERR) error stop "define field compression"
    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) error stop "end definition"
    status = nf90_put_var(ncid, var_particle, particle)
    if (status /= NF90_NOERR) error stop "write particle coordinate"
    status = nf90_put_var(ncid, var_timestep, timestep)
    if (status /= NF90_NOERR) error stop "write timestep coordinate"
    status = nf90_put_var(ncid, var_field, values)
    if (status /= NF90_NOERR) error stop "write field"
    status = nf90_close(ncid)
    if (status /= NF90_NOERR) error stop "close"

    call execute_command_line("python3 "//trim(verifier)//" "//trim(path), &
        exitstat=command_status)
    if (command_status /= 0) error stop "independent NetCDF-4 deflate oracle failed"

    status = nf90_open(trim(path), NF90_NOWRITE, ncid)
    if (status /= NF90_NOERR) error stop "reopen compressed NetCDF-4"
    status = nf90_inq_varid(ncid, "field", var_field)
    if (status /= NF90_NOERR) error stop "find compressed field"
    status = nf90_get_var(ncid, var_field, readback)
    if (status /= NF90_NOERR) error stop "read compressed field"
    if (any(readback /= values)) error stop "compressed field roundtrip differs"
    status = nf90_close(ncid)
    if (status /= NF90_NOERR) error stop "close compressed NetCDF-4 reader"
end program test_netcdf4_deflate
