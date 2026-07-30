program test_netcdf_rabe_compat
    use, intrinsic :: iso_fortran_env, only: real64
    use netcdf, only: nf90_close, nf90_create, nf90_def_var, nf90_enddef, &
        nf90_put_att, nf90_put_var, NF90_CLASSIC_MODEL, NF90_CLOBBER, NF90_DOUBLE, &
        NF90_GLOBAL, NF90_NOERR
    implicit none

    character(len=1024) :: path
    integer :: ncid, status, variable_id

    call get_command_argument(1, path)
    if (len_trim(path) == 0) path = "rabe-compat.nc"

    status = nf90_create(trim(path), ior(NF90_CLOBBER, NF90_CLASSIC_MODEL), ncid)
    if (status /= NF90_NOERR) error stop "create"
    status = nf90_put_att(ncid, NF90_GLOBAL, "title", &
        "RABE Bootstrap Current Analysis Results")
    if (status /= NF90_NOERR) error stop "put global title"
    status = nf90_def_var(ncid, "off_factor_a", NF90_DOUBLE, varid=variable_id)
    if (status /= NF90_NOERR) error stop "define scalar"
    status = nf90_put_att(ncid, variable_id, "long_name", "1/sqrt(nu_star) factor")
    if (status /= NF90_NOERR) error stop "put variable attribute"
    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) error stop "end definition"
    status = nf90_put_var(ncid, variable_id, 1.23456789_real64)
    if (status /= NF90_NOERR) error stop "put scalar"
    status = nf90_close(ncid)
    if (status /= NF90_NOERR) error stop "close"
end program test_netcdf_rabe_compat
