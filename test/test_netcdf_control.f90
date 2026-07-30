program test_netcdf_control
    use netcdf
    implicit none

    integer :: ncid, dimid, group_id, status, varid

    status = nf90_create("control.nc", NF90_CLOBBER, ncid)
    if (status /= NF90_NOERR) error stop "create failed"
    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) error stop "enddef failed"
    status = nf90_redef(ncid)
    if (status /= NF90_NOERR) error stop "redef failed"
    status = nf90_def_dim(ncid, "items", 2, dimid)
    if (status /= NF90_NOERR) error stop "dimension after redef failed"
    status = nf90_def_var(ncid, "after_redef", NF90_INT, dimid, varid)
    if (status /= NF90_NOERR) error stop "definition after redef failed"
    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) error stop "second enddef failed"
    status = nf90_put_var(ncid, varid, [42, 43])
    if (status /= NF90_NOERR) error stop "write after redef failed"

    status = nf90_def_grp(ncid, "unsupported", group_id)
    if (status /= NF90_ENOTSUPPORT) error stop "groups must report unsupported"
    if (group_id /= -1) error stop "unsupported group returned an ID"

    status = nf90_inq_ncid(ncid, "unsupported", group_id)
    if (status /= NF90_ENOTSUPPORT) error stop "group inquiry must report unsupported"
    if (group_id /= -1) error stop "unsupported group inquiry returned an ID"

    status = nf90_close(ncid)
    if (status /= NF90_NOERR) error stop "close failed"

    status = nf90_create("control-second.nc", NF90_CLOBBER, ncid)
    if (status /= NF90_NOERR) error stop "second create failed"
    status = nf90_def_var(ncid, "second_write", NF90_INT, varid=varid)
    if (status /= NF90_NOERR) error stop "second definition failed"
    status = nf90_close(ncid)
    if (status /= NF90_NOERR) error stop "second close failed"
end program test_netcdf_control
