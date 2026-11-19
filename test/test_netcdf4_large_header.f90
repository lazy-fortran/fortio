program test_netcdf4_large_header
    ! A NetCDF-4 root header with 120 kB of attributes needs a four-byte
    ! chunk size field; ncdump (independent reader) and fortio read it back.
    ! One attribute above the 65535-byte message limit is rejected.
    use netcdf, only: NF90_CLOBBER, NF90_GLOBAL, NF90_NETCDF4, NF90_NOERR, NF90_NOWRITE, &
        NF90_DOUBLE, nf90_close, nf90_create, nf90_def_var, nf90_enddef, nf90_get_att, &
        nf90_open, nf90_put_att, nf90_put_var
    implicit none
    character(len=1024) :: path, command
    character(len=40000) :: text, back
    character(len=70000) :: oversized
    integer :: ncid, varid, command_status, i
    call get_command_argument(1, path)
    if (len_trim(path) == 0) path = "large-header.nc"
    do i = 1, len(text)
        text(i:i) = achar(iachar('a') + mod(i, 26))
    end do
    oversized = repeat('x', len(oversized))
    if (nf90_create(trim(path), ior(NF90_NETCDF4, NF90_CLOBBER), ncid) /= NF90_NOERR) &
        error stop "create"
    do i = 1, 3
        if (nf90_put_att(ncid, NF90_GLOBAL, "text"//achar(iachar('0') + i), text) &
            /= NF90_NOERR) error stop "put large attribute"
    end do
    if (nf90_put_att(ncid, NF90_GLOBAL, "oversized", oversized) == NF90_NOERR) &
        error stop "attribute above the message limit accepted"
    if (nf90_def_var(ncid, "value", NF90_DOUBLE, varid) /= NF90_NOERR) error stop "def"
    if (nf90_enddef(ncid) /= NF90_NOERR) error stop "enddef"
    if (nf90_put_var(ncid, varid, 2.5d0) /= NF90_NOERR) error stop "put"
    if (nf90_close(ncid) /= NF90_NOERR) error stop "close"
    command = "ncdump -h "//trim(path)//" > /dev/null"
    call execute_command_line(trim(command), exitstat=command_status)
    if (command_status /= 0) error stop "ncdump cannot read the header"
    if (nf90_open(trim(path), NF90_NOWRITE, ncid) /= NF90_NOERR) error stop "open"
    if (nf90_get_att(ncid, NF90_GLOBAL, "text3", back) /= NF90_NOERR) error stop "get"
    if (back /= text) error stop "attribute round trip"
    if (nf90_close(ncid) /= NF90_NOERR) error stop "close after read"
end program test_netcdf4_large_header
