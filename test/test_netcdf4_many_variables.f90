program test_netcdf4_many_variables
    ! ncgen (independent writer) stores the links of a NetCDF-4 root group with
    ! 300 variables in a version 2 B-tree of depth one. Every variable,
    ! including the records held in the internal node, must be found and read.
    use, intrinsic :: iso_fortran_env, only: real64
    use netcdf, only: NF90_NOERR, NF90_NOWRITE, nf90_close, nf90_get_var, &
        nf90_inq_varid, nf90_open
    implicit none
    integer, parameter :: n = 300
    character(len=1024) :: path, cdl, command
    character(len=8) :: name
    real(real64) :: value
    integer :: ncid, varid, unit, i, command_status

    call get_command_argument(1, path)
    if (len_trim(path) == 0) path = "many-variables.nc"
    cdl = trim(path)//".cdl"
    open(newunit=unit, file=trim(cdl), status="replace", action="write")
    write(unit, '(a)') "netcdf many {", "variables:"
    do i = 1, n
        write(unit, '("  double v", i4.4, " ;")') i
    end do
    write(unit, '(a)') "data:"
    do i = 1, n
        write(unit, '("  v", i4.4, " = ", i0, " ;")') i, i
    end do
    write(unit, '(a)') "}"
    close(unit)
    command = "ncgen -k netCDF-4 -o "//trim(path)//" "//trim(cdl)
    call execute_command_line(trim(command), exitstat=command_status)
    if (command_status /= 0) error stop "ncgen failed"

    if (nf90_open(trim(path), NF90_NOWRITE, ncid) /= NF90_NOERR) error stop "open"
    do i = 1, n
        write(name, '("v", i4.4)') i
        if (nf90_inq_varid(ncid, trim(name), varid) /= NF90_NOERR) &
            error stop "variable not found"
        if (nf90_get_var(ncid, varid, value) /= NF90_NOERR) error stop "read"
        if (value /= real(i, real64)) error stop "value differs"
    end do
    if (nf90_close(ncid) /= NF90_NOERR) error stop "close"
end program test_netcdf4_many_variables
