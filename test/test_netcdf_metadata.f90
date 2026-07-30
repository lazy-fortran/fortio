program test_netcdf_metadata
    use netcdf, only: NF90_DOUBLE, NF90_NOERR, NF90_NOWRITE, nf90_close, &
                     nf90_inq_dimid, nf90_inq_varid, nf90_inquire_dimension, &
                     nf90_inquire_variable, nf90_open
    implicit none

    character(len=1024) :: fixture, cdl, command
    character(len=64) :: name
    integer :: ncid, dimid, varid, length, xtype, ndims, natts, dimids(3)
    integer :: command_status
    logical :: fixture_exists

    call get_command_argument(1, fixture)
    if (len_trim(fixture) == 0) fixture = "build/metadata-oracle.nc"
    call get_command_argument(2, cdl)
    if (len_trim(cdl) == 0) cdl = "test/fixtures/oracle.cdl"
    inquire (file=trim(fixture), exist=fixture_exists)
    if (.not. fixture_exists) then
        command = "ncgen -k classic -o "//trim(fixture)//" "//trim(cdl)
        call execute_command_line(trim(command), exitstat=command_status)
        if (command_status /= 0) error stop "ncgen oracle generation failed"
    end if

    if (nf90_open(trim(fixture), NF90_NOWRITE, ncid) /= NF90_NOERR) &
        error stop "metadata oracle open failed"
    if (nf90_inq_dimid(ncid, "x", dimid) /= NF90_NOERR) &
        error stop "dimension lookup failed"
    if (nf90_inquire_dimension(ncid, dimid, name=name, len=length) /= NF90_NOERR) &
        error stop "dimension inquiry failed"
    if (trim(name) /= "x" .or. length /= 3) error stop "dimension metadata differs"

    if (nf90_inq_varid(ncid, "matrix", varid) /= NF90_NOERR) &
        error stop "variable lookup failed"
    if (nf90_inquire_variable(ncid, varid, name, xtype, ndims, dimids, natts) /= &
        NF90_NOERR) error stop "variable inquiry failed"
    if (trim(name) /= "matrix" .or. xtype /= NF90_DOUBLE .or. ndims /= 2) &
        error stop "variable metadata differs"
    if (natts /= 0) error stop "matrix attribute count differs"
    if (nf90_inquire_dimension(ncid, dimids(1), name=name, len=length) /= NF90_NOERR) &
        error stop "first variable dimension inquiry failed"
    if (trim(name) /= "x" .or. length /= 3) error stop "Fortran dimension order differs"
    if (nf90_inquire_dimension(ncid, dimids(2), name=name, len=length) /= NF90_NOERR) &
        error stop "second variable dimension inquiry failed"
    if (trim(name) /= "y" .or. length /= 2) error stop "Fortran dimension order differs"

    if (nf90_inq_varid(ncid, "x_values", varid) /= NF90_NOERR) &
        error stop "attributed variable lookup failed"
    if (nf90_inquire_variable(ncid, varid, natts=natts) /= NF90_NOERR) &
        error stop "attribute count inquiry failed"
    if (natts /= 1) error stop "attribute count differs from ncgen oracle"
    if (nf90_close(ncid) /= NF90_NOERR) error stop "metadata oracle close failed"
end program test_netcdf_metadata
