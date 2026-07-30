program test_netcdf_attributes
    use, intrinsic :: iso_fortran_env, only: real64
    use netcdf, only: NF90_CHAR, NF90_DOUBLE, NF90_GLOBAL, NF90_NOERR, NF90_NOWRITE, &
                     nf90_close, nf90_get_att, nf90_inq_varid, &
                     nf90_inquire_attribute, nf90_open
    implicit none

    character(len=1024) :: fixture, cdl, command
    character(len=32) :: text
    integer :: ncid, varid, xtype, length, command_status
    real(real64) :: rho_lcfs
    logical :: fixture_exists

    call get_command_argument(1, fixture)
    if (len_trim(fixture) == 0) fixture = "build/attribute-oracle.nc"
    call get_command_argument(2, cdl)
    if (len_trim(cdl) == 0) cdl = "test/fixtures/oracle.cdl"
    inquire (file=trim(fixture), exist=fixture_exists)
    if (.not. fixture_exists) then
        command = "ncgen -k classic -o "//trim(fixture)//" "//trim(cdl)
        call execute_command_line(trim(command), exitstat=command_status)
        if (command_status /= 0) error stop "ncgen attribute oracle generation failed"
    end if

    if (nf90_open(trim(fixture), NF90_NOWRITE, ncid) /= NF90_NOERR) error stop "open failed"
    if (nf90_inquire_attribute(ncid, NF90_GLOBAL, "coordinate_system", xtype, length) /= &
        NF90_NOERR) error stop "global text attribute inquiry failed"
    if (xtype /= NF90_CHAR .or. length /= 9) error stop "global text metadata differs"
    if (nf90_get_att(ncid, NF90_GLOBAL, "coordinate_system", text) /= NF90_NOERR) &
        error stop "global text attribute read failed"
    if (trim(text) /= "cartesian") error stop "global text attribute differs"

    if (nf90_inquire_attribute(ncid, NF90_GLOBAL, "rho_lcfs", xtype, length) /= &
        NF90_NOERR) error stop "global numeric attribute inquiry failed"
    if (xtype /= NF90_DOUBLE .or. length /= 1) error stop "numeric metadata differs"
    if (nf90_get_att(ncid, NF90_GLOBAL, "rho_lcfs", rho_lcfs) /= NF90_NOERR) &
        error stop "global numeric attribute read failed"
    if (abs(rho_lcfs - 1.5_real64) > 1.0e-12_real64) &
        error stop "global numeric attribute differs"

    if (nf90_inq_varid(ncid, "x_values", varid) /= NF90_NOERR) error stop "var lookup failed"
    if (nf90_get_att(ncid, varid, "units", text) /= NF90_NOERR) &
        error stop "variable text attribute read failed"
    if (trim(text) /= "m") error stop "variable text attribute differs"
    if (nf90_close(ncid) /= NF90_NOERR) error stop "close failed"
end program test_netcdf_attributes
