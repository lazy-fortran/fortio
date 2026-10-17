program test_netcdf_attributes
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use netcdf, only: NF90_CHAR, NF90_CLOBBER, NF90_DOUBLE, NF90_ENOTATT, &
                     NF90_GLOBAL, NF90_INT, NF90_NOERR, NF90_NOWRITE, &
                     nf90_close, nf90_create, nf90_def_var, nf90_enddef, &
                     nf90_get_att, nf90_inq_varid, nf90_inquire_attribute, &
                     nf90_open, nf90_put_att
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
    call long_attribute_names(trim(fixture)//".long-names.nc")

contains

    subroutine long_attribute_names(path)
        character(len=*), intent(in) :: path
        character(len=256), parameter :: prefix = repeat("n", 256)
        character(len=257), parameter :: first = prefix//"a", second = prefix//"b"
        character(len=16) :: value
        integer :: file_id, variable_id
        integer(int32) :: number

        ! Both names must survive with distinct suffixes beyond character 256.
        if (nf90_create(path, NF90_CLOBBER, file_id) /= NF90_NOERR) &
            error stop "create long attribute names"
        if (nf90_def_var(file_id, "scalar", NF90_INT, varid=variable_id) /= &
            NF90_NOERR) error stop "define long attribute names variable"
        if (nf90_put_att(file_id, NF90_GLOBAL, first, "first") /= NF90_NOERR) &
            error stop "write first long global attribute"
        if (nf90_put_att(file_id, NF90_GLOBAL, second, "second") /= NF90_NOERR) &
            error stop "write second long global attribute"
        if (nf90_put_att(file_id, variable_id, first, 17_int32) /= NF90_NOERR) &
            error stop "write first long variable attribute"
        if (nf90_put_att(file_id, variable_id, second, 29_int32) /= NF90_NOERR) &
            error stop "write second long variable attribute"
        if (nf90_enddef(file_id) /= NF90_NOERR) error stop "end long names definition"
        if (nf90_close(file_id) /= NF90_NOERR) error stop "close long names writer"

        if (nf90_open(path, NF90_NOWRITE, file_id) /= NF90_NOERR) &
            error stop "open long attribute names"
        if (nf90_get_att(file_id, NF90_GLOBAL, first, value) /= NF90_NOERR) &
            error stop "read first long global attribute"
        if (value /= "first") error stop "first long global attribute differs"
        if (nf90_get_att(file_id, NF90_GLOBAL, second, value) /= NF90_NOERR) &
            error stop "read second long global attribute"
        if (value /= "second") error stop "second long global attribute differs"
        if (nf90_inq_varid(file_id, "scalar", variable_id) /= NF90_NOERR) &
            error stop "find long attribute names variable"
        if (nf90_get_att(file_id, variable_id, first, number) /= NF90_NOERR) &
            error stop "read first long variable attribute"
        if (number /= 17_int32) error stop "first long variable attribute differs"
        if (nf90_get_att(file_id, variable_id, second, number) /= NF90_NOERR) &
            error stop "read second long variable attribute"
        if (number /= 29_int32) error stop "second long variable attribute differs"
        if (nf90_inquire_attribute(file_id, NF90_GLOBAL, prefix) /= NF90_ENOTATT) &
            error stop "long attribute prefix aliases a complete name"
        if (nf90_close(file_id) /= NF90_NOERR) error stop "close long names reader"
    end subroutine long_attribute_names
end program test_netcdf_attributes
