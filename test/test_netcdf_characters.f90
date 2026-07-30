program test_netcdf_characters
    use netcdf, only: NF90_NOERR, NF90_NOWRITE, nf90_close, nf90_get_var, &
                     nf90_inq_varid, nf90_open
    implicit none

    character(len=1024) :: fixture, cdl, command
    character(len=5) :: groups(2)
    character(len=1) :: mode
    integer :: ncid, varid, command_status
    logical :: fixture_exists

    call get_command_argument(1, fixture)
    if (len_trim(fixture) == 0) fixture = "build/character-oracle.nc"
    call get_command_argument(2, cdl)
    if (len_trim(cdl) == 0) cdl = "test/fixtures/oracle.cdl"
    inquire (file=trim(fixture), exist=fixture_exists)
    if (.not. fixture_exists) then
        command = "ncgen -k classic -o "//trim(fixture)//" "//trim(cdl)
        call execute_command_line(trim(command), exitstat=command_status)
        if (command_status /= 0) error stop "ncgen character oracle generation failed"
    end if

    if (nf90_open(trim(fixture), NF90_NOWRITE, ncid) /= NF90_NOERR) error stop "open failed"
    if (nf90_inq_varid(ncid, "coil_group", varid) /= NF90_NOERR) error stop "lookup failed"
    if (nf90_get_var(ncid, varid, groups) /= NF90_NOERR) error stop "string array read failed"
    if (groups(1) /= "alpha" .or. groups(2) /= "beta ") &
        error stop "string array differs from ncgen oracle"
    if (nf90_inq_varid(ncid, "mgrid_mode", varid) /= NF90_NOERR) error stop "lookup failed"
    if (nf90_get_var(ncid, varid, mode) /= NF90_NOERR) error stop "character read failed"
    if (mode /= "S") error stop "character differs from ncgen oracle"
    if (nf90_close(ncid) /= NF90_NOERR) error stop "close failed"
end program test_netcdf_characters
