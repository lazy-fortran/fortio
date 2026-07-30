program test_netcdf_hyperslab
    use, intrinsic :: iso_fortran_env, only: real64
    use netcdf, only: NF90_NOERR, NF90_NOWRITE, nf90_close, nf90_get_var, &
                     nf90_inq_varid, nf90_open
    implicit none

    character(len=1024) :: fixture, cdl, command
    integer :: ncid, varid, command_status
    real(real64) :: slab(2, 2, 1), vector(2)
    logical :: fixture_exists

    call get_command_argument(1, fixture)
    if (len_trim(fixture) == 0) fixture = "build/hyperslab-oracle.nc"
    call get_command_argument(2, cdl)
    if (len_trim(cdl) == 0) cdl = "test/fixtures/oracle.cdl"
    inquire (file=trim(fixture), exist=fixture_exists)
    if (.not. fixture_exists) then
        command = "ncgen -k classic -o "//trim(fixture)//" "//trim(cdl)
        call execute_command_line(trim(command), exitstat=command_status)
        if (command_status /= 0) error stop "ncgen hyperslab oracle generation failed"
    end if

    if (nf90_open(trim(fixture), NF90_NOWRITE, ncid) /= NF90_NOERR) error stop "open failed"
    if (nf90_inq_varid(ncid, "cube", varid) /= NF90_NOERR) error stop "cube lookup failed"
    if (nf90_get_var(ncid, varid, slab, start=[2, 1, 2], count=[2, 2, 1]) /= &
        NF90_NOERR) error stop "rank-3 hyperslab read failed"
    if (any(abs(slab - reshape([8, 9, 11, 12], [2, 2, 1])) > 1.0e-12_real64)) &
        error stop "rank-3 hyperslab differs from ncgen oracle"

    if (nf90_inq_varid(ncid, "x_values", varid) /= NF90_NOERR) &
        error stop "vector lookup failed"
    if (nf90_get_var(ncid, varid, vector, start=[2], count=[2]) /= NF90_NOERR) &
        error stop "rank-1 hyperslab read failed"
    if (any(abs(vector - [-2.5_real64, 4.75_real64]) > 1.0e-12_real64)) &
        error stop "rank-1 hyperslab differs from ncgen oracle"
    if (nf90_close(ncid) /= NF90_NOERR) error stop "close failed"
end program test_netcdf_hyperslab
