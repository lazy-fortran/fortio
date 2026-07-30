program test_netcdf4_read
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use netcdf, only: NF90_NOERR, NF90_NOWRITE, nf90_close, nf90_get_var, &
        nf90_inq_dimid, nf90_inq_varid, nf90_inquire_dimension, nf90_open
    implicit none

    character(len=1024) :: fixture, cdl, command
    integer(int32) :: count, indices(2)
    real(real64) :: radius_values(3), coefficients(3, 2)
    integer :: ncid, dimid, varid, length, command_status

    call get_command_argument(1, fixture)
    if (len_trim(fixture) == 0) fixture = "build/netcdf4-oracle.nc"
    call get_command_argument(2, cdl)
    if (len_trim(cdl) == 0) cdl = "test/fixtures/netcdf4-oracle.cdl"
    command = "ncgen -k netCDF-4 -o "//trim(fixture)//" "//trim(cdl)
    call execute_command_line(trim(command), exitstat=command_status)
    if (command_status /= 0) error stop "system ncgen NetCDF-4 generation failed"

    if (nf90_open(trim(fixture), NF90_NOWRITE, ncid) /= NF90_NOERR) &
        error stop "NetCDF-4 oracle open failed"
    if (nf90_inq_dimid(ncid, "radius", dimid) /= NF90_NOERR) &
        error stop "NetCDF-4 dimension lookup failed"
    if (nf90_inquire_dimension(ncid, dimid, len=length) /= NF90_NOERR) &
        error stop "NetCDF-4 dimension inquiry failed"
    if (length /= 3) error stop "NetCDF-4 dimension length differs"

    if (nf90_inq_varid(ncid, "count", varid) /= NF90_NOERR) &
        error stop "NetCDF-4 scalar lookup failed"
    if (nf90_get_var(ncid, varid, count) /= NF90_NOERR) &
        error stop "NetCDF-4 scalar read failed"
    if (count /= 3) error stop "NetCDF-4 scalar value differs"
    if (nf90_inq_varid(ncid, "indices", varid) /= NF90_NOERR) &
        error stop "NetCDF-4 integer vector lookup failed"
    if (nf90_get_var(ncid, varid, indices) /= NF90_NOERR) &
        error stop "NetCDF-4 integer vector read failed"
    if (any(indices /= [4_int32, 9_int32])) error stop "NetCDF-4 integer vector differs"
    if (nf90_inq_varid(ncid, "radius_values", varid) /= NF90_NOERR) &
        error stop "NetCDF-4 real vector lookup failed"
    if (nf90_get_var(ncid, varid, radius_values) /= NF90_NOERR) &
        error stop "NetCDF-4 real vector read failed"
    if (any(abs(radius_values - [1.25_real64, -2.5_real64, 4.75_real64]) > 1.0e-12_real64)) &
        error stop "NetCDF-4 real vector differs"
    if (nf90_inq_varid(ncid, "coefficients", varid) /= NF90_NOERR) &
        error stop "NetCDF-4 matrix lookup failed"
    if (nf90_get_var(ncid, varid, coefficients) /= NF90_NOERR) &
        error stop "NetCDF-4 matrix read failed"
    if (any(abs(coefficients - reshape([1, 2, 3, 4, 5, 6], [3, 2])) > 1.0e-12_real64)) &
        error stop "NetCDF-4 matrix differs"
    if (nf90_close(ncid) /= NF90_NOERR) error stop "NetCDF-4 oracle close failed"
end program test_netcdf4_read
