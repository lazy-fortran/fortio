program test_netcdf4_slot_reuse
    ! A plain HDF5 file with a complex (compound) dataset, opened through
    ! nf90_open, must not leave NetCDF-4 metadata in its released slot, so a
    ! later NetCDF-4 open that reuses the slot succeeds.
    use, intrinsic :: iso_fortran_env, only: real64
    use hdf5_tools, only: hid_t, h5_init, h5_deinit, h5_create, h5_add, h5_close
    use netcdf, only: NF90_NOERR, NF90_NOWRITE, nf90_open, nf90_close
    implicit none

    character(len=1024) :: plain, fixture, cdl, command
    integer(hid_t) :: h5id
    real(real64) :: u(3, 2)
    complex(real64) :: c(3, 2)
    integer :: ncid, command_status, k

    call get_command_argument(1, plain)
    if (len_trim(plain) == 0) plain = "build/slot-reuse-plain.h5"
    call get_command_argument(2, fixture)
    if (len_trim(fixture) == 0) fixture = "build/slot-reuse-netcdf4.nc"
    call get_command_argument(3, cdl)
    if (len_trim(cdl) == 0) cdl = "test/fixtures/netcdf4-oracle.cdl"
    u = 1.0_real64
    call h5_init()
    call h5_create(trim(plain), h5id)
    call h5_add(h5id, 'u', u, lbound(u), ubound(u))
    call h5_add(h5id, 'n', 3)
    c = (1.0_real64, 2.0_real64)
    call h5_add(h5id, 'c', c, lbound(c), ubound(c))
    call h5_close(h5id)
    call h5_deinit()
    command = "ncgen -k netCDF-4 -o "//trim(fixture)//" "//trim(cdl)
    call execute_command_line(trim(command), exitstat=command_status)
    if (command_status /= 0) error stop "NetCDF-4 fixture generation failed"

    do k = 1, 2
        if (nf90_open(trim(plain), NF90_NOWRITE, ncid) == NF90_NOERR) then
            if (nf90_close(ncid) /= NF90_NOERR) error stop "plain HDF5 close failed"
        end if
        if (nf90_open(trim(fixture), NF90_NOWRITE, ncid) /= NF90_NOERR) &
            error stop "NetCDF-4 open after a rejected HDF5 file failed"
        if (nf90_close(ncid) /= NF90_NOERR) error stop "NetCDF-4 close failed"
    end do
    print *, "PASS"
end program test_netcdf4_slot_reuse
