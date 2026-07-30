program test_dense_attribute
    use, intrinsic :: iso_fortran_env, only: real64
    use netcdf, only: NF90_GLOBAL, NF90_NOERR, NF90_NOWRITE, nf90_close, &
        nf90_get_att, nf90_open
    implicit none
    character(len=1024) :: path, generator, command
    real(real64) :: torflux
    integer :: ncid, command_status

    call get_command_argument(1, path)
    call get_command_argument(2, generator)
    if (len_trim(path) == 0) path = "dense-attribute-oracle.h5"
    if (len_trim(generator) == 0) generator = "test/fixtures/make_hdf5_oracle.py"
    command = "python3 "//trim(generator)//" "//trim(path)
    call execute_command_line(trim(command), exitstat=command_status)
    if (command_status /= 0) error stop "system HDF5 generation failed"
    if (nf90_open(trim(path), NF90_NOWRITE, ncid) /= NF90_NOERR) error stop "open"
    if (nf90_get_att(ncid, NF90_GLOBAL, "torflux", torflux) /= NF90_NOERR) &
        error stop "torflux"
    if (abs(torflux + 803450571.635625_real64) > 1.0e-6_real64) error stop "value"
    if (nf90_close(ncid) /= NF90_NOERR) error stop "close"
end program test_dense_attribute
