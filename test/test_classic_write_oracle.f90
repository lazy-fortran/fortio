program test_classic_write_oracle
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use netcdf, only: nf90_create, nf90_def_dim, nf90_def_var, nf90_enddef, &
                      nf90_put_var, nf90_close, NF90_CLOBBER, NF90_DOUBLE, &
                      NF90_INT, NF90_NOERR
    implicit none

    integer :: ncid, x_dim, y_dim, scalar_var, x_var, matrix_var, status
    character(len=1024) :: path
    real(real64) :: x(3), matrix(3, 2)

    call get_command_argument(1, path)
    if (len_trim(path) == 0) path = "build/fortio-written.nc"

    status = nf90_create(trim(path), NF90_CLOBBER, ncid)
    if (status /= NF90_NOERR) error stop "create"
    status = nf90_def_dim(ncid, "x", 3, x_dim)
    if (status /= NF90_NOERR) error stop "define x"
    status = nf90_def_dim(ncid, "y", 2, y_dim)
    if (status /= NF90_NOERR) error stop "define y"
    status = nf90_def_var(ncid, "scalar", NF90_INT, scalar_var)
    if (status /= NF90_NOERR) error stop "define scalar"
    status = nf90_def_var(ncid, "x_values", NF90_DOUBLE, [x_dim], x_var)
    if (status /= NF90_NOERR) error stop "define x values"
    status = nf90_def_var(ncid, "matrix", NF90_DOUBLE, [x_dim, y_dim], matrix_var)
    if (status /= NF90_NOERR) error stop "define matrix"
    status = nf90_enddef(ncid)
    if (status /= NF90_NOERR) error stop "end definition"

    x = [1.25_real64, -2.5_real64, 4.75_real64]
    matrix = reshape([1, 2, 3, 4, 5, 6], shape(matrix))
    status = nf90_put_var(ncid, scalar_var, 42_int32)
    if (status /= NF90_NOERR) error stop "put scalar"
    status = nf90_put_var(ncid, x_var, x)
    if (status /= NF90_NOERR) error stop "put x"
    status = nf90_put_var(ncid, matrix_var, matrix)
    if (status /= NF90_NOERR) error stop "put matrix"
    status = nf90_close(ncid)
    if (status /= NF90_NOERR) error stop "close"
end program test_classic_write_oracle
