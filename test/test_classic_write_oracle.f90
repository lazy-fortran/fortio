program test_classic_write_oracle
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use netcdf, only: nf90_create, nf90_def_dim, nf90_def_var, nf90_enddef, &
                      nf90_put_att, nf90_put_var, nf90_close, NF90_CLOBBER, &
                      NF90_CHAR, NF90_DOUBLE, NF90_EEXIST, NF90_GLOBAL, NF90_INT, &
                      NF90_NETCDF4, NF90_NOCLOBBER, NF90_NOERR, nf90_inq_varid
    implicit none

    integer :: ncid, x_dim, y_dim, string_dim, group_dim
    integer :: scalar_var, x_var, matrix_var, group_var, mode_var, status
    character(len=1024) :: path, command, line
    real(real64) :: x(3), matrix(3, 2)
    integer :: unit, io_status, command_status
    logical :: found_global, found_units, found_bounds, found_groups, found_mode
    character(len=5) :: groups(2)

    call get_command_argument(1, path)
    if (len_trim(path) == 0) path = "build/fortio-written.nc"

    status = nf90_create(trim(path), ior(NF90_CLOBBER, NF90_NETCDF4), ncid)
    if (status /= NF90_NOERR) error stop "create"
    status = nf90_def_dim(ncid, "x", 3, x_dim)
    if (status /= NF90_NOERR) error stop "define x"
    status = nf90_def_dim(ncid, "y", 2, y_dim)
    if (status /= NF90_NOERR) error stop "define y"
    status = nf90_def_dim(ncid, "string_length", 5, string_dim)
    if (status /= NF90_NOERR) error stop "define string length"
    status = nf90_def_dim(ncid, "group", 2, group_dim)
    if (status /= NF90_NOERR) error stop "define group"
    status = nf90_def_var(ncid, "scalar", NF90_INT, scalar_var)
    if (status /= NF90_NOERR) error stop "define scalar"
    status = nf90_def_var(ncid, "x_values", NF90_DOUBLE, [x_dim], x_var)
    if (status /= NF90_NOERR) error stop "define x values"
    status = nf90_def_var(ncid, "matrix", NF90_DOUBLE, [x_dim, y_dim], matrix_var)
    if (status /= NF90_NOERR) error stop "define matrix"
    status = nf90_def_var(ncid, "coil_group", NF90_CHAR, [string_dim, group_dim], group_var)
    if (status /= NF90_NOERR) error stop "define character array"
    status = nf90_def_var(ncid, "mgrid_mode", NF90_CHAR, [group_dim], mode_var)
    if (status /= NF90_NOERR) error stop "define character vector"
    status = nf90_put_att(ncid, NF90_GLOBAL, "coordinate_system", "cartesian")
    if (status /= NF90_NOERR) error stop "put global text attribute"
    status = nf90_put_att(ncid, x_var, "units", "m")
    if (status /= NF90_NOERR) error stop "put variable text attribute"
    status = nf90_put_att(ncid, matrix_var, "lbound", [1_int32, -2_int32])
    if (status /= NF90_NOERR) error stop "put integer vector attribute"
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
    groups = ["alpha", "beta "]
    status = nf90_inq_varid(ncid, "coil_group", group_var)
    if (status /= NF90_NOERR) error stop "inquire writer character array"
    status = nf90_put_var(ncid, group_var, groups)
    if (status /= NF90_NOERR) error stop "put character array"
    status = nf90_inq_varid(ncid, "mgrid_mode", mode_var)
    if (status /= NF90_NOERR) error stop "inquire writer character vector"
    status = nf90_put_var(ncid, mode_var, "RS")
    if (status /= NF90_NOERR) error stop "put character vector"
    status = nf90_close(ncid)
    if (status /= NF90_NOERR) error stop "close"
    status = nf90_create(trim(path), ior(NF90_NOCLOBBER, NF90_NETCDF4), ncid)
    if (status /= NF90_EEXIST) error stop "noclobber did not preserve existing file"

    command = "ncdump -v coil_group,mgrid_mode "//trim(path)//" > "//trim(path)//".header"
    call execute_command_line(trim(command), exitstat=command_status)
    if (command_status /= 0) error stop "ncdump rejected fortio attribute encoding"
    open(newunit=unit, file=trim(path)//".header", status="old", action="read")
    found_global = .false.
    found_units = .false.
    found_bounds = .false.
    found_groups = .false.
    found_mode = .false.
    do
        read(unit, '(A)', iostat=io_status) line
        if (io_status /= 0) exit
        if (index(line, ':coordinate_system = "cartesian"') > 0) found_global = .true.
        if (index(line, 'x_values:units = "m"') > 0) found_units = .true.
        if (index(line, 'matrix:lbound = 1, -2') > 0) found_bounds = .true.
        if (index(line, '"alpha",') > 0) found_groups = .true.
        if (index(line, 'mgrid_mode = "RS"') > 0) found_mode = .true.
    end do
    close(unit)
    if (.not. found_global) error stop "ncdump global attribute differs"
    if (.not. found_units) error stop "ncdump variable attribute differs"
    if (.not. found_bounds) error stop "ncdump integer attribute differs"
    if (.not. found_groups) error stop "ncdump character array differs"
    if (.not. found_mode) error stop "ncdump character vector differs"
end program test_classic_write_oracle
