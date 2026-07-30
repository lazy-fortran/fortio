program netcdf4_threaded
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use netcdf, only: NF90_CLOBBER, NF90_DOUBLE, NF90_NETCDF4, NF90_NOERR, &
        nf90_close, nf90_create, nf90_def_dim, nf90_def_var, &
        nf90_def_var_deflate, nf90_enddef, nf90_put_var
    implicit none

    integer, parameter :: files = 16, nx = 512, ny = 512
    character(len=512) :: prefix
    integer(int64) :: tick_begin, tick_end, tick_rate
    integer :: file
    real(real64) :: elapsed

    call get_command_argument(1, prefix)
    if (len_trim(prefix) == 0) prefix = "netcdf4-threaded"

    call system_clock(tick_begin, tick_rate)
    !$omp parallel do default(none) shared(prefix) private(file) schedule(static)
    do file = 1, files
        call write_file(prefix, file)
    end do
    !$omp end parallel do
    call system_clock(tick_end)

    elapsed = real(tick_end - tick_begin, real64)/real(tick_rate, real64)
    write (*, '(a,es24.16,1x,a,es24.16)') "seconds=", elapsed, &
        "checksum=", real(files*nx*ny, real64)

contains

    subroutine write_file(output_prefix, file_number)
        character(len=*), intent(in) :: output_prefix
        integer, intent(in) :: file_number
        character(len=640) :: path
        integer :: dim_x, dim_y, i, j, ncid, status, variable
        real(real64) :: values(nx, ny)

        do j = 1, ny
            do i = 1, nx
                values(i, j) = real(i + j + file_number, real64)
            end do
        end do
        write (path, '(a,"-",i0,".nc")') trim(output_prefix), file_number
        status = nf90_create(trim(path), ior(NF90_CLOBBER, NF90_NETCDF4), ncid)
        call require_success(status)
        status = nf90_def_dim(ncid, "x", nx, dim_x)
        call require_success(status)
        status = nf90_def_dim(ncid, "y", ny, dim_y)
        call require_success(status)
        status = nf90_def_var(ncid, "values", NF90_DOUBLE, [dim_x, dim_y], variable)
        call require_success(status)
        status = nf90_def_var_deflate(ncid, variable, 1, 1, 4)
        call require_success(status)
        status = nf90_enddef(ncid)
        call require_success(status)
        status = nf90_put_var(ncid, variable, values)
        call require_success(status)
        status = nf90_close(ncid)
        call require_success(status)
    end subroutine write_file

    subroutine require_success(status)
        integer, intent(in) :: status

        if (status /= NF90_NOERR) error stop "threaded compressed write failed"
    end subroutine require_success

end program netcdf4_threaded
