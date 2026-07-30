program test_concurrent_netcdf4_writes
    use, intrinsic :: iso_fortran_env, only: real64
    use netcdf, only: NF90_CLOBBER, NF90_DOUBLE, NF90_INT, NF90_NETCDF4, NF90_NOERR, &
        nf90_close, nf90_create, nf90_def_dim, nf90_def_var, &
        nf90_def_var_deflate, nf90_enddef, nf90_put_var
    implicit none

    integer, parameter :: files = 16
    character(len=512) :: prefix, verifier
    integer :: command_status, file

    call get_command_argument(1, prefix)
    call get_command_argument(2, verifier)
    if (len_trim(prefix) == 0) prefix = "concurrent-netcdf4"
    if (len_trim(verifier) == 0) &
        verifier = "test/fixtures/verify_netcdf4_deflate.py"

    !$omp parallel do default(none) shared(prefix) private(file)
    do file = 1, files
        call write_file(prefix, file)
    end do
    !$omp end parallel do

    do file = 1, files
        call execute_command_line("python3 "//trim(verifier)//" "// &
            file_path(prefix, file), exitstat=command_status)
        if (command_status /= 0) &
            error stop "independent concurrent NetCDF-4 oracle failed"
    end do

contains

    subroutine write_file(output_prefix, file_number)
        character(len=*), intent(in) :: output_prefix
        integer, intent(in) :: file_number
        integer :: dim_particle, dim_timestep, i, j, ncid, status
        integer :: var_field, var_particle, var_timestep
        integer :: particle(64), timestep(32)
        real(real64) :: values(64, 32)

        do j = 1, size(values, 2)
            do i = 1, size(values, 1)
                values(i, j) = real(i + 10*j, real64)
            end do
        end do
        particle = [(i - 1, i=1, size(particle))]
        timestep = [(i - 1, i=1, size(timestep))]

        status = nf90_create(file_path(output_prefix, file_number), &
            ior(NF90_CLOBBER, NF90_NETCDF4), ncid)
        call require_success(status)
        status = nf90_def_dim(ncid, "particle", size(particle), dim_particle)
        call require_success(status)
        status = nf90_def_dim(ncid, "timestep", size(timestep), dim_timestep)
        call require_success(status)
        status = nf90_def_var(ncid, "particle", NF90_INT, dim_particle, var_particle)
        call require_success(status)
        status = nf90_def_var(ncid, "timestep", NF90_INT, dim_timestep, var_timestep)
        call require_success(status)
        status = nf90_def_var(ncid, "field", NF90_DOUBLE, &
            [dim_particle, dim_timestep], var_field)
        call require_success(status)
        status = nf90_def_var_deflate(ncid, var_field, 1, 1, 4)
        call require_success(status)
        status = nf90_enddef(ncid)
        call require_success(status)
        status = nf90_put_var(ncid, var_particle, particle)
        call require_success(status)
        status = nf90_put_var(ncid, var_timestep, timestep)
        call require_success(status)
        status = nf90_put_var(ncid, var_field, values)
        call require_success(status)
        status = nf90_close(ncid)
        call require_success(status)
    end subroutine write_file

    pure function file_path(output_prefix, file_number) result(path)
        character(len=*), intent(in) :: output_prefix
        integer, intent(in) :: file_number
        character(len=640) :: path

        write (path, '(a,"-",i0,".nc")') trim(output_prefix), file_number
    end function file_path

    subroutine require_success(status)
        integer, intent(in) :: status

        if (status /= NF90_NOERR) error stop "concurrent NetCDF-4 write failed"
    end subroutine require_success

end program test_concurrent_netcdf4_writes
