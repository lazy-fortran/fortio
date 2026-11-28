program test_h5write
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use fortio_status, only: fortio_status_t
    use fortio_string, only: string_t, string, assignment(=), operator(//)
    use fortio_h5write, only: h5_writer_t
    implicit none

    type(h5_writer_t) :: h5
    type(fortio_status_t) :: st
    character(len=4096) :: buffer
    type(string_t) :: path, script
    integer :: n, exit_code, i
    real(real64) :: grid(6), field(3 * 6)
    type(string_t), allocatable :: labels(:)

    call get_command_argument(1, buffer, n)
    path = buffer(1:n)
    call get_command_argument(2, buffer, n)
    script = buffer(1:n)

    grid = [(0.2_real64 * i, i=0, 5)]
    field = [(real(i, real64) / 7, i=1, 18)]
    labels = [string("H"), string("He"), string("")]

    call h5%create(path%str(), st)
    call check()
    call h5%attribute("/", "Conventions", "IMAS", st)
    call check()
    call h5%attribute("/", "data_dictionary_version", "4.1.1", st)
    call check()
    call h5%create_group("/core_profiles/0", st)
    call check()
    call h5%dimension("/core_profiles/0/time", 3_int64, st)
    call check()
    call h5%write("/core_profiles/0/profiles_1d.grid.rho_tor_norm:i", grid, st)
    call check()
    call h5%write("/core_profiles/0/profiles_1d.j_tor", field, st, dims=[3_int64, 6_int64])
    call check()
    call h5%attach_scales("/core_profiles/0/profiles_1d.j_tor", &
        [string("/core_profiles/0/time"), string("/core_profiles/0/profiles_1d.grid.rho_tor_norm:i")], st)
    call check()
    call h5%attribute("/core_profiles/0/profiles_1d.j_tor", "units", "A.m^-2", st)
    call check()
    call h5%attribute("/core_profiles/0/profiles_1d.j_tor", "_FillValue", [9.969209968386869e36_real64], st)
    call check()
    call h5%write("/core_profiles/0/profiles_1d.ion.label", labels, st)
    call check()
    call h5%write("/core_profiles/0/ids_properties.comment", [string("é text with UTF-8")], st, &
        dims=[integer(int64) ::])
    call check()
    call h5%write("/core_profiles/0/ids_properties.homogeneous_time", [1_int32], st, &
        dims=[integer(int64) ::])
    call check()
    call h5%write("/core_profiles/0/big_index", [(int(i, int64) * 3000000000_int64, i=1, 4)], st)
    call check()
    call h5%write("/core_profiles/0/anonymous_complex", [(1.0_real64, -2.0_real64), (0.5_real64, 4.0_real64)], st)
    call check()
    call h5%commit_complex("/_PFNC_DOUBLE_COMPLEX_TYPE", st)
    call check()
    call h5%write("/core_profiles/0/named_complex", [(3.0_real64, 1.5_real64)], st, &
        named_type="/_PFNC_DOUBLE_COMPLEX_TYPE")
    call check()
    call h5%write("/core_profiles/0/empty", [real(real64) ::], st, dims=[0_int64])
    call check()
    call h5%attribute("/core_profiles/0", "names", [string("a"), string("bcd")], st)
    call check()
    call h5%attribute("/core_profiles/0", "counts", [1_int64, -2_int64], st)
    call check()
    call h5%close(st)
    call check()
    call execute_command_line("python3 " // script%str() // " " // path%str(), exitstat=exit_code)
    if (exit_code /= 0) error stop "h5py or netCDF4 disagree with the written file"

contains

    subroutine check()
        if (.not. st%ok()) error stop st%message
    end subroutine check

end program test_h5write
