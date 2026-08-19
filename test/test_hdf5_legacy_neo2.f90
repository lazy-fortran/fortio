program test_hdf5_legacy_neo2
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use fortio, only: fortio_file_t, fortio_status_t, hdf5_attribute_t
    implicit none

    type(fortio_file_t) :: file
    type(fortio_status_t) :: status
    type(hdf5_attribute_t), allocatable :: attributes(:)
    character(len=1024) :: fixture, generator, command, mode
    integer(int32) :: radial_points, species
    real(real64), allocatable :: boozer_s(:), profile(:, :)
    integer :: command_status
    logical :: fixture_exists

    call get_command_argument(1, fixture)
    if (len_trim(fixture) == 0) fixture = "build/legacy-neo2.h5"
    call get_command_argument(2, generator)
    if (len_trim(generator) == 0) generator = "test/fixtures/make_hdf5_legacy_neo2.py"
    call get_command_argument(3, mode)
    inquire (file=trim(fixture), exist=fixture_exists)
    if (.not. fixture_exists) then
        command = "python3 "//trim(generator)//" "//trim(fixture)
        call execute_command_line(trim(command), exitstat=command_status)
        if (command_status /= 0) error stop "legacy NEO-2 HDF5 fixture generation failed"
    end if

    call file%open(trim(fixture), status)
    if (.not. status%ok()) error stop status%message

    if (trim(mode) == "supplier") then
        call file%read("/num_radial_pts", radial_points, status)
        if (.not. status%ok()) error stop status%message
        call file%read("/num_species", species, status)
        if (.not. status%ok()) error stop status%message
        if (radial_points /= 56 .or. species /= 2) &
            error stop "supplier NEO-2 scalar values differ"
        call file%read("/boozer_s", boozer_s, status)
        if (.not. status%ok()) error stop status%message
        if (size(boozer_s) /= 56 .or. abs(boozer_s(1) - 0.01_real64) > 1.0e-12_real64) &
            error stop "supplier NEO-2 radial grid differs"
        call file%read("/T_prof", profile, status)
        if (.not. status%ok()) error stop status%message
        if (any(shape(profile) /= [56, 2])) error stop "supplier NEO-2 profile shape differs"
        call file%get_attributes("/boozer_s", attributes, status)
        if (.not. status%ok()) error stop status%message
        call file%close(status)
        if (.not. status%ok()) error stop status%message
        stop
    end if

    call file%read("/num_radial_pts", radial_points, status)
    if (.not. status%ok()) error stop status%message
    call file%read("/num_species", species, status)
    if (.not. status%ok()) error stop status%message
    if (radial_points /= 3 .or. species /= 2) &
        error stop "legacy NEO-2 scalar values differ from oracle"

    call file%read("/boozer_s", boozer_s, status)
    if (.not. status%ok()) error stop status%message
    if (any(abs(boozer_s - [0.0_real64, 0.25_real64, 1.0_real64]) > 1.0e-12_real64)) &
        error stop "legacy NEO-2 vector values differ from oracle"

    call file%read("/T_prof", profile, status)
    if (.not. status%ok()) error stop status%message
    if (any(abs(profile - reshape([1.0_real64, 2.0_real64, 3.0_real64, &
        4.0_real64, 5.0_real64, 6.0_real64], [3, 2])) > 1.0e-12_real64)) &
        error stop "legacy NEO-2 matrix values differ from oracle"

    call file%get_attributes("/boozer_s", attributes, status)
    if (.not. status%ok()) error stop status%message

    call file%close(status)
    if (.not. status%ok()) error stop status%message
end program test_hdf5_legacy_neo2
