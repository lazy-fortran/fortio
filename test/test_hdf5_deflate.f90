program test_hdf5_deflate
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use fortio_hdf5_writer, only: hdf5_writer_t
    use fortio_status, only: fortio_status_t
    implicit none

    type(hdf5_writer_t) :: writer
    type(fortio_status_t) :: status
    real(real64) :: values(64, 32)
    integer(int32) :: particle(64), timestep(32)
    character(len=512) :: path, verifier
    integer :: command_status, i, j

    call get_command_argument(1, path)
    call get_command_argument(2, verifier)
    if (len_trim(path) == 0) path = "fortio-hdf5-deflate.h5"
    if (len_trim(verifier) == 0) verifier = "test/fixtures/verify_hdf5_deflate.py"
    do j = 1, size(values, 2)
        do i = 1, size(values, 1)
            values(i, j) = real(i + 10*j, real64)
        end do
    end do
    particle = [(int(i - 1, int32), i=1, size(particle))]
    timestep = [(int(i - 1, int32), i=1, size(timestep))]

    call writer%create(trim(path), status)
    if (.not. status%ok()) error stop status%message
    call writer%add_i32_1("particle", particle, status)
    if (.not. status%ok()) error stop status%message
    call writer%mark_dimension_scale("particle", status)
    if (.not. status%ok()) error stop status%message
    call writer%add_i32_attribute("particle", "_Netcdf4Dimid", [0_int32], status)
    if (.not. status%ok()) error stop status%message
    call writer%add_i32_1("timestep", timestep, status)
    if (.not. status%ok()) error stop status%message
    call writer%mark_dimension_scale("timestep", status)
    if (.not. status%ok()) error stop status%message
    call writer%add_i32_attribute("timestep", "_Netcdf4Dimid", [1_int32], status)
    if (.not. status%ok()) error stop status%message
    call writer%add_r64_2("field", values, status)
    if (.not. status%ok()) error stop status%message
    call writer%set_dimension_list("field", ["particle", "timestep"], status)
    if (.not. status%ok()) error stop status%message
    call writer%set_deflate("field", .true., 4, status)
    if (.not. status%ok()) error stop status%message
    call writer%close(status)
    if (.not. status%ok()) error stop status%message

    call execute_command_line("python3 "//trim(verifier)//" "//trim(path), &
        exitstat=command_status)
    if (command_status /= 0) error stop "independent HDF5 deflate oracle failed"
end program test_hdf5_deflate
