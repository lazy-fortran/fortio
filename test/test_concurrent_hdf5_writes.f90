program test_concurrent_hdf5_writes
    use hdf5_tools, only: HID_T, h5_add, h5_close, h5_create, h5_open_rw
    implicit none

    integer, parameter :: repetitions = 64
    character(len=1024) :: path, verifier
    character(len=4096) :: command
    integer :: command_status, iteration
    integer(HID_T) :: file_id

    call get_command_argument(1, path)
    call get_command_argument(2, verifier)
    if (len_trim(path) == 0) path = "build/concurrent-writes.h5"
    if (len_trim(verifier) == 0) &
        verifier = "test/fixtures/verify_thread_writes.py"

    call h5_create(trim(path), file_id)
    call h5_close(file_id)
    !$omp parallel do default(none) shared(path) private(iteration)
    do iteration = 1, repetitions
        call write_value(path, iteration)
    end do
    !$omp end parallel do

    command = "python3 "//trim(verifier)//" "//trim(path)//" 64"
    call execute_command_line(trim(command), exitstat=command_status)
    if (command_status /= 0) error stop "concurrent same-file writes lost data"

contains

    subroutine write_value(output_path, value)
        character(len=*), intent(in) :: output_path
        integer, intent(in) :: value
        character(len=32) :: dataset
        integer(HID_T) :: output_id

        write (dataset, '("thread_",i0)') value
        call h5_open_rw(output_path, output_id)
        call h5_add(output_id, trim(dataset), value)
        call h5_close(output_id)
    end subroutine write_value

end program test_concurrent_hdf5_writes
