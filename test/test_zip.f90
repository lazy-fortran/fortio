program test_zip
    use, intrinsic :: iso_fortran_env, only: int8
    use fortio, only: fortio_status_t, zip_writer_t
    implicit none

    type(zip_writer_t) :: archive
    type(fortio_status_t) :: status
    integer(int8) :: bytes(5)
    character(len=512) :: archive_path, script, source_path
    integer :: command_status, io_status, unit

    call get_command_argument(1, archive_path)
    call get_command_argument(2, source_path)
    call get_command_argument(3, script)
    if (len_trim(archive_path) == 0) archive_path = "fortio-test.zip"
    if (len_trim(source_path) == 0) source_path = "fortio-source.bin"
    if (len_trim(script) == 0) script = "test/fixtures/verify_zip.py"

    bytes = [0_int8, 1_int8, 2_int8, 126_int8, -1_int8]
    open(newunit=unit, file=trim(source_path), access="stream", form="unformatted", &
        status="replace", action="write", iostat=io_status)
    if (io_status /= 0) error stop "cannot create ZIP source fixture"
    write(unit, iostat=io_status) bytes
    close(unit)
    if (io_status /= 0) error stop "cannot write ZIP source fixture"

    call archive%open(trim(archive_path), status)
    if (.not. status%ok()) error stop trim(status%message)
    call archive%add("hello.txt", "Fortio ZIP"//new_line("a"), status)
    if (.not. status%ok()) error stop trim(status%message)
    call archive%add("empty.bin", [integer(int8) ::], status)
    if (.not. status%ok()) error stop trim(status%message)
    call archive%add("bytes.bin", bytes, status, level=4)
    if (.not. status%ok()) error stop trim(status%message)
    call archive%add_file(trim(source_path), status, archive_name="nested/source.bin")
    if (.not. status%ok()) error stop trim(status%message)
    call archive%close(status)
    if (.not. status%ok()) error stop trim(status%message)

    call execute_command_line("python3 "//trim(script)//" "//trim(archive_path), &
        exitstat=command_status)
    if (command_status /= 0) error stop "Python zipfile rejected Fortio archive"
end program test_zip
