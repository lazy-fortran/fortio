program test_deflate
    use, intrinsic :: iso_fortran_env, only: int8, real64
    use fortio_compression, only: compress_zlib, decompress_zlib
    use fortio_deflate, only: deflate_compress, deflate_uncompress, shuffle_bytes, &
        unshuffle_bytes
    use fortio_status, only: fortio_status_t
    implicit none

    integer(int8) :: input(4096)
    integer(int8), allocatable :: compressed(:), output(:), raw(:), shuffled(:)
    type(fortio_status_t) :: status
    real(real64) :: values(64, 32)
    character(len=512) :: compressed_path, oracle_path, raw_path, script
    integer :: command_status, i, j

    input = [(int(mod(i, 127), int8), i=1, size(input))]
    call deflate_compress(input, 4, compressed, status)
    if (.not. status%ok()) error stop trim(status%message)
    call deflate_uncompress(compressed, int(size(input), kind=8), output, status)
    if (.not. status%ok()) error stop trim(status%message)
    if (any(output /= input)) error stop "deflate roundtrip differs"

    do j = 1, size(values, 2)
        do i = 1, size(values, 1)
            values(i, j) = real(i + 10*j, real64)
        end do
    end do
    raw = transfer(values, raw, size(values)*storage_size(values)/8)
    call shuffle_bytes(raw, 8, shuffled)
    call deflate_compress(shuffled, 4, compressed, status)
    if (.not. status%ok()) error stop trim(status%message)
    call deflate_uncompress(compressed, int(size(shuffled), kind=8), output, status)
    if (.not. status%ok()) error stop trim(status%message)
    call unshuffle_bytes(output, 8, raw)
    if (any(reshape(transfer(raw, [0.0_real64]), shape(values)) /= values)) &
        error stop "shuffled roundtrip differs"

    call get_command_argument(1, compressed_path)
    call get_command_argument(2, raw_path)
    call get_command_argument(3, oracle_path)
    call get_command_argument(4, script)
    if (len_trim(script) == 0) stop
    call execute_command_line("python3 "//trim(script)//" generate "// &
        trim(oracle_path)//" "//trim(raw_path), exitstat=command_status)
    if (command_status /= 0) error stop "dynamic zlib oracle generation failed"
    call read_file(trim(oracle_path), compressed)
    call read_file(trim(raw_path), raw)
    call decompress_zlib(compressed, output, status)
    if (.not. status%ok()) error stop trim(status%message)
    if (any(output /= raw)) error stop "dynamic zlib oracle differs"
    call compress_zlib(raw, compressed, status, level=4)
    if (.not. status%ok()) error stop trim(status%message)
    call write_file(trim(compressed_path), compressed)
    call execute_command_line("python3 "//trim(script)//" verify "// &
        trim(compressed_path)//" "//trim(raw_path), exitstat=command_status)
    if (command_status /= 0) error stop "Python rejected native zlib stream"

contains

    subroutine read_file(path, bytes)
        character(len=*), intent(in) :: path
        integer(int8), allocatable, intent(out) :: bytes(:)
        integer(kind=8) :: length
        integer :: io_status, unit

        inquire(file=path, size=length, iostat=io_status)
        if (io_status /= 0) error stop "cannot inspect compression fixture"
        allocate(bytes(length))
        open(newunit=unit, file=path, access="stream", form="unformatted", &
            status="old", action="read", iostat=io_status)
        if (io_status /= 0) error stop "cannot open compression fixture"
        if (length > 0) read(unit, iostat=io_status) bytes
        close(unit)
        if (io_status /= 0) error stop "cannot read compression fixture"
    end subroutine read_file

    subroutine write_file(path, bytes)
        character(len=*), intent(in) :: path
        integer(int8), intent(in) :: bytes(:)
        integer :: io_status, unit

        open(newunit=unit, file=path, access="stream", form="unformatted", &
            status="replace", action="write", iostat=io_status)
        if (io_status /= 0) error stop "cannot create compression fixture"
        if (size(bytes) > 0) write(unit, iostat=io_status) bytes
        close(unit)
        if (io_status /= 0) error stop "cannot write compression fixture"
    end subroutine write_file

end program test_deflate
