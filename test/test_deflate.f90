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

    call check_stored_block()
    call check_empty_stream()

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

    !> zlib.compress(b"", 1): the decoded stream must be empty.
    subroutine check_empty_stream()
        integer(int8), allocatable :: decoded(:)
        type(fortio_status_t) :: st

        call decompress_zlib(int([120, 1, 3, 0, 0, 0, 0, 1], int8), decoded, st)
        if (.not. st%ok()) error stop trim(st%message)
        if (size(decoded) /= 0) error stop "empty zlib stream decodes to bytes"
    end subroutine check_empty_stream

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

    !> A zlib stream of one stored block (RFC 1950 and RFC 1951, 3.2.4) with a length
    !> above 255, so both bytes of LEN and NLEN matter.
    subroutine check_stored_block()
        integer(int8) :: stream(2 + 5 + 300 + 4), payload(300)
        integer(int8), allocatable :: decoded(:)
        type(fortio_status_t) :: st
        integer :: k, a, b

        payload = [(int(mod(7 * k, 251) - 125, int8), k=1, size(payload))]
        a = 1
        b = 0
        do k = 1, size(payload)
            a = mod(a + iand(int(payload(k)), 255), 65521)
            b = mod(b + a, 65521)
        end do
        stream(1:2) = [int(z'78', int8), int(z'01', int8)]
        stream(3) = 1_int8
        stream(4:7) = [int(44, int8), int(1, int8), int(-45, int8), int(-2, int8)]
        stream(8:307) = payload
        stream(308:311) = [int(b / 256 - merge(256, 0, b / 256 > 127), int8), &
            int(mod(b, 256) - merge(256, 0, mod(b, 256) > 127), int8), &
            int(a / 256 - merge(256, 0, a / 256 > 127), int8), &
            int(mod(a, 256) - merge(256, 0, mod(a, 256) > 127), int8)]
        call decompress_zlib(stream, decoded, st)
        if (.not. st%ok()) error stop "stored block: " // st%message
        if (size(decoded) /= size(payload)) error stop "stored block length"
        if (any(decoded /= payload)) error stop "stored block content"
    end subroutine check_stored_block

end program test_deflate
