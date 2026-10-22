program test_bytes
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
    use fortio_bytes, only: byte_writer_t, decode_be_i16, decode_be_i32, decode_be_i64
    use fortio_status, only: fortio_status_t
    implicit none

    integer(int8) :: b2(2), b4(4), b8(8)

    b2 = [int(z'12', int8), int(z'34', int8)]
    if (decode_be_i16(b2) /= int(z'1234', int16)) error stop "i16 decoding"

    b4 = [int(z'89', int8), int(z'ab', int8), int(z'cd', int8), int(z'ef', int8)]
    if (decode_be_i32(b4) /= int(z'89abcdef', int32)) error stop "i32 decoding"

    b8 = [int(z'01', int8), int(z'23', int8), int(z'45', int8), int(z'67', int8), &
          int(z'89', int8), int(z'ab', int8), int(z'cd', int8), int(z'ef', int8)]
    if (decode_be_i64(b8) /= int(z'0123456789abcdef', int64)) error stop "i64 decoding"

    call check_reset_length()

contains

    subroutine check_reset_length()
        type(byte_writer_t) :: writer
        type(fortio_status_t) :: status
        integer(int8) :: received(16)
        integer :: unit
        integer(int64) :: file_size
        character(len=*), parameter :: path = "fortio-bytes-reset.bin"

        call writer%open(path, status)
        if (.not. status%ok()) error stop status%message
        call writer%write_bytes(b8, status)
        if (.not. status%ok()) error stop status%message
        call writer%reset(status, 4_int64)
        if (.not. status%ok()) error stop status%message
        if (writer%position /= 1_int64) error stop "reset did not rewind"
        call writer%write_bytes(b2, status)
        if (.not. status%ok()) error stop status%message
        call writer%close(status)
        if (.not. status%ok()) error stop status%message

        inquire(file=path, size=file_size)
        if (file_size /= 4_int64) error stop "reset did not shrink output"
        open(newunit=unit, file=path, access="stream", form="unformatted", status="old")
        read(unit) received(:4)
        close(unit)
        if (any(received(:4) /= [b2, b8(3:4)])) &
            error stop "reset discarded retained bytes"

        call writer%reopen(path, status)
        if (.not. status%ok()) error stop status%message
        call writer%reset(status, 16_int64)
        if (.not. status%ok()) error stop status%message
        call writer%close(status)
        if (.not. status%ok()) error stop status%message
        inquire(file=path, size=file_size)
        if (file_size /= 16_int64) error stop "reset did not grow output"
        open(newunit=unit, file=path, access="stream", form="unformatted", status="old")
        read(unit) received
        close(unit)
        if (any(received(:4) /= [b2, b8(3:4)])) &
            error stop "growing reset discarded retained bytes"
        if (any(received(5:) /= 0_int8)) error stop "reset extension was not zero filled"

        call writer%reopen(path, status)
        if (.not. status%ok()) error stop status%message
        call writer%reset(status)
        if (.not. status%ok()) error stop status%message
        call writer%close(status)
        if (.not. status%ok()) error stop status%message
        inquire(file=path, size=file_size)
        if (file_size /= 0_int64) error stop "default reset did not truncate output"
        open(newunit=unit, file=path, status="old")
        close(unit, status="delete")
    end subroutine check_reset_length
end program test_bytes
