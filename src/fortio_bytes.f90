module fortio_bytes
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    use fortio_status, only: fortio_status_t, FORTIO_EIO
    implicit none
    private

    type, public :: byte_reader_t
        integer :: unit = -1
        integer(int64) :: position = 1_int64
    contains
        procedure :: open => reader_open
        procedure :: close => reader_close
        procedure :: seek => reader_seek
        procedure :: read_i8 => reader_read_i8
        procedure :: read_be_i16 => reader_read_be_i16
        procedure :: read_be_i32 => reader_read_be_i32
        procedure :: read_be_i64 => reader_read_be_i64
        procedure :: read_be_r32 => reader_read_be_r32
        procedure :: read_be_r64 => reader_read_be_r64
        procedure :: read_le_i16 => reader_read_le_i16
        procedure :: read_le_i32 => reader_read_le_i32
        procedure :: read_le_i64 => reader_read_le_i64
        procedure :: read_le_r32 => reader_read_le_r32
        procedure :: read_le_r64 => reader_read_le_r64
        procedure :: read_bytes => reader_read_bytes
    end type byte_reader_t

    type, public :: byte_writer_t
        integer :: unit = -1
        integer(int64) :: position = 1_int64
    contains
        procedure :: open => writer_open
        procedure :: close => writer_close
        procedure :: seek => writer_seek
        procedure :: write_i8 => writer_write_i8
        procedure :: write_be_i16 => writer_write_be_i16
        procedure :: write_be_i32 => writer_write_be_i32
        procedure :: write_be_i64 => writer_write_be_i64
        procedure :: write_be_r32 => writer_write_be_r32
        procedure :: write_be_r64 => writer_write_be_r64
        procedure :: write_le_i32 => writer_write_le_i32
        procedure :: write_le_i64 => writer_write_le_i64
        procedure :: write_le_r32 => writer_write_le_r32
        procedure :: write_le_r64 => writer_write_le_r64
        procedure :: write_bytes => writer_write_bytes
    end type byte_writer_t

    public :: decode_be_i16, decode_be_i32, decode_be_i64

contains

    subroutine writer_open(this, path, status)
        class(byte_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status
        integer :: io_status
        character(len=512) :: io_message

        call status%clear()
        open(newunit=this%unit, file=path, access="stream", form="unformatted", &
            action="write", status="replace", iostat=io_status, iomsg=io_message)
        if (io_status /= 0) then
            call status%set(FORTIO_EIO, trim(io_message))
            this%unit = -1
            return
        end if
        this%position = 1_int64
    end subroutine writer_open

    subroutine writer_close(this, status)
        class(byte_writer_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status
        integer :: io_status
        character(len=512) :: io_message

        call status%clear()
        if (this%unit == -1) return
        close(this%unit, iostat=io_status, iomsg=io_message)
        if (io_status /= 0) call status%set(FORTIO_EIO, trim(io_message))
        this%unit = -1
        this%position = 1_int64
    end subroutine writer_close

    subroutine writer_seek(this, position)
        class(byte_writer_t), intent(inout) :: this
        integer(int64), intent(in) :: position

        this%position = position
    end subroutine writer_seek

    subroutine writer_write_bytes(this, values, status)
        class(byte_writer_t), intent(inout) :: this
        integer(int8), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer :: io_status
        character(len=512) :: io_message

        call status%clear()
        write(this%unit, pos=this%position, iostat=io_status, iomsg=io_message) values
        if (io_status /= 0) then
            call status%set(FORTIO_EIO, trim(io_message))
            return
        end if
        this%position = this%position + size(values, kind=int64)
    end subroutine writer_write_bytes

    subroutine writer_write_i8(this, value, status)
        class(byte_writer_t), intent(inout) :: this
        integer(int8), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status

        call this%write_bytes([value], status)
    end subroutine writer_write_i8

    subroutine writer_write_be_i16(this, value, status)
        class(byte_writer_t), intent(inout) :: this
        integer(int16), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: bytes(2)

        bytes(1) = int(iand(shiftr(value, 8), int(z'ff', int16)), int8)
        bytes(2) = int(iand(value, int(z'ff', int16)), int8)
        call this%write_bytes(bytes, status)
    end subroutine writer_write_be_i16

    subroutine writer_write_be_i32(this, value, status)
        class(byte_writer_t), intent(inout) :: this
        integer(int32), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: bytes(4)
        integer :: i

        do i = 1, 4
            bytes(i) = int(iand(shiftr(value, 8*(4 - i)), int(z'ff', int32)), int8)
        end do
        call this%write_bytes(bytes, status)
    end subroutine writer_write_be_i32

    subroutine writer_write_be_i64(this, value, status)
        class(byte_writer_t), intent(inout) :: this
        integer(int64), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: bytes(8)
        integer :: i

        do i = 1, 8
            bytes(i) = int(iand(shiftr(value, 8*(8 - i)), int(z'ff', int64)), int8)
        end do
        call this%write_bytes(bytes, status)
    end subroutine writer_write_be_i64

    subroutine writer_write_be_r32(this, value, status)
        class(byte_writer_t), intent(inout) :: this
        real(real32), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status

        call this%write_be_i32(transfer(value, 0_int32), status)
    end subroutine writer_write_be_r32

    subroutine writer_write_be_r64(this, value, status)
        class(byte_writer_t), intent(inout) :: this
        real(real64), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status

        call this%write_be_i64(transfer(value, 0_int64), status)
    end subroutine writer_write_be_r64

    subroutine writer_write_le_i32(this, value, status)
        class(byte_writer_t), intent(inout) :: this
        integer(int32), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: bytes(4)
        integer :: i

        do i = 1, 4
            bytes(i) = int(iand(shiftr(value, 8*(i - 1)), int(z'ff', int32)), int8)
        end do
        call this%write_bytes(bytes, status)
    end subroutine writer_write_le_i32

    subroutine writer_write_le_i64(this, value, status)
        class(byte_writer_t), intent(inout) :: this
        integer(int64), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: bytes(8)
        integer :: i

        do i = 1, 8
            bytes(i) = int(iand(shiftr(value, 8*(i - 1)), int(z'ff', int64)), int8)
        end do
        call this%write_bytes(bytes, status)
    end subroutine writer_write_le_i64

    subroutine writer_write_le_r64(this, value, status)
        class(byte_writer_t), intent(inout) :: this
        real(real64), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status

        call this%write_le_i64(transfer(value, 0_int64), status)
    end subroutine writer_write_le_r64

    subroutine writer_write_le_r32(this, value, status)
        class(byte_writer_t), intent(inout) :: this
        real(real32), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status

        call this%write_le_i32(transfer(value, 0_int32), status)
    end subroutine writer_write_le_r32

    subroutine reader_open(this, path, status)
        class(byte_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status
        integer :: io_status
        character(len=512) :: io_message

        call status%clear()
        open(newunit=this%unit, file=path, access="stream", form="unformatted", &
            action="read", status="old", iostat=io_status, iomsg=io_message)
        if (io_status /= 0) then
            call status%set(FORTIO_EIO, trim(io_message))
            this%unit = -1
            return
        end if
        this%position = 1_int64
    end subroutine reader_open

    subroutine reader_close(this, status)
        class(byte_reader_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status
        integer :: io_status
        character(len=512) :: io_message

        call status%clear()
        if (this%unit == -1) return
        close(this%unit, iostat=io_status, iomsg=io_message)
        if (io_status /= 0) call status%set(FORTIO_EIO, trim(io_message))
        this%unit = -1
        this%position = 1_int64
    end subroutine reader_close

    subroutine reader_seek(this, position)
        class(byte_reader_t), intent(inout) :: this
        integer(int64), intent(in) :: position

        this%position = position
    end subroutine reader_seek

    subroutine reader_read_bytes(this, values, status)
        class(byte_reader_t), intent(inout) :: this
        integer(int8), intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer :: io_status
        character(len=512) :: io_message

        call status%clear()
        read(this%unit, pos=this%position, iostat=io_status, iomsg=io_message) values
        if (io_status /= 0) then
            call status%set(FORTIO_EIO, trim(io_message))
            return
        end if
        this%position = this%position + size(values, kind=int64)
    end subroutine reader_read_bytes

    subroutine reader_read_i8(this, value, status)
        class(byte_reader_t), intent(inout) :: this
        integer(int8), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: bytes(1)

        call this%read_bytes(bytes, status)
        if (.not. status%ok()) return
        value = bytes(1)
    end subroutine reader_read_i8

    subroutine reader_read_be_i16(this, value, status)
        class(byte_reader_t), intent(inout) :: this
        integer(int16), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: bytes(2)

        call this%read_bytes(bytes, status)
        if (.not. status%ok()) return
        value = decode_be_i16(bytes)
    end subroutine reader_read_be_i16

    subroutine reader_read_be_i32(this, value, status)
        class(byte_reader_t), intent(inout) :: this
        integer(int32), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: bytes(4)

        call this%read_bytes(bytes, status)
        if (.not. status%ok()) return
        value = decode_be_i32(bytes)
    end subroutine reader_read_be_i32

    subroutine reader_read_be_i64(this, value, status)
        class(byte_reader_t), intent(inout) :: this
        integer(int64), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: bytes(8)

        call this%read_bytes(bytes, status)
        if (.not. status%ok()) return
        value = decode_be_i64(bytes)
    end subroutine reader_read_be_i64

    subroutine reader_read_be_r32(this, value, status)
        class(byte_reader_t), intent(inout) :: this
        real(real32), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int32) :: bits

        call this%read_be_i32(bits, status)
        if (.not. status%ok()) return
        value = transfer(bits, value)
    end subroutine reader_read_be_r32

    subroutine reader_read_be_r64(this, value, status)
        class(byte_reader_t), intent(inout) :: this
        real(real64), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int64) :: bits

        call this%read_be_i64(bits, status)
        if (.not. status%ok()) return
        value = transfer(bits, value)
    end subroutine reader_read_be_r64

    subroutine reader_read_le_i16(this, value, status)
        class(byte_reader_t), intent(inout) :: this
        integer(int16), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: bytes(2)

        call this%read_bytes(bytes, status)
        if (.not. status%ok()) return
        value = int(ior(byte_value(bytes(1)), &
            shiftl(byte_value(bytes(2)), 8)), int16)
    end subroutine reader_read_le_i16

    subroutine reader_read_le_i32(this, value, status)
        class(byte_reader_t), intent(inout) :: this
        integer(int32), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: bytes(4)
        integer :: i

        call this%read_bytes(bytes, status)
        if (.not. status%ok()) return
        value = 0_int32
        do i = 1, 4
            value = ior(value, shiftl(byte_value(bytes(i)), 8*(i - 1)))
        end do
    end subroutine reader_read_le_i32

    subroutine reader_read_le_i64(this, value, status)
        class(byte_reader_t), intent(inout) :: this
        integer(int64), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: bytes(8)
        integer :: i

        call this%read_bytes(bytes, status)
        if (.not. status%ok()) return
        value = 0_int64
        do i = 1, 8
            value = ior(value, shiftl(int(byte_value(bytes(i)), int64), 8*(i - 1)))
        end do
    end subroutine reader_read_le_i64

    subroutine reader_read_le_r32(this, value, status)
        class(byte_reader_t), intent(inout) :: this
        real(real32), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int32) :: bits

        call this%read_le_i32(bits, status)
        if (.not. status%ok()) return
        value = transfer(bits, value)
    end subroutine reader_read_le_r32

    subroutine reader_read_le_r64(this, value, status)
        class(byte_reader_t), intent(inout) :: this
        real(real64), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int64) :: bits

        call this%read_le_i64(bits, status)
        if (.not. status%ok()) return
        value = transfer(bits, value)
    end subroutine reader_read_le_r64

    pure integer(int16) function decode_be_i16(bytes)
        integer(int8), intent(in) :: bytes(2)

        decode_be_i16 = int(ior(shiftl(byte_value(bytes(1)), 8), &
            byte_value(bytes(2))), int16)
    end function decode_be_i16

    pure integer(int32) function decode_be_i32(bytes)
        integer(int8), intent(in) :: bytes(4)
        integer(int32) :: result
        integer :: i

        result = 0_int32
        do i = 1, 4
            result = ior(shiftl(result, 8), byte_value(bytes(i)))
        end do
        decode_be_i32 = result
    end function decode_be_i32

    pure integer(int64) function decode_be_i64(bytes)
        integer(int8), intent(in) :: bytes(8)
        integer(int64) :: result
        integer :: i

        result = 0_int64
        do i = 1, 8
            result = ior(shiftl(result, 8), int(byte_value(bytes(i)), int64))
        end do
        decode_be_i64 = result
    end function decode_be_i64

    pure integer(int32) function byte_value(value)
        integer(int8), intent(in) :: value

        byte_value = iand(int(value, int32), int(z'ff', int32))
    end function byte_value

end module fortio_bytes
