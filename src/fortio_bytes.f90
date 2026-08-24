module fortio_bytes
    use, intrinsic :: iso_c_binding, only: c_associated, c_int, c_int64_t, c_loc, &
        c_null_char, c_null_ptr, c_ptr, c_size_t
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    use fortio_posix, only: mapped_close, mapped_copy, mapped_copy_swap64, mapped_open, &
        posix_close, posix_create_write, posix_open_read, posix_pwrite, &
        posix_pwrite_swap64, posix_truncate
    use fortio_status, only: fortio_status_t, FORTIO_EIO
    implicit none
    private

    type, public :: byte_reader_t
        integer :: unit = -1
        integer :: native_unit = -1
        integer(c_int) :: descriptor = -1_c_int
        type(c_ptr) :: mapping = c_null_ptr
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
        procedure :: read_be_r64_array => reader_read_be_r64_array
        procedure :: read_le_i16 => reader_read_le_i16
        procedure :: read_le_i32 => reader_read_le_i32
        procedure :: read_le_i64 => reader_read_le_i64
        procedure :: read_le_r32 => reader_read_le_r32
        procedure :: read_le_r64 => reader_read_le_r64
        procedure :: read_le_r64_array => reader_read_le_r64_array
        procedure :: read_bytes => reader_read_bytes
    end type byte_reader_t

    type, public :: byte_writer_t
        integer(c_int) :: descriptor = -1_c_int
        integer(int64) :: position = 1_int64
    contains
        procedure :: open => writer_open
        procedure :: close => writer_close
        procedure :: reset => writer_reset
        procedure :: seek => writer_seek
        procedure :: write_i8 => writer_write_i8
        procedure :: write_be_i16 => writer_write_be_i16
        procedure :: write_be_i32 => writer_write_be_i32
        procedure :: write_be_i64 => writer_write_be_i64
        procedure :: write_be_r32 => writer_write_be_r32
        procedure :: write_be_r64 => writer_write_be_r64
        procedure :: write_be_r64_array => writer_write_be_r64_array
        procedure :: write_le_i32 => writer_write_le_i32
        procedure :: write_le_i64 => writer_write_le_i64
        procedure :: write_le_r32 => writer_write_le_r32
        procedure :: write_le_r64 => writer_write_le_r64
        procedure :: write_le_r64_array => writer_write_le_r64_array
        procedure :: write_bytes => writer_write_bytes
    end type byte_writer_t

    public :: decode_be_i16, decode_be_i32, decode_be_i64

contains

    subroutine writer_open(this, path, status)
        class(byte_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status
        call status%clear()
        this%descriptor = posix_create_write(trim(path)//c_null_char)
        if (this%descriptor < 0_c_int) then
            call status%set(FORTIO_EIO, "POSIX create failed")
            return
        end if
        this%position = 1_int64
    end subroutine writer_open

    subroutine writer_close(this, status)
        class(byte_writer_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status
        integer :: io_status

        call status%clear()
        if (this%descriptor < 0_c_int) return
        io_status = posix_close(this%descriptor)
        if (io_status /= 0) call status%set(FORTIO_EIO, "POSIX close failed")
        this%descriptor = -1_c_int
        this%position = 1_int64
    end subroutine writer_close

    subroutine writer_reset(this, status)
        class(byte_writer_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status
        integer(c_int) :: io_status

        call status%clear()
        if (this%descriptor < 0_c_int) then
            call status%set(FORTIO_EIO, "cannot reset a closed file")
            return
        end if
        io_status = posix_truncate(this%descriptor, 0_c_int64_t)
        if (io_status /= 0_c_int) then
            call status%set(FORTIO_EIO, "file truncate failed")
            return
        end if
        this%position = 1_int64
    end subroutine writer_reset

    subroutine writer_seek(this, position)
        class(byte_writer_t), intent(inout) :: this
        integer(int64), intent(in) :: position

        this%position = position
    end subroutine writer_seek

    subroutine writer_write_bytes(this, values, status)
        class(byte_writer_t), intent(inout) :: this
        integer(int8), contiguous, target, intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(c_int64_t) :: byte_count, bytes_written

        call status%clear()
        byte_count = size(values, kind=c_int64_t)
        bytes_written = posix_pwrite(this%descriptor, c_loc(values), &
            int(byte_count, c_size_t), int(this%position - 1_int64, c_int64_t))
        if (bytes_written /= byte_count) then
            call status%set(FORTIO_EIO, "POSIX write returned incomplete data")
            return
        end if
        this%position = this%position + int(byte_count, int64)
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

    subroutine writer_write_be_r64_array(this, values, status)
        class(byte_writer_t), intent(inout) :: this
        real(real64), contiguous, target, intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(c_int64_t) :: byte_count, bytes_written

        call status%clear()
        byte_count = 8_c_int64_t*size(values, kind=c_int64_t)
        if (host_is_little_endian()) then
            bytes_written = posix_pwrite_swap64(this%descriptor, c_loc(values), &
                int(byte_count, c_size_t), int(this%position - 1_int64, c_int64_t))
        else
            bytes_written = posix_pwrite(this%descriptor, c_loc(values), &
                int(byte_count, c_size_t), int(this%position - 1_int64, c_int64_t))
        end if
        if (bytes_written /= byte_count) then
            call status%set(FORTIO_EIO, "POSIX write returned incomplete data")
            return
        end if
        this%position = this%position + int(byte_count, int64)
    end subroutine writer_write_be_r64_array

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

    subroutine writer_write_le_r64_array(this, values, status)
        class(byte_writer_t), intent(inout) :: this
        real(real64), contiguous, target, intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8), allocatable :: bytes(:)
        integer(c_int64_t) :: byte_count, bytes_written

        call status%clear()
        if (host_is_little_endian()) then
            byte_count = 8_c_int64_t*size(values, kind=c_int64_t)
            bytes_written = posix_pwrite(this%descriptor, c_loc(values), &
                int(byte_count, c_size_t), int(this%position - 1_int64, c_int64_t))
            if (bytes_written /= byte_count) then
                call status%set(FORTIO_EIO, "POSIX write returned incomplete data")
                return
            end if
            this%position = this%position + int(byte_count, int64)
        else
            allocate(bytes(8*size(values)))
            bytes = transfer(values, bytes)
            call reverse_elements(bytes, 8)
            call this%write_bytes(bytes, status)
        end if
    end subroutine writer_write_le_r64_array

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

        call status%clear()
        this%descriptor = posix_open_read(trim(path)//c_null_char)
        if (this%descriptor < 0_c_int) then
            call status%set(FORTIO_EIO, "POSIX open failed")
            return
        end if
        this%mapping = mapped_open(this%descriptor)
        if (.not. c_associated(this%mapping)) then
            io_status = posix_close(this%descriptor)
            this%descriptor = -1_c_int
            call status%set(FORTIO_EIO, "memory mapping failed")
            return
        end if
        this%position = 1_int64
    end subroutine reader_open

    subroutine reader_close(this, status)
        class(byte_reader_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status
        integer :: io_status

        call status%clear()
        if (c_associated(this%mapping)) io_status = mapped_close(this%mapping)
        if (this%descriptor >= 0_c_int) io_status = posix_close(this%descriptor)
        this%descriptor = -1_c_int
        this%mapping = c_null_ptr
        this%position = 1_int64
    end subroutine reader_close

    subroutine reader_seek(this, position)
        class(byte_reader_t), intent(inout) :: this
        integer(int64), intent(in) :: position

        this%position = position
    end subroutine reader_seek

    subroutine reader_read_bytes(this, values, status)
        class(byte_reader_t), intent(inout) :: this
        integer(int8), contiguous, target, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(c_int64_t) :: byte_count, bytes_read

        call status%clear()
        byte_count = size(values, kind=c_int64_t)
        bytes_read = mapped_copy(this%mapping, c_loc(values), int(byte_count, c_size_t), &
            int(this%position - 1_int64, c_int64_t))
        if (bytes_read /= byte_count) then
            call status%set(FORTIO_EIO, "mapped read returned incomplete data at position " // &
                int_to_text(this%position) // " for " // int_to_text(byte_count) // " bytes")
            return
        end if
        this%position = this%position + int(byte_count, int64)
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

    subroutine reader_read_be_r64_array(this, values, status)
        class(byte_reader_t), intent(inout) :: this
        real(real64), contiguous, target, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(c_int64_t) :: byte_count, bytes_read

        call status%clear()
        byte_count = 8_c_int64_t*size(values, kind=c_int64_t)
        if (host_is_little_endian()) then
            bytes_read = mapped_copy_swap64(this%mapping, c_loc(values), &
                int(byte_count, c_size_t), int(this%position - 1_int64, c_int64_t))
        else
            bytes_read = mapped_copy(this%mapping, c_loc(values), &
                int(byte_count, c_size_t), int(this%position - 1_int64, c_int64_t))
        end if
        if (bytes_read /= byte_count) then
            call status%set(FORTIO_EIO, "mapped read returned incomplete data at position " // &
                int_to_text(this%position) // " for " // int_to_text(byte_count) // " bytes")
            return
        end if
        this%position = this%position + int(byte_count, int64)
    end subroutine reader_read_be_r64_array

    function int_to_text(value) result(text)
        integer(int64), intent(in) :: value
        character(len=:), allocatable :: text
        character(len=32) :: buffer

        write (buffer, '(i0)') value
        text = trim(buffer)
    end function int_to_text

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

    subroutine reader_read_le_r64_array(this, values, status)
        class(byte_reader_t), intent(inout) :: this
        real(real64), contiguous, target, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(c_int64_t) :: bytes_read, byte_count

        call status%clear()
        if (host_is_little_endian()) then
            byte_count = 8_c_int64_t*size(values, kind=c_int64_t)
            bytes_read = mapped_copy(this%mapping, c_loc(values), &
                int(byte_count, c_size_t), int(this%position - 1_int64, c_int64_t))
            if (bytes_read /= byte_count) then
                call status%set(FORTIO_EIO, "POSIX read returned incomplete data")
                return
            end if
            this%position = this%position + int(byte_count, int64)
        else
            call read_le_r64_array_portable(this, values, status)
        end if
    end subroutine reader_read_le_r64_array

    subroutine read_le_r64_array_portable(this, values, status)
        class(byte_reader_t), intent(inout) :: this
        real(real64), intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8), allocatable :: bytes(:)

        allocate(bytes(8*size(values)))
        call this%read_bytes(bytes, status)
        if (.not. status%ok()) return
        call reverse_elements(bytes, 8)
        values = transfer(bytes, values)
    end subroutine read_le_r64_array_portable

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

    logical function host_is_little_endian()
        integer(int16) :: one
        integer(int8) :: bytes(2)

        one = 1_int16
        bytes = transfer(one, bytes)
        host_is_little_endian = bytes(1) == 1_int8
    end function host_is_little_endian

    subroutine reverse_elements(bytes, element_size)
        integer(int8), intent(inout) :: bytes(:)
        integer, intent(in) :: element_size
        integer(int8) :: temporary
        integer :: element, left, right

        do element = 0, size(bytes)/element_size - 1
            do left = 1, element_size/2
                right = element_size + 1 - left
                temporary = bytes(element*element_size + left)
                bytes(element*element_size + left) = bytes(element*element_size + right)
                bytes(element*element_size + right) = temporary
            end do
        end do
    end subroutine reverse_elements

end module fortio_bytes
