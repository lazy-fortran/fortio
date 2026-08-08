module fortio_checksum
    use, intrinsic :: iso_fortran_env, only: int8, int32, int64
    implicit none
    intrinsic :: ior
    private

    integer(int64), parameter :: MASK32 = int(z'ffffffff', int64)
    integer(int64), parameter :: DEAD_BEEF = int(z'deadbeef', int64)

    public :: lookup3_checksum

contains

    pure integer(int32) function lookup3_checksum(bytes, initial_value) result(checksum)
        integer(int8), intent(in) :: bytes(:)
        integer(int32), intent(in), optional :: initial_value
        integer(int64) :: a, b, c, initial
        integer :: offset, remaining

        initial = 0_int64
        if (present(initial_value)) initial = iand(int(initial_value, int64), MASK32)
        a = wrap32(DEAD_BEEF + size(bytes, kind=int64) + initial)
        b = a
        c = a
        offset = 1
        remaining = size(bytes)
        do while (remaining > 12)
            a = add32(a, word32(bytes(offset:offset + 3)))
            b = add32(b, word32(bytes(offset + 4:offset + 7)))
            c = add32(c, word32(bytes(offset + 8:offset + 11)))
            call mix(a, b, c)
            offset = offset + 12
            remaining = remaining - 12
        end do
        if (remaining >= 12) c = add32(c, ishft(byte_value(bytes(offset + 11)), 24))
        if (remaining >= 11) c = add32(c, ishft(byte_value(bytes(offset + 10)), 16))
        if (remaining >= 10) c = add32(c, ishft(byte_value(bytes(offset + 9)), 8))
        if (remaining >= 9) c = add32(c, byte_value(bytes(offset + 8)))
        if (remaining >= 8) b = add32(b, ishft(byte_value(bytes(offset + 7)), 24))
        if (remaining >= 7) b = add32(b, ishft(byte_value(bytes(offset + 6)), 16))
        if (remaining >= 6) b = add32(b, ishft(byte_value(bytes(offset + 5)), 8))
        if (remaining >= 5) b = add32(b, byte_value(bytes(offset + 4)))
        if (remaining >= 4) a = add32(a, ishft(byte_value(bytes(offset + 3)), 24))
        if (remaining >= 3) a = add32(a, ishft(byte_value(bytes(offset + 2)), 16))
        if (remaining >= 2) a = add32(a, ishft(byte_value(bytes(offset + 1)), 8))
        if (remaining >= 1) a = add32(a, byte_value(bytes(offset)))
        if (remaining > 0) call final_mix(a, b, c)
        checksum = int(c, int32)
    end function lookup3_checksum

    pure subroutine mix(a, b, c)
        integer(int64), intent(inout) :: a, b, c

        a = xor32(sub32(a, c), rotate32(c, 4)); c = add32(c, b)
        b = xor32(sub32(b, a), rotate32(a, 6)); a = add32(a, c)
        c = xor32(sub32(c, b), rotate32(b, 8)); b = add32(b, a)
        a = xor32(sub32(a, c), rotate32(c, 16)); c = add32(c, b)
        b = xor32(sub32(b, a), rotate32(a, 19)); a = add32(a, c)
        c = xor32(sub32(c, b), rotate32(b, 4)); b = add32(b, a)
    end subroutine mix

    pure subroutine final_mix(a, b, c)
        integer(int64), intent(inout) :: a, b, c

        c = sub32(xor32(c, b), rotate32(b, 14))
        a = sub32(xor32(a, c), rotate32(c, 11))
        b = sub32(xor32(b, a), rotate32(a, 25))
        c = sub32(xor32(c, b), rotate32(b, 16))
        a = sub32(xor32(a, c), rotate32(c, 4))
        b = sub32(xor32(b, a), rotate32(a, 14))
        c = sub32(xor32(c, b), rotate32(b, 24))
    end subroutine final_mix

    pure integer(int64) function word32(bytes) result(value)
        integer(int8), intent(in) :: bytes(4)

        value = byte_value(bytes(1)) + ishft(byte_value(bytes(2)), 8) + &
                ishft(byte_value(bytes(3)), 16) + ishft(byte_value(bytes(4)), 24)
        value = wrap32(value)
    end function word32

    pure integer(int64) function byte_value(byte) result(value)
        integer(int8), intent(in) :: byte

        value = iand(int(byte, int64), 255_int64)
    end function byte_value

    pure integer(int64) function add32(left, right) result(value)
        integer(int64), intent(in) :: left, right

        value = wrap32(left + right)
    end function add32

    pure integer(int64) function sub32(left, right) result(value)
        integer(int64), intent(in) :: left, right

        value = wrap32(left - right)
    end function sub32

    pure integer(int64) function xor32(left, right) result(value)
        integer(int64), intent(in) :: left, right

        value = iand(ieor(left, right), MASK32)
    end function xor32

    pure integer(int64) function rotate32(value_in, count) result(value)
        integer(int64), intent(in) :: value_in
        integer, intent(in) :: count

        value = iand(ior(ishft(value_in, count), ishft(value_in, count - 32)), MASK32)
    end function rotate32

    pure integer(int64) function wrap32(value_in) result(value)
        integer(int64), intent(in) :: value_in

        value = iand(value_in, MASK32)
    end function wrap32

end module fortio_checksum
