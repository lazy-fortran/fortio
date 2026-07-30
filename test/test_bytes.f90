program test_bytes
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
    use fortio_bytes, only: decode_be_i16, decode_be_i32, decode_be_i64
    implicit none

    integer(int8) :: b2(2), b4(4), b8(8)

    b2 = [int(z'12', int8), int(z'34', int8)]
    if (decode_be_i16(b2) /= int(z'1234', int16)) error stop "i16 decoding"

    b4 = [int(z'89', int8), int(z'ab', int8), int(z'cd', int8), int(z'ef', int8)]
    if (decode_be_i32(b4) /= int(z'89abcdef', int32)) error stop "i32 decoding"

    b8 = [int(z'01', int8), int(z'23', int8), int(z'45', int8), int(z'67', int8), &
          int(z'89', int8), int(z'ab', int8), int(z'cd', int8), int(z'ef', int8)]
    if (decode_be_i64(b8) /= int(z'0123456789abcdef', int64)) error stop "i64 decoding"
end program test_bytes
