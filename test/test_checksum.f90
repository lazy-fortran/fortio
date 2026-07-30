program test_checksum
    use, intrinsic :: iso_fortran_env, only: int8, int32
    use fortio_checksum, only: lookup3_checksum
    implicit none

    integer(int8), parameter :: superblock(44) = [ &
        int(z'89', int8), int(z'48', int8), int(z'44', int8), int(z'46', int8), &
        int(z'0d', int8), int(z'0a', int8), int(z'1a', int8), int(z'0a', int8), &
        int(z'03', int8), int(z'08', int8), int(z'08', int8), int(z'00', int8), &
        0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, &
        -1_int8, -1_int8, -1_int8, -1_int8, -1_int8, -1_int8, -1_int8, -1_int8, &
        int(z'08', int8), int(z'08', int8), 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, &
        int(z'30', int8), 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8]

    if (lookup3_checksum(superblock) /= int(z'd5211342', int32)) &
        error stop "lookup3 differs from system HDF5 superblock checksum"
end program test_checksum
