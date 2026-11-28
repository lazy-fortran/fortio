module fortio_compression
    !! Dependency-free Deflate framing and checksum operations.
    use, intrinsic :: iso_c_binding, only: c_int, c_int8_t, c_size_t
    use, intrinsic :: iso_fortran_env, only: int8
    use fortio_deflate_checksums, only: calculate_crc32 => crc32_calculate
    use fortio_deflate_compress, only: compress_raw => deflate_compress, &
        zlib_compress_into
    use fortio_status, only: fortio_status_t, FORTIO_EIO
    implicit none
    private

    public :: compress_zlib, decompress_zlib, compress_raw, calculate_crc32

    interface
        function c_inflate_zlib(input, input_size, output, capacity, produced) &
                bind(C, name="fortio_inflate_zlib") result(code)
            import :: c_int, c_int8_t, c_size_t
            integer(c_int8_t), intent(in) :: input(*)
            integer(c_size_t), value :: input_size
            integer(c_int8_t), intent(inout) :: output(*)
            integer(c_size_t), value :: capacity
            integer(c_size_t), intent(out) :: produced
            integer(c_int) :: code
        end function c_inflate_zlib
    end interface

contains

    subroutine compress_zlib(input, output, status, level)
        !! Encode bytes as a zlib-framed Deflate stream.
        integer(int8), intent(in) :: input(:)
        integer(int8), allocatable, intent(out) :: output(:)
        type(fortio_status_t), intent(inout) :: status
        integer, intent(in), optional :: level
        integer :: selected_level, output_size

        selected_level = 6
        if (present(level)) selected_level = level
        call status%clear()
        if (selected_level < 0 .or. selected_level > 9) then
            call status%set(FORTIO_EIO, "compression level must be between zero and nine")
            return
        end if
        call zlib_compress_into(input, size(input), output, output_size, selected_level)
    end subroutine compress_zlib

    subroutine decompress_zlib(input, output, status, expected_size)
        !! Decode and checksum a zlib-framed Deflate stream. A known decompressed size
        !! in `expected_size` lets the output be allocated once at its final size.
        integer(int8), intent(in) :: input(:)
        integer(int8), allocatable, intent(out) :: output(:)
        type(fortio_status_t), intent(inout) :: status
        integer, intent(in), optional :: expected_size
        integer(int8), allocatable :: kept(:)
        integer(c_size_t) :: capacity, produced
        integer(c_int) :: code
        character(len=96) :: message

        call status%clear()
        capacity = max(256_c_size_t, 4 * size(input, kind=c_size_t))
        if (present(expected_size)) capacity = max(1_c_size_t, int(expected_size, c_size_t))
        do
            allocate (output(capacity))
            code = c_inflate_zlib(input, size(input, kind=c_size_t), output, capacity, produced)
            ! 1: the output did not fit, retry with twice the room
            if (code /= 1) exit
            deallocate (output)
            capacity = 2 * capacity
        end do
        if (code /= 0) then
            write (message, '("zlib decompression failed with code ",i0)') code
            call status%set(FORTIO_EIO, trim(message))
            deallocate (output)
            allocate (output(0))
            return
        end if
        if (produced < capacity) then
            allocate (kept(produced))
            kept = output(1:produced)
            call move_alloc(kept, output)
        end if
    end subroutine decompress_zlib

end module fortio_compression
