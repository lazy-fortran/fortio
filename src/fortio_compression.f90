module fortio_compression
    !! Dependency-free Deflate framing and checksum operations.
    use, intrinsic :: iso_fortran_env, only: int8
    use fortio_deflate_checksums, only: calculate_crc32 => crc32_calculate
    use fortio_deflate_compress, only: compress_raw => deflate_compress, &
        zlib_compress_into
    use fortio_deflate_decompress, only: zlib_decompress_impl => zlib_decompress
    use fortio_status, only: fortio_status_t, FORTIO_EIO
    implicit none
    private

    public :: compress_zlib, decompress_zlib, compress_raw, calculate_crc32

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
        call zlib_compress_into(input, size(input), output, output_size)
    end subroutine compress_zlib

    subroutine decompress_zlib(input, output, status)
        !! Decode and checksum a zlib-framed Deflate stream.
        integer(int8), intent(in) :: input(:)
        integer(int8), allocatable, intent(out) :: output(:)
        type(fortio_status_t), intent(inout) :: status
        integer :: code
        character(len=96) :: message

        call status%clear()
        output = zlib_decompress_impl(input, size(input), code, .true.)
        if (code /= 0) then
            write (message, '("zlib decompression failed with code ",i0)') code
            call status%set(FORTIO_EIO, trim(message))
        end if
    end subroutine decompress_zlib

end module fortio_compression
