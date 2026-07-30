module fortio_deflate
    use, intrinsic :: iso_c_binding, only: c_double, c_int, c_int8_t, c_size_t
    use, intrinsic :: iso_fortran_env, only: int8, int64, real64
    use fortio_deflate_compress, only: zlib_compress_into
    use fortio_deflate_decompress, only: zlib_decompress
    use fortio_status, only: fortio_status_t, FORTIO_EIO
    implicit none
    private

    public :: deflate_compress, deflate_uncompress, shuffle_bytes, unshuffle_bytes, &
        unshuffle_r64

    interface
        function c_inflate_fixed_zlib(input, input_size, output, output_size) &
                bind(C, name="fortio_inflate_fixed_zlib") result(code)
            import :: c_int, c_int8_t, c_size_t
            integer(c_int8_t), intent(in) :: input(*)
            integer(c_size_t), value :: input_size
            integer(c_int8_t), intent(out) :: output(*)
            integer(c_size_t), value :: output_size
            integer(c_int) :: code
        end function c_inflate_fixed_zlib

        subroutine c_shuffle(input, output, count, element_size) &
                bind(C, name="fortio_shuffle")
            import :: c_int8_t, c_size_t
            integer(c_int8_t), intent(in) :: input(*)
            integer(c_int8_t), intent(out) :: output(*)
            integer(c_size_t), value :: count, element_size
        end subroutine c_shuffle

        subroutine c_unshuffle(input, output, count, element_size) &
                bind(C, name="fortio_unshuffle")
            import :: c_int8_t, c_size_t
            integer(c_int8_t), intent(in) :: input(*)
            integer(c_int8_t), intent(out) :: output(*)
            integer(c_size_t), value :: count, element_size
        end subroutine c_unshuffle

        subroutine c_unshuffle_r64(input, output, count) &
                bind(C, name="fortio_unshuffle_r64")
            import :: c_double, c_int8_t, c_size_t
            integer(c_int8_t), intent(in) :: input(*)
            real(c_double), intent(out) :: output(*)
            integer(c_size_t), value :: count
        end subroutine c_unshuffle_r64
    end interface

contains

    subroutine deflate_compress(input, level, output, status)
        integer(int8), intent(in) :: input(:)
        integer, intent(in) :: level
        integer(int8), allocatable, intent(out) :: output(:)
        type(fortio_status_t), intent(inout) :: status
        integer :: output_size

        call status%clear()
        if (level < 0 .or. level > 9) then
            call status%set(FORTIO_EIO, "deflate compression level is invalid")
            return
        end if
        call zlib_compress_into(input, size(input), output, output_size)
    end subroutine deflate_compress

    subroutine deflate_uncompress(input, expected_size, output, status)
        integer(int8), intent(in) :: input(:)
        integer(int64), intent(in) :: expected_size
        integer(int8), allocatable, intent(out) :: output(:)
        type(fortio_status_t), intent(inout) :: status
        integer :: code
        integer(int8), allocatable :: decoded(:)
        character(len=96) :: message

        call status%clear()
        allocate(output(int(expected_size)))
        code = c_inflate_fixed_zlib(input, int(size(input), c_size_t), output, &
            int(expected_size, c_size_t))
        if (code == 0) return
        deallocate(output)
        decoded = zlib_decompress(input, size(input), code, .true.)
        if (code /= 0 .or. int(size(decoded), int64) /= expected_size) then
            write (message, '("deflate decompression failed with code ",i0)') code
            call status%set(FORTIO_EIO, trim(message))
            return
        end if
        call move_alloc(decoded, output)
    end subroutine deflate_uncompress

    subroutine shuffle_bytes(input, element_size, output)
        integer(int8), intent(in) :: input(:)
        integer, intent(in) :: element_size
        integer(int8), allocatable, intent(out) :: output(:)

        allocate(output(size(input)))
        call c_shuffle(input, output, int(size(input)/element_size, c_size_t), &
            int(element_size, c_size_t))
    end subroutine shuffle_bytes

    subroutine unshuffle_bytes(input, element_size, output)
        integer(int8), intent(in) :: input(:)
        integer, intent(in) :: element_size
        integer(int8), allocatable, intent(out) :: output(:)

        allocate(output(size(input)))
        call c_unshuffle(input, output, int(size(input)/element_size, c_size_t), &
            int(element_size, c_size_t))
    end subroutine unshuffle_bytes

    subroutine unshuffle_r64(input, output)
        integer(int8), intent(in) :: input(:)
        real(real64), contiguous, intent(out) :: output(:)

        call c_unshuffle_r64(input, output, int(size(output), c_size_t))
    end subroutine unshuffle_r64

end module fortio_deflate
