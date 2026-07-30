module fortio_deflate
    use, intrinsic :: iso_c_binding, only: c_double, c_int, c_int8_t, c_size_t
    use, intrinsic :: iso_fortran_env, only: int8, int64, real64
    use fortio_status, only: fortio_status_t, FORTIO_EIO
    implicit none
    private

    public :: deflate_compress, deflate_uncompress, shuffle_bytes, unshuffle_bytes, &
        unshuffle_r64

    interface
        function c_deflate_bound(input_size) bind(C, name="fortio_deflate_bound") &
                result(output_size)
            import :: c_size_t
            integer(c_size_t), value :: input_size
            integer(c_size_t) :: output_size
        end function c_deflate_bound

        function c_deflate_compress(input, input_size, output, output_size, level) &
                bind(C, name="fortio_deflate_compress") result(code)
            import :: c_int, c_int8_t, c_size_t
            integer(c_int8_t), intent(in) :: input(*)
            integer(c_size_t), value :: input_size
            integer(c_int8_t), intent(out) :: output(*)
            integer(c_size_t), intent(inout) :: output_size
            integer(c_int), value :: level
            integer(c_int) :: code
        end function c_deflate_compress

        function c_deflate_uncompress(input, input_size, output, output_size) &
                bind(C, name="fortio_deflate_uncompress") result(code)
            import :: c_int, c_int8_t, c_size_t
            integer(c_int8_t), intent(in) :: input(*)
            integer(c_size_t), value :: input_size
            integer(c_int8_t), intent(out) :: output(*)
            integer(c_size_t), intent(inout) :: output_size
            integer(c_int) :: code
        end function c_deflate_uncompress

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
        integer(c_int) :: code
        integer(c_size_t) :: output_size

        call status%clear()
        output_size = c_deflate_bound(int(size(input), c_size_t))
        allocate(output(int(output_size, int64)))
        code = c_deflate_compress(input, int(size(input), c_size_t), output, &
            output_size, int(level, c_int))
        if (code /= 0_c_int) then
            call status%set(FORTIO_EIO, "deflate compression failed")
            deallocate(output)
            return
        end if
        output = output(:int(output_size, int64))
    end subroutine deflate_compress

    subroutine deflate_uncompress(input, expected_size, output, status)
        integer(int8), intent(in) :: input(:)
        integer(int64), intent(in) :: expected_size
        integer(int8), allocatable, intent(out) :: output(:)
        type(fortio_status_t), intent(inout) :: status
        integer(c_int) :: code
        integer(c_size_t) :: output_size

        call status%clear()
        allocate(output(expected_size))
        output_size = int(expected_size, c_size_t)
        code = c_deflate_uncompress(input, int(size(input), c_size_t), output, output_size)
        if (code /= 0_c_int .or. int(output_size, int64) /= expected_size) then
            call status%set(FORTIO_EIO, "deflate decompression failed")
            deallocate(output)
        end if
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
