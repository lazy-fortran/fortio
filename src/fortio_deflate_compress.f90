module fortio_deflate_compress
    !! Deflate encoder and zlib framing (dynamic, fixed or stored blocks, see
    !! fortio_deflate_encode.c).
    use, intrinsic :: iso_c_binding, only: c_int, c_int8_t, c_size_t
    use, intrinsic :: iso_fortran_env, only: int8, int32
    implicit none
    private

    public :: bit_reverse, deflate_compress, init_fixed_huffman_tables
    public :: zlib_compress_into

    interface
        function c_deflate_raw(input, input_size, output, output_capacity, level) &
                bind(C, name="fortio_deflate_raw") result(output_size)
            import :: c_int, c_int8_t, c_size_t
            integer(c_int8_t), intent(in) :: input(*)
            integer(c_size_t), value :: input_size
            integer(c_int8_t), intent(out) :: output(*)
            integer(c_size_t), value :: output_capacity
            integer(c_int), value :: level
            integer(c_size_t) :: output_size
        end function c_deflate_raw

        function c_zlib_compress(input, input_size, output, output_capacity, level) &
                bind(C, name="fortio_zlib_compress") result(output_size)
            import :: c_int, c_int8_t, c_size_t
            integer(c_int8_t), intent(in) :: input(*)
            integer(c_size_t), value :: input_size
            integer(c_int8_t), intent(out) :: output(*)
            integer(c_size_t), value :: output_capacity
            integer(c_int), value :: level
            integer(c_size_t) :: output_size
        end function c_zlib_compress

        pure function c_zlib_bound(input_size) bind(C, name="fortio_zlib_bound") result(bound)
            import :: c_size_t
            integer(c_size_t), value :: input_size
            integer(c_size_t) :: bound
        end function c_zlib_bound
    end interface

contains

    subroutine deflate_compress(input_data, input_len, output_data, output_len, level)
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer(int8), allocatable, intent(out) :: output_data(:)
        integer, intent(out) :: output_len
        integer, intent(in), optional :: level

        call encode(input_data, input_len, output_data, output_len, level, .false.)
    end subroutine deflate_compress

    !> Encode into a buffer of the worst-case size, then keep the used bytes.
    subroutine encode(input_data, input_len, output_data, output_len, level, zlib)
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer(int8), allocatable, intent(out) :: output_data(:)
        integer, intent(out) :: output_len
        integer, intent(in), optional :: level
        logical, intent(in) :: zlib
        integer(int8), allocatable :: buffer(:)
        integer(c_size_t) :: n
        integer(c_int) :: lv

        lv = 6
        if (present(level)) lv = int(level, c_int)
        allocate (buffer(c_zlib_bound(int(input_len, c_size_t))))
        if (zlib) then
            n = c_zlib_compress(input_data, int(input_len, c_size_t), buffer, &
                int(size(buffer), c_size_t), lv)
        else
            n = c_deflate_raw(input_data, int(input_len, c_size_t), buffer, &
                int(size(buffer), c_size_t), lv)
        end if
        output_len = int(n)
        allocate (output_data(output_len))
        output_data = buffer(1:output_len)
    end subroutine encode

    subroutine init_fixed_huffman_tables(literal_codes, literal_lengths, distance_codes, &
            distance_lengths)
        integer, intent(out) :: literal_codes(0:285)
        integer, intent(out) :: literal_lengths(0:285)
        integer, intent(out) :: distance_codes(0:29)
        integer, intent(out) :: distance_lengths(0:29)
        integer :: code, i

        code = 0
        do i = 0, 143
            literal_codes(i) = code + 48
            literal_lengths(i) = 8
            code = code + 1
        end do
        code = 0
        do i = 144, 255
            literal_codes(i) = code + 400
            literal_lengths(i) = 9
            code = code + 1
        end do
        code = 0
        do i = 256, 279
            literal_codes(i) = code
            literal_lengths(i) = 7
            code = code + 1
        end do
        code = 0
        do i = 280, 285
            literal_codes(i) = code + 192
            literal_lengths(i) = 8
            code = code + 1
        end do
        do i = 0, 29
            distance_codes(i) = i
            distance_lengths(i) = 5
        end do
    end subroutine init_fixed_huffman_tables

    pure integer function bit_reverse(value, num_bits) result(reversed_value)
        integer, intent(in) :: value, num_bits
        integer :: i

        reversed_value = 0
        do i = 0, num_bits - 1
            if (btest(value, i)) reversed_value = ibset(reversed_value, num_bits - 1 - i)
        end do
    end function bit_reverse

    subroutine zlib_compress_into(input_data, input_len, output_data, output_len, level)
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer(int8), allocatable, intent(out) :: output_data(:)
        integer, intent(out) :: output_len
        integer, intent(in), optional :: level

        call encode(input_data, input_len, output_data, output_len, level, .true.)
    end subroutine zlib_compress_into

end module fortio_deflate_compress
