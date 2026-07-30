module fortio_deflate_compress
    !! Fixed-Huffman Deflate encoder and zlib framing.
    use, intrinsic :: iso_c_binding, only: c_int8_t, c_size_t
    use, intrinsic :: iso_fortran_env, only: int8, int32
    use fortio_deflate_checksums, only: calculate_adler32
    implicit none
    private

    public :: bit_reverse, deflate_compress, init_fixed_huffman_tables
    public :: zlib_compress_into

    interface
        function c_deflate_fixed(input, input_size, output, output_capacity) &
                bind(C, name="fortio_deflate_fixed") result(output_size)
            import :: c_int8_t, c_size_t
            integer(c_int8_t), intent(in) :: input(*)
            integer(c_size_t), value :: input_size
            integer(c_int8_t), intent(out) :: output(*)
            integer(c_size_t), value :: output_capacity
            integer(c_size_t) :: output_size
        end function c_deflate_fixed
    end interface

contains

    subroutine deflate_compress(input_data, input_len, output_data, output_len)
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer(int8), allocatable, intent(out) :: output_data(:)
        integer, intent(out) :: output_len
        integer(c_size_t) :: encoded_size

        allocate(output_data(max(64, 2*input_len + 64)))
        encoded_size = c_deflate_fixed(input_data, int(input_len, c_size_t), &
            output_data, int(size(output_data), c_size_t))
        output_len = int(encoded_size)
        output_data = output_data(:output_len)
    end subroutine deflate_compress

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

    subroutine zlib_compress_into(input_data, input_len, output_data, output_len)
        integer(int8), intent(in) :: input_data(*)
        integer, intent(in) :: input_len
        integer(int8), allocatable, intent(out) :: output_data(:)
        integer, intent(out) :: output_len
        integer(int8), allocatable :: compressed_block(:)
        integer(int32) :: checksum
        integer :: block_size, position

        call deflate_compress(input_data, input_len, compressed_block, block_size)
        output_len = block_size + 6
        allocate(output_data(output_len))
        output_data(1:2) = [int(z'78', int8), int(z'5E', int8)]
        output_data(3:block_size + 2) = compressed_block
        checksum = calculate_adler32(input_data, input_len)
        position = block_size + 3
        output_data(position) = int(iand(shiftr(checksum, 24), 255), int8)
        output_data(position + 1) = int(iand(shiftr(checksum, 16), 255), int8)
        output_data(position + 2) = int(iand(shiftr(checksum, 8), 255), int8)
        output_data(position + 3) = int(iand(checksum, 255), int8)
    end subroutine zlib_compress_into

end module fortio_deflate_compress
