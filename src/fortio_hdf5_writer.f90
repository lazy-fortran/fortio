module fortio_hdf5_writer
    use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
    use fortio_bytes, only: byte_writer_t
    use fortio_checksum, only: lookup3_checksum
    use fortio_status, only: fortio_status_t, FORTIO_ESTATE, FORTIO_ESHAPE, &
                            FORTIO_ENOTSUP
    implicit none
    private

    integer, parameter :: TYPE_I32 = 1
    integer, parameter :: TYPE_R64 = 2
    integer(int64), parameter :: ROOT_ADDRESS = 48_int64

    type :: hdf5_output_dataset_t
        character(len=:), allocatable :: name
        integer :: type_code = 0
        integer(int64), allocatable :: dimensions(:)
        integer(int32), allocatable :: values_i32(:)
        real(real64), allocatable :: values_r64(:)
        integer(int64) :: object_address = 0_int64
        integer(int64) :: data_address = 0_int64
    end type hdf5_output_dataset_t

    type, public :: hdf5_writer_t
        character(len=:), allocatable :: path
        type(hdf5_output_dataset_t), allocatable :: datasets(:)
        logical :: opened = .false.
    contains
        procedure :: create => hdf5_writer_create
        procedure :: add_i32_scalar => hdf5_add_i32_scalar
        procedure :: add_i32_1 => hdf5_add_i32_1
        procedure :: add_r64_scalar => hdf5_add_r64_scalar
        procedure :: add_r64_1 => hdf5_add_r64_1
        procedure :: add_r64_2 => hdf5_add_r64_2
        procedure :: add_r64_3 => hdf5_add_r64_3
        procedure :: close => hdf5_writer_close
    end type hdf5_writer_t

contains

    subroutine hdf5_writer_create(this, path, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status

        call status%clear()
        this%path = trim(path)
        allocate(this%datasets(0))
        this%opened = .true.
    end subroutine hdf5_writer_create

    subroutine hdf5_add_i32_scalar(this, name, value, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(int32), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status

        call add_i32_flat(this, name, [integer(int64) ::], [value], status)
    end subroutine hdf5_add_i32_scalar

    subroutine hdf5_add_i32_1(this, name, values, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(int32), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status

        call add_i32_flat(this, name, [int(size(values), int64)], values, status)
    end subroutine hdf5_add_i32_1

    subroutine hdf5_add_r64_scalar(this, name, value, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status

        call add_r64_flat(this, name, [integer(int64) ::], [value], status)
    end subroutine hdf5_add_r64_scalar

    subroutine hdf5_add_r64_1(this, name, values, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status

        call add_r64_flat(this, name, [int(size(values), int64)], values, status)
    end subroutine hdf5_add_r64_1

    subroutine hdf5_add_r64_2(this, name, values, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: values(:, :)
        type(fortio_status_t), intent(inout) :: status

        call add_r64_flat(this, name, int(shape(values), int64), &
                          reshape(values, [size(values)]), status)
    end subroutine hdf5_add_r64_2

    subroutine hdf5_add_r64_3(this, name, values, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: values(:, :, :)
        type(fortio_status_t), intent(inout) :: status

        call add_r64_flat(this, name, int(shape(values), int64), &
                          reshape(values, [size(values)]), status)
    end subroutine hdf5_add_r64_3

    subroutine add_i32_flat(this, name, dimensions, values, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: dimensions(:)
        integer(int32), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_output_dataset_t) :: dataset

        if (.not. prepare_dataset(this, name, dimensions, size(values, kind=int64), status)) &
            return
        dataset%name = trim(name)
        dataset%type_code = TYPE_I32
        dataset%dimensions = dimensions
        dataset%values_i32 = values
        call append_dataset(this%datasets, dataset)
    end subroutine add_i32_flat

    subroutine add_r64_flat(this, name, dimensions, values, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: dimensions(:)
        real(real64), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_output_dataset_t) :: dataset

        if (.not. prepare_dataset(this, name, dimensions, size(values, kind=int64), status)) &
            return
        dataset%name = trim(name)
        dataset%type_code = TYPE_R64
        dataset%dimensions = dimensions
        dataset%values_r64 = values
        call append_dataset(this%datasets, dataset)
    end subroutine add_r64_flat

    logical function prepare_dataset(this, name, dimensions, count, status)
        class(hdf5_writer_t), intent(in) :: this
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: dimensions(:), count
        type(fortio_status_t), intent(inout) :: status
        integer :: i

        call status%clear()
        prepare_dataset = this%opened
        if (.not. prepare_dataset) then
            call status%set(FORTIO_ESTATE, "HDF5 writer is not open")
            return
        end if
        if (len_trim(name) == 0 .or. len_trim(name) > 255 .or. index(name, "/") > 0) then
            call status%set(FORTIO_ENOTSUP, "root dataset name is not supported")
            prepare_dataset = .false.
            return
        end if
        if (product(dimensions) /= count) then
            call status%set(FORTIO_ESHAPE, "HDF5 dataset shape does not match values")
            prepare_dataset = .false.
            return
        end if
        do i = 1, size(this%datasets)
            if (this%datasets(i)%name == trim(name)) then
                call status%set(FORTIO_ESTATE, "duplicate HDF5 dataset name")
                prepare_dataset = .false.
                return
            end if
        end do
    end function prepare_dataset

    subroutine append_dataset(datasets, dataset)
        type(hdf5_output_dataset_t), allocatable, intent(inout) :: datasets(:)
        type(hdf5_output_dataset_t), intent(in) :: dataset
        type(hdf5_output_dataset_t), allocatable :: temporary(:)
        integer :: count

        count = size(datasets)
        allocate(temporary(count + 1))
        if (count > 0) temporary(:count) = datasets
        temporary(count + 1) = dataset
        call move_alloc(temporary, datasets)
    end subroutine append_dataset

    subroutine hdf5_writer_close(this, status)
        class(hdf5_writer_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status
        type(byte_writer_t) :: writer
        integer(int8), allocatable :: metadata(:)
        integer(int64) :: next_address, eof_address
        integer :: i, j

        call status%clear()
        if (.not. this%opened) return
        next_address = ROOT_ADDRESS + root_header_size(this%datasets)
        do i = 1, size(this%datasets)
            this%datasets(i)%object_address = next_address
            next_address = next_address + dataset_header_size(this%datasets(i))
        end do
        do i = 1, size(this%datasets)
            this%datasets(i)%data_address = next_address
            next_address = next_address + dataset_data_size(this%datasets(i))
        end do
        eof_address = next_address

        call writer%open(this%path, status)
        if (.not. status%ok()) return
        metadata = make_superblock(eof_address)
        call writer%write_bytes(metadata, status)
        if (.not. status%ok()) return
        metadata = make_root_header(this%datasets)
        call writer%write_bytes(metadata, status)
        if (.not. status%ok()) return
        do i = 1, size(this%datasets)
            metadata = make_dataset_header(this%datasets(i))
            call writer%write_bytes(metadata, status)
            if (.not. status%ok()) return
        end do
        do i = 1, size(this%datasets)
            select case (this%datasets(i)%type_code)
            case (TYPE_I32)
                do j = 1, size(this%datasets(i)%values_i32)
                    call writer%write_le_i32(this%datasets(i)%values_i32(j), status)
                    if (.not. status%ok()) return
                end do
            case (TYPE_R64)
                do j = 1, size(this%datasets(i)%values_r64)
                    call writer%write_le_r64(this%datasets(i)%values_r64(j), status)
                    if (.not. status%ok()) return
                end do
            end select
        end do
        call writer%close(status)
        this%opened = .false.
    end subroutine hdf5_writer_close

    integer(int64) function root_header_size(datasets) result(total)
        type(hdf5_output_dataset_t), intent(in) :: datasets(:)
        integer(int64) :: chunk_size
        integer :: i, width

        chunk_size = 28_int64
        do i = 1, size(datasets)
            chunk_size = chunk_size + 15 + len(datasets(i)%name)
        end do
        width = merge(1, 2, chunk_size <= 255)
        total = 6 + width + chunk_size + 4
    end function root_header_size

    integer(int64) function dataset_header_size(dataset) result(total)
        type(hdf5_output_dataset_t), intent(in) :: dataset
        integer(int64) :: chunk_size

        chunk_size = dataset_chunk_size(dataset)
        total = 7 + chunk_size + 4
    end function dataset_header_size

    integer(int64) function dataset_chunk_size(dataset) result(total)
        type(hdf5_output_dataset_t), intent(in) :: dataset
        integer :: datatype_size

        datatype_size = merge(12, 20, dataset%type_code == TYPE_I32)
        total = 4 + (4 + 8*size(dataset%dimensions)) + 4 + datatype_size + 4 + 18
    end function dataset_chunk_size

    integer(int64) function dataset_data_size(dataset) result(total)
        type(hdf5_output_dataset_t), intent(in) :: dataset

        if (dataset%type_code == TYPE_I32) then
            total = 4_int64*size(dataset%values_i32, kind=int64)
        else
            total = 8_int64*size(dataset%values_r64, kind=int64)
        end if
    end function dataset_data_size

    function make_superblock(eof_address) result(bytes)
        integer(int64), intent(in) :: eof_address
        integer(int8), allocatable :: bytes(:)

        allocate(bytes(0))
        call append_values(bytes, [int(z'89', int8), int(z'48', int8), int(z'44', int8), &
            int(z'46', int8), int(z'0d', int8), int(z'0a', int8), int(z'1a', int8), &
            int(z'0a', int8)])
        call append_u8(bytes, 3)
        call append_u8(bytes, 8)
        call append_u8(bytes, 8)
        call append_u8(bytes, 0)
        call append_le64(bytes, 0_int64)
        call append_le64(bytes, -1_int64)
        call append_le64(bytes, eof_address)
        call append_le64(bytes, ROOT_ADDRESS)
        call append_checksum(bytes)
    end function make_superblock

    function make_root_header(datasets) result(bytes)
        type(hdf5_output_dataset_t), intent(in) :: datasets(:)
        integer(int8), allocatable :: bytes(:), chunk(:), payload(:)
        integer(int64) :: chunk_size
        integer :: i, width, flags

        allocate(chunk(0))
        allocate(payload(0))
        call append_u8(payload, 0)
        call append_u8(payload, 0)
        call append_le64(payload, -1_int64)
        call append_le64(payload, -1_int64)
        call append_message(chunk, 2, 0, payload)
        deallocate(payload)
        allocate(payload(0))
        call append_u8(payload, 0)
        call append_u8(payload, 0)
        call append_message(chunk, 10, 1, payload)
        do i = 1, size(datasets)
            deallocate(payload)
            allocate(payload(0))
            call append_u8(payload, 1)
            call append_u8(payload, 0)
            call append_u8(payload, len(datasets(i)%name))
            call append_text(payload, datasets(i)%name)
            call append_le64(payload, datasets(i)%object_address)
            call append_message(chunk, 6, 0, payload)
        end do
        chunk_size = size(chunk, kind=int64)
        width = merge(1, 2, chunk_size <= 255)
        flags = merge(0, 1, width == 1)
        allocate(bytes(0))
        call append_text(bytes, "OHDR")
        call append_u8(bytes, 2)
        call append_u8(bytes, flags)
        call append_unsigned(bytes, chunk_size, width)
        call append_values(bytes, chunk)
        call append_checksum(bytes)
    end function make_root_header

    function make_dataset_header(dataset) result(bytes)
        type(hdf5_output_dataset_t), intent(in) :: dataset
        integer(int8), allocatable :: bytes(:), chunk(:), payload(:)
        integer(int64) :: chunk_size
        integer :: i

        allocate(chunk(0), payload(0))
        call append_u8(payload, 2)
        call append_u8(payload, size(dataset%dimensions))
        call append_u8(payload, 0)
        call append_u8(payload, merge(0, 1, size(dataset%dimensions) == 0))
        do i = size(dataset%dimensions), 1, -1
            call append_le64(payload, dataset%dimensions(i))
        end do
        call append_message(chunk, 1, 0, payload)
        deallocate(payload)
        allocate(payload(0))
        if (dataset%type_code == TYPE_I32) then
            call append_values(payload, [int(z'10', int8), int(z'08', int8), 0_int8, &
                0_int8, 4_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, &
                int(z'20', int8), 0_int8])
        else
            call append_values(payload, [int(z'11', int8), int(z'20', int8), &
                int(z'3f', int8), 0_int8, 8_int8, 0_int8, 0_int8, 0_int8, &
                0_int8, 0_int8, int(z'40', int8), 0_int8, int(z'34', int8), &
                int(z'0b', int8), 0_int8, int(z'34', int8), -1_int8, 3_int8, &
                0_int8, 0_int8])
        end if
        call append_message(chunk, 3, 1, payload)
        deallocate(payload)
        allocate(payload(0))
        call append_u8(payload, 3)
        call append_u8(payload, 1)
        call append_le64(payload, dataset%data_address)
        call append_le64(payload, dataset_data_size(dataset))
        call append_message(chunk, 8, 0, payload)
        chunk_size = size(chunk, kind=int64)
        allocate(bytes(0))
        call append_text(bytes, "OHDR")
        call append_u8(bytes, 2)
        call append_u8(bytes, 0)
        call append_u8(bytes, int(chunk_size))
        call append_values(bytes, chunk)
        call append_checksum(bytes)
    end function make_dataset_header

    subroutine append_message(bytes, message_type, flags, payload)
        integer(int8), allocatable, intent(inout) :: bytes(:)
        integer, intent(in) :: message_type, flags
        integer(int8), intent(in) :: payload(:)

        call append_u8(bytes, message_type)
        call append_le16(bytes, size(payload))
        call append_u8(bytes, flags)
        call append_values(bytes, payload)
    end subroutine append_message

    subroutine append_checksum(bytes)
        integer(int8), allocatable, intent(inout) :: bytes(:)

        call append_le32(bytes, lookup3_checksum(bytes))
    end subroutine append_checksum

    subroutine append_text(bytes, text)
        integer(int8), allocatable, intent(inout) :: bytes(:)
        character(len=*), intent(in) :: text
        integer(int8), allocatable :: values(:)
        integer :: i

        allocate(values(len(text)))
        do i = 1, len(text)
            values(i) = int(iachar(text(i:i)), int8)
        end do
        call append_values(bytes, values)
    end subroutine append_text

    subroutine append_u8(bytes, value)
        integer(int8), allocatable, intent(inout) :: bytes(:)
        integer, intent(in) :: value

        call append_values(bytes, [int(value, int8)])
    end subroutine append_u8

    subroutine append_le16(bytes, value)
        integer(int8), allocatable, intent(inout) :: bytes(:)
        integer, intent(in) :: value

        call append_values(bytes, [int(iand(value, 255), int8), &
                                   int(iand(shiftr(value, 8), 255), int8)])
    end subroutine append_le16

    subroutine append_le32(bytes, value)
        integer(int8), allocatable, intent(inout) :: bytes(:)
        integer(int32), intent(in) :: value
        integer(int8) :: values(4)
        integer :: i

        do i = 1, 4
            values(i) = int(iand(shiftr(value, 8*(i - 1)), int(z'ff', int32)), int8)
        end do
        call append_values(bytes, values)
    end subroutine append_le32

    subroutine append_le64(bytes, value)
        integer(int8), allocatable, intent(inout) :: bytes(:)
        integer(int64), intent(in) :: value
        integer(int8) :: values(8)
        integer :: i

        do i = 1, 8
            values(i) = int(iand(shiftr(value, 8*(i - 1)), int(z'ff', int64)), int8)
        end do
        call append_values(bytes, values)
    end subroutine append_le64

    subroutine append_unsigned(bytes, value, width)
        integer(int8), allocatable, intent(inout) :: bytes(:)
        integer(int64), intent(in) :: value
        integer, intent(in) :: width
        integer :: i

        do i = 1, width
            call append_u8(bytes, int(iand(shiftr(value, 8*(i - 1)), 255_int64)))
        end do
    end subroutine append_unsigned

    subroutine append_values(bytes, values)
        integer(int8), allocatable, intent(inout) :: bytes(:)
        integer(int8), intent(in) :: values(:)
        integer(int8), allocatable :: temporary(:)
        integer :: count

        count = size(bytes)
        allocate(temporary(count + size(values)))
        if (count > 0) temporary(:count) = bytes
        if (size(values) > 0) temporary(count + 1:) = values
        call move_alloc(temporary, bytes)
    end subroutine append_values

end module fortio_hdf5_writer
