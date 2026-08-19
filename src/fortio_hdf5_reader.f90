module fortio_hdf5_reader
    use, intrinsic :: iso_c_binding, only: c_f_pointer, c_loc
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    use fortio_bytes, only: byte_reader_t
    use fortio_deflate, only: deflate_uncompress, unshuffle_bytes, unshuffle_r64
    use fortio_status, only: fortio_status_t, FORTIO_EFORMAT, FORTIO_ENOTFOUND, &
        FORTIO_ENOTSUP, FORTIO_ESHAPE, FORTIO_ETYPE
    implicit none
    private

    integer, parameter :: H5_MSG_DATASPACE = 1
    integer, parameter :: H5_MSG_LINK_INFO = 2
    integer, parameter :: H5_MSG_LINK = 6
    integer, parameter :: H5_MSG_LAYOUT = 8
    integer, parameter :: H5_MSG_FILTER_PIPELINE = 11
    integer, parameter :: H5_MSG_ATTRIBUTE = 12
    integer, parameter :: H5_MSG_CONTINUATION = 16
    integer, parameter :: H5_MSG_SYMBOL_TABLE = 17
    integer, parameter :: H5_MSG_ATTRIBUTE_INFO = 21
    integer, parameter :: H5_TYPE_INTEGER = 0
    integer, parameter :: H5_TYPE_FLOAT = 1
    integer, parameter :: H5_TYPE_STRING = 3
    integer, parameter :: H5_TYPE_COMPOUND = 6
    integer, parameter :: H5_LAYOUT_CONTIGUOUS = 1
    integer, parameter :: H5_LAYOUT_CHUNKED = 2

    type :: hdf5_link_t
        character(len=:), allocatable :: name
        integer(int64) :: address = -1_int64
    end type hdf5_link_t

    type, public :: hdf5_attribute_t
        character(len=:), allocatable :: name
        integer(int32), allocatable :: values_i32(:)
        integer(int64), allocatable :: values_i64(:)
        real(real64), allocatable :: values_r64(:)
        character(len=:), allocatable :: value_text
    end type hdf5_attribute_t

    type :: hdf5_dataset_t
        integer(int64), allocatable :: dimensions(:)
        integer :: type_class = -1
        integer :: element_size = 0
        logical :: little_endian = .true.
        integer(int64) :: data_address = -1_int64
        integer(int64) :: data_size = 0_int64
        integer :: filter_mask = 0
        integer :: shuffle_index = -1
        integer :: deflate_index = -1
        type(hdf5_attribute_t), allocatable :: attributes(:)
    end type hdf5_dataset_t

    type, public :: hdf5_file_t
        type(byte_reader_t) :: reader
        integer :: superblock_version = -1
        integer :: offset_size = 0
        integer :: length_size = 0
        integer(int64) :: base_address = 0_int64
        integer(int64) :: root_address = -1_int64
        integer(int64) :: root_btree_address = -1_int64
        integer(int64) :: root_heap_address = -1_int64
        character(len=:), allocatable :: cached_dataset_path
        type(hdf5_dataset_t) :: cached_dataset
        logical :: opened = .false.
    contains
        procedure :: open => hdf5_open
        procedure :: close => hdf5_close
        procedure :: read_i32_scalar => hdf5_read_i32_scalar
        procedure :: read_i32_1 => hdf5_read_i32_1
        procedure :: read_i32_2 => hdf5_read_i32_2
        procedure :: read_i32_3 => hdf5_read_i32_3
        procedure :: read_i64_1 => hdf5_read_i64_1
        procedure :: read_r64_scalar => hdf5_read_r64_scalar
        procedure :: read_r64_1 => hdf5_read_r64_1
        procedure :: read_r64_2 => hdf5_read_r64_2
        procedure :: read_into_r64_2 => hdf5_read_into_r64_2
        procedure :: read_r64_3 => hdf5_read_r64_3
        procedure :: read_r64_4 => hdf5_read_r64_4
        procedure :: read_r64_5 => hdf5_read_r64_5
        procedure :: read_c64_1 => hdf5_read_c64_1
        procedure :: read_c64_2 => hdf5_read_c64_2
        procedure :: read_c64_3 => hdf5_read_c64_3
        procedure :: read_i32_attribute => hdf5_read_i32_attribute
        procedure :: read_text_scalar => hdf5_read_text_scalar
        procedure :: exists => hdf5_exists
        procedure :: describe => hdf5_describe
        procedure :: list_children => hdf5_list_children
        procedure :: get_attributes => hdf5_get_attributes
    end type hdf5_file_t

contains

    subroutine hdf5_describe(this, path, is_group, type_class, dimensions, status, element_size)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        logical, intent(out) :: is_group
        integer, intent(out) :: type_class
        integer(int64), allocatable, intent(out) :: dimensions(:)
        type(fortio_status_t), intent(inout) :: status
        integer, intent(out), optional :: element_size
        type(hdf5_dataset_t) :: dataset
        integer(int64) :: address

        call resolve_object_address(this, path, address, status)
        if (.not. status%ok()) return
        call parse_dataset_header(this, address, dataset, status)
        if (.not. status%ok()) return
        is_group = .not. allocated(dataset%dimensions)
        if (is_group) then
            type_class = -1
            if (present(element_size)) element_size = 0
            allocate(dimensions(0))
        else
            type_class = dataset%type_class
            if (present(element_size)) element_size = dataset%element_size
            dimensions = dataset%dimensions
        end if
    end subroutine hdf5_describe

    subroutine hdf5_list_children(this, path, names, group_flags, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: names(:)
        logical, allocatable, intent(out) :: group_flags(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_link_t), allocatable :: links(:)
        type(hdf5_dataset_t) :: dataset
        integer(int64) :: address
        integer :: i, name_length

        call resolve_object_address(this, path, address, status)
        if (.not. status%ok()) return
        call parse_links(this, address, links, status)
        if (.not. status%ok()) return
        name_length = 1
        do i = 1, size(links)
            name_length = max(name_length, len(links(i)%name))
        end do
        allocate(character(len=name_length) :: names(size(links)))
        allocate(group_flags(size(links)))
        do i = 1, size(links)
            names(i) = links(i)%name
            call parse_dataset_header(this, links(i)%address, dataset, status)
            if (.not. status%ok()) return
            group_flags(i) = .not. allocated(dataset%dimensions)
        end do
    end subroutine hdf5_list_children

    subroutine hdf5_get_attributes(this, path, attributes, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(hdf5_attribute_t), allocatable, intent(out) :: attributes(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset

        if (len_trim(path) == 0 .or. trim(path) == "/") then
            call parse_dataset_header(this, this%root_address, dataset, status)
        else
            call find_dataset(this, path, dataset, status)
        end if
        if (.not. status%ok()) return
        if (allocated(dataset%attributes)) then
            attributes = dataset%attributes
        else
            allocate(attributes(0))
        end if
    end subroutine hdf5_get_attributes

    subroutine hdf5_open(this, path, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: signature(8), version_byte, size_byte
        integer(int16) :: ignored_k
        integer(int32) :: ignored_flags, cache_type
        integer(int64) :: ignored_address, name_offset

        if (allocated(this%cached_dataset_path)) deallocate(this%cached_dataset_path)
        call this%reader%open(path, status)
        if (.not. status%ok()) return
        call this%reader%read_bytes(signature, status)
        if (.not. status%ok()) return
        if (.not. valid_signature(signature)) then
            call status%set(FORTIO_EFORMAT, "not an HDF5 file")
            return
        end if
        call this%reader%read_i8(version_byte, status)
        if (.not. status%ok()) return
        this%superblock_version = byte_value(version_byte)
        if (this%superblock_version == 0) then
            ! Version-0 superblocks keep the root group in the old symbol-table
            ! format. The four version/reserved bytes precede the offset sizes.
            call skip_bytes(this%reader, 4_int64)
            call this%reader%read_i8(size_byte, status)
            this%offset_size = byte_value(size_byte)
            call this%reader%read_i8(size_byte, status)
            this%length_size = byte_value(size_byte)
            call skip_bytes(this%reader, 1_int64)
            call this%reader%read_le_i16(ignored_k, status)
            call this%reader%read_le_i16(ignored_k, status)
            call this%reader%read_le_i32(ignored_flags, status)
            if (this%offset_size /= 8 .or. this%length_size /= 8) then
                call status%set(FORTIO_ENOTSUP, &
                    "only 64-bit HDF5 offsets and lengths are supported")
                return
            end if
            call read_unsigned(this%reader, this%offset_size, this%base_address, status)
            call read_unsigned(this%reader, this%offset_size, ignored_address, status)
            call read_unsigned(this%reader, this%offset_size, ignored_address, status)
            call read_unsigned(this%reader, this%offset_size, ignored_address, status)
            call read_unsigned(this%reader, this%offset_size, name_offset, status)
            call read_unsigned(this%reader, this%offset_size, this%root_address, status)
            call this%reader%read_le_i32(cache_type, status)
            call skip_bytes(this%reader, 4_int64)
            call read_unsigned(this%reader, this%offset_size, this%root_btree_address, status)
            call read_unsigned(this%reader, this%offset_size, this%root_heap_address, status)
            if (.not. status%ok()) return
            this%opened = .true.
            return
        end if
        if (this%superblock_version /= 2 .and. this%superblock_version /= 3) then
            call status%set(FORTIO_ENOTSUP, "HDF5 superblock version is not supported")
            return
        end if
        call this%reader%read_i8(size_byte, status)
        this%offset_size = byte_value(size_byte)
        call this%reader%read_i8(size_byte, status)
        this%length_size = byte_value(size_byte)
        call this%reader%read_i8(size_byte, status)
        ignored_flags = byte_value(size_byte)
        if (this%offset_size /= 8 .or. this%length_size /= 8) then
            call status%set(FORTIO_ENOTSUP, "only 64-bit HDF5 offsets and lengths are supported")
            return
        end if
        call this%reader%read_le_i64(this%base_address, status)
        call this%reader%read_le_i64(ignored_address, status)
        call this%reader%read_le_i64(ignored_address, status)
        call this%reader%read_le_i64(this%root_address, status)
        if (.not. status%ok()) return
        this%root_btree_address = -1_int64
        this%root_heap_address = -1_int64
        this%opened = .true.
    end subroutine hdf5_open

    subroutine hdf5_close(this, status)
        class(hdf5_file_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status

        call this%reader%close(status)
        if (allocated(this%cached_dataset_path)) deallocate(this%cached_dataset_path)
        this%opened = .false.
    end subroutine hdf5_close

    subroutine hdf5_read_i32_scalar(this, path, value, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int32), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int32), allocatable :: values(:)

        call read_i32_flat(this, path, values, status)
        if (.not. status%ok()) return
        if (size(values) /= 1) then
            call status%set(FORTIO_ESHAPE, "dataset is not scalar")
            return
        end if
        value = values(1)
    end subroutine hdf5_read_i32_scalar

    subroutine hdf5_read_i32_1(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int32), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (size(dataset%dimensions) /= 1) then
            call status%set(FORTIO_ESHAPE, "dataset rank does not match rank 1")
            return
        end if
        call read_i32_flat(this, path, values, status)
    end subroutine hdf5_read_i32_1

    subroutine hdf5_read_i32_2(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int32), allocatable, intent(out) :: values(:, :)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
        integer(int32), allocatable :: flat(:)

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (size(dataset%dimensions) /= 2) then
            call status%set(FORTIO_ESHAPE, "dataset rank does not match rank 2")
            return
        end if
        call read_i32_flat(this, path, flat, status)
        if (.not. status%ok()) return
        allocate(values(dataset%dimensions(2), dataset%dimensions(1)))
        values = reshape(flat, shape(values))
    end subroutine hdf5_read_i32_2

    subroutine hdf5_read_i32_3(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int32), allocatable, intent(out) :: values(:, :, :)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
        integer(int32), allocatable :: flat(:)

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (size(dataset%dimensions) /= 3) then
            call status%set(FORTIO_ESHAPE, "dataset rank does not match rank 3")
            return
        end if
        call read_i32_flat(this, path, flat, status)
        if (.not. status%ok()) return
        allocate(values(dataset%dimensions(3), dataset%dimensions(2), &
            dataset%dimensions(1)))
        values = reshape(flat, shape(values))
    end subroutine hdf5_read_i32_3

    subroutine hdf5_read_i64_1(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int64), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
        integer :: i

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (size(dataset%dimensions) /= 1) then
            call status%set(FORTIO_ESHAPE, "64-bit integer dataset is not rank 1")
            return
        end if
        if (dataset%type_class /= H5_TYPE_INTEGER .or. dataset%element_size /= 8) then
            call status%set(FORTIO_ETYPE, "dataset is not a 64-bit integer")
            return
        end if
        allocate(values(dataset%dimensions(1)))
        call this%reader%seek(this%base_address + dataset%data_address + 1)
        do i = 1, size(values)
            call this%reader%read_le_i64(values(i), status)
            if (.not. status%ok()) return
        end do
    end subroutine hdf5_read_i64_1

    subroutine read_i32_flat(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int32), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
        integer(int8), allocatable :: bytes(:)
        integer(int64) :: count
        integer :: i

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (dataset%type_class /= H5_TYPE_INTEGER .or. dataset%element_size /= 4) then
            call status%set(FORTIO_ETYPE, "dataset is not a 32-bit integer")
            return
        end if
        count = product(dataset%dimensions)
        allocate(values(count))
        call read_dataset_bytes(this, dataset, bytes, status)
        if (.not. status%ok()) return
        do i = 1, size(values)
            values(i) = decode_i32(bytes(4*i - 3:4*i), dataset%little_endian)
        end do
    end subroutine read_i32_flat

    pure integer(int32) function decode_i32(bytes, little_endian) result(value)
        integer(int8), intent(in) :: bytes(4)
        logical, intent(in) :: little_endian
        integer(int64) :: bits
        integer :: i, position

        bits = 0_int64
        do i = 1, 4
            if (little_endian) then
                position = i - 1
            else
                position = 4 - i
            end if
            bits = ior(bits, shiftl(iand(int(bytes(i), int64), 255_int64), &
                8*position))
        end do
        value = int(bits, int32)
    end function decode_i32

    subroutine hdf5_read_i32_attribute(this, path, name, values, found, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path, name
        integer(int32), allocatable, intent(out) :: values(:)
        logical, intent(out) :: found
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
        integer :: i

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        found = .false.
        allocate(values(0))
        if (.not. allocated(dataset%attributes)) return
        do i = 1, size(dataset%attributes)
            if (dataset%attributes(i)%name == name) then
                if (allocated(dataset%attributes(i)%values_i32)) then
                    values = dataset%attributes(i)%values_i32
                else if (allocated(dataset%attributes(i)%values_i64)) then
                    if (any(dataset%attributes(i)%values_i64 > int(huge(0_int32), int64)) .or. &
                        any(dataset%attributes(i)%values_i64 < &
                        int(-huge(0_int32) - 1_int32, int64))) then
                        call status%set(FORTIO_ETYPE, &
                            "64-bit HDF5 attribute does not fit a 32-bit integer")
                        return
                    end if
                    values = int(dataset%attributes(i)%values_i64, int32)
                else
                    call status%set(FORTIO_ETYPE, "HDF5 attribute is not an integer")
                    return
                end if
                found = .true.
                return
            end if
        end do
    end subroutine hdf5_read_i32_attribute

    subroutine hdf5_read_text_scalar(this, path, value, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
        integer(int8) :: byte
        integer :: i

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (dataset%type_class /= H5_TYPE_STRING) then
            call status%set(FORTIO_ETYPE, "dataset is not a fixed-width string")
            return
        end if
        if (size(dataset%dimensions) /= 0) then
            call status%set(FORTIO_ESHAPE, "string dataset is not scalar")
            return
        end if
        allocate(character(len=dataset%element_size) :: value)
        call this%reader%seek(this%base_address + dataset%data_address + 1)
        do i = 1, len(value)
            call this%reader%read_i8(byte, status)
            if (.not. status%ok()) return
            value(i:i) = achar(byte_value(byte))
        end do
    end subroutine hdf5_read_text_scalar

    subroutine hdf5_exists(this, path, exists, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        logical, intent(out) :: exists
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_link_t), allocatable :: links(:)
        character(len=:), allocatable :: remaining, component
        integer(int64) :: address
        integer :: separator, i
        logical :: matched

        call status%clear()
        exists = .false.
        remaining = trim(adjustl(path))
        do while (len(remaining) > 0)
            if (remaining(1:1) /= "/") exit
            remaining = remaining(2:)
        end do
        if (len(remaining) == 0) then
            exists = .true.
            return
        end if
        address = this%root_address
        do
            separator = index(remaining, "/")
            if (separator == 0) then
                component = remaining
                remaining = ""
            else
                component = remaining(:separator - 1)
                remaining = remaining(separator + 1:)
            end if
            if (len(component) == 0) then
                if (len(remaining) == 0) exit
                cycle
            end if
            call parse_links(this, address, links, status)
            if (.not. status%ok()) return
            matched = .false.
            do i = 1, size(links)
                if (links(i)%name == component) then
                    address = links(i)%address
                    matched = .true.
                    exit
                end if
            end do
            if (.not. matched) return
            if (len(remaining) == 0) exit
        end do
        exists = .true.
    end subroutine hdf5_exists

    subroutine hdf5_read_r64_scalar(this, path, value, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        real(real64), allocatable :: values(:)

        call read_r64_flat(this, path, values, status)
        if (.not. status%ok()) return
        if (size(values) /= 1) then
            call status%set(FORTIO_ESHAPE, "dataset is not scalar")
            return
        end if
        value = values(1)
    end subroutine hdf5_read_r64_scalar

    subroutine hdf5_read_r64_1(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (size(dataset%dimensions) /= 1) then
            call status%set(FORTIO_ESHAPE, "dataset rank does not match rank 1")
            return
        end if
        call read_r64_flat(this, path, values, status)
    end subroutine hdf5_read_r64_1

    subroutine hdf5_read_r64_2(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), allocatable, target, intent(out) :: values(:, :)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
        real(real64), pointer :: flat(:)

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (size(dataset%dimensions) /= 2) then
            call status%set(FORTIO_ESHAPE, "dataset rank does not match rank 2")
            return
        end if
        ! HDF5 stores C dimension order; expose native Fortran order.
        allocate(values(dataset%dimensions(2), dataset%dimensions(1)))
        flat(1:size(values)) => values
        call read_r64_values(this, dataset, flat, status)
    end subroutine hdf5_read_r64_2

    subroutine hdf5_read_into_r64_2(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), contiguous, target, intent(out) :: values(:, :)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
        real(real64), pointer :: flat(:)

        if (allocated(this%cached_dataset_path)) then
            if (this%cached_dataset_path == trim(adjustl(path))) then
                call validate_r64_2_destination(this%cached_dataset, values, status)
                if (.not. status%ok()) return
                flat(1:size(values)) => values
                call read_r64_values(this, this%cached_dataset, flat, status)
                return
            end if
        end if
        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        call validate_r64_2_destination(dataset, values, status)
        if (.not. status%ok()) return
        flat(1:size(values)) => values
        call read_r64_values(this, dataset, flat, status)
    end subroutine hdf5_read_into_r64_2

    subroutine validate_r64_2_destination(dataset, values, status)
        type(hdf5_dataset_t), intent(in) :: dataset
        real(real64), intent(in) :: values(:, :)
        type(fortio_status_t), intent(inout) :: status

        call status%clear()
        if (size(dataset%dimensions) /= 2) then
            call status%set(FORTIO_ESHAPE, "dataset rank does not match rank 2")
            return
        end if
        if (any(shape(values) /= [dataset%dimensions(2), dataset%dimensions(1)])) &
            call status%set(FORTIO_ESHAPE, "destination shape does not match dataset")
    end subroutine validate_r64_2_destination

    subroutine hdf5_read_r64_3(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), allocatable, intent(out) :: values(:, :, :)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
        real(real64), allocatable :: flat(:)

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (size(dataset%dimensions) /= 3) then
            call status%set(FORTIO_ESHAPE, "dataset rank does not match rank 3")
            return
        end if
        call read_r64_flat(this, path, flat, status)
        if (.not. status%ok()) return
        allocate(values(dataset%dimensions(3), dataset%dimensions(2), &
            dataset%dimensions(1)))
        values = reshape(flat, shape(values))
    end subroutine hdf5_read_r64_3

    subroutine hdf5_read_r64_4(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), allocatable, intent(out) :: values(:, :, :, :)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
        real(real64), allocatable :: flat(:)

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (size(dataset%dimensions) /= 4) then
            call status%set(FORTIO_ESHAPE, "dataset rank does not match rank 4")
            return
        end if
        call read_r64_flat(this, path, flat, status)
        if (.not. status%ok()) return
        allocate(values(dataset%dimensions(4), dataset%dimensions(3), &
            dataset%dimensions(2), dataset%dimensions(1)))
        values = reshape(flat, shape(values))
    end subroutine hdf5_read_r64_4

    subroutine hdf5_read_r64_5(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), allocatable, intent(out) :: values(:, :, :, :, :)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
        real(real64), allocatable :: flat(:)

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (size(dataset%dimensions) /= 5) then
            call status%set(FORTIO_ESHAPE, "dataset rank does not match rank 5")
            return
        end if
        call read_r64_flat(this, path, flat, status)
        if (.not. status%ok()) return
        allocate(values(dataset%dimensions(5), dataset%dimensions(4), &
            dataset%dimensions(3), dataset%dimensions(2), &
            dataset%dimensions(1)))
        values = reshape(flat, shape(values))
    end subroutine hdf5_read_r64_5

    subroutine read_r64_flat(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
        integer(int64) :: count

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (dataset%type_class /= H5_TYPE_FLOAT) then
            call status%set(FORTIO_ETYPE, "dataset is not floating point")
            return
        end if
        count = product(dataset%dimensions)
        allocate(values(count))
        call read_r64_values(this, dataset, values, status)
    end subroutine read_r64_flat

    subroutine read_r64_values(this, dataset, values, status)
        class(hdf5_file_t), intent(inout) :: this
        type(hdf5_dataset_t), intent(in) :: dataset
        real(real64), contiguous, target, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        real(real32) :: value_r32
        integer(int8), allocatable :: bytes(:), inflated(:), ordered(:), stored(:)
        integer(int8), pointer :: direct_bytes(:)
        logical :: apply_deflate, apply_shuffle, native_little_endian
        integer :: i

        native_little_endian = host_is_little_endian()
        apply_deflate = filter_is_applied(dataset%deflate_index, dataset%filter_mask)
        apply_shuffle = filter_is_applied(dataset%shuffle_index, dataset%filter_mask)
        if (dataset%element_size == 8) then
            if (dataset%little_endian .eqv. native_little_endian) then
                if (apply_deflate) then
                    if (apply_shuffle) then
                        allocate(stored(dataset%data_size))
                        call this%reader%seek( &
                            this%base_address + dataset%data_address + 1)
                        call this%reader%read_bytes(stored, status)
                        if (.not. status%ok()) return
                        call deflate_uncompress(stored, 8_int64*size(values), inflated, &
                            status)
                        if (.not. status%ok()) return
                        call unshuffle_r64(inflated, values)
                        return
                    end if
                end if
                if (.not. apply_deflate) then
                    if (.not. apply_shuffle) then
                        call c_f_pointer(c_loc(values), direct_bytes, [8*size(values)])
                        call this%reader%seek(this%base_address + dataset%data_address + 1)
                        call this%reader%read_bytes(direct_bytes, status)
                        return
                    end if
                end if
            end if
        end if
        call read_dataset_bytes(this, dataset, bytes, status)
        if (.not. status%ok()) return
        select case (dataset%element_size)
        case (8)
            if (dataset%little_endian .eqv. native_little_endian) then
                values = transfer(bytes, values)
            else
                ordered = bytes
                call reverse_element_bytes(ordered, 8)
                values = transfer(ordered, values)
            end if
        case (4)
            if (dataset%little_endian .eqv. native_little_endian) then
                do i = 1, size(values)
                    value_r32 = transfer(bytes(4*i - 3:4*i), value_r32)
                    values(i) = real(value_r32, real64)
                end do
            else
                ordered = bytes
                call reverse_element_bytes(ordered, 4)
                do i = 1, size(values)
                    value_r32 = transfer(ordered(4*i - 3:4*i), value_r32)
                    values(i) = real(value_r32, real64)
                end do
            end if
        case default
            call status%set(FORTIO_ETYPE, "floating-point width is not supported")
        end select
    end subroutine read_r64_values

    pure logical function host_is_little_endian()
        integer(int8) :: bytes(4)

        bytes = transfer(1_int32, bytes)
        host_is_little_endian = bytes(1) == 1_int8
    end function host_is_little_endian

    pure subroutine reverse_element_bytes(bytes, element_size)
        integer(int8), intent(inout) :: bytes(:)
        integer, intent(in) :: element_size
        integer(int8) :: temporary
        integer :: element, first, left, right

        do element = 1, size(bytes)/element_size
            first = (element - 1)*element_size
            do left = 1, element_size/2
                right = element_size - left + 1
                temporary = bytes(first + left)
                bytes(first + left) = bytes(first + right)
                bytes(first + right) = temporary
            end do
        end do
    end subroutine reverse_element_bytes

    subroutine read_dataset_bytes(this, dataset, bytes, status)
        class(hdf5_file_t), intent(inout) :: this
        type(hdf5_dataset_t), intent(in) :: dataset
        integer(int8), allocatable, intent(out) :: bytes(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8), allocatable :: stored(:), inflated(:)
        integer(int64) :: expected_size
        logical :: apply_deflate, apply_shuffle

        call status%clear()
        allocate(stored(dataset%data_size))
        call this%reader%seek(this%base_address + dataset%data_address + 1)
        call this%reader%read_bytes(stored, status)
        if (.not. status%ok()) return
        apply_deflate = filter_is_applied(dataset%deflate_index, dataset%filter_mask)
        apply_shuffle = filter_is_applied(dataset%shuffle_index, dataset%filter_mask)
        expected_size = product(dataset%dimensions)*dataset%element_size
        if (size(dataset%dimensions) == 0) expected_size = dataset%element_size
        if (apply_deflate) then
            call deflate_uncompress(stored, expected_size, inflated, status)
            if (.not. status%ok()) return
        else
            inflated = stored
        end if
        if (apply_shuffle) then
            call unshuffle_bytes(inflated, dataset%element_size, bytes)
        else
            bytes = inflated
        end if
    end subroutine read_dataset_bytes

    logical function filter_is_applied(index, mask)
        integer, intent(in) :: index, mask

        filter_is_applied = .false.
        if (index < 0) return
        filter_is_applied = .not. btest(mask, index)
    end function filter_is_applied

    pure real(real64) function decode_r64(bytes, little_endian) result(value)
        integer(int8), intent(in) :: bytes(8)
        logical, intent(in) :: little_endian
        integer(int8) :: ordered(8)

        if (little_endian) then
            ordered = bytes
        else
            ordered = bytes(8:1:-1)
        end if
        value = transfer(ordered, value)
    end function decode_r64

    pure real(real32) function decode_r32(bytes, little_endian) result(value)
        integer(int8), intent(in) :: bytes(4)
        logical, intent(in) :: little_endian
        integer(int8) :: ordered(4)

        if (little_endian) then
            ordered = bytes
        else
            ordered = bytes(4:1:-1)
        end if
        value = transfer(ordered, value)
    end function decode_r32

    subroutine hdf5_read_c64_1(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        complex(real64), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (size(dataset%dimensions) /= 1) then
            call status%set(FORTIO_ESHAPE, "complex dataset rank does not match rank 1")
            return
        end if
        call read_c64_flat(this, path, values, status)
    end subroutine hdf5_read_c64_1

    subroutine hdf5_read_c64_2(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        complex(real64), allocatable, intent(out) :: values(:, :)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
        complex(real64), allocatable :: flat(:)

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (size(dataset%dimensions) /= 2) then
            call status%set(FORTIO_ESHAPE, "complex dataset rank does not match rank 2")
            return
        end if
        call read_c64_flat(this, path, flat, status)
        if (.not. status%ok()) return
        allocate(values(dataset%dimensions(2), dataset%dimensions(1)))
        values = reshape(flat, shape(values))
    end subroutine hdf5_read_c64_2

    subroutine hdf5_read_c64_3(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        complex(real64), allocatable, intent(out) :: values(:, :, :)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
        complex(real64), allocatable :: flat(:)

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (size(dataset%dimensions) /= 3) then
            call status%set(FORTIO_ESHAPE, "complex dataset rank does not match rank 3")
            return
        end if
        call read_c64_flat(this, path, flat, status)
        if (.not. status%ok()) return
        allocate(values(dataset%dimensions(3), dataset%dimensions(2), &
            dataset%dimensions(1)))
        values = reshape(flat, shape(values))
    end subroutine hdf5_read_c64_3

    subroutine read_c64_flat(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        complex(real64), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
        real(real64) :: real_part, imaginary_part
        integer(int64) :: count
        integer :: i

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (dataset%type_class /= H5_TYPE_COMPOUND .or. dataset%element_size /= 16) then
            call status%set(FORTIO_ETYPE, "dataset is not a double complex compound")
            return
        end if
        count = product(dataset%dimensions)
        allocate(values(count))
        call this%reader%seek(this%base_address + dataset%data_address + 1)
        do i = 1, size(values)
            call this%reader%read_le_r64(real_part, status)
            if (.not. status%ok()) return
            call this%reader%read_le_r64(imaginary_part, status)
            if (.not. status%ok()) return
            values(i) = cmplx(real_part, imaginary_part, real64)
        end do
    end subroutine read_c64_flat

    subroutine find_dataset(this, path, dataset, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(hdf5_dataset_t), intent(out) :: dataset
        type(fortio_status_t), intent(inout) :: status
        character(len=:), allocatable :: remaining, component
        integer(int64) :: address, child_address
        integer :: separator

        call status%clear()
        remaining = trim(adjustl(path))
        do while (len(remaining) > 0)
            if (remaining(1:1) /= "/") exit
            remaining = remaining(2:)
        end do
        if (len(remaining) == 0) then
            call status%set(FORTIO_ENOTFOUND, "HDF5 dataset path is empty")
            return
        end if
        if (allocated(this%cached_dataset_path)) then
            if (this%cached_dataset_path == remaining) then
                dataset = this%cached_dataset
                return
            end if
        end if
        address = this%root_address
        do
            separator = index(remaining, "/")
            if (separator == 0) then
                component = remaining
                remaining = ""
            else
                component = remaining(:separator - 1)
                remaining = remaining(separator + 1:)
            end if
            if (len(component) == 0) cycle
            call find_child(this, address, component, child_address, status)
            if (.not. status%ok()) return
            address = child_address
            if (len(remaining) == 0) exit
        end do
        call parse_dataset_header(this, address, dataset, status)
        if (status%ok()) then
            this%cached_dataset_path = trim(adjustl(path))
            do while (len(this%cached_dataset_path) > 0)
                if (this%cached_dataset_path(1:1) /= "/") exit
                this%cached_dataset_path = this%cached_dataset_path(2:)
            end do
            this%cached_dataset = dataset
        end if
    end subroutine find_dataset

    subroutine resolve_object_address(this, path, address, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int64), intent(out) :: address
        type(fortio_status_t), intent(inout) :: status
        character(len=:), allocatable :: remaining, component
        integer(int64) :: child_address
        integer :: separator

        call status%clear()
        remaining = trim(adjustl(path))
        if (remaining == ".") remaining = ""
        do while (len(remaining) > 0)
            if (remaining(1:1) /= "/") exit
            remaining = remaining(2:)
        end do
        address = this%root_address
        do while (len(remaining) > 0)
            separator = index(remaining, "/")
            if (separator == 0) then
                component = remaining
                remaining = ""
            else
                component = remaining(:separator - 1)
                remaining = remaining(separator + 1:)
            end if
            if (len(component) == 0) cycle
            call find_child(this, address, component, child_address, status)
            if (.not. status%ok()) return
            address = child_address
        end do
    end subroutine resolve_object_address

    subroutine find_child(this, object_address, name, child_address, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: object_address
        character(len=*), intent(in) :: name
        integer(int64), intent(out) :: child_address
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_link_t), allocatable :: links(:)
        integer :: i

        call parse_links(this, object_address, links, status)
        if (.not. status%ok()) return
        do i = 1, size(links)
            if (links(i)%name == name) then
                child_address = links(i)%address
                return
            end if
        end do
        call status%set(FORTIO_ENOTFOUND, "HDF5 object not found: "//name)
    end subroutine find_child

    subroutine parse_links(this, address, links, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: address
        type(hdf5_link_t), allocatable, intent(out) :: links(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: ignored_dataset

        allocate(links(0))
        call parse_object_header(this, address, links, ignored_dataset, .true., status)
    end subroutine parse_links

    subroutine parse_dataset_header(this, address, dataset, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: address
        type(hdf5_dataset_t), intent(out) :: dataset
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_link_t), allocatable :: ignored_links(:)

        allocate(ignored_links(0))
        call parse_object_header(this, address, ignored_links, dataset, .false., status)
    end subroutine parse_dataset_header

    subroutine parse_object_header(this, address, links, dataset, want_links, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: address
        type(hdf5_link_t), allocatable, intent(inout) :: links(:)
        type(hdf5_dataset_t), intent(inout) :: dataset
        logical, intent(in) :: want_links
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: signature(4), version_byte, flags_byte
        integer(int64) :: chunk_size, chunk_start
        integer :: flags, size_width, i

        call this%reader%seek(this%base_address + address + 1)
        call this%reader%read_bytes(signature, status)
        if (.not. status%ok()) return
        if (byte_value(signature(1)) == 1) then
            call parse_legacy_object_header(this, address, links, dataset, want_links, status)
            return
        end if
        if (bytes_text(signature) /= "OHDR") then
            call status%set(FORTIO_ENOTSUP, "only HDF5 v2 object headers are supported")
            return
        end if
        call this%reader%read_i8(version_byte, status)
        call this%reader%read_i8(flags_byte, status)
        if (.not. status%ok()) return
        if (byte_value(version_byte) /= 2) then
            call status%set(FORTIO_ENOTSUP, "HDF5 object header version is not supported")
            return
        end if
        flags = byte_value(flags_byte)
        if (btest(flags, 5)) then
            do i = 1, 4
                call skip_bytes(this%reader, 4_int64)
            end do
        end if
        if (btest(flags, 4)) call skip_bytes(this%reader, 4_int64)
        size_width = 2**iand(flags, 3)
        call read_unsigned(this%reader, size_width, chunk_size, status)
        if (.not. status%ok()) return
        chunk_start = this%reader%position
        call parse_message_chunk(this, chunk_start, chunk_size, links, dataset, &
            want_links, flags, status)
    end subroutine parse_object_header

    subroutine parse_legacy_object_header(this, address, links, dataset, want_links, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: address
        type(hdf5_link_t), allocatable, intent(inout) :: links(:)
        type(hdf5_dataset_t), intent(inout) :: dataset
        logical, intent(in) :: want_links
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: version_byte, reserved_byte
        integer(int16) :: message_count_i16, message_type_i16, message_size_i16
        integer(int32) :: reference_count, chunk_size_i32, message_flags
        integer(int64) :: chunk_start, chunk_end, data_start, next_message
        integer(int64) :: btree_address, heap_address

        btree_address = -1_int64
        heap_address = -1_int64
        call this%reader%seek(this%base_address + address + 1)
        call this%reader%read_i8(version_byte, status)
        call this%reader%read_i8(reserved_byte, status)
        call this%reader%read_le_i16(message_count_i16, status)
        call this%reader%read_le_i32(reference_count, status)
        call this%reader%read_le_i32(chunk_size_i32, status)
        call this%reader%read_le_i32(message_flags, status)
        if (.not. status%ok()) return
        if (byte_value(version_byte) /= 1) then
            call status%set(FORTIO_ENOTSUP, "HDF5 legacy object header version is not supported")
            return
        end if
        if (chunk_size_i32 < 16) then
            call status%set(FORTIO_EFORMAT, "invalid HDF5 legacy object header size")
            return
        end if
        chunk_start = this%reader%position
        chunk_end = chunk_start + int(chunk_size_i32, int64) - 16_int64
        do while (this%reader%position + 8_int64 <= chunk_end)
            call this%reader%read_le_i16(message_type_i16, status)
            call this%reader%read_le_i16(message_size_i16, status)
            call this%reader%read_le_i32(message_flags, status)
            if (.not. status%ok()) return
            data_start = this%reader%position
            next_message = data_start + int(message_size_i16, int64)
            if (message_type_i16 == 0) exit
            select case (int(message_type_i16))
            case (H5_MSG_SYMBOL_TABLE)
                if (want_links) then
                    call read_unsigned(this%reader, this%offset_size, btree_address, status)
                    call read_unsigned(this%reader, this%offset_size, heap_address, status)
                end if
            case (H5_MSG_DATASPACE)
                if (.not. want_links) call parse_dataspace_message(this, dataset, status)
            case (3)
                if (.not. want_links) call parse_datatype_message(this, dataset, status)
            case (H5_MSG_LAYOUT)
                if (.not. want_links) call parse_layout_message(this, dataset, status)
            case (H5_MSG_ATTRIBUTE)
                if (.not. want_links) call parse_attribute_message(this, dataset, status)
            end select
            if (.not. status%ok()) return
            call this%reader%seek(next_message)
        end do
        if (want_links) then
            if (btree_address < 0 .or. heap_address < 0) then
                call status%set(FORTIO_EFORMAT, "HDF5 legacy group lacks a symbol table")
                return
            end if
            call parse_legacy_symbol_table(this, btree_address, heap_address, links, status)
        end if
    end subroutine parse_legacy_object_header

    subroutine parse_legacy_symbol_table(this, btree_address, heap_address, links, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: btree_address, heap_address
        type(hdf5_link_t), allocatable, intent(inout) :: links(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: signature(4), node_type, level
        integer(int16) :: entry_count_i16
        integer(int64) :: ignored_key, child_address, entry_end
        integer :: i, entry_count

        call this%reader%seek(this%base_address + btree_address + 1)
        call this%reader%read_bytes(signature, status)
        call this%reader%read_i8(node_type, status)
        call this%reader%read_i8(level, status)
        call this%reader%read_le_i16(entry_count_i16, status)
        if (.not. status%ok()) return
        if (bytes_text(signature) /= "TREE" .or. byte_value(node_type) /= 0 .or. &
            byte_value(level) /= 0) then
            call status%set(FORTIO_ENOTSUP, &
                "only level-zero HDF5 legacy symbol tables are supported")
            return
        end if
        entry_count = int(entry_count_i16)
        call skip_bytes(this%reader, 2_int64*this%offset_size)
        do i = 1, entry_count
            call read_unsigned(this%reader, this%offset_size, ignored_key, status)
            call read_unsigned(this%reader, this%offset_size, child_address, status)
            if (.not. status%ok()) return
            if (child_address < 0) cycle
            entry_end = this%reader%position
            call parse_legacy_symbol_node(this, child_address, heap_address, links, status)
            if (.not. status%ok()) return
            call this%reader%seek(entry_end)
        end do
    end subroutine parse_legacy_symbol_table

    subroutine parse_legacy_symbol_node(this, address, heap_address, links, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: address, heap_address
        type(hdf5_link_t), allocatable, intent(inout) :: links(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: signature(4), version_byte, reserved_byte
        integer(int16) :: entry_count_i16
        integer(int64) :: name_offset, object_address, entry_end
        character(len=:), allocatable :: name
        integer :: i, entry_count

        call this%reader%seek(this%base_address + address + 1)
        call this%reader%read_bytes(signature, status)
        call this%reader%read_i8(version_byte, status)
        call this%reader%read_i8(reserved_byte, status)
        call this%reader%read_le_i16(entry_count_i16, status)
        if (.not. status%ok()) return
        if (bytes_text(signature) /= "SNOD" .or. byte_value(version_byte) /= 1) then
            call status%set(FORTIO_ENOTSUP, "HDF5 legacy symbol node version is not supported")
            return
        end if
        entry_count = int(entry_count_i16)
        do i = 1, entry_count
            call read_unsigned(this%reader, this%offset_size, name_offset, status)
            call read_unsigned(this%reader, this%offset_size, object_address, status)
            call skip_bytes(this%reader, 24_int64)
            if (.not. status%ok()) return
            entry_end = this%reader%position
            call read_legacy_heap_string(this, heap_address, name_offset, name, status)
            if (.not. status%ok()) return
            call append_link(links, name, object_address)
            call this%reader%seek(entry_end)
        end do
    end subroutine parse_legacy_symbol_node

    subroutine read_legacy_heap_string(this, heap_address, name_offset, name, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: heap_address, name_offset
        character(len=:), allocatable, intent(out) :: name
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: signature(4), version_byte, reserved_byte, byte
        integer(int64) :: data_address, data_size, free_list_address
        integer :: i, length

        call this%reader%seek(this%base_address + heap_address + 1)
        call this%reader%read_bytes(signature, status)
        call this%reader%read_i8(version_byte, status)
        call this%reader%read_i8(reserved_byte, status)
        call skip_bytes(this%reader, 2_int64)
        call read_unsigned(this%reader, this%length_size, data_size, status)
        call read_unsigned(this%reader, this%offset_size, free_list_address, status)
        call read_unsigned(this%reader, this%offset_size, data_address, status)
        if (.not. status%ok()) return
        if (bytes_text(signature) /= "HEAP" .or. byte_value(version_byte) /= 0) then
            call status%set(FORTIO_ENOTSUP, "HDF5 legacy local heap version is not supported")
            return
        end if
        if (name_offset < 0 .or. name_offset >= data_size) then
            call status%set(FORTIO_EFORMAT, "HDF5 legacy link name is outside the local heap")
            return
        end if
        call this%reader%seek(this%base_address + data_address + name_offset + 1)
        length = 0
        do while (int(name_offset, int64) + length < data_size)
            call this%reader%read_i8(byte, status)
            if (.not. status%ok()) return
            if (byte_value(byte) == 0) exit
            length = length + 1
        end do
        if (int(name_offset, int64) + length >= data_size .and. byte_value(byte) /= 0) then
            call status%set(FORTIO_EFORMAT, "unterminated HDF5 legacy link name")
            return
        end if
        allocate(character(len=length) :: name)
        if (length == 0) return
        call this%reader%seek(this%base_address + data_address + name_offset + 1)
        do i = 1, length
            call this%reader%read_i8(byte, status)
            if (.not. status%ok()) return
            name(i:i) = achar(byte_value(byte))
        end do
    end subroutine read_legacy_heap_string

    subroutine append_link(links, name, address)
        type(hdf5_link_t), allocatable, intent(inout) :: links(:)
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: address
        type(hdf5_link_t), allocatable :: temporary(:)
        integer :: count

        count = size(links)
        allocate(temporary(count + 1))
        if (count > 0) temporary(:count) = links
        temporary(count + 1)%name = name
        temporary(count + 1)%address = address
        call move_alloc(temporary, links)
    end subroutine append_link

    recursive subroutine parse_message_chunk(this, start, length, links, dataset, &
            want_links, header_flags, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: start, length
        type(hdf5_link_t), allocatable, intent(inout) :: links(:)
        type(hdf5_dataset_t), intent(inout) :: dataset
        logical, intent(in) :: want_links
        integer, intent(in) :: header_flags
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: type_byte, message_flags
        integer(int16) :: size_value
        integer(int64) :: data_start, next_message, continuation_address, &
            continuation_size
        integer :: message_type

        call this%reader%seek(start)
        do while (this%reader%position + 4 <= start + length)
            call this%reader%read_i8(type_byte, status)
            call this%reader%read_le_i16(size_value, status)
            call this%reader%read_i8(message_flags, status)
            if (.not. status%ok()) return
            message_type = byte_value(type_byte)
            if (btest(header_flags, 2)) call skip_bytes(this%reader, 2_int64)
            data_start = this%reader%position
            next_message = data_start + int(size_value, int64)
            select case (message_type)
            case (H5_MSG_LINK_INFO)
                if (want_links) call parse_link_info_message(this, links, status)
            case (H5_MSG_LINK)
                if (want_links) call parse_link_message(this, links, status)
            case (H5_MSG_DATASPACE)
                if (.not. want_links) call parse_dataspace_message(this, dataset, status)
            case (3)
                if (.not. want_links) call parse_datatype_message(this, dataset, status)
            case (H5_MSG_LAYOUT)
                if (.not. want_links) call parse_layout_message(this, dataset, status)
            case (H5_MSG_FILTER_PIPELINE)
                if (.not. want_links) call parse_filter_message(this, dataset, status)
            case (H5_MSG_ATTRIBUTE)
                if (.not. want_links) call parse_attribute_message(this, dataset, status)
            case (H5_MSG_ATTRIBUTE_INFO)
                if (.not. want_links) call parse_attribute_info_message(this, dataset, status)
            case (H5_MSG_CONTINUATION)
                call this%reader%read_le_i64(continuation_address, status)
                call this%reader%read_le_i64(continuation_size, status)
                if (.not. status%ok()) return
                call parse_continuation(this, continuation_address, continuation_size, &
                    links, dataset, want_links, header_flags, status)
            end select
            if (.not. status%ok()) return
            call this%reader%seek(next_message)
        end do
    end subroutine parse_message_chunk

    recursive subroutine parse_continuation(this, address, length, links, dataset, want_links, &
            header_flags, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: address, length
        type(hdf5_link_t), allocatable, intent(inout) :: links(:)
        type(hdf5_dataset_t), intent(inout) :: dataset
        logical, intent(in) :: want_links
        integer, intent(in) :: header_flags
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: signature(4)
        integer(int64) :: chunk_start

        call this%reader%seek(this%base_address + address + 1)
        call this%reader%read_bytes(signature, status)
        if (.not. status%ok()) return
        if (bytes_text(signature) /= "OCHK") then
            call status%set(FORTIO_EFORMAT, "invalid HDF5 continuation chunk")
            return
        end if
        ! Copy the position before passing it: passing the component directly
        ! aliases the reader state that parse_message_chunk advances.
        chunk_start = this%reader%position
        call parse_message_chunk(this, chunk_start, length - 8, links, &
            dataset, want_links, header_flags, status)
    end subroutine parse_continuation

    subroutine parse_link_message(this, links, status)
        class(hdf5_file_t), intent(inout) :: this
        type(hdf5_link_t), allocatable, intent(inout) :: links(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_link_t), allocatable :: temporary(:)
        integer(int8) :: version_byte, flags_byte, optional_byte
        integer(int64) :: name_length, address
        integer :: flags, length_width, i, count
        character(len=:), allocatable :: name

        call this%reader%read_i8(version_byte, status)
        call this%reader%read_i8(flags_byte, status)
        if (.not. status%ok()) return
        if (byte_value(version_byte) /= 1) then
            call status%set(FORTIO_ENOTSUP, "HDF5 link message version is not supported")
            return
        end if
        flags = byte_value(flags_byte)
        length_width = 2**iand(flags, 3)
        if (btest(flags, 3)) then
            call this%reader%read_i8(optional_byte, status)
            if (byte_value(optional_byte) /= 0) then
                call status%set(FORTIO_ENOTSUP, "only HDF5 hard links are supported")
                return
            end if
        end if
        if (btest(flags, 2)) call skip_bytes(this%reader, 8_int64)
        if (btest(flags, 4)) call skip_bytes(this%reader, 1_int64)
        call read_unsigned(this%reader, length_width, name_length, status)
        if (.not. status%ok()) return
        allocate(character(len=name_length) :: name)
        do i = 1, int(name_length)
            call this%reader%read_i8(optional_byte, status)
            name(i:i) = achar(byte_value(optional_byte))
        end do
        call this%reader%read_le_i64(address, status)
        if (.not. status%ok()) return
        count = size(links)
        allocate(temporary(count + 1))
        if (count > 0) temporary(:count) = links
        temporary(count + 1)%name = name
        temporary(count + 1)%address = address
        call move_alloc(temporary, links)
    end subroutine parse_link_message

    subroutine parse_link_info_message(this, links, status)
        class(hdf5_file_t), intent(inout) :: this
        type(hdf5_link_t), allocatable, intent(inout) :: links(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: byte
        integer(int64) :: heap_address, tree_address
        integer :: flags

        call this%reader%read_i8(byte, status)
        if (.not. status%ok()) return
        if (byte_value(byte) /= 0) return
        call this%reader%read_i8(byte, status)
        if (.not. status%ok()) return
        flags = byte_value(byte)
        if (btest(flags, 0)) call skip_bytes(this%reader, 8_int64)
        call this%reader%read_le_i64(heap_address, status)
        call this%reader%read_le_i64(tree_address, status)
        if (.not. status%ok()) return
        if (btest(flags, 1)) call skip_bytes(this%reader, 8_int64)
        if (heap_address == -1_int64 .or. tree_address == -1_int64) return
        call parse_dense_links(this, heap_address, tree_address, links, status)
    end subroutine parse_link_info_message

    subroutine parse_dense_links(this, heap_address, tree_address, links, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: heap_address, tree_address
        type(hdf5_link_t), allocatable, intent(inout) :: links(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: signature(4), byte
        integer(int16) :: record_size_i16, depth_i16, record_count_i16
        integer(int32) :: node_size
        integer(int64) :: root_address, ignored_i64, child_address, child_count_i64
        integer(int64) :: next_pointer
        integer :: tree_type, record_size, depth, record_count, child_count, i

        call this%reader%seek(this%base_address + tree_address + 1)
        call this%reader%read_bytes(signature, status)
        call this%reader%read_i8(byte, status)
        call this%reader%read_i8(byte, status)
        tree_type = byte_value(byte)
        call this%reader%read_le_i32(node_size, status)
        call this%reader%read_le_i16(record_size_i16, status)
        call this%reader%read_le_i16(depth_i16, status)
        call skip_bytes(this%reader, 2_int64)
        call this%reader%read_le_i64(root_address, status)
        call this%reader%read_le_i16(record_count_i16, status)
        call this%reader%read_le_i64(ignored_i64, status)
        if (.not. status%ok()) return
        record_size = iand(int(record_size_i16), int(z'ffff'))
        depth = iand(int(depth_i16), int(z'ffff'))
        record_count = iand(int(record_count_i16), int(z'ffff'))
        if (bytes_text(signature) /= "BTHD" .or. tree_type /= 5 .or. &
            record_size /= 11 .or. depth < 0 .or. depth > 1) then
            call status%set(FORTIO_ENOTSUP, &
                "dense HDF5 group B-tree form is not supported")
            return
        end if
        call this%reader%seek(this%base_address + root_address + 1)
        call this%reader%read_bytes(signature, status)
        call this%reader%read_i8(byte, status)
        call this%reader%read_i8(byte, status)
        if (.not. status%ok()) return
        if (depth == 0) then
            if (bytes_text(signature) /= "BTLF" .or. byte_value(byte) /= 5) then
                call status%set(FORTIO_EFORMAT, "invalid dense HDF5 group B-tree leaf")
                return
            end if
            call parse_dense_leaf_records(this, heap_address, record_count, links, status)
            return
        end if
        if (bytes_text(signature) /= "BTIN" .or. byte_value(byte) /= 5) then
            call status%set(FORTIO_EFORMAT, "invalid dense HDF5 group B-tree internal node")
            return
        end if
        call skip_bytes(this%reader, int(record_count*record_size, int64))
        do i = 1, record_count + 1
            call this%reader%read_le_i64(child_address, status)
            call read_unsigned(this%reader, 1, child_count_i64, status)
            if (.not. status%ok()) return
            child_count = int(child_count_i64)
            next_pointer = this%reader%position
            call this%reader%seek(this%base_address + child_address + 1)
            call this%reader%read_bytes(signature, status)
            call this%reader%read_i8(byte, status)
            call this%reader%read_i8(byte, status)
            if (.not. status%ok()) return
            if (bytes_text(signature) /= "BTLF" .or. byte_value(byte) /= 5) then
                call status%set(FORTIO_EFORMAT, "invalid dense HDF5 group B-tree leaf")
                return
            end if
            call parse_dense_leaf_records(this, heap_address, child_count, links, status)
            if (.not. status%ok()) return
            call this%reader%seek(next_pointer)
        end do
    end subroutine parse_dense_links

    subroutine parse_dense_leaf_records(this, heap_address, record_count, links, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: heap_address
        integer, intent(in) :: record_count
        type(hdf5_link_t), allocatable, intent(inout) :: links(:)
        type(fortio_status_t), intent(inout) :: status
        integer :: i

        do i = 1, record_count
            call skip_bytes(this%reader, 4_int64)
            call parse_dense_heap_id(this, heap_address, links, status)
            if (.not. status%ok()) return
        end do
    end subroutine parse_dense_leaf_records

    subroutine parse_dense_heap_id(this, heap_address, links, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: heap_address
        type(hdf5_link_t), allocatable, intent(inout) :: links(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: byte
        integer(int16) :: heap_id_length_i16, max_heap_bits_i16, current_rows_i16
        integer(int16) :: table_width_i16
        integer(int64) :: direct_address, object_offset, object_length, saved_position
        integer(int64) :: starting_block_size, block_offset
        integer :: heap_id_length, offset_width, length_width, current_rows
        integer :: table_width, column

        saved_position = this%reader%position
        call this%reader%read_i8(byte, status)
        if (.not. status%ok()) return
        if (byte_value(byte) /= 0) then
            call status%set(FORTIO_ENOTSUP, "only managed dense HDF5 links are supported")
            return
        end if
        call this%reader%seek(this%base_address + heap_address + 6)
        call this%reader%read_le_i16(heap_id_length_i16, status)
        call this%reader%seek(this%base_address + heap_address + 129)
        call this%reader%read_le_i16(max_heap_bits_i16, status)
        call this%reader%seek(this%base_address + heap_address + 111)
        call this%reader%read_le_i16(table_width_i16, status)
        call this%reader%read_le_i64(starting_block_size, status)
        call this%reader%seek(this%base_address + heap_address + 133)
        call this%reader%read_le_i64(direct_address, status)
        call this%reader%read_le_i16(current_rows_i16, status)
        if (.not. status%ok()) return
        current_rows = iand(int(current_rows_i16), int(z'ffff'))
        table_width = iand(int(table_width_i16), int(z'ffff'))
        heap_id_length = iand(int(heap_id_length_i16), int(z'ffff'))
        offset_width = (iand(int(max_heap_bits_i16), int(z'ffff')) + 7)/8
        length_width = heap_id_length - 1 - offset_width
        call this%reader%seek(saved_position + 1)
        call read_unsigned(this%reader, offset_width, object_offset, status)
        call read_unsigned(this%reader, length_width, object_length, status)
        saved_position = this%reader%position
        if (.not. status%ok()) return
        block_offset = 0_int64
        if (current_rows /= 0) then
            if (current_rows /= 1 .or. starting_block_size <= 0 .or. table_width <= 0) then
                call status%set(FORTIO_ENOTSUP, &
                    "multi-row dense HDF5 group heap is not supported")
                return
            end if
            column = int(object_offset/starting_block_size)
            if (column < 0 .or. column >= table_width) then
                call status%set(FORTIO_EFORMAT, "dense HDF5 heap offset is outside its root row")
                return
            end if
            call this%reader%seek(this%base_address + direct_address + 18 + &
                int(column, int64)*8_int64)
            call this%reader%read_le_i64(direct_address, status)
            if (.not. status%ok()) return
            block_offset = int(column, int64)*starting_block_size
        end if
        call parse_dense_link_object(this, direct_address, object_offset, &
            block_offset, object_length, links, status)
        call this%reader%seek(saved_position)
    end subroutine parse_dense_heap_id

    subroutine parse_dense_link_object(this, direct_address, object_offset, &
            block_offset, object_length, links, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: direct_address, object_offset, block_offset
        integer(int64), intent(in) :: object_length
        type(hdf5_link_t), allocatable, intent(inout) :: links(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: signature(4), byte
        integer(int64) :: object_start

        call this%reader%seek(this%base_address + direct_address + 1)
        call this%reader%read_bytes(signature, status)
        call this%reader%read_i8(byte, status)
        if (.not. status%ok()) return
        if (bytes_text(signature) /= "FHDB" .or. byte_value(byte) /= 0) then
            call status%set(FORTIO_EFORMAT, "invalid HDF5 fractal-heap direct block")
            return
        end if
        ! Managed heap offsets are relative to the direct-block signature and
        ! therefore already include the direct-block header.
        object_start = this%base_address + direct_address + object_offset - block_offset + 1
        call this%reader%seek(object_start)
        call parse_link_message(this, links, status)
        if (.not. status%ok()) return
        if (this%reader%position > object_start + object_length) &
            call status%set(FORTIO_EFORMAT, "dense HDF5 link exceeds heap object")
    end subroutine parse_dense_link_object

    subroutine parse_attribute_info_message(this, dataset, status)
        class(hdf5_file_t), intent(inout) :: this
        type(hdf5_dataset_t), intent(inout) :: dataset
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: byte
        integer(int64) :: heap_address, tree_address
        integer :: flags

        call this%reader%read_i8(byte, status)
        if (.not. status%ok()) return
        if (byte_value(byte) /= 0) return
        call this%reader%read_i8(byte, status)
        if (.not. status%ok()) return
        flags = byte_value(byte)
        if (btest(flags, 0)) call skip_bytes(this%reader, 2_int64)
        call this%reader%read_le_i64(heap_address, status)
        call this%reader%read_le_i64(tree_address, status)
        if (.not. status%ok()) return
        if (heap_address == -1_int64 .or. tree_address == -1_int64) return
        call parse_dense_attributes(this, heap_address, tree_address, dataset, status)
    end subroutine parse_attribute_info_message

    subroutine parse_dense_attributes(this, heap_address, tree_address, dataset, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: heap_address, tree_address
        type(hdf5_dataset_t), intent(inout) :: dataset
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: signature(4), byte
        integer(int16) :: record_size_i16, depth_i16, record_count_i16
        integer(int32) :: node_size
        integer(int64) :: root_address, ignored_i64
        integer :: tree_type, record_size, depth, record_count, i

        call this%reader%seek(this%base_address + tree_address + 1)
        call this%reader%read_bytes(signature, status)
        call this%reader%read_i8(byte, status)
        call this%reader%read_i8(byte, status)
        tree_type = byte_value(byte)
        call this%reader%read_le_i32(node_size, status)
        call this%reader%read_le_i16(record_size_i16, status)
        call this%reader%read_le_i16(depth_i16, status)
        call skip_bytes(this%reader, 2_int64)
        call this%reader%read_le_i64(root_address, status)
        call this%reader%read_le_i16(record_count_i16, status)
        call this%reader%read_le_i64(ignored_i64, status)
        if (.not. status%ok()) return
        record_size = iand(int(record_size_i16), int(z'ffff'))
        depth = iand(int(depth_i16), int(z'ffff'))
        record_count = iand(int(record_count_i16), int(z'ffff'))
        if (bytes_text(signature) /= "BTHD" .or. tree_type /= 8 .or. &
            record_size /= 17 .or. depth /= 0) then
            call status%set(FORTIO_ENOTSUP, &
                "dense HDF5 attribute B-tree form is not supported")
            return
        end if
        call this%reader%seek(this%base_address + root_address + 1)
        call this%reader%read_bytes(signature, status)
        call this%reader%read_i8(byte, status)
        call this%reader%read_i8(byte, status)
        if (.not. status%ok()) return
        if (bytes_text(signature) /= "BTLF" .or. byte_value(byte) /= 8) then
            call status%set(FORTIO_EFORMAT, "invalid dense HDF5 attribute B-tree leaf")
            return
        end if
        do i = 1, record_count
            call parse_dense_attribute_heap_id(this, heap_address, dataset, status)
            if (.not. status%ok()) return
            call skip_bytes(this%reader, 9_int64)
        end do
    end subroutine parse_dense_attributes

    subroutine parse_dense_attribute_heap_id(this, heap_address, dataset, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: heap_address
        type(hdf5_dataset_t), intent(inout) :: dataset
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: byte, signature(4)
        integer(int16) :: heap_id_length_i16, max_heap_bits_i16, current_rows_i16
        integer(int16) :: table_width_i16
        integer(int64) :: direct_address, object_offset, object_length, saved_position
        integer(int64) :: starting_block_size, block_offset, object_start
        integer :: heap_id_length, offset_width, length_width, current_rows
        integer :: table_width, column

        saved_position = this%reader%position
        call this%reader%read_i8(byte, status)
        if (.not. status%ok()) return
        if (byte_value(byte) /= 0) then
            call status%set(FORTIO_ENOTSUP, "only managed dense HDF5 attributes are supported")
            return
        end if
        call this%reader%seek(this%base_address + heap_address + 6)
        call this%reader%read_le_i16(heap_id_length_i16, status)
        call this%reader%seek(this%base_address + heap_address + 129)
        call this%reader%read_le_i16(max_heap_bits_i16, status)
        call this%reader%seek(this%base_address + heap_address + 111)
        call this%reader%read_le_i16(table_width_i16, status)
        call this%reader%read_le_i64(starting_block_size, status)
        call this%reader%seek(this%base_address + heap_address + 133)
        call this%reader%read_le_i64(direct_address, status)
        call this%reader%read_le_i16(current_rows_i16, status)
        if (.not. status%ok()) return
        current_rows = iand(int(current_rows_i16), int(z'ffff'))
        table_width = iand(int(table_width_i16), int(z'ffff'))
        heap_id_length = iand(int(heap_id_length_i16), int(z'ffff'))
        offset_width = (iand(int(max_heap_bits_i16), int(z'ffff')) + 7)/8
        length_width = heap_id_length - 1 - offset_width
        call this%reader%seek(saved_position + 1)
        call read_unsigned(this%reader, offset_width, object_offset, status)
        call read_unsigned(this%reader, length_width, object_length, status)
        saved_position = this%reader%position
        if (.not. status%ok()) return
        block_offset = 0_int64
        if (current_rows /= 0) then
            if (current_rows /= 1 .or. starting_block_size <= 0 .or. table_width <= 0) then
                call status%set(FORTIO_ENOTSUP, &
                    "multi-row dense HDF5 attribute heap is not supported")
                return
            end if
            column = int(object_offset/starting_block_size)
            if (column < 0 .or. column >= table_width) then
                call status%set(FORTIO_EFORMAT, &
                    "dense HDF5 attribute heap offset is outside its root row")
                return
            end if
            call this%reader%seek(this%base_address + direct_address + 18 + &
                int(column, int64)*8_int64)
            call this%reader%read_le_i64(direct_address, status)
            if (.not. status%ok()) return
            block_offset = int(column, int64)*starting_block_size
        end if
        call this%reader%seek(this%base_address + direct_address + 1)
        call this%reader%read_bytes(signature, status)
        call this%reader%read_i8(byte, status)
        if (.not. status%ok()) return
        if (bytes_text(signature) /= "FHDB" .or. byte_value(byte) /= 0) then
            call status%set(FORTIO_EFORMAT, "invalid dense HDF5 attribute heap block")
            return
        end if
        object_start = this%base_address + direct_address + object_offset - block_offset + 1
        call this%reader%seek(object_start)
        call parse_attribute_message(this, dataset, status)
        if (status%ok() .and. this%reader%position > object_start + object_length) &
            call status%set(FORTIO_EFORMAT, "dense HDF5 attribute exceeds heap object")
        call this%reader%seek(saved_position)
    end subroutine parse_dense_attribute_heap_id

    subroutine parse_dataspace_message(this, dataset, status)
        class(hdf5_file_t), intent(inout) :: this
        type(hdf5_dataset_t), intent(inout) :: dataset
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: version_byte, rank_byte, flags_byte, type_byte
        integer :: i, flags

        call this%reader%read_i8(version_byte, status)
        call this%reader%read_i8(rank_byte, status)
        call this%reader%read_i8(flags_byte, status)
        call this%reader%read_i8(type_byte, status)
        if (.not. status%ok()) return
        if (byte_value(version_byte) == 1) then
            if (byte_value(rank_byte) == 0 .and. btest(byte_value(flags_byte), 0)) then
                call status%set(FORTIO_EFORMAT, "invalid scalar HDF5 dataspace")
                return
            end if
            call skip_bytes(this%reader, 4_int64)
            allocate(dataset%dimensions(byte_value(rank_byte)))
            do i = 1, size(dataset%dimensions)
                call read_unsigned(this%reader, this%length_size, dataset%dimensions(i), status)
            end do
            flags = byte_value(flags_byte)
            if (btest(flags, 0)) call skip_bytes(this%reader, &
                int(this%length_size, int64)*size(dataset%dimensions))
            return
        end if
        if (byte_value(version_byte) /= 2 .or. byte_value(type_byte) > 1) then
            call status%set(FORTIO_ENOTSUP, "HDF5 dataspace form is not supported")
            return
        end if
        if (byte_value(type_byte) == 0 .and. byte_value(rank_byte) /= 0) then
            call status%set(FORTIO_EFORMAT, "invalid scalar HDF5 dataspace")
            return
        end if
        allocate(dataset%dimensions(byte_value(rank_byte)))
        do i = 1, size(dataset%dimensions)
            call this%reader%read_le_i64(dataset%dimensions(i), status)
        end do
        flags = byte_value(flags_byte)
        if (btest(flags, 0)) call skip_bytes(this%reader, 8_int64*size(dataset%dimensions))
    end subroutine parse_dataspace_message

    subroutine parse_datatype_message(this, dataset, status)
        class(hdf5_file_t), intent(inout) :: this
        type(hdf5_dataset_t), intent(inout) :: dataset
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: class_version, class_bits(3)
        integer(int32) :: size_value

        call this%reader%read_i8(class_version, status)
        call this%reader%read_bytes(class_bits, status)
        call this%reader%read_le_i32(size_value, status)
        if (.not. status%ok()) return
        dataset%type_class = iand(byte_value(class_version), 15)
        dataset%element_size = size_value
        dataset%little_endian = .not. btest(byte_value(class_bits(1)), 0)
        if (dataset%type_class /= H5_TYPE_INTEGER .and. &
            dataset%type_class /= H5_TYPE_FLOAT .and. &
            dataset%type_class /= H5_TYPE_STRING .and. &
            dataset%type_class /= H5_TYPE_COMPOUND) then
            call status%set(FORTIO_ENOTSUP, "HDF5 datatype class is not supported")
        end if
    end subroutine parse_datatype_message

    subroutine parse_layout_message(this, dataset, status)
        class(hdf5_file_t), intent(inout) :: this
        type(hdf5_dataset_t), intent(inout) :: dataset
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: version_byte, class_byte, flags_byte, rank_byte
        integer(int8) :: width_byte, index_byte
        integer(int32) :: mask
        integer(int64) :: ignored
        integer :: i, version, layout_class, rank, width

        call this%reader%read_i8(version_byte, status)
        call this%reader%read_i8(class_byte, status)
        if (.not. status%ok()) return
        version = byte_value(version_byte)
        layout_class = byte_value(class_byte)
        if ((version == 3 .or. version == 4) .and. &
            layout_class == H5_LAYOUT_CONTIGUOUS) then
            call this%reader%read_le_i64(dataset%data_address, status)
            call this%reader%read_le_i64(dataset%data_size, status)
            return
        end if
        if (version == 3 .and. layout_class == H5_LAYOUT_CHUNKED) then
            call this%reader%read_i8(rank_byte, status)
            call read_unsigned(this%reader, this%offset_size, ignored, status)
            if (.not. status%ok()) return
            rank = byte_value(rank_byte)
            do i = 1, rank
                call this%reader%read_le_i32(mask, status)
                if (.not. status%ok()) return
            end do
            call parse_v1_chunk_btree(this, ignored, rank, dataset, status)
            return
        end if
        if ((version /= 4 .and. version /= 5) .or. &
            layout_class /= H5_LAYOUT_CHUNKED) then
            call status%set(FORTIO_ENOTSUP, "HDF5 storage layout is not supported")
            return
        end if
        call this%reader%read_i8(flags_byte, status)
        call this%reader%read_i8(rank_byte, status)
        call this%reader%read_i8(width_byte, status)
        if (.not. status%ok()) return
        rank = byte_value(rank_byte)
        width = byte_value(width_byte)
        do i = 1, rank
            call read_unsigned(this%reader, width, ignored, status)
            if (.not. status%ok()) return
        end do
        call this%reader%read_i8(index_byte, status)
        if (.not. status%ok()) return
        if (byte_value(index_byte) /= 1) then
            call status%set(FORTIO_ENOTSUP, &
                "only single-chunk HDF5 filtered datasets are supported")
            return
        end if
        if (.not. btest(byte_value(flags_byte), 1)) then
            call status%set(FORTIO_EFORMAT, "single HDF5 chunk lacks filtered size")
            return
        end if
        call read_unsigned(this%reader, this%length_size, dataset%data_size, status)
        call this%reader%read_le_i32(mask, status)
        call read_unsigned(this%reader, this%offset_size, dataset%data_address, status)
        dataset%filter_mask = mask
    end subroutine parse_layout_message

    subroutine parse_v1_chunk_btree(this, address, rank, dataset, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: address
        integer, intent(in) :: rank
        type(hdf5_dataset_t), intent(inout) :: dataset
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: signature(4), node_type, level
        integer(int16) :: entries_i16
        integer(int32) :: chunk_size_i32, filter_mask_i32
        integer(int64) :: ignored
        integer :: i, entries

        call this%reader%seek(this%base_address + address + 1)
        call this%reader%read_bytes(signature, status)
        call this%reader%read_i8(node_type, status)
        call this%reader%read_i8(level, status)
        call this%reader%read_le_i16(entries_i16, status)
        if (.not. status%ok()) return
        entries = int(entries_i16)
        if (any(signature /= [int(iachar("T"), int8), int(iachar("R"), int8), &
            int(iachar("E"), int8), int(iachar("E"), int8)]) .or. &
            byte_value(node_type) /= 1 .or. byte_value(level) /= 0 .or. &
            entries /= 1) then
            call status%set(FORTIO_ENOTSUP, &
                "only one leaf chunk in a version-1 HDF5 chunk index is supported")
            return
        end if
        call skip_bytes(this%reader, 2_int64*this%offset_size)
        call this%reader%read_le_i32(chunk_size_i32, status)
        call this%reader%read_le_i32(filter_mask_i32, status)
        do i = 1, rank
            call this%reader%read_le_i64(ignored, status)
        end do
        call read_unsigned(this%reader, this%offset_size, dataset%data_address, status)
        if (.not. status%ok()) return
        dataset%data_size = int(chunk_size_i32, int64)
        dataset%filter_mask = filter_mask_i32
    end subroutine parse_v1_chunk_btree

    subroutine parse_filter_message(this, dataset, status)
        class(hdf5_file_t), intent(inout) :: this
        type(hdf5_dataset_t), intent(inout) :: dataset
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: version_byte, count_byte
        integer(int16) :: filter_id_i16, name_length_i16, flags_i16, values_i16
        integer(int32) :: value
        integer :: count, filter_id, i, j, name_length, value_count, version

        call this%reader%read_i8(version_byte, status)
        call this%reader%read_i8(count_byte, status)
        if (.not. status%ok()) return
        version = byte_value(version_byte)
        count = byte_value(count_byte)
        if (version == 1) call skip_bytes(this%reader, 6_int64)
        do i = 0, count - 1
            call this%reader%read_le_i16(filter_id_i16, status)
            if (.not. status%ok()) return
            filter_id = int(filter_id_i16)
            name_length = 0
            if (version == 1 .or. filter_id >= 256) then
                call this%reader%read_le_i16(name_length_i16, status)
                name_length = int(name_length_i16)
            end if
            call this%reader%read_le_i16(flags_i16, status)
            call this%reader%read_le_i16(values_i16, status)
            if (.not. status%ok()) return
            value_count = int(values_i16)
            if (name_length > 0) call skip_bytes(this%reader, int(name_length, int64))
            do j = 1, value_count
                call this%reader%read_le_i32(value, status)
                if (.not. status%ok()) return
            end do
            if (version == 1 .and. mod(value_count, 2) == 1) &
                call skip_bytes(this%reader, 4_int64)
            if (filter_id == 2) dataset%shuffle_index = i
            if (filter_id == 1) dataset%deflate_index = i
        end do
    end subroutine parse_filter_message

    subroutine parse_attribute_message(this, dataset, status)
        class(hdf5_file_t), intent(inout) :: this
        type(hdf5_dataset_t), intent(inout) :: dataset
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: byte
        integer(int16) :: name_size_i16, datatype_size_i16, dataspace_size_i16
        integer(int32) :: element_size
        integer(int64) :: datatype_start, dataspace_start, value_count
        integer :: version, name_size, datatype_size, dataspace_size, rank, i, type_class
        character(len=:), allocatable :: name
        integer(int32), allocatable :: values(:)
        integer(int64), allocatable :: values_i64(:)
        real(real64), allocatable :: real_values(:)
        character(len=:), allocatable :: text_value

        call this%reader%read_i8(byte, status)
        version = byte_value(byte)
        call this%reader%read_i8(byte, status)
        call this%reader%read_le_i16(name_size_i16, status)
        call this%reader%read_le_i16(datatype_size_i16, status)
        call this%reader%read_le_i16(dataspace_size_i16, status)
        call this%reader%read_i8(byte, status)
        if (.not. status%ok()) return
        if (version /= 3) return
        name_size = iand(int(name_size_i16), int(z'ffff'))
        datatype_size = iand(int(datatype_size_i16), int(z'ffff'))
        dataspace_size = iand(int(dataspace_size_i16), int(z'ffff'))
        if (name_size < 1) return
        allocate(character(len=name_size - 1) :: name)
        do i = 1, name_size
            call this%reader%read_i8(byte, status)
            if (i < name_size) name(i:i) = achar(byte_value(byte))
        end do
        if (.not. status%ok()) return
        datatype_start = this%reader%position
        call this%reader%read_i8(byte, status)
        type_class = iand(byte_value(byte), 15)
        call skip_bytes(this%reader, 3_int64)
        call this%reader%read_le_i32(element_size, status)
        if (.not. status%ok()) return
        call this%reader%seek(datatype_start + datatype_size)
        dataspace_start = this%reader%position
        call this%reader%read_i8(byte, status)
        call this%reader%read_i8(byte, status)
        rank = byte_value(byte)
        call skip_bytes(this%reader, 2_int64)
        value_count = 1_int64
        do i = 1, rank
            call this%reader%read_le_i64(datatype_start, status)
            value_count = value_count*datatype_start
        end do
        if (.not. status%ok()) return
        call this%reader%seek(dataspace_start + dataspace_size)
        select case (type_class)
        case (H5_TYPE_INTEGER)
            if (element_size == 4) then
                allocate(values(value_count))
                do i = 1, size(values)
                    call this%reader%read_le_i32(values(i), status)
                    if (.not. status%ok()) return
                end do
                call append_i32_attribute(dataset, name, values)
            else if (element_size == 8) then
                allocate(values_i64(value_count))
                do i = 1, size(values_i64)
                    call this%reader%read_le_i64(values_i64(i), status)
                    if (.not. status%ok()) return
                end do
                call append_i64_attribute(dataset, name, values_i64)
            end if
        case (H5_TYPE_FLOAT)
            if (element_size /= 8) return
            allocate(real_values(value_count))
            do i = 1, size(real_values)
                call this%reader%read_le_r64(real_values(i), status)
                if (.not. status%ok()) return
            end do
            call append_r64_attribute(dataset, name, real_values)
        case (H5_TYPE_STRING)
            if (element_size < 1 .or. value_count /= 1) return
            allocate(character(len=element_size) :: text_value)
            do i = 1, element_size
                call this%reader%read_i8(byte, status)
                if (.not. status%ok()) return
                if (byte_value(byte) == 0) then
                    text_value(i:i) = " "
                else
                    text_value(i:i) = achar(byte_value(byte))
                end if
            end do
            call append_text_attribute(dataset, name, trim(text_value))
        end select
    end subroutine parse_attribute_message

    subroutine append_i32_attribute(dataset, name, values)
        type(hdf5_dataset_t), intent(inout) :: dataset
        character(len=*), intent(in) :: name
        integer(int32), intent(in) :: values(:)
        type(hdf5_attribute_t), allocatable :: temporary(:)
        integer :: count

        if (allocated(dataset%attributes)) then
            count = size(dataset%attributes)
        else
            count = 0
        end if
        allocate(temporary(count + 1))
        if (count > 0) temporary(:count) = dataset%attributes
        temporary(count + 1)%name = name
        temporary(count + 1)%values_i32 = values
        call move_alloc(temporary, dataset%attributes)
    end subroutine append_i32_attribute

    subroutine append_i64_attribute(dataset, name, values)
        type(hdf5_dataset_t), intent(inout) :: dataset
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: values(:)
        type(hdf5_attribute_t), allocatable :: temporary(:)
        integer :: count

        if (allocated(dataset%attributes)) then
            count = size(dataset%attributes)
        else
            count = 0
        end if
        allocate(temporary(count + 1))
        if (count > 0) temporary(:count) = dataset%attributes
        temporary(count + 1)%name = name
        temporary(count + 1)%values_i64 = values
        call move_alloc(temporary, dataset%attributes)
    end subroutine append_i64_attribute

    subroutine append_r64_attribute(dataset, name, values)
        type(hdf5_dataset_t), intent(inout) :: dataset
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: values(:)
        type(hdf5_attribute_t), allocatable :: temporary(:)
        integer :: count

        if (allocated(dataset%attributes)) then
            count = size(dataset%attributes)
        else
            count = 0
        end if
        allocate(temporary(count + 1))
        if (count > 0) temporary(:count) = dataset%attributes
        temporary(count + 1)%name = name
        temporary(count + 1)%values_r64 = values
        call move_alloc(temporary, dataset%attributes)
    end subroutine append_r64_attribute

    subroutine append_text_attribute(dataset, name, value)
        type(hdf5_dataset_t), intent(inout) :: dataset
        character(len=*), intent(in) :: name, value
        type(hdf5_attribute_t), allocatable :: temporary(:)
        integer :: count

        if (allocated(dataset%attributes)) then
            count = size(dataset%attributes)
        else
            count = 0
        end if
        allocate(temporary(count + 1))
        if (count > 0) temporary(:count) = dataset%attributes
        temporary(count + 1)%name = name
        temporary(count + 1)%value_text = value
        call move_alloc(temporary, dataset%attributes)
    end subroutine append_text_attribute

    subroutine read_unsigned(reader, width, value, status)
        type(byte_reader_t), intent(inout) :: reader
        integer, intent(in) :: width
        integer(int64), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: byte
        integer :: i

        value = 0_int64
        do i = 1, width
            call reader%read_i8(byte, status)
            if (.not. status%ok()) return
            value = ior(value, shiftl(int(byte_value(byte), int64), 8*(i - 1)))
        end do
    end subroutine read_unsigned

    subroutine skip_bytes(reader, count)
        type(byte_reader_t), intent(inout) :: reader
        integer(int64), intent(in) :: count

        call reader%seek(reader%position + count)
    end subroutine skip_bytes

    pure logical function valid_signature(bytes)
        integer(int8), intent(in) :: bytes(8)

        valid_signature = byte_value(bytes(1)) == 137
        if (valid_signature) valid_signature = bytes_text(bytes(2:4)) == "HDF"
        if (valid_signature) valid_signature = byte_value(bytes(5)) == 13
        if (valid_signature) valid_signature = byte_value(bytes(6)) == 10
        if (valid_signature) valid_signature = byte_value(bytes(7)) == 26
        if (valid_signature) valid_signature = byte_value(bytes(8)) == 10
    end function valid_signature

    pure function bytes_text(bytes) result(text)
        integer(int8), intent(in) :: bytes(:)
        character(len=size(bytes)) :: text
        integer :: i

        do i = 1, size(bytes)
            text(i:i) = achar(byte_value(bytes(i)))
        end do
    end function bytes_text

    pure integer function byte_value(value)
        integer(int8), intent(in) :: value

        byte_value = iand(int(value), 255)
    end function byte_value

end module fortio_hdf5_reader
