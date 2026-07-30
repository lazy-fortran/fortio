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
    integer, parameter :: TYPE_TEXT = 3
    integer(int64), parameter :: ROOT_ADDRESS = 48_int64

    type :: hdf5_output_attribute_t
        character(len=:), allocatable :: name
        integer :: type_code = 0
        integer(int32), allocatable :: values_i32(:)
        real(real64), allocatable :: values_r64(:)
        character(len=:), allocatable :: value_text
    end type hdf5_output_attribute_t

    type :: hdf5_output_dataset_t
        character(len=:), allocatable :: name
        integer :: parent_group = 1
        integer :: type_code = 0
        integer(int64), allocatable :: dimensions(:)
        integer(int32), allocatable :: values_i32(:)
        real(real64), allocatable :: values_r64(:)
        type(hdf5_output_attribute_t), allocatable :: attributes(:)
        integer(int64) :: object_address = 0_int64
        integer(int64) :: data_address = 0_int64
    end type hdf5_output_dataset_t

    type :: hdf5_output_group_t
        character(len=:), allocatable :: path
        character(len=:), allocatable :: name
        integer :: parent_group = 0
        integer(int64) :: object_address = 0_int64
    end type hdf5_output_group_t

    type, public :: hdf5_writer_t
        character(len=:), allocatable :: path
        type(hdf5_output_group_t), allocatable :: groups(:)
        type(hdf5_output_dataset_t), allocatable :: datasets(:)
        logical :: opened = .false.
    contains
        procedure :: create => hdf5_writer_create
        procedure :: define_group => hdf5_define_group
        procedure :: add_i32_scalar => hdf5_add_i32_scalar
        procedure :: add_i32_1 => hdf5_add_i32_1
        procedure :: add_i32_2 => hdf5_add_i32_2
        procedure :: add_i32_3 => hdf5_add_i32_3
        procedure :: add_r64_scalar => hdf5_add_r64_scalar
        procedure :: add_r64_1 => hdf5_add_r64_1
        procedure :: add_r64_2 => hdf5_add_r64_2
        procedure :: add_r64_3 => hdf5_add_r64_3
        procedure :: add_r64_4 => hdf5_add_r64_4
        procedure :: add_r64_5 => hdf5_add_r64_5
        procedure :: add_text_attribute => hdf5_add_text_attribute
        procedure :: add_i32_attribute => hdf5_add_i32_attribute
        procedure :: add_r64_attribute => hdf5_add_r64_attribute
        procedure :: close => hdf5_writer_close
    end type hdf5_writer_t

contains

    subroutine hdf5_writer_create(this, path, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status

        call status%clear()
        this%path = trim(path)
        allocate(this%datasets(0), this%groups(1))
        this%groups(1)%path = ""
        this%groups(1)%name = ""
        this%groups(1)%parent_group = 0
        this%groups(1)%object_address = ROOT_ADDRESS
        this%opened = .true.
    end subroutine hdf5_writer_create

    subroutine hdf5_add_i32_scalar(this, name, value, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(int32), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status

        call add_i32_flat(this, name, [integer(int64) ::], [value], status)
    end subroutine hdf5_add_i32_scalar

    subroutine hdf5_define_group(this, path, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status
        integer :: group_id

        call status%clear()
        if (.not. this%opened) then
            call status%set(FORTIO_ESTATE, "HDF5 writer is not open")
            return
        end if
        call ensure_group_path(this, path, group_id, status)
    end subroutine hdf5_define_group

    subroutine hdf5_add_i32_1(this, name, values, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(int32), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status

        call add_i32_flat(this, name, [int(size(values), int64)], values, status)
    end subroutine hdf5_add_i32_1

    subroutine hdf5_add_i32_2(this, name, values, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(int32), intent(in) :: values(:, :)
        type(fortio_status_t), intent(inout) :: status

        call add_i32_flat(this, name, int(shape(values), int64), &
                          reshape(values, [size(values)]), status)
    end subroutine hdf5_add_i32_2

    subroutine hdf5_add_i32_3(this, name, values, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(int32), intent(in) :: values(:, :, :)
        type(fortio_status_t), intent(inout) :: status

        call add_i32_flat(this, name, int(shape(values), int64), &
                          reshape(values, [size(values)]), status)
    end subroutine hdf5_add_i32_3

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

    subroutine hdf5_add_r64_4(this, name, values, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: values(:, :, :, :)
        type(fortio_status_t), intent(inout) :: status

        call add_r64_flat(this, name, int(shape(values), int64), &
                          reshape(values, [size(values)]), status)
    end subroutine hdf5_add_r64_4

    subroutine hdf5_add_r64_5(this, name, values, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: values(:, :, :, :, :)
        type(fortio_status_t), intent(inout) :: status

        call add_r64_flat(this, name, int(shape(values), int64), &
                          reshape(values, [size(values)]), status)
    end subroutine hdf5_add_r64_5

    subroutine add_i32_flat(this, name, dimensions, values, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: dimensions(:)
        integer(int32), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_output_dataset_t) :: dataset
        character(len=:), allocatable :: leaf_name
        integer :: parent_group

        if (.not. prepare_dataset(this, name, dimensions, size(values, kind=int64), &
                                  parent_group, leaf_name, status)) &
            return
        dataset%name = leaf_name
        dataset%parent_group = parent_group
        dataset%type_code = TYPE_I32
        dataset%dimensions = dimensions
        dataset%values_i32 = values
        allocate(dataset%attributes(0))
        call append_dataset(this%datasets, dataset)
    end subroutine add_i32_flat

    subroutine add_r64_flat(this, name, dimensions, values, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: dimensions(:)
        real(real64), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_output_dataset_t) :: dataset
        character(len=:), allocatable :: leaf_name
        integer :: parent_group

        if (.not. prepare_dataset(this, name, dimensions, size(values, kind=int64), &
                                  parent_group, leaf_name, status)) &
            return
        dataset%name = leaf_name
        dataset%parent_group = parent_group
        dataset%type_code = TYPE_R64
        dataset%dimensions = dimensions
        dataset%values_r64 = values
        allocate(dataset%attributes(0))
        call append_dataset(this%datasets, dataset)
    end subroutine add_r64_flat

    subroutine hdf5_add_text_attribute(this, dataset_name, name, value, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: dataset_name, name, value
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_output_attribute_t) :: attribute
        integer :: dataset_id

        call find_dataset(this, dataset_name, dataset_id, status)
        if (.not. status%ok()) return
        attribute%name = trim(name)
        attribute%type_code = TYPE_TEXT
        attribute%value_text = value
        call append_attribute(this%datasets(dataset_id)%attributes, attribute, status)
    end subroutine hdf5_add_text_attribute

    subroutine hdf5_add_i32_attribute(this, dataset_name, name, values, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: dataset_name, name
        integer(int32), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_output_attribute_t) :: attribute
        integer :: dataset_id

        call find_dataset(this, dataset_name, dataset_id, status)
        if (.not. status%ok()) return
        attribute%name = trim(name)
        attribute%type_code = TYPE_I32
        attribute%values_i32 = values
        call append_attribute(this%datasets(dataset_id)%attributes, attribute, status)
    end subroutine hdf5_add_i32_attribute

    subroutine hdf5_add_r64_attribute(this, dataset_name, name, value, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: dataset_name, name
        real(real64), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_output_attribute_t) :: attribute
        integer :: dataset_id

        call find_dataset(this, dataset_name, dataset_id, status)
        if (.not. status%ok()) return
        attribute%name = trim(name)
        attribute%type_code = TYPE_R64
        attribute%values_r64 = [value]
        call append_attribute(this%datasets(dataset_id)%attributes, attribute, status)
    end subroutine hdf5_add_r64_attribute

    subroutine find_dataset(this, name, dataset_id, status)
        class(hdf5_writer_t), intent(in) :: this
        character(len=*), intent(in) :: name
        integer, intent(out) :: dataset_id
        type(fortio_status_t), intent(inout) :: status
        character(len=:), allocatable :: normalized, parent_path, leaf_name
        integer :: i, parent_group, separator

        call status%clear()
        normalized = normalized_path(name)
        separator = scan(normalized, "/", back=.true.)
        if (separator == 0) then
            parent_path = ""
            leaf_name = normalized
        else
            parent_path = normalized(:separator - 1)
            leaf_name = normalized(separator + 1:)
        end if
        parent_group = group_by_path(this%groups, parent_path)
        dataset_id = 0
        do i = 1, size(this%datasets)
            if (this%datasets(i)%parent_group == parent_group) then
                if (this%datasets(i)%name == leaf_name) then
                    dataset_id = i
                    return
                end if
            end if
        end do
        call status%set(FORTIO_ESTATE, "HDF5 attribute dataset does not exist")
    end subroutine find_dataset

    subroutine append_attribute(attributes, attribute, status)
        type(hdf5_output_attribute_t), allocatable, intent(inout) :: attributes(:)
        type(hdf5_output_attribute_t), intent(in) :: attribute
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_output_attribute_t), allocatable :: temporary(:)
        integer :: count, i

        if (len(attribute%name) == 0 .or. len(attribute%name) > 255) then
            call status%set(FORTIO_ENOTSUP, "HDF5 attribute name is not supported")
            return
        end if
        do i = 1, size(attributes)
            if (attributes(i)%name == attribute%name) then
                call status%set(FORTIO_ESTATE, "duplicate HDF5 attribute name")
                return
            end if
        end do
        count = size(attributes)
        allocate(temporary(count + 1))
        if (count > 0) temporary(:count) = attributes
        temporary(count + 1) = attribute
        call move_alloc(temporary, attributes)
    end subroutine append_attribute

    logical function prepare_dataset(this, name, dimensions, count, parent_group, &
                                     leaf_name, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: dimensions(:), count
        integer, intent(out) :: parent_group
        character(len=:), allocatable, intent(out) :: leaf_name
        type(fortio_status_t), intent(inout) :: status
        character(len=:), allocatable :: normalized, parent_path
        integer :: i, separator

        call status%clear()
        prepare_dataset = this%opened
        if (.not. prepare_dataset) then
            call status%set(FORTIO_ESTATE, "HDF5 writer is not open")
            return
        end if
        normalized = normalized_path(name)
        separator = scan(normalized, "/", back=.true.)
        if (separator == 0) then
            parent_path = ""
            leaf_name = normalized
        else
            parent_path = normalized(:separator - 1)
            leaf_name = normalized(separator + 1:)
        end if
        if (len(leaf_name) == 0 .or. len(leaf_name) > 255) then
            call status%set(FORTIO_ENOTSUP, "HDF5 dataset name is not supported")
            prepare_dataset = .false.
            return
        end if
        call ensure_group_path(this, parent_path, parent_group, status)
        if (.not. status%ok()) then
            prepare_dataset = .false.
            return
        end if
        if (product(dimensions) /= count) then
            call status%set(FORTIO_ESHAPE, "HDF5 dataset shape does not match values")
            prepare_dataset = .false.
            return
        end if
        do i = 1, size(this%datasets)
            if (this%datasets(i)%parent_group == parent_group .and. &
                this%datasets(i)%name == leaf_name) then
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

    subroutine ensure_group_path(this, path, group_id, status)
        class(hdf5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer, intent(out) :: group_id
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_output_group_t) :: group
        character(len=:), allocatable :: normalized, component, current_path, remaining
        integer :: separator, existing

        call status%clear()
        normalized = normalized_path(path)
        if (len(normalized) == 0) then
            group_id = 1
            return
        end if
        remaining = normalized
        current_path = ""
        group_id = 1
        do
            separator = index(remaining, "/")
            if (separator == 0) then
                component = remaining
                remaining = ""
            else
                component = remaining(:separator - 1)
                remaining = remaining(separator + 1:)
            end if
            if (len(component) == 0 .or. len(component) > 255) then
                call status%set(FORTIO_ENOTSUP, "HDF5 group name is not supported")
                return
            end if
            if (len(current_path) == 0) then
                current_path = component
            else
                current_path = current_path//"/"//component
            end if
            existing = group_by_path(this%groups, current_path)
            if (existing == 0) then
                group%path = current_path
                group%name = component
                group%parent_group = group_id
                call append_group(this%groups, group)
                group_id = size(this%groups)
            else
                group_id = existing
            end if
            if (len(remaining) == 0) exit
        end do
    end subroutine ensure_group_path

    subroutine append_group(groups, group)
        type(hdf5_output_group_t), allocatable, intent(inout) :: groups(:)
        type(hdf5_output_group_t), intent(in) :: group
        type(hdf5_output_group_t), allocatable :: temporary(:)
        integer :: count

        count = size(groups)
        allocate(temporary(count + 1))
        temporary(:count) = groups
        temporary(count + 1) = group
        call move_alloc(temporary, groups)
    end subroutine append_group

    integer function group_by_path(groups, path) result(group_id)
        type(hdf5_output_group_t), intent(in) :: groups(:)
        character(len=*), intent(in) :: path
        integer :: i

        group_id = 0
        do i = 1, size(groups)
            if (groups(i)%path == path) then
                group_id = i
                return
            end if
        end do
    end function group_by_path

    pure function normalized_path(path) result(normalized)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: normalized
        integer :: first, last

        normalized = trim(adjustl(path))
        first = 1
        last = len(normalized)
        do while (first <= last)
            if (normalized(first:first) /= "/") exit
            first = first + 1
        end do
        do while (last >= first)
            if (normalized(last:last) /= "/") exit
            last = last - 1
        end do
        if (first > last) then
            normalized = ""
        else
            normalized = normalized(first:last)
        end if
    end function normalized_path

    subroutine hdf5_writer_close(this, status)
        class(hdf5_writer_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status
        type(byte_writer_t) :: writer
        integer(int8), allocatable :: metadata(:)
        integer(int64) :: next_address, eof_address
        integer :: i, j

        call status%clear()
        if (.not. this%opened) return
        next_address = ROOT_ADDRESS + group_header_size(1, this%groups, this%datasets)
        do i = 2, size(this%groups)
            this%groups(i)%object_address = next_address
            next_address = next_address + group_header_size(i, this%groups, this%datasets)
        end do
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
        metadata = make_group_header(1, this%groups, this%datasets)
        call writer%write_bytes(metadata, status)
        if (.not. status%ok()) return
        do i = 2, size(this%groups)
            metadata = make_group_header(i, this%groups, this%datasets)
            call writer%write_bytes(metadata, status)
            if (.not. status%ok()) return
        end do
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

    integer(int64) function group_header_size(group_id, groups, datasets) result(total)
        integer, intent(in) :: group_id
        type(hdf5_output_group_t), intent(in) :: groups(:)
        type(hdf5_output_dataset_t), intent(in) :: datasets(:)
        integer(int64) :: chunk_size
        integer :: i, width

        chunk_size = 28_int64
        do i = 2, size(groups)
            if (groups(i)%parent_group == group_id) &
                chunk_size = chunk_size + 15 + len(groups(i)%name)
        end do
        do i = 1, size(datasets)
            if (datasets(i)%parent_group == group_id) &
                chunk_size = chunk_size + 15 + len(datasets(i)%name)
        end do
        width = merge(1, 2, chunk_size <= 255)
        total = 6 + width + chunk_size + 4
    end function group_header_size

    integer(int64) function dataset_header_size(dataset) result(total)
        type(hdf5_output_dataset_t), intent(in) :: dataset
        integer(int64) :: chunk_size
        integer :: width

        chunk_size = dataset_chunk_size(dataset)
        width = merge(1, 2, chunk_size <= 255)
        total = 6 + width + chunk_size + 4
    end function dataset_header_size

    integer(int64) function dataset_chunk_size(dataset) result(total)
        type(hdf5_output_dataset_t), intent(in) :: dataset
        integer :: datatype_size, i

        datatype_size = merge(12, 20, dataset%type_code == TYPE_I32)
        total = 4 + (4 + 8*size(dataset%dimensions)) + 4 + datatype_size + 4 + 18
        do i = 1, size(dataset%attributes)
            total = total + 4 + attribute_payload_size(dataset%attributes(i))
        end do
    end function dataset_chunk_size

    integer(int64) function attribute_payload_size(attribute) result(total)
        type(hdf5_output_attribute_t), intent(in) :: attribute
        integer :: datatype_size, dataspace_size, data_size

        select case (attribute%type_code)
        case (TYPE_TEXT)
            datatype_size = 8
            dataspace_size = 4
            data_size = max(1, len_trim(attribute%value_text))
        case (TYPE_I32)
            datatype_size = 12
            dataspace_size = 12
            data_size = 4*size(attribute%values_i32)
        case (TYPE_R64)
            datatype_size = 20
            dataspace_size = 4
            data_size = 8
        case default
            datatype_size = 0
            dataspace_size = 0
            data_size = 0
        end select
        total = 9 + len(attribute%name) + 1 + datatype_size + dataspace_size + data_size
    end function attribute_payload_size

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

    function make_group_header(group_id, groups, datasets) result(bytes)
        integer, intent(in) :: group_id
        type(hdf5_output_group_t), intent(in) :: groups(:)
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
        do i = 2, size(groups)
            if (groups(i)%parent_group /= group_id) cycle
            deallocate(payload)
            allocate(payload(0))
            call append_u8(payload, 1)
            call append_u8(payload, 0)
            call append_u8(payload, len(groups(i)%name))
            call append_text(payload, groups(i)%name)
            call append_le64(payload, groups(i)%object_address)
            call append_message(chunk, 6, 0, payload)
        end do
        do i = 1, size(datasets)
            if (datasets(i)%parent_group /= group_id) cycle
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
    end function make_group_header

    function make_dataset_header(dataset) result(bytes)
        type(hdf5_output_dataset_t), intent(in) :: dataset
        integer(int8), allocatable :: bytes(:), chunk(:), payload(:)
        integer(int64) :: chunk_size
        integer :: i, width, flags

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
        do i = 1, size(dataset%attributes)
            payload = make_attribute_payload(dataset%attributes(i))
            call append_message(chunk, 12, 0, payload)
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
    end function make_dataset_header

    function make_attribute_payload(attribute) result(payload)
        type(hdf5_output_attribute_t), intent(in) :: attribute
        integer(int8), allocatable :: payload(:)
        integer :: i, text_size

        allocate(payload(0))
        call append_u8(payload, 3)
        call append_u8(payload, 0)
        call append_le16(payload, len(attribute%name) + 1)
        select case (attribute%type_code)
        case (TYPE_TEXT)
            call append_le16(payload, 8)
            call append_le16(payload, 4)
        case (TYPE_I32)
            call append_le16(payload, 12)
            call append_le16(payload, 12)
        case (TYPE_R64)
            call append_le16(payload, 20)
            call append_le16(payload, 4)
        end select
        call append_u8(payload, 0)
        call append_text(payload, attribute%name)
        call append_u8(payload, 0)
        select case (attribute%type_code)
        case (TYPE_TEXT)
            text_size = max(1, len_trim(attribute%value_text))
            call append_values(payload, [int(z'13', int8), 1_int8, 0_int8, 0_int8])
            call append_le32(payload, int(text_size, int32))
        case (TYPE_I32)
            call append_values(payload, [int(z'10', int8), int(z'08', int8), 0_int8, &
                0_int8, 4_int8, 0_int8, 0_int8, 0_int8, 0_int8, 0_int8, &
                int(z'20', int8), 0_int8])
        case (TYPE_R64)
            call append_values(payload, [int(z'11', int8), int(z'20', int8), &
                int(z'3f', int8), 0_int8, 8_int8, 0_int8, 0_int8, 0_int8, &
                0_int8, 0_int8, int(z'40', int8), 0_int8, int(z'34', int8), &
                int(z'0b', int8), 0_int8, int(z'34', int8), -1_int8, 3_int8, &
                0_int8, 0_int8])
        end select
        if (attribute%type_code == TYPE_I32) then
            call append_values(payload, [2_int8, 1_int8, 0_int8, 1_int8])
            call append_le64(payload, int(size(attribute%values_i32), int64))
        else
            call append_values(payload, [2_int8, 0_int8, 0_int8, 0_int8])
        end if
        select case (attribute%type_code)
        case (TYPE_TEXT)
            if (len_trim(attribute%value_text) == 0) then
                call append_u8(payload, 0)
            else
                call append_text(payload, attribute%value_text(:len_trim(attribute%value_text)))
            end if
        case (TYPE_I32)
            do i = 1, size(attribute%values_i32)
                call append_le32(payload, attribute%values_i32(i))
            end do
        case (TYPE_R64)
            call append_le_r64(payload, attribute%values_r64(1))
        end select
    end function make_attribute_payload

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

    subroutine append_le_r64(bytes, value)
        integer(int8), allocatable, intent(inout) :: bytes(:)
        real(real64), intent(in) :: value

        call append_le64(bytes, transfer(value, 0_int64))
    end subroutine append_le_r64

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
