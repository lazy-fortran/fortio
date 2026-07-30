module fortio_hdf5_reader
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    use fortio_bytes, only: byte_reader_t
    use fortio_status, only: fortio_status_t, FORTIO_EFORMAT, FORTIO_ENOTFOUND, &
                            FORTIO_ENOTSUP, FORTIO_ESHAPE, FORTIO_ETYPE
    implicit none
    private

    integer, parameter :: H5_MSG_DATASPACE = 1
    integer, parameter :: H5_MSG_LINK_INFO = 2
    integer, parameter :: H5_MSG_LINK = 6
    integer, parameter :: H5_MSG_LAYOUT = 8
    integer, parameter :: H5_MSG_ATTRIBUTE = 12
    integer, parameter :: H5_MSG_CONTINUATION = 16
    integer, parameter :: H5_TYPE_INTEGER = 0
    integer, parameter :: H5_TYPE_FLOAT = 1
    integer, parameter :: H5_TYPE_STRING = 3
    integer, parameter :: H5_TYPE_COMPOUND = 6
    integer, parameter :: H5_LAYOUT_CONTIGUOUS = 1

    type :: hdf5_link_t
        character(len=:), allocatable :: name
        integer(int64) :: address = -1_int64
    end type hdf5_link_t

    type :: hdf5_attribute_t
        character(len=:), allocatable :: name
        integer(int32), allocatable :: values_i32(:)
    end type hdf5_attribute_t

    type :: hdf5_dataset_t
        integer(int64), allocatable :: dimensions(:)
        integer :: type_class = -1
        integer :: element_size = 0
        logical :: little_endian = .true.
        integer(int64) :: data_address = -1_int64
        integer(int64) :: data_size = 0_int64
        type(hdf5_attribute_t), allocatable :: attributes(:)
    end type hdf5_dataset_t

    type, public :: hdf5_file_t
        type(byte_reader_t) :: reader
        integer :: offset_size = 0
        integer :: length_size = 0
        integer(int64) :: base_address = 0_int64
        integer(int64) :: root_address = -1_int64
        logical :: opened = .false.
    contains
        procedure :: open => hdf5_open
        procedure :: close => hdf5_close
        procedure :: read_i32_scalar => hdf5_read_i32_scalar
        procedure :: read_i32_1 => hdf5_read_i32_1
        procedure :: read_i32_2 => hdf5_read_i32_2
        procedure :: read_i32_3 => hdf5_read_i32_3
        procedure :: read_r64_scalar => hdf5_read_r64_scalar
        procedure :: read_r64_1 => hdf5_read_r64_1
        procedure :: read_r64_2 => hdf5_read_r64_2
        procedure :: read_r64_3 => hdf5_read_r64_3
        procedure :: read_r64_4 => hdf5_read_r64_4
        procedure :: read_r64_5 => hdf5_read_r64_5
        procedure :: read_c64_1 => hdf5_read_c64_1
        procedure :: read_c64_2 => hdf5_read_c64_2
        procedure :: read_c64_3 => hdf5_read_c64_3
        procedure :: read_i32_attribute => hdf5_read_i32_attribute
        procedure :: read_text_scalar => hdf5_read_text_scalar
        procedure :: exists => hdf5_exists
    end type hdf5_file_t

contains

    subroutine hdf5_open(this, path, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: signature(8), version_byte, size_byte
        integer(int32) :: ignored_flags
        integer(int64) :: ignored_address

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
        if (byte_value(version_byte) /= 2 .and. byte_value(version_byte) /= 3) then
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
        this%opened = .true.
    end subroutine hdf5_open

    subroutine hdf5_close(this, status)
        class(hdf5_file_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status

        call this%reader%close(status)
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

    subroutine read_i32_flat(this, path, values, status)
        class(hdf5_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int32), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
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
        call this%reader%seek(this%base_address + dataset%data_address + 1)
        do i = 1, size(values)
            if (dataset%little_endian) then
                call this%reader%read_le_i32(values(i), status)
            else
                call this%reader%read_be_i32(values(i), status)
            end if
            if (.not. status%ok()) return
        end do
    end subroutine read_i32_flat

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
                if (.not. allocated(dataset%attributes(i)%values_i32)) then
                    call status%set(FORTIO_ETYPE, "HDF5 attribute is not a 32-bit integer")
                    return
                end if
                values = dataset%attributes(i)%values_i32
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
        real(real64), allocatable, intent(out) :: values(:, :)
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_dataset_t) :: dataset
        real(real64), allocatable :: flat(:)

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (size(dataset%dimensions) /= 2) then
            call status%set(FORTIO_ESHAPE, "dataset rank does not match rank 2")
            return
        end if
        call read_r64_flat(this, path, flat, status)
        if (.not. status%ok()) return
        ! HDF5 stores C dimension order; expose native Fortran order.
        allocate(values(dataset%dimensions(2), dataset%dimensions(1)))
        values = reshape(flat, shape(values))
    end subroutine hdf5_read_r64_2

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
        real(real32) :: value_r32
        integer(int64) :: count
        integer :: i

        call find_dataset(this, path, dataset, status)
        if (.not. status%ok()) return
        if (dataset%type_class /= H5_TYPE_FLOAT) then
            call status%set(FORTIO_ETYPE, "dataset is not floating point")
            return
        end if
        count = product(dataset%dimensions)
        allocate(values(count))
        call this%reader%seek(this%base_address + dataset%data_address + 1)
        select case (dataset%element_size)
        case (8)
            do i = 1, size(values)
                if (dataset%little_endian) then
                    call this%reader%read_le_r64(values(i), status)
                else
                    call this%reader%read_be_r64(values(i), status)
                end if
                if (.not. status%ok()) return
            end do
        case (4)
            do i = 1, size(values)
                if (dataset%little_endian) then
                    call this%reader%read_le_r32(value_r32, status)
                else
                    call this%reader%read_be_r32(value_r32, status)
                end if
                if (.not. status%ok()) return
                values(i) = real(value_r32, real64)
            end do
        case default
            call status%set(FORTIO_ETYPE, "floating-point width is not supported")
        end select
    end subroutine read_r64_flat

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
        integer(int64) :: address
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
            call find_child(this, address, component, address, status)
            if (.not. status%ok()) return
            if (len(remaining) == 0) exit
        end do
        call parse_dataset_header(this, address, dataset, status)
    end subroutine find_dataset

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
            case (H5_MSG_ATTRIBUTE)
                if (.not. want_links) call parse_attribute_message(this, dataset, status)
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

    subroutine parse_continuation(this, address, length, links, dataset, want_links, &
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
        if (bytes_text(signature) /= "BTHD" .or. tree_type /= 5 .or. &
            record_size /= 11 .or. depth /= 0) then
            call status%set(FORTIO_ENOTSUP, &
                            "dense HDF5 group B-tree form is not supported")
            return
        end if
        call this%reader%seek(this%base_address + root_address + 1)
        call this%reader%read_bytes(signature, status)
        call this%reader%read_i8(byte, status)
        call this%reader%read_i8(byte, status)
        if (.not. status%ok()) return
        if (bytes_text(signature) /= "BTLF" .or. byte_value(byte) /= 5) then
            call status%set(FORTIO_EFORMAT, "invalid dense HDF5 group B-tree leaf")
            return
        end if
        do i = 1, record_count
            call skip_bytes(this%reader, 4_int64)
            call parse_dense_heap_id(this, heap_address, links, status)
            if (.not. status%ok()) return
        end do
    end subroutine parse_dense_links

    subroutine parse_dense_heap_id(this, heap_address, links, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: heap_address
        type(hdf5_link_t), allocatable, intent(inout) :: links(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: byte
        integer(int16) :: heap_id_length_i16, max_heap_bits_i16, current_rows_i16
        integer(int64) :: direct_address, object_offset, object_length, saved_position
        integer :: heap_id_length, offset_width, length_width

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
        call skip_bytes(this%reader, 2_int64)
        call this%reader%read_le_i64(direct_address, status)
        call this%reader%read_le_i16(current_rows_i16, status)
        if (.not. status%ok()) return
        if (iand(int(current_rows_i16), int(z'ffff')) /= 0) then
            call status%set(FORTIO_ENOTSUP, &
                            "indirect dense HDF5 group heap is not supported")
            return
        end if
        heap_id_length = iand(int(heap_id_length_i16), int(z'ffff'))
        offset_width = (iand(int(max_heap_bits_i16), int(z'ffff')) + 7)/8
        length_width = heap_id_length - 1 - offset_width
        call this%reader%seek(saved_position + 1)
        call read_unsigned(this%reader, offset_width, object_offset, status)
        call read_unsigned(this%reader, length_width, object_length, status)
        saved_position = this%reader%position
        if (.not. status%ok()) return
        call parse_dense_link_object(this, direct_address, object_offset, &
                                     object_length, links, status)
        call this%reader%seek(saved_position)
    end subroutine parse_dense_heap_id

    subroutine parse_dense_link_object(this, direct_address, object_offset, &
                                       object_length, links, status)
        class(hdf5_file_t), intent(inout) :: this
        integer(int64), intent(in) :: direct_address, object_offset, object_length
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
        object_start = this%base_address + direct_address + object_offset + 1
        call this%reader%seek(object_start)
        call parse_link_message(this, links, status)
        if (.not. status%ok()) return
        if (this%reader%position > object_start + object_length) &
            call status%set(FORTIO_EFORMAT, "dense HDF5 link exceeds heap object")
    end subroutine parse_dense_link_object

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
        integer(int8) :: version_byte, class_byte

        call this%reader%read_i8(version_byte, status)
        call this%reader%read_i8(class_byte, status)
        if (.not. status%ok()) return
        if ((byte_value(version_byte) /= 3 .and. byte_value(version_byte) /= 4) .or. &
            byte_value(class_byte) /= H5_LAYOUT_CONTIGUOUS) then
            call status%set(FORTIO_ENOTSUP, &
                            "only contiguous HDF5 layout versions 3 and 4 are supported")
            return
        end if
        call this%reader%read_le_i64(dataset%data_address, status)
        call this%reader%read_le_i64(dataset%data_size, status)
    end subroutine parse_layout_message

    subroutine parse_attribute_message(this, dataset, status)
        class(hdf5_file_t), intent(inout) :: this
        type(hdf5_dataset_t), intent(inout) :: dataset
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: byte
        integer(int16) :: name_size_i16, datatype_size_i16, dataspace_size_i16
        integer(int32) :: element_size
        integer(int64) :: datatype_start, dataspace_start, value_count
        integer :: version, name_size, datatype_size, dataspace_size, rank, i
        character(len=:), allocatable :: name
        integer(int32), allocatable :: values(:)

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
        if (iand(byte_value(byte), 15) /= H5_TYPE_INTEGER) return
        call skip_bytes(this%reader, 3_int64)
        call this%reader%read_le_i32(element_size, status)
        if (.not. status%ok()) return
        if (element_size /= 4) return
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
        allocate(values(value_count))
        do i = 1, size(values)
            call this%reader%read_le_i32(values(i), status)
            if (.not. status%ok()) return
        end do
        call append_i32_attribute(dataset, name, values)
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
