module fortio_netcdf_classic
    use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real32, real64
    use fortio_bytes, only: byte_reader_t
    use fortio_status, only: fortio_status_t, FORTIO_EFORMAT, FORTIO_ENOTFOUND, &
        FORTIO_ENOTSUP, FORTIO_ESHAPE, FORTIO_ETYPE
    implicit none
    private

    integer(int32), parameter :: NC_DIMENSION = 10
    integer(int32), parameter :: NC_VARIABLE = 11
    integer(int32), parameter :: NC_ATTRIBUTE = 12

    integer, parameter, public :: NC_BYTE = 1
    integer, parameter, public :: NC_CHAR = 2
    integer, parameter, public :: NC_SHORT = 3
    integer, parameter, public :: NC_INT = 4
    integer, parameter, public :: NC_FLOAT = 5
    integer, parameter, public :: NC_DOUBLE = 6

    type, public :: classic_dimension_t
        character(len=:), allocatable :: name
        integer(int64) :: length = 0_int64
        logical :: unlimited = .false.
    end type classic_dimension_t

    type, public :: classic_attribute_t
        character(len=:), allocatable :: name
        integer :: type_code = 0
        integer :: element_count = 0
        integer(int8), allocatable :: bytes(:)
    end type classic_attribute_t

    type, public :: classic_variable_t
        character(len=:), allocatable :: name
        integer, allocatable :: dimension_ids(:)
        integer :: type_code = 0
        integer(int64) :: value_size = 0_int64
        integer(int64) :: begin_offset = 0_int64
        integer(int64) :: element_count = 0_int64
        logical :: record_variable = .false.
        integer :: attribute_count = 0
        type(classic_attribute_t), allocatable :: attributes(:)
    end type classic_variable_t

    type, public :: classic_file_t
        type(byte_reader_t) :: reader
        integer :: version = 0
        integer(int64) :: record_count = 0_int64
        integer(int64) :: record_size = 0_int64
        type(classic_dimension_t), allocatable :: dimensions(:)
        type(classic_attribute_t), allocatable :: global_attributes(:)
        type(classic_variable_t), allocatable :: variables(:)
        logical :: opened = .false.
    contains
        procedure :: open => classic_open
        procedure :: close => classic_close
        procedure :: variable_id => classic_variable_id
        procedure :: dimension_id => classic_dimension_id
        procedure :: variable_shape => classic_variable_shape
        procedure :: read_i32_scalar => classic_read_i32_scalar
        procedure :: read_i32_1 => classic_read_i32_1
        procedure :: read_char_scalar => classic_read_char_scalar
        procedure :: read_char_1 => classic_read_char_1
        procedure :: read_r64_scalar => classic_read_r64_scalar
        procedure :: read_r64_1 => classic_read_r64_1
        procedure :: read_r64_2 => classic_read_r64_2
        procedure :: read_r64_2_into => classic_read_r64_2_into
        procedure :: read_r64_3 => classic_read_r64_3
        procedure :: read_r64_4 => classic_read_r64_4
    end type classic_file_t

contains

    subroutine classic_open(this, path, status)
        class(classic_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: magic(4)
        integer(int32) :: record_count_32

        call this%reader%open(path, status)
        if (.not. status%ok()) return

        call this%reader%read_bytes(magic, status)
        if (.not. status%ok()) return
        if (achar(byte_unsigned(magic(1))) /= "C") then
            call status%set(FORTIO_EFORMAT, "not a classic NetCDF file")
            call close_after_error(this)
            return
        end if
        if (achar(byte_unsigned(magic(2))) /= "D") then
            call status%set(FORTIO_EFORMAT, "not a classic NetCDF file")
            call close_after_error(this)
            return
        end if
        if (achar(byte_unsigned(magic(3))) /= "F") then
            call status%set(FORTIO_EFORMAT, "not a classic NetCDF file")
            call close_after_error(this)
            return
        end if

        this%version = byte_unsigned(magic(4))
        if (this%version /= 1 .and. this%version /= 2) then
            call status%set(FORTIO_ENOTSUP, "only CDF-1 and CDF-2 are supported")
            call close_after_error(this)
            return
        end if

        call this%reader%read_be_i32(record_count_32, status)
        if (.not. status%ok()) return
        this%record_count = unsigned_i32(record_count_32)
        call read_dimensions(this, status)
        if (.not. status%ok()) return
        call read_attributes(this%reader, this%global_attributes, status)
        if (.not. status%ok()) return
        call read_variables(this, status)
        if (.not. status%ok()) return
        call finalize_variable_layout(this, status)
        if (.not. status%ok()) return
        this%opened = .true.
    end subroutine classic_open

    subroutine classic_close(this, status)
        class(classic_file_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status

        call this%reader%close(status)
        this%opened = .false.
        if (allocated(this%dimensions)) deallocate(this%dimensions)
        if (allocated(this%global_attributes)) deallocate(this%global_attributes)
        if (allocated(this%variables)) deallocate(this%variables)
    end subroutine classic_close

    integer function classic_variable_id(this, name)
        class(classic_file_t), intent(in) :: this
        character(len=*), intent(in) :: name
        integer :: i

        classic_variable_id = 0
        do i = 1, size(this%variables)
            if (this%variables(i)%name == trim(name)) then
                classic_variable_id = i
                return
            end if
        end do
    end function classic_variable_id

    integer function classic_dimension_id(this, name)
        class(classic_file_t), intent(in) :: this
        character(len=*), intent(in) :: name
        integer :: i

        classic_dimension_id = 0
        do i = 1, size(this%dimensions)
            if (this%dimensions(i)%name == trim(name)) then
                classic_dimension_id = i
                return
            end if
        end do
    end function classic_dimension_id

    function classic_variable_shape(this, variable_id) result(shape)
        class(classic_file_t), intent(in) :: this
        integer, intent(in) :: variable_id
        integer(int64), allocatable :: shape(:)
        integer :: i, dimension_id, rank

        rank = size(this%variables(variable_id)%dimension_ids)
        allocate(shape(rank))
        do i = 1, size(shape)
            ! NetCDF stores dimensions in C order. The Fortran API presents
            ! the leftmost, contiguous Fortran dimension first.
            dimension_id = this%variables(variable_id)%dimension_ids(rank - i + 1) + 1
            shape(i) = this%dimensions(dimension_id)%length
            if (this%dimensions(dimension_id)%unlimited) shape(i) = this%record_count
        end do
    end function classic_variable_shape

    subroutine classic_read_i32_scalar(this, name, value, status)
        class(classic_file_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(int32), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int32), allocatable :: values(:)

        call this%read_i32_1(name, values, status)
        if (.not. status%ok()) return
        if (size(values) /= 1) then
            call status%set(FORTIO_ESHAPE, "variable is not scalar")
            return
        end if
        value = values(1)
    end subroutine classic_read_i32_scalar

    subroutine classic_read_i32_1(this, name, values, status)
        class(classic_file_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(int32), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer :: variable_id, i

        call status%clear()
        variable_id = this%variable_id(name)
        if (variable_id == 0) then
            call status%set(FORTIO_ENOTFOUND, "variable not found: "//trim(name))
            return
        end if
        if (this%variables(variable_id)%type_code /= NC_INT) then
            call status%set(FORTIO_ETYPE, "variable is not a 32-bit integer")
            return
        end if
        allocate(values(this%variables(variable_id)%element_count))
        call seek_variable(this, variable_id, status)
        if (.not. status%ok()) return
        do i = 1, size(values)
            call this%reader%read_be_i32(values(i), status)
            if (.not. status%ok()) return
        end do
    end subroutine classic_read_i32_1

    subroutine classic_read_char_scalar(this, name, value, status)
        class(classic_file_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        character(len=1) :: temporary(1)

        if (len(value) /= 1) then
            call status%set(FORTIO_ESHAPE, "character scalar must have length one")
            return
        end if
        call this%read_char_1(name, temporary, status)
        if (status%ok()) value = temporary(1)
    end subroutine classic_read_char_scalar

    subroutine classic_read_char_1(this, name, values, status)
        class(classic_file_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: byte
        integer :: variable_id, i, j

        call status%clear()
        variable_id = this%variable_id(name)
        if (variable_id == 0) then
            call status%set(FORTIO_ENOTFOUND, "variable not found: "//trim(name))
            return
        end if
        if (this%variables(variable_id)%type_code /= NC_CHAR) then
            call status%set(FORTIO_ETYPE, "variable is not character data")
            return
        end if
        if (len(values)*size(values, kind=int64) /= &
            this%variables(variable_id)%element_count) then
            call status%set(FORTIO_ESHAPE, "character destination shape mismatch")
            return
        end if
        call seek_variable(this, variable_id, status)
        if (.not. status%ok()) return
        do i = 1, size(values)
            do j = 1, len(values)
                call this%reader%read_i8(byte, status)
                if (.not. status%ok()) return
                values(i)(j:j) = achar(byte_unsigned(byte))
            end do
        end do
    end subroutine classic_read_char_1

    subroutine classic_read_r64_scalar(this, name, value, status)
        class(classic_file_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(real64), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        real(real64), allocatable :: values(:)

        call read_r64_flat(this, name, values, status)
        if (.not. status%ok()) return
        if (size(values) /= 1) then
            call status%set(FORTIO_ESHAPE, "variable is not scalar")
            return
        end if
        value = values(1)
    end subroutine classic_read_r64_scalar

    subroutine classic_read_r64_1(this, name, values, status)
        class(classic_file_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(real64), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int64), allocatable :: shape(:)
        integer :: variable_id

        variable_id = this%variable_id(name)
        if (variable_id == 0) then
            call status%set(FORTIO_ENOTFOUND, "variable not found: "//trim(name))
            return
        end if
        shape = this%variable_shape(variable_id)
        if (size(shape) /= 1) then
            call status%set(FORTIO_ESHAPE, "variable rank does not match rank 1")
            return
        end if
        call read_r64_flat(this, name, values, status)
    end subroutine classic_read_r64_1

    subroutine classic_read_r64_2(this, name, values, status)
        class(classic_file_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(real64), allocatable, target, intent(out) :: values(:, :)
        type(fortio_status_t), intent(inout) :: status
        real(real64), pointer :: flat(:)
        integer(int64), allocatable :: shape(:)
        integer :: variable_id

        variable_id = this%variable_id(name)
        if (variable_id == 0) then
            call status%set(FORTIO_ENOTFOUND, "variable not found: "//trim(name))
            return
        end if
        shape = this%variable_shape(variable_id)
        if (size(shape) /= 2) then
            call status%set(FORTIO_ESHAPE, "variable rank does not match rank 2")
            return
        end if
        allocate(values(shape(1), shape(2)))
        flat(1:size(values)) => values
        call read_r64_values(this, variable_id, flat, status)
    end subroutine classic_read_r64_2

    subroutine classic_read_r64_2_into(this, name, values, status)
        class(classic_file_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(real64), contiguous, target, intent(out) :: values(:, :)
        type(fortio_status_t), intent(inout) :: status
        real(real64), pointer :: flat(:)
        integer(int64), allocatable :: variable_shape(:)
        integer :: variable_id

        variable_id = this%variable_id(name)
        if (variable_id == 0) then
            call status%set(FORTIO_ENOTFOUND, "variable not found: "//trim(name))
            return
        end if
        variable_shape = this%variable_shape(variable_id)
        if (size(variable_shape) /= 2) then
            call status%set(FORTIO_ESHAPE, "variable rank does not match rank 2")
            return
        end if
        if (any(variable_shape /= int(shape(values), int64))) then
            call status%set(FORTIO_ESHAPE, "variable shape does not match destination")
            return
        end if
        flat(1:size(values)) => values
        call read_r64_values(this, variable_id, flat, status)
    end subroutine classic_read_r64_2_into

    subroutine classic_read_r64_3(this, name, values, status)
        class(classic_file_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(real64), allocatable, intent(out) :: values(:, :, :)
        type(fortio_status_t), intent(inout) :: status
        real(real64), allocatable :: flat(:)
        integer(int64), allocatable :: shape(:)
        integer :: variable_id

        variable_id = this%variable_id(name)
        if (variable_id == 0) then
            call status%set(FORTIO_ENOTFOUND, "variable not found: "//trim(name))
            return
        end if
        shape = this%variable_shape(variable_id)
        if (size(shape) /= 3) then
            call status%set(FORTIO_ESHAPE, "variable rank does not match rank 3")
            return
        end if
        call read_r64_flat(this, name, flat, status)
        if (.not. status%ok()) return
        allocate(values(shape(1), shape(2), shape(3)))
        values = reshape(flat, [int(shape(1)), int(shape(2)), int(shape(3))])
    end subroutine classic_read_r64_3

    subroutine classic_read_r64_4(this, name, values, status)
        class(classic_file_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(real64), allocatable, intent(out) :: values(:, :, :, :)
        type(fortio_status_t), intent(inout) :: status
        real(real64), allocatable :: flat(:)
        integer(int64), allocatable :: shape(:)
        integer :: variable_id

        variable_id = this%variable_id(name)
        if (variable_id == 0) then
            call status%set(FORTIO_ENOTFOUND, "variable not found: "//trim(name))
            return
        end if
        shape = this%variable_shape(variable_id)
        if (size(shape) /= 4) then
            call status%set(FORTIO_ESHAPE, "variable rank does not match rank 4")
            return
        end if
        call read_r64_flat(this, name, flat, status)
        if (.not. status%ok()) return
        allocate(values(shape(1), shape(2), shape(3), shape(4)))
        values = reshape(flat, [int(shape(1)), int(shape(2)), int(shape(3)), int(shape(4))])
    end subroutine classic_read_r64_4

    subroutine read_r64_flat(this, name, values, status)
        class(classic_file_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        real(real64), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer :: variable_id

        call status%clear()
        variable_id = this%variable_id(name)
        if (variable_id == 0) then
            call status%set(FORTIO_ENOTFOUND, "variable not found: "//trim(name))
            return
        end if
        allocate(values(this%variables(variable_id)%element_count))
        call read_r64_values(this, variable_id, values, status)
    end subroutine read_r64_flat

    subroutine read_r64_values(this, variable_id, values, status)
        class(classic_file_t), intent(inout) :: this
        integer, intent(in) :: variable_id
        real(real64), intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer :: i
        real(real32) :: value_r32
        integer(int32) :: value_i32

        call status%clear()
        call seek_variable(this, variable_id, status)
        if (.not. status%ok()) return
        select case (this%variables(variable_id)%type_code)
        case (NC_DOUBLE)
            call this%reader%read_be_r64_array(values, status)
        case (NC_FLOAT)
            do i = 1, size(values)
                call this%reader%read_be_r32(value_r32, status)
                if (.not. status%ok()) return
                values(i) = real(value_r32, real64)
            end do
        case (NC_INT)
            do i = 1, size(values)
                call this%reader%read_be_i32(value_i32, status)
                if (.not. status%ok()) return
                values(i) = real(value_i32, real64)
            end do
        case default
            call status%set(FORTIO_ETYPE, "variable cannot be converted to real64")
        end select
    end subroutine read_r64_values

    subroutine read_dimensions(this, status)
        class(classic_file_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status
        integer(int32) :: tag, count, length
        integer :: i

        call this%reader%read_be_i32(tag, status)
        if (.not. status%ok()) return
        call this%reader%read_be_i32(count, status)
        if (.not. status%ok()) return
        if (tag == 0 .and. count == 0) then
            allocate(this%dimensions(0))
            return
        end if
        if (tag /= NC_DIMENSION .or. count < 0) then
            call status%set(FORTIO_EFORMAT, "invalid NetCDF dimension list")
            return
        end if
        allocate(this%dimensions(count))
        do i = 1, count
            call read_name(this%reader, this%dimensions(i)%name, status)
            if (.not. status%ok()) return
            call this%reader%read_be_i32(length, status)
            if (.not. status%ok()) return
            this%dimensions(i)%length = unsigned_i32(length)
            this%dimensions(i)%unlimited = length == 0
        end do
    end subroutine read_dimensions

    subroutine read_variables(this, status)
        class(classic_file_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status
        integer(int32) :: tag, count, rank, id, type_code, value_size, begin_32
        integer(int64) :: begin_64
        integer :: i, j

        call this%reader%read_be_i32(tag, status)
        if (.not. status%ok()) return
        call this%reader%read_be_i32(count, status)
        if (.not. status%ok()) return
        if (tag == 0 .and. count == 0) then
            allocate(this%variables(0))
            return
        end if
        if (tag /= NC_VARIABLE .or. count < 0) then
            call status%set(FORTIO_EFORMAT, "invalid NetCDF variable list")
            return
        end if
        allocate(this%variables(count))
        do i = 1, count
            call read_name(this%reader, this%variables(i)%name, status)
            if (.not. status%ok()) return
            call this%reader%read_be_i32(rank, status)
            if (.not. status%ok()) return
            if (rank < 0) then
                call status%set(FORTIO_EFORMAT, "negative NetCDF variable rank")
                return
            end if
            allocate(this%variables(i)%dimension_ids(rank))
            do j = 1, rank
                call this%reader%read_be_i32(id, status)
                if (.not. status%ok()) return
                this%variables(i)%dimension_ids(j) = id
            end do
            call read_attributes(this%reader, this%variables(i)%attributes, status)
            if (.not. status%ok()) return
            this%variables(i)%attribute_count = size(this%variables(i)%attributes)
            call this%reader%read_be_i32(type_code, status)
            if (.not. status%ok()) return
            this%variables(i)%type_code = type_code
            call this%reader%read_be_i32(value_size, status)
            if (.not. status%ok()) return
            this%variables(i)%value_size = unsigned_i32(value_size)
            if (this%version == 1) then
                call this%reader%read_be_i32(begin_32, status)
                begin_64 = unsigned_i32(begin_32)
            else
                call this%reader%read_be_i64(begin_64, status)
            end if
            if (.not. status%ok()) return
            this%variables(i)%begin_offset = begin_64
        end do
    end subroutine read_variables

    subroutine finalize_variable_layout(this, status)
        class(classic_file_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status
        integer(int64), allocatable :: shape(:)
        integer :: i

        call status%clear()
        this%record_size = 0_int64
        do i = 1, size(this%variables)
            shape = this%variable_shape(i)
            this%variables(i)%element_count = product(shape)
            if (size(this%variables(i)%dimension_ids) > 0) then
                this%variables(i)%record_variable = &
                    this%dimensions(this%variables(i)%dimension_ids(1) + 1)%unlimited
            end if
            if (this%variables(i)%record_variable) then
                this%record_size = this%record_size + this%variables(i)%value_size
            end if
        end do
    end subroutine finalize_variable_layout

    subroutine read_attributes(reader, attributes, status)
        type(byte_reader_t), intent(inout) :: reader
        type(classic_attribute_t), allocatable, intent(out) :: attributes(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int32) :: tag, count, type_code, element_count
        integer(int64) :: bytes
        integer :: i

        call reader%read_be_i32(tag, status)
        if (.not. status%ok()) return
        call reader%read_be_i32(count, status)
        if (.not. status%ok()) return
        if (tag == 0 .and. count == 0) then
            allocate(attributes(0))
            return
        end if
        if (tag /= NC_ATTRIBUTE .or. count < 0) then
            call status%set(FORTIO_EFORMAT, "invalid NetCDF attribute list")
            return
        end if
        allocate(attributes(count))
        do i = 1, count
            call read_name(reader, attributes(i)%name, status)
            if (.not. status%ok()) return
            call reader%read_be_i32(type_code, status)
            if (.not. status%ok()) return
            call reader%read_be_i32(element_count, status)
            if (.not. status%ok()) return
            attributes(i)%type_code = type_code
            attributes(i)%element_count = element_count
            bytes = int(element_count, int64)*type_width(type_code)
            if (bytes < 0) then
                call status%set(FORTIO_EFORMAT, "invalid NetCDF attribute")
                return
            end if
            allocate(attributes(i)%bytes(bytes))
            call reader%read_bytes(attributes(i)%bytes, status)
            if (.not. status%ok()) return
            call reader%seek(reader%position + padded_size(bytes) - bytes)
        end do
    end subroutine read_attributes

    subroutine read_name(reader, name, status)
        type(byte_reader_t), intent(inout) :: reader
        character(len=:), allocatable, intent(out) :: name
        type(fortio_status_t), intent(inout) :: status
        integer(int32) :: length
        integer(int8), allocatable :: bytes(:)
        integer :: i

        call reader%read_be_i32(length, status)
        if (.not. status%ok()) return
        if (length < 0) then
            call status%set(FORTIO_EFORMAT, "negative NetCDF name length")
            return
        end if
        allocate(bytes(length))
        call reader%read_bytes(bytes, status)
        if (.not. status%ok()) return
        allocate(character(len=length) :: name)
        do i = 1, length
            name(i:i) = achar(byte_unsigned(bytes(i)))
        end do
        call reader%seek(reader%position + padded_size(int(length, int64)) - length)
    end subroutine read_name

    subroutine seek_variable(this, variable_id, status)
        class(classic_file_t), intent(inout) :: this
        integer, intent(in) :: variable_id
        type(fortio_status_t), intent(inout) :: status

        call status%clear()
        if (this%variables(variable_id)%record_variable .and. this%record_count > 1) then
            call status%set(FORTIO_ENOTSUP, &
                "multi-record variables require strided record reads")
            return
        end if
        call this%reader%seek(this%variables(variable_id)%begin_offset + 1_int64)
    end subroutine seek_variable

    subroutine close_after_error(this)
        class(classic_file_t), intent(inout) :: this
        type(fortio_status_t) :: ignored

        call this%reader%close(ignored)
    end subroutine close_after_error

    pure integer function byte_unsigned(value)
        integer(int8), intent(in) :: value

        byte_unsigned = iand(int(value), 255)
    end function byte_unsigned

    pure integer(int64) function unsigned_i32(value)
        integer(int32), intent(in) :: value

        unsigned_i32 = iand(int(value, int64), int(z'ffffffff', int64))
    end function unsigned_i32

    pure integer(int64) function padded_size(value)
        integer(int64), intent(in) :: value

        padded_size = 4_int64*((value + 3_int64)/4_int64)
    end function padded_size

    pure integer(int64) function type_width(type_code)
        integer, intent(in) :: type_code

        select case (type_code)
        case (NC_BYTE, NC_CHAR)
            type_width = 1
        case (NC_SHORT)
            type_width = 2
        case (NC_INT, NC_FLOAT)
            type_width = 4
        case (NC_DOUBLE)
            type_width = 8
        case default
            type_width = -1
        end select
    end function type_width

end module fortio_netcdf_classic
