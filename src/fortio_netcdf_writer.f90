module fortio_netcdf_writer
    use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
    use fortio_bytes, only: byte_writer_t
    use fortio_hdf5_writer, only: hdf5_writer_t
    use fortio_netcdf_classic, only: NC_BYTE, NC_CHAR, NC_INT, NC_DOUBLE
    use fortio_status, only: fortio_status_t, FORTIO_ESTATE, FORTIO_ETYPE, &
        FORTIO_ESHAPE, FORTIO_ENOTFOUND
    implicit none
    private

    integer(int32), parameter :: NC_DIMENSION = 10
    integer(int32), parameter :: NC_VARIABLE = 11
    integer(int32), parameter :: NC_ATTRIBUTE = 12

    type :: writer_attribute_t
        character(len=:), allocatable :: name
        integer :: type_code = 0
        character(len=:), allocatable :: value_text
        integer(int32), allocatable :: values_i32(:)
        real(real64), allocatable :: values_r64(:)
    end type writer_attribute_t

    type, public :: writer_dimension_t
        character(len=:), allocatable :: name
        integer(int64) :: length = 0_int64
        logical :: unlimited = .false.
    end type writer_dimension_t

    type, public :: writer_variable_t
        character(len=:), allocatable :: name
        integer, allocatable :: dimension_ids(:)
        integer :: type_code = 0
        integer(int8), allocatable :: values_i8(:)
        integer(int32), allocatable :: values_i32(:)
        real(real64), allocatable :: values_r64(:)
        character(len=:), allocatable :: value_chars
        type(writer_attribute_t), allocatable :: attributes(:)
        logical :: shuffle = .false.
        logical :: deflate = .false.
        integer :: deflate_level = 0
        integer(int64) :: begin_offset = 0_int64
        integer(int64) :: value_size = 0_int64
    end type writer_variable_t

    type, public :: classic_writer_t
        character(len=:), allocatable :: path
        type(writer_dimension_t), allocatable :: dimensions(:)
        type(writer_variable_t), allocatable :: variables(:)
        type(writer_attribute_t), allocatable :: global_attributes(:)
        logical :: defining = .true.
        logical :: opened = .false.
    contains
        procedure :: create => classic_writer_create
        procedure :: define_dimension => classic_writer_define_dimension
        procedure :: define_variable => classic_writer_define_variable
        procedure :: end_definition => classic_writer_end_definition
        procedure :: set_deflate => classic_writer_set_deflate
        procedure :: put_attribute_text => classic_writer_put_attribute_text
        procedure :: put_attribute_i32 => classic_writer_put_attribute_i32
        procedure :: put_attribute_r64 => classic_writer_put_attribute_r64
        procedure :: put_i8_1 => classic_writer_put_i8_1
        procedure :: put_i32_scalar => classic_writer_put_i32_scalar
        procedure :: put_i32_1 => classic_writer_put_i32_1
        procedure :: put_i32_2 => classic_writer_put_i32_2
        procedure :: put_char_scalar => classic_writer_put_char_scalar
        procedure :: put_char_1 => classic_writer_put_char_1
        procedure :: put_r64_scalar => classic_writer_put_r64_scalar
        procedure :: put_r64_1 => classic_writer_put_r64_1
        procedure :: put_r64_slice => classic_writer_put_r64_slice
        procedure :: put_r64_2 => classic_writer_put_r64_2
        procedure :: put_r64_3 => classic_writer_put_r64_3
        procedure :: put_r64_4 => classic_writer_put_r64_4
        procedure :: close => classic_writer_close
        procedure :: close_netcdf4 => classic_writer_close_netcdf4
    end type classic_writer_t

contains

    subroutine classic_writer_create(this, path, status)
        class(classic_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status

        call status%clear()
        this%path = path
        if (allocated(this%dimensions)) deallocate(this%dimensions)
        if (allocated(this%variables)) deallocate(this%variables)
        if (allocated(this%global_attributes)) deallocate(this%global_attributes)
        allocate(this%dimensions(0), this%variables(0), this%global_attributes(0))
        this%defining = .true.
        this%opened = .true.
    end subroutine classic_writer_create

    subroutine classic_writer_define_dimension(this, name, length, unlimited, id, status)
        class(classic_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: length
        logical, intent(in) :: unlimited
        integer, intent(out) :: id
        type(fortio_status_t), intent(inout) :: status
        type(writer_dimension_t), allocatable :: temporary(:)
        integer :: count

        call status%clear()
        if (.not. this%opened .or. .not. this%defining) then
            call status%set(FORTIO_ESTATE, "dimension definition requires define mode")
            id = -1
            return
        end if
        count = size(this%dimensions)
        allocate(temporary(count + 1))
        if (count > 0) temporary(:count) = this%dimensions
        temporary(count + 1)%name = trim(name)
        temporary(count + 1)%length = length
        temporary(count + 1)%unlimited = unlimited
        call move_alloc(temporary, this%dimensions)
        id = count
    end subroutine classic_writer_define_dimension

    subroutine classic_writer_define_variable(this, name, type_code, dimension_ids, &
            id, status)
        class(classic_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: type_code
        integer, intent(in) :: dimension_ids(:)
        integer, intent(out) :: id
        type(fortio_status_t), intent(inout) :: status
        type(writer_variable_t), allocatable :: temporary(:)
        integer(int64) :: count_values
        integer :: count, i

        call status%clear()
        if (.not. this%opened .or. .not. this%defining) then
            call status%set(FORTIO_ESTATE, "variable definition requires define mode")
            id = -1
            return
        end if
        do i = 1, size(dimension_ids)
            if (dimension_ids(i) < 0 .or. dimension_ids(i) >= size(this%dimensions)) then
                call status%set(FORTIO_ENOTFOUND, "invalid dimension ID")
                id = -1
                return
            end if
        end do
        count = size(this%variables)
        allocate(temporary(count + 1))
        if (count > 0) temporary(:count) = this%variables
        temporary(count + 1)%name = trim(name)
        temporary(count + 1)%type_code = type_code
        allocate(temporary(count + 1)%attributes(0))
        ! The compatibility API accepts Fortran dimension order. Classic
        ! headers store C dimension order.
        temporary(count + 1)%dimension_ids = dimension_ids(size(dimension_ids):1:-1)
        count_values = 1
        do i = 1, size(dimension_ids)
            count_values = count_values*this%dimensions(dimension_ids(i) + 1)%length
        end do
        select case (type_code)
        case (NC_BYTE)
            allocate(temporary(count + 1)%values_i8(count_values), source=0_int8)
            temporary(count + 1)%value_size = padded_size(count_values)
        case (NC_CHAR)
            allocate(character(len=count_values) :: temporary(count + 1)%value_chars)
            temporary(count + 1)%value_chars = repeat(achar(0), int(count_values))
            temporary(count + 1)%value_size = padded_size(count_values)
        case (NC_INT)
            allocate(temporary(count + 1)%values_i32(count_values), source=0_int32)
            temporary(count + 1)%value_size = padded_size(4_int64*count_values)
        case (NC_DOUBLE)
            allocate(temporary(count + 1)%values_r64(count_values), source=0.0_real64)
            temporary(count + 1)%value_size = padded_size(8_int64*count_values)
        case default
            call status%set(FORTIO_ETYPE, "writer type is not implemented")
            id = -1
            return
        end select
        call move_alloc(temporary, this%variables)
        id = count
    end subroutine classic_writer_define_variable

    subroutine classic_writer_end_definition(this, status)
        class(classic_writer_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status

        call status%clear()
        if (.not. this%opened) then
            call status%set(FORTIO_ESTATE, "file is not open")
            return
        end if
        this%defining = .false.
    end subroutine classic_writer_end_definition

    subroutine classic_writer_set_deflate(this, id, shuffle, deflate, level, status)
        class(classic_writer_t), intent(inout) :: this
        integer, intent(in) :: id, level
        logical, intent(in) :: shuffle, deflate
        type(fortio_status_t), intent(inout) :: status

        call status%clear()
        if (.not. this%opened) then
            call status%set(FORTIO_ESTATE, "invalid variable for deflate")
            return
        end if
        if (id < 0 .or. id >= size(this%variables)) then
            call status%set(FORTIO_ESTATE, "invalid variable for deflate")
            return
        end if
        this%variables(id + 1)%shuffle = shuffle
        this%variables(id + 1)%deflate = deflate
        this%variables(id + 1)%deflate_level = level
    end subroutine classic_writer_set_deflate

    subroutine classic_writer_put_attribute_text(this, id, name, value, status)
        class(classic_writer_t), intent(inout) :: this
        integer, intent(in) :: id
        character(len=*), intent(in) :: name, value
        type(fortio_status_t), intent(inout) :: status
        type(writer_attribute_t) :: attribute

        if (.not. prepare_attribute(this, id, status)) return
        attribute%name = trim(name)
        attribute%type_code = 2
        attribute%value_text = value
        if (id == -1) then
            call store_attribute(this%global_attributes, attribute)
        else
            call store_attribute(this%variables(id + 1)%attributes, attribute)
        end if
    end subroutine classic_writer_put_attribute_text

    subroutine classic_writer_put_attribute_i32(this, id, name, values, status)
        class(classic_writer_t), intent(inout) :: this
        integer, intent(in) :: id
        character(len=*), intent(in) :: name
        integer(int32), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(writer_attribute_t) :: attribute

        if (.not. prepare_attribute(this, id, status)) return
        attribute%name = trim(name)
        attribute%type_code = NC_INT
        attribute%values_i32 = values
        if (id == -1) then
            call store_attribute(this%global_attributes, attribute)
        else
            call store_attribute(this%variables(id + 1)%attributes, attribute)
        end if
    end subroutine classic_writer_put_attribute_i32

    subroutine classic_writer_put_attribute_r64(this, id, name, values, status)
        class(classic_writer_t), intent(inout) :: this
        integer, intent(in) :: id
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(writer_attribute_t) :: attribute

        if (.not. prepare_attribute(this, id, status)) return
        attribute%name = trim(name)
        attribute%type_code = NC_DOUBLE
        attribute%values_r64 = values
        if (id == -1) then
            call store_attribute(this%global_attributes, attribute)
        else
            call store_attribute(this%variables(id + 1)%attributes, attribute)
        end if
    end subroutine classic_writer_put_attribute_r64

    subroutine classic_writer_put_i8_1(this, id, values, status)
        class(classic_writer_t), intent(inout) :: this
        integer, intent(in) :: id
        integer(int8), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status

        if (.not. prepare_put(this, id, NC_BYTE, size(values, kind=int64), status)) return
        this%variables(id + 1)%values_i8 = values
    end subroutine classic_writer_put_i8_1

    subroutine classic_writer_put_i32_scalar(this, id, value, status)
        class(classic_writer_t), intent(inout) :: this
        integer, intent(in) :: id
        integer(int32), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status

        call this%put_i32_1(id, [value], status)
    end subroutine classic_writer_put_i32_scalar

    subroutine classic_writer_put_i32_1(this, id, values, status)
        class(classic_writer_t), intent(inout) :: this
        integer, intent(in) :: id
        integer(int32), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status

        if (.not. prepare_put(this, id, NC_INT, size(values, kind=int64), status)) return
        this%variables(id + 1)%values_i32 = values
    end subroutine classic_writer_put_i32_1

    subroutine classic_writer_put_i32_2(this, id, values, status)
        class(classic_writer_t), intent(inout) :: this
        integer, intent(in) :: id
        integer(int32), intent(in) :: values(:, :)
        type(fortio_status_t), intent(inout) :: status

        call this%put_i32_1(id, reshape(values, [size(values)]), status)
    end subroutine classic_writer_put_i32_2

    subroutine classic_writer_put_char_scalar(this, id, value, status)
        class(classic_writer_t), intent(inout) :: this
        integer, intent(in) :: id
        character(len=*), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status

        if (.not. prepare_put(this, id, NC_CHAR, int(len(value), int64), status)) return
        this%variables(id + 1)%value_chars = value
    end subroutine classic_writer_put_char_scalar

    subroutine classic_writer_put_char_1(this, id, values, status)
        class(classic_writer_t), intent(inout) :: this
        integer, intent(in) :: id
        character(len=*), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int64) :: count
        integer :: i, first, last

        count = int(len(values), int64)*size(values, kind=int64)
        if (.not. prepare_put(this, id, NC_CHAR, count, status)) return
        do i = 1, size(values)
            first = (i - 1)*len(values) + 1
            last = first + len(values) - 1
            this%variables(id + 1)%value_chars(first:last) = values(i)
        end do
    end subroutine classic_writer_put_char_1

    subroutine classic_writer_put_r64_scalar(this, id, value, status)
        class(classic_writer_t), intent(inout) :: this
        integer, intent(in) :: id
        real(real64), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status

        call this%put_r64_1(id, [value], status)
    end subroutine classic_writer_put_r64_scalar

    subroutine classic_writer_put_r64_1(this, id, values, status)
        class(classic_writer_t), intent(inout) :: this
        integer, intent(in) :: id
        real(real64), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status

        if (.not. prepare_put(this, id, NC_DOUBLE, size(values, kind=int64), status)) return
        this%variables(id + 1)%values_r64 = values
    end subroutine classic_writer_put_r64_1

    subroutine classic_writer_put_r64_slice(this, id, values, start, count, status)
        class(classic_writer_t), intent(inout) :: this
        integer, intent(in) :: id
        real(real64), intent(in) :: values(:)
        integer, intent(in) :: start(:), count(:)
        type(fortio_status_t), intent(inout) :: status
        integer, allocatable :: variable_shape(:)
        integer :: coordinate, dimension, linear, output_index, remaining, stride

        call status%clear()
        if (.not. prepare_variable(this, id, NC_DOUBLE, status)) return
        allocate(variable_shape(size(this%variables(id + 1)%dimension_ids)))
        do dimension = 1, size(variable_shape)
            variable_shape(dimension) = int(this%dimensions( &
                this%variables(id + 1)%dimension_ids(size(variable_shape) - dimension + 1) &
                + 1)%length)
        end do
        if (size(start) /= size(variable_shape) .or. size(count) /= size(variable_shape)) then
            call status%set(FORTIO_ESHAPE, "hyperslab rank does not match variable")
            return
        end if
        if (any(start < 1) .or. any(count < 1)) then
            call status%set(FORTIO_ESHAPE, "hyperslab start and count must be positive")
            return
        end if
        if (any(start + count - 1 > variable_shape)) then
            call status%set(FORTIO_ESHAPE, "hyperslab is outside variable")
            return
        end if
        if (product(int(count, int64)) /= size(values, kind=int64)) then
            call status%set(FORTIO_ESHAPE, "hyperslab count does not match value size")
            return
        end if
        if (size(variable_shape) == 2) then
            if (count(1) == 1) then
                output_index = start(1) + (start(2) - 1)*variable_shape(1)
                this%variables(id + 1)%values_r64( &
                    output_index:output_index + (count(2) - 1)*variable_shape(1): &
                    variable_shape(1)) = values
                return
            end if
        end if
        do linear = 1, size(values)
            remaining = linear - 1
            output_index = 1
            stride = 1
            do dimension = 1, size(variable_shape)
                coordinate = mod(remaining, count(dimension))
                remaining = remaining/count(dimension)
                output_index = output_index + &
                    (start(dimension) - 1 + coordinate)*stride
                stride = stride*variable_shape(dimension)
            end do
            this%variables(id + 1)%values_r64(output_index) = values(linear)
        end do
    end subroutine classic_writer_put_r64_slice

    subroutine classic_writer_put_r64_2(this, id, values, status)
        class(classic_writer_t), intent(inout) :: this
        integer, intent(in) :: id
        real(real64), intent(in) :: values(:, :)
        type(fortio_status_t), intent(inout) :: status

        call this%put_r64_1(id, reshape(values, [size(values)]), status)
    end subroutine classic_writer_put_r64_2

    subroutine classic_writer_put_r64_3(this, id, values, status)
        class(classic_writer_t), intent(inout) :: this
        integer, intent(in) :: id
        real(real64), intent(in) :: values(:, :, :)
        type(fortio_status_t), intent(inout) :: status

        call this%put_r64_1(id, reshape(values, [size(values)]), status)
    end subroutine classic_writer_put_r64_3

    subroutine classic_writer_put_r64_4(this, id, values, status)
        class(classic_writer_t), intent(inout) :: this
        integer, intent(in) :: id
        real(real64), intent(in) :: values(:, :, :, :)
        type(fortio_status_t), intent(inout) :: status

        call this%put_r64_1(id, reshape(values, [size(values)]), status)
    end subroutine classic_writer_put_r64_4

    subroutine classic_writer_close(this, status)
        class(classic_writer_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status
        type(byte_writer_t) :: writer
        integer(int64) :: offset
        integer :: i

        call status%clear()
        if (.not. this%opened) return
        call assign_offsets(this)
        call writer%open(this%path, status)
        if (.not. status%ok()) return
        call write_header(this, writer, status)
        if (.not. status%ok()) return
        do i = 1, size(this%variables)
            call writer%seek(this%variables(i)%begin_offset + 1)
            select case (this%variables(i)%type_code)
            case (NC_BYTE)
                call writer%write_bytes(this%variables(i)%values_i8, status)
            case (NC_CHAR)
                call write_text_value(writer, this%variables(i)%value_chars, status)
            case (NC_INT)
                call write_i32_values(writer, this%variables(i)%values_i32, status)
            case (NC_DOUBLE)
                call write_r64_values(writer, this%variables(i)%values_r64, status)
            end select
            if (.not. status%ok()) return
            offset = writer%position - 1
            call write_padding(writer, padded_size(offset) - offset, status)
            if (.not. status%ok()) return
        end do
        call writer%close(status)
        this%opened = .false.
    end subroutine classic_writer_close

    subroutine classic_writer_close_netcdf4(this, status)
        class(classic_writer_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status
        type(hdf5_writer_t) :: writer
        logical, allocatable :: coordinate_variables(:)
        integer(int32), allocatable :: coordinate_values(:)
        character(len=256), allocatable :: scale_names(:)
        integer :: i, j, rank

        call status%clear()
        if (.not. this%opened) return
        call writer%create(this%path, status)
        if (.not. status%ok()) return
        allocate(coordinate_variables(size(this%dimensions)), source=.false.)
        do i = 1, size(this%dimensions)
            do j = 1, size(this%variables)
                if (this%variables(j)%name /= this%dimensions(i)%name) cycle
                if (size(this%variables(j)%dimension_ids) /= 1) cycle
                if (this%variables(j)%dimension_ids(1) /= i - 1) cycle
                coordinate_variables(i) = .true.
                exit
            end do
            if (coordinate_variables(i)) cycle
            coordinate_values = [(int(j - 1, int32), &
                j=1, int(this%dimensions(i)%length))]
            call writer%add_i32_1(this%dimensions(i)%name, coordinate_values, status)
            if (.not. status%ok()) return
            call mark_netcdf4_dimension_scale(writer, this%dimensions(i)%name, i - 1, &
                status, coordinate_variable=.false.)
            if (.not. status%ok()) return
        end do
        do i = 1, size(this%variables)
            call add_netcdf4_variable(writer, this%variables(i), this%dimensions, status)
            if (.not. status%ok()) return
            call copy_netcdf4_attributes(writer, this%variables(i)%name, &
                this%variables(i)%attributes, status)
            if (.not. status%ok()) return
            if (size(this%variables(i)%dimension_ids) > 0) then
                call writer%add_i32_attribute(this%variables(i)%name, &
                    "_Netcdf4Coordinates", &
                    int(this%variables(i)%dimension_ids, int32), status)
                if (.not. status%ok()) return
            end if
        end do
        do i = 1, size(this%dimensions)
            if (.not. coordinate_variables(i)) cycle
            call mark_netcdf4_dimension_scale(writer, this%dimensions(i)%name, i - 1, &
                status)
            if (.not. status%ok()) return
        end do
        do i = 1, size(this%variables)
            rank = size(this%variables(i)%dimension_ids)
            if (rank == 0) cycle
            if (is_coordinate_variable(this%variables(i), this%dimensions)) cycle
            allocate(scale_names(rank))
            do j = 1, rank
                scale_names(j) = this%dimensions( &
                    this%variables(i)%dimension_ids(rank - j + 1) + 1)%name
            end do
            call writer%set_dimension_list(this%variables(i)%name, scale_names, status)
            deallocate(scale_names)
            if (.not. status%ok()) return
        end do
        call copy_netcdf4_root_attributes(writer, this%global_attributes, status)
        if (.not. status%ok()) return
        call writer%close(status)
        if (status%ok()) this%opened = .false.
    end subroutine classic_writer_close_netcdf4

    subroutine mark_netcdf4_dimension_scale(writer, name, dimension_id, status, &
            coordinate_variable)
        type(hdf5_writer_t), intent(inout) :: writer
        character(len=*), intent(in) :: name
        integer, intent(in) :: dimension_id
        type(fortio_status_t), intent(inout) :: status
        logical, intent(in), optional :: coordinate_variable

        if (present(coordinate_variable)) then
            call writer%mark_dimension_scale(name, status, coordinate_variable)
        else
            call writer%mark_dimension_scale(name, status)
        end if
        if (.not. status%ok()) return
        call writer%add_i32_attribute(name, "_Netcdf4Dimid", &
            [int(dimension_id, int32)], status)
    end subroutine mark_netcdf4_dimension_scale

    subroutine add_netcdf4_variable(writer, variable, dimensions, status)
        type(hdf5_writer_t), intent(inout) :: writer
        type(writer_variable_t), intent(in) :: variable
        type(writer_dimension_t), intent(in) :: dimensions(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int64), allocatable :: array_shape(:)
        integer :: i, rank

        rank = size(variable%dimension_ids)
        allocate(array_shape(rank))
        do i = 1, rank
            array_shape(i) = dimensions(variable%dimension_ids(rank - i + 1) + 1)%length
        end do
        select case (variable%type_code)
        case (NC_BYTE)
            call writer%add_i8(variable%name, array_shape, variable%values_i8, status)
        case (NC_CHAR)
            call writer%add_char(variable%name, array_shape, variable%value_chars, status)
        case (NC_INT)
            call writer%add_i32_values(variable%name, array_shape, &
                variable%values_i32, status)
        case (NC_DOUBLE)
            call writer%add_r64_values(variable%name, array_shape, &
                variable%values_r64, status)
        end select
        if (.not. status%ok()) return
        if (variable%deflate) then
            call writer%set_deflate(variable%name, variable%shuffle, &
                variable%deflate_level, status)
        end if
    end subroutine add_netcdf4_variable

    subroutine copy_netcdf4_attributes(writer, dataset_name, attributes, status)
        type(hdf5_writer_t), intent(inout) :: writer
        character(len=*), intent(in) :: dataset_name
        type(writer_attribute_t), intent(in) :: attributes(:)
        type(fortio_status_t), intent(inout) :: status
        integer :: i

        do i = 1, size(attributes)
            select case (attributes(i)%type_code)
            case (NC_CHAR)
                call writer%add_text_attribute(dataset_name, attributes(i)%name, &
                    attributes(i)%value_text, status)
            case (NC_INT)
                call writer%add_i32_attribute(dataset_name, attributes(i)%name, &
                    attributes(i)%values_i32, status)
            case (NC_DOUBLE)
                call writer%add_r64_attribute(dataset_name, attributes(i)%name, &
                    attributes(i)%values_r64(1), status)
            end select
            if (.not. status%ok()) return
        end do
    end subroutine copy_netcdf4_attributes

    subroutine copy_netcdf4_root_attributes(writer, attributes, status)
        type(hdf5_writer_t), intent(inout) :: writer
        type(writer_attribute_t), intent(in) :: attributes(:)
        type(fortio_status_t), intent(inout) :: status
        integer :: i

        do i = 1, size(attributes)
            select case (attributes(i)%type_code)
            case (NC_CHAR)
                call writer%add_root_text_attribute(attributes(i)%name, &
                    attributes(i)%value_text, status)
            case (NC_INT)
                call writer%add_root_i32_attribute(attributes(i)%name, &
                    attributes(i)%values_i32, status)
            case (NC_DOUBLE)
                call writer%add_root_r64_attribute(attributes(i)%name, &
                    attributes(i)%values_r64(1), status)
            end select
            if (.not. status%ok()) return
        end do
    end subroutine copy_netcdf4_root_attributes

    logical function is_coordinate_variable(variable, dimensions)
        type(writer_variable_t), intent(in) :: variable
        type(writer_dimension_t), intent(in) :: dimensions(:)

        is_coordinate_variable = size(variable%dimension_ids) == 1
        if (.not. is_coordinate_variable) return
        is_coordinate_variable = variable%name == &
            dimensions(variable%dimension_ids(1) + 1)%name
    end function is_coordinate_variable

    logical function prepare_put(this, id, type_code, count, status)
        class(classic_writer_t), intent(in) :: this
        integer, intent(in) :: id, type_code
        integer(int64), intent(in) :: count
        type(fortio_status_t), intent(inout) :: status

        prepare_put = prepare_variable(this, id, type_code, status)
        if (.not. prepare_put) return
        select case (type_code)
        case (NC_BYTE)
            prepare_put = size(this%variables(id + 1)%values_i8, kind=int64) == count
        case (NC_CHAR)
            prepare_put = len(this%variables(id + 1)%value_chars, kind=int64) == count
        case (NC_INT)
            prepare_put = size(this%variables(id + 1)%values_i32, kind=int64) == count
        case (NC_DOUBLE)
            prepare_put = size(this%variables(id + 1)%values_r64, kind=int64) == count
        end select
        if (.not. prepare_put) call status%set(FORTIO_ESHAPE, "variable shape mismatch")
    end function prepare_put

    logical function prepare_variable(this, id, type_code, status)
        class(classic_writer_t), intent(in) :: this
        integer, intent(in) :: id, type_code
        type(fortio_status_t), intent(inout) :: status

        call status%clear()
        prepare_variable = this%opened
        if (.not. prepare_variable) then
            call status%set(FORTIO_ESTATE, "data write requires an open file")
            return
        end if
        prepare_variable = id >= 0
        if (prepare_variable) prepare_variable = id < size(this%variables)
        if (.not. prepare_variable) then
            call status%set(FORTIO_ENOTFOUND, "invalid variable ID")
            return
        end if
        prepare_variable = this%variables(id + 1)%type_code == type_code
        if (.not. prepare_variable) &
            call status%set(FORTIO_ETYPE, "variable type does not match value")
    end function prepare_variable

    logical function prepare_attribute(this, id, status)
        class(classic_writer_t), intent(in) :: this
        integer, intent(in) :: id
        type(fortio_status_t), intent(inout) :: status

        call status%clear()
        prepare_attribute = this%opened
        if (prepare_attribute) prepare_attribute = this%defining
        if (.not. prepare_attribute) then
            call status%set(FORTIO_ESTATE, "attribute write requires define mode")
            return
        end if
        prepare_attribute = id == -1
        if (.not. prepare_attribute) then
            prepare_attribute = id >= 0
            if (prepare_attribute) prepare_attribute = id < size(this%variables)
        end if
        if (.not. prepare_attribute) call status%set(FORTIO_ENOTFOUND, "invalid variable ID")
    end function prepare_attribute

    subroutine store_attribute(attributes, attribute)
        type(writer_attribute_t), allocatable, intent(inout) :: attributes(:)
        type(writer_attribute_t), intent(in) :: attribute
        type(writer_attribute_t), allocatable :: temporary(:)
        integer :: i, count

        do i = 1, size(attributes)
            if (attributes(i)%name == attribute%name) then
                attributes(i) = attribute
                return
            end if
        end do
        count = size(attributes)
        allocate(temporary(count + 1))
        if (count > 0) temporary(:count) = attributes
        temporary(count + 1) = attribute
        call move_alloc(temporary, attributes)
    end subroutine store_attribute

    subroutine assign_offsets(this)
        class(classic_writer_t), intent(inout) :: this
        integer(int64) :: offset
        integer :: i

        offset = header_size(this)
        do i = 1, size(this%variables)
            this%variables(i)%begin_offset = offset
            offset = offset + this%variables(i)%value_size
        end do
    end subroutine assign_offsets

    integer(int64) function header_size(this)
        class(classic_writer_t), intent(in) :: this
        integer :: i

        header_size = 4 + 4
        if (size(this%dimensions) == 0) then
            header_size = header_size + 8
        else
            header_size = header_size + 8
            do i = 1, size(this%dimensions)
                header_size = header_size + name_size(this%dimensions(i)%name) + 4
            end do
        end if
        header_size = header_size + attributes_size(this%global_attributes)
        if (size(this%variables) == 0) then
            header_size = header_size + 8
        else
            header_size = header_size + 8
            do i = 1, size(this%variables)
                header_size = header_size + name_size(this%variables(i)%name) + 4 + &
                    4*size(this%variables(i)%dimension_ids) + &
                    attributes_size(this%variables(i)%attributes) + 4 + 4 + 4
            end do
        end if
    end function header_size

    subroutine write_header(this, writer, status)
        class(classic_writer_t), intent(in) :: this
        type(byte_writer_t), intent(inout) :: writer
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: magic(4)
        integer :: i, j

        magic = [int(iachar("C"), int8), int(iachar("D"), int8), &
            int(iachar("F"), int8), 1_int8]
        call writer%write_bytes(magic, status)
        if (.not. status%ok()) return
        call writer%write_be_i32(0_int32, status)
        if (.not. status%ok()) return
        if (size(this%dimensions) == 0) then
            call write_absent(writer, status)
        else
            call writer%write_be_i32(NC_DIMENSION, status)
            call writer%write_be_i32(int(size(this%dimensions), int32), status)
            do i = 1, size(this%dimensions)
                call write_name(writer, this%dimensions(i)%name, status)
                if (this%dimensions(i)%unlimited) then
                    call writer%write_be_i32(0_int32, status)
                else
                    call writer%write_be_i32(int(this%dimensions(i)%length, int32), status)
                end if
            end do
        end if
        call write_attributes(writer, this%global_attributes, status)
        if (size(this%variables) == 0) then
            call write_absent(writer, status)
            return
        end if
        call writer%write_be_i32(NC_VARIABLE, status)
        call writer%write_be_i32(int(size(this%variables), int32), status)
        do i = 1, size(this%variables)
            call write_name(writer, this%variables(i)%name, status)
            call writer%write_be_i32(int(size(this%variables(i)%dimension_ids), int32), status)
            do j = 1, size(this%variables(i)%dimension_ids)
                call writer%write_be_i32(int(this%variables(i)%dimension_ids(j), int32), status)
            end do
            call write_attributes(writer, this%variables(i)%attributes, status)
            call writer%write_be_i32(int(this%variables(i)%type_code, int32), status)
            call writer%write_be_i32(int(this%variables(i)%value_size, int32), status)
            call writer%write_be_i32(int(this%variables(i)%begin_offset, int32), status)
        end do
    end subroutine write_header

    integer(int64) function attributes_size(attributes) result(total)
        type(writer_attribute_t), intent(in) :: attributes(:)
        integer(int64) :: bytes
        integer :: i

        total = 8_int64
        do i = 1, size(attributes)
            select case (attributes(i)%type_code)
            case (2)
                bytes = len(attributes(i)%value_text)
            case (NC_INT)
                bytes = 4_int64*size(attributes(i)%values_i32, kind=int64)
            case (NC_DOUBLE)
                bytes = 8_int64*size(attributes(i)%values_r64, kind=int64)
            case default
                bytes = 0
            end select
            total = total + name_size(attributes(i)%name) + 8 + padded_size(bytes)
        end do
    end function attributes_size

    subroutine write_attributes(writer, attributes, status)
        type(byte_writer_t), intent(inout) :: writer
        type(writer_attribute_t), intent(in) :: attributes(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8), allocatable :: text_bytes(:)
        integer(int64) :: bytes
        integer :: i, j

        if (size(attributes) == 0) then
            call write_absent(writer, status)
            return
        end if
        call writer%write_be_i32(NC_ATTRIBUTE, status)
        call writer%write_be_i32(int(size(attributes), int32), status)
        do i = 1, size(attributes)
            call write_name(writer, attributes(i)%name, status)
            call writer%write_be_i32(int(attributes(i)%type_code, int32), status)
            select case (attributes(i)%type_code)
            case (2)
                call writer%write_be_i32(int(len(attributes(i)%value_text), int32), status)
                allocate(text_bytes(len(attributes(i)%value_text)))
                do j = 1, size(text_bytes)
                    text_bytes(j) = int(iachar(attributes(i)%value_text(j:j)), int8)
                end do
                call writer%write_bytes(text_bytes, status)
                bytes = size(text_bytes, kind=int64)
                deallocate(text_bytes)
            case (NC_INT)
                call writer%write_be_i32(int(size(attributes(i)%values_i32), int32), status)
                call write_i32_values(writer, attributes(i)%values_i32, status)
                bytes = 4_int64*size(attributes(i)%values_i32, kind=int64)
            case (NC_DOUBLE)
                call writer%write_be_i32(int(size(attributes(i)%values_r64), int32), status)
                call write_r64_values(writer, attributes(i)%values_r64, status)
                bytes = 8_int64*size(attributes(i)%values_r64, kind=int64)
            end select
            if (.not. status%ok()) return
            call write_padding(writer, padded_size(bytes) - bytes, status)
            if (.not. status%ok()) return
        end do
    end subroutine write_attributes

    subroutine write_absent(writer, status)
        type(byte_writer_t), intent(inout) :: writer
        type(fortio_status_t), intent(inout) :: status

        call writer%write_be_i32(0_int32, status)
        if (.not. status%ok()) return
        call writer%write_be_i32(0_int32, status)
    end subroutine write_absent

    subroutine write_name(writer, name, status)
        type(byte_writer_t), intent(inout) :: writer
        character(len=*), intent(in) :: name
        type(fortio_status_t), intent(inout) :: status
        integer(int8), allocatable :: bytes(:)
        integer :: i

        call writer%write_be_i32(int(len(name), int32), status)
        if (.not. status%ok()) return
        allocate(bytes(len(name)))
        do i = 1, len(name)
            bytes(i) = int(iachar(name(i:i)), int8)
        end do
        call writer%write_bytes(bytes, status)
        if (.not. status%ok()) return
        call write_padding(writer, padded_size(int(len(name), int64)) - len(name), status)
    end subroutine write_name

    subroutine write_i32_values(writer, values, status)
        type(byte_writer_t), intent(inout) :: writer
        integer(int32), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer :: i

        do i = 1, size(values)
            call writer%write_be_i32(values(i), status)
            if (.not. status%ok()) return
        end do
    end subroutine write_i32_values

    subroutine write_text_value(writer, value, status)
        type(byte_writer_t), intent(inout) :: writer
        character(len=*), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status
        integer(int8), allocatable :: bytes(:)
        integer :: i

        allocate(bytes(len(value)))
        do i = 1, len(value)
            bytes(i) = int(iachar(value(i:i)), int8)
        end do
        call writer%write_bytes(bytes, status)
    end subroutine write_text_value

    subroutine write_r64_values(writer, values, status)
        type(byte_writer_t), intent(inout) :: writer
        real(real64), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        call writer%write_be_r64_array(values, status)
    end subroutine write_r64_values

    subroutine write_padding(writer, count, status)
        type(byte_writer_t), intent(inout) :: writer
        integer(int64), intent(in) :: count
        type(fortio_status_t), intent(inout) :: status
        integer :: i

        if (count <= 0) return
        call writer%write_bytes([(0_int8, i=1, int(count))], status)
    end subroutine write_padding

    pure integer(int64) function padded_size(value)
        integer(int64), intent(in) :: value

        padded_size = 4_int64*((value + 3_int64)/4_int64)
    end function padded_size

    pure integer(int64) function name_size(name)
        character(len=*), intent(in) :: name

        name_size = 4_int64 + padded_size(int(len(name), int64))
    end function name_size

end module fortio_netcdf_writer
