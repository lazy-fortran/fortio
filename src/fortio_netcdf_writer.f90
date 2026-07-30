module fortio_netcdf_writer
    use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
    use fortio_bytes, only: byte_writer_t
    use fortio_netcdf_classic, only: NC_INT, NC_DOUBLE
    use fortio_status, only: fortio_status_t, FORTIO_ESTATE, FORTIO_ETYPE, &
                            FORTIO_ESHAPE, FORTIO_ENOTFOUND
    implicit none
    private

    integer(int32), parameter :: NC_DIMENSION = 10
    integer(int32), parameter :: NC_VARIABLE = 11

    type, public :: writer_dimension_t
        character(len=:), allocatable :: name
        integer(int64) :: length = 0_int64
        logical :: unlimited = .false.
    end type writer_dimension_t

    type, public :: writer_variable_t
        character(len=:), allocatable :: name
        integer, allocatable :: dimension_ids(:)
        integer :: type_code = 0
        integer(int32), allocatable :: values_i32(:)
        real(real64), allocatable :: values_r64(:)
        integer(int64) :: begin_offset = 0_int64
        integer(int64) :: value_size = 0_int64
    end type writer_variable_t

    type, public :: classic_writer_t
        character(len=:), allocatable :: path
        type(writer_dimension_t), allocatable :: dimensions(:)
        type(writer_variable_t), allocatable :: variables(:)
        logical :: defining = .true.
        logical :: opened = .false.
    contains
        procedure :: create => classic_writer_create
        procedure :: define_dimension => classic_writer_define_dimension
        procedure :: define_variable => classic_writer_define_variable
        procedure :: end_definition => classic_writer_end_definition
        procedure :: put_i32_scalar => classic_writer_put_i32_scalar
        procedure :: put_i32_1 => classic_writer_put_i32_1
        procedure :: put_r64_scalar => classic_writer_put_r64_scalar
        procedure :: put_r64_1 => classic_writer_put_r64_1
        procedure :: put_r64_2 => classic_writer_put_r64_2
        procedure :: put_r64_3 => classic_writer_put_r64_3
        procedure :: close => classic_writer_close
    end type classic_writer_t

contains

    subroutine classic_writer_create(this, path, status)
        class(classic_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status

        call status%clear()
        this%path = path
        allocate(this%dimensions(0), this%variables(0))
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
        ! The compatibility API accepts Fortran dimension order. Classic
        ! headers store C dimension order.
        temporary(count + 1)%dimension_ids = dimension_ids(size(dimension_ids):1:-1)
        count_values = 1
        do i = 1, size(dimension_ids)
            count_values = count_values*this%dimensions(dimension_ids(i) + 1)%length
        end do
        select case (type_code)
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

    logical function prepare_put(this, id, type_code, count, status)
        class(classic_writer_t), intent(in) :: this
        integer, intent(in) :: id, type_code
        integer(int64), intent(in) :: count
        type(fortio_status_t), intent(inout) :: status

        call status%clear()
        prepare_put = this%opened
        if (prepare_put) prepare_put = .not. this%defining
        if (.not. prepare_put) then
            call status%set(FORTIO_ESTATE, "data write requires data mode")
            return
        end if
        prepare_put = id >= 0 .and. id < size(this%variables)
        if (.not. prepare_put) then
            call status%set(FORTIO_ENOTFOUND, "invalid variable ID")
            return
        end if
        prepare_put = this%variables(id + 1)%type_code == type_code
        if (.not. prepare_put) then
            call status%set(FORTIO_ETYPE, "variable type does not match value")
            return
        end if
        select case (type_code)
        case (NC_INT)
            prepare_put = size(this%variables(id + 1)%values_i32, kind=int64) == count
        case (NC_DOUBLE)
            prepare_put = size(this%variables(id + 1)%values_r64, kind=int64) == count
        end select
        if (.not. prepare_put) call status%set(FORTIO_ESHAPE, "variable shape mismatch")
    end function prepare_put

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
        header_size = header_size + 8 ! global attributes absent
        if (size(this%variables) == 0) then
            header_size = header_size + 8
        else
            header_size = header_size + 8
            do i = 1, size(this%variables)
                header_size = header_size + name_size(this%variables(i)%name) + 4 + &
                              4*size(this%variables(i)%dimension_ids) + 8 + 4 + 4 + 4
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
        call write_absent(writer, status)
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
            call write_absent(writer, status)
            call writer%write_be_i32(int(this%variables(i)%type_code, int32), status)
            call writer%write_be_i32(int(this%variables(i)%value_size, int32), status)
            call writer%write_be_i32(int(this%variables(i)%begin_offset, int32), status)
        end do
    end subroutine write_header

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

    subroutine write_r64_values(writer, values, status)
        type(byte_writer_t), intent(inout) :: writer
        real(real64), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer :: i

        do i = 1, size(values)
            call writer%write_be_r64(values(i), status)
            if (.not. status%ok()) return
        end do
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
