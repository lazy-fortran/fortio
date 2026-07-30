module netcdf
    use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real32, real64
    use fortio_bytes, only: decode_be_i32, decode_be_i64
    use fortio_netcdf_classic, only: classic_file_t, NC_BYTE, NC_CHAR, NC_SHORT, &
                                    NC_INT, NC_FLOAT, NC_DOUBLE
    use fortio_netcdf_writer, only: classic_writer_t
    use fortio_status, only: fortio_status_t, FORTIO_SUCCESS, FORTIO_ENOTFOUND, &
                            FORTIO_ESTATE, FORTIO_ENOTSUP, FORTIO_ESHAPE, FORTIO_EEXIST
    implicit none
    private

    integer, parameter, public :: NF90_NOERR = FORTIO_SUCCESS
    integer, parameter, public :: NF90_EBADID = FORTIO_ESTATE
    integer, parameter, public :: NF90_ENOTVAR = FORTIO_ENOTFOUND
    integer, parameter, public :: NF90_ENOTATT = FORTIO_ENOTFOUND
    integer, parameter, public :: NF90_EBADDIM = FORTIO_ENOTFOUND
    integer, parameter, public :: NF90_ENOTSUPPORT = FORTIO_ENOTSUP
    integer, parameter, public :: NF90_EINVAL = FORTIO_ESHAPE
    integer, parameter, public :: NF90_EEXIST = FORTIO_EEXIST
    integer, parameter, public :: NF90_NOWRITE = 0
    integer, parameter, public :: NF90_WRITE = 1
    integer, parameter, public :: NF90_CLOBBER = 0
    integer, parameter, public :: NF90_NOCLOBBER = 4
    integer, parameter, public :: NF90_NETCDF4 = 4096
    integer, parameter, public :: NF90_UNLIMITED = 0
    integer, parameter, public :: NF90_GLOBAL = -1
    integer, parameter, public :: NF90_MAX_NAME = 256
    integer, parameter, public :: NF90_MAX_VAR_DIMS = 1024
    integer, parameter, public :: NF90_BYTE = NC_BYTE
    integer, parameter, public :: NF90_CHAR = NC_CHAR
    integer, parameter, public :: NF90_SHORT = NC_SHORT
    integer, parameter, public :: NF90_INT = NC_INT
    integer, parameter, public :: NF90_FLOAT = NC_FLOAT
    integer, parameter, public :: NF90_DOUBLE = NC_DOUBLE
    integer, parameter, public :: NF90_INT64 = 10
    integer, parameter, public :: NF90_STRING = 12

    integer, parameter :: MAX_OPEN_FILES = 32
    type(classic_file_t), save :: files(MAX_OPEN_FILES)
    type(classic_writer_t), save :: writers(MAX_OPEN_FILES)
    logical, save :: in_use(MAX_OPEN_FILES) = .false.
    logical, save :: writing(MAX_OPEN_FILES) = .false.
    character(len=512), save :: last_error = ""

    interface nf90_get_var
        module procedure get_char_scalar, get_char_rank1
        module procedure get_i32_scalar, get_i32_rank1
        module procedure get_r64_scalar, get_r64_rank1, get_r64_rank2, get_r64_rank3
        module procedure get_r64_rank4
    end interface nf90_get_var

    interface nf90_def_var
        module procedure def_var_scalar, def_var_one_dimension, def_var_array
    end interface nf90_def_var

    interface nf90_put_var
        module procedure put_char_scalar, put_char_rank1
        module procedure put_i32_scalar, put_i32_rank1
        module procedure put_r64_scalar, put_r64_rank1, put_r64_rank2, put_r64_rank3
    end interface nf90_put_var

    interface nf90_get_att
        module procedure get_att_text
        module procedure get_att_i32_scalar, get_att_i32_rank1
        module procedure get_att_r64_scalar, get_att_r64_rank1
    end interface nf90_get_att

    interface nf90_put_att
        module procedure put_att_text
        module procedure put_att_i32_scalar, put_att_i32_rank1
        module procedure put_att_r64_scalar, put_att_r64_rank1
    end interface nf90_put_att

    public :: nf90_open, nf90_create, nf90_close, nf90_def_dim, nf90_def_var
    public :: nf90_enddef, nf90_inq_varid, nf90_get_var, nf90_put_var, nf90_strerror
    public :: nf90_inq_dimid, nf90_inquire_dimension, nf90_inquire_variable
    public :: nf90_inquire_attribute, nf90_get_att
    public :: nf90_put_att, nf90_redef, nf90_def_grp, nf90_inq_ncid

contains

    integer function nf90_open(path, mode, ncid) result(code)
        character(len=*), intent(in) :: path
        integer, intent(in) :: mode
        integer, intent(out) :: ncid
        type(fortio_status_t) :: status
        integer :: slot

        ncid = -1
        if (mode /= NF90_NOWRITE) then
            code = NF90_ENOTSUPPORT
            last_error = "classic compatibility writer is not implemented"
            return
        end if
        slot = first_free_slot()
        if (slot == 0) then
            code = NF90_EBADID
            last_error = "fortio open-file table is full"
            return
        end if
        call files(slot)%open(path, status)
        code = status%code
        if (.not. status%ok()) then
            last_error = status%message
            return
        end if
        in_use(slot) = .true.
        ncid = slot
    end function nf90_open

    integer function nf90_create(path, cmode, ncid) result(code)
        character(len=*), intent(in) :: path
        integer, intent(in) :: cmode
        integer, intent(out) :: ncid
        type(fortio_status_t) :: status
        integer :: slot
        logical :: exists

        ncid = -1
        if (iand(cmode, NF90_NOCLOBBER) /= 0) then
            inquire (file=trim(path), exist=exists)
        else
            exists = .false.
        end if
        if (exists) then
            code = NF90_EEXIST
            last_error = "file already exists: "//trim(path)
            return
        end if
        slot = first_free_slot()
        if (slot == 0) then
            code = NF90_EBADID
            last_error = "fortio open-file table is full"
            return
        end if
        call writers(slot)%create(path, status)
        code = status%code
        if (.not. status%ok()) then
            last_error = status%message
            return
        end if
        in_use(slot) = .true.
        writing(slot) = .true.
        ncid = slot
    end function nf90_create

    integer function nf90_close(ncid) result(code)
        integer, intent(in) :: ncid
        type(fortio_status_t) :: status

        if (.not. valid_id(ncid)) then
            code = NF90_EBADID
            return
        end if
        if (writing(ncid)) then
            call writers(ncid)%close(status)
        else
            call files(ncid)%close(status)
        end if
        in_use(ncid) = .false.
        writing(ncid) = .false.
        code = status%code
        if (.not. status%ok()) last_error = status%message
    end function nf90_close

    integer function nf90_def_dim(ncid, name, length, dimid) result(code)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        integer, intent(in) :: length
        integer, intent(out) :: dimid
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            dimid = -1
            return
        end if
        call writers(ncid)%define_dimension(name, int(length, int64), &
                                            length == NF90_UNLIMITED, dimid, status)
        code = finish_status(status)
    end function nf90_def_dim

    integer function def_var_scalar(ncid, name, type_code, varid) result(code)
        integer, intent(in) :: ncid, type_code
        character(len=*), intent(in) :: name
        integer, intent(out) :: varid

        code = def_var_array(ncid, name, type_code, [integer ::], varid)
    end function def_var_scalar

    integer function def_var_one_dimension(ncid, name, type_code, dimension_id, &
                                           varid) result(code)
        integer, intent(in) :: ncid, type_code, dimension_id
        character(len=*), intent(in) :: name
        integer, intent(out) :: varid

        code = def_var_array(ncid, name, type_code, [dimension_id], varid)
    end function def_var_one_dimension

    integer function def_var_array(ncid, name, type_code, dimension_ids, varid) result(code)
        integer, intent(in) :: ncid, type_code
        character(len=*), intent(in) :: name
        integer, intent(in) :: dimension_ids(:)
        integer, intent(out) :: varid
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            varid = -1
            return
        end if
        call writers(ncid)%define_variable(name, type_code, dimension_ids, varid, status)
        code = finish_status(status)
    end function def_var_array

    integer function nf90_enddef(ncid) result(code)
        integer, intent(in) :: ncid
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%end_definition(status)
        code = finish_status(status)
    end function nf90_enddef

    integer function nf90_redef(ncid) result(code)
        integer, intent(in) :: ncid

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        writers(ncid)%defining = .true.
        code = NF90_NOERR
    end function nf90_redef

    integer function nf90_def_grp(ncid, name, new_ncid) result(code)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        integer, intent(out) :: new_ncid

        associate(unused_name => name)
        end associate
        new_ncid = -1
        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
        else
            code = NF90_ENOTSUPPORT
        end if
    end function nf90_def_grp

    integer function nf90_inq_ncid(ncid, name, group_ncid) result(code)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        integer, intent(out) :: group_ncid

        associate(unused_name => name)
        end associate
        group_ncid = -1
        if (.not. valid_id(ncid)) then
            code = NF90_EBADID
        else
            code = NF90_ENOTSUPPORT
        end if
    end function nf90_inq_ncid

    integer function nf90_inq_varid(ncid, name, varid) result(code)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        integer, intent(out) :: varid

        if (.not. valid_id(ncid)) then
            code = NF90_EBADID
            varid = -1
            return
        end if
        if (writing(ncid)) then
            varid = writer_variable_id(ncid, name)
            if (varid < 0) then
                code = NF90_ENOTVAR
                return
            end if
        else
            varid = files(ncid)%variable_id(name)
            if (varid == 0) then
                code = NF90_ENOTVAR
                varid = -1
                return
            end if
        end if
        code = NF90_NOERR
    end function nf90_inq_varid

    integer function nf90_inq_dimid(ncid, name, dimid) result(code)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        integer, intent(out) :: dimid

        if (.not. valid_reader(ncid)) then
            code = NF90_EBADID
            dimid = -1
            return
        end if
        dimid = files(ncid)%dimension_id(name)
        if (dimid == 0) then
            code = NF90_EBADDIM
            dimid = -1
        else
            code = NF90_NOERR
        end if
    end function nf90_inq_dimid

    integer function nf90_inquire_dimension(ncid, dimid, name, len) result(code)
        integer, intent(in) :: ncid, dimid
        character(len=*), intent(out), optional :: name
        integer, intent(out), optional :: len

        if (.not. valid_reader(ncid)) then
            code = NF90_EBADID
            return
        end if
        if (dimid < 1 .or. dimid > size(files(ncid)%dimensions)) then
            code = NF90_EBADDIM
            return
        end if
        if (present(name)) name = files(ncid)%dimensions(dimid)%name
        if (present(len)) then
            if (files(ncid)%dimensions(dimid)%unlimited) then
                len = int(files(ncid)%record_count)
            else
                len = int(files(ncid)%dimensions(dimid)%length)
            end if
        end if
        code = NF90_NOERR
    end function nf90_inquire_dimension

    integer function nf90_inquire_variable(ncid, varid, name, xtype, ndims, dimids, &
                                           natts) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(out), optional :: name
        integer, intent(out), optional :: xtype, ndims, dimids(:), natts
        integer :: rank, i

        if (.not. valid_reader(ncid)) then
            code = NF90_EBADID
            return
        end if
        if (varid < 1 .or. varid > size(files(ncid)%variables)) then
            code = NF90_ENOTVAR
            return
        end if
        rank = size(files(ncid)%variables(varid)%dimension_ids)
        if (present(name)) name = files(ncid)%variables(varid)%name
        if (present(xtype)) xtype = files(ncid)%variables(varid)%type_code
        if (present(ndims)) ndims = rank
        if (present(natts)) natts = files(ncid)%variables(varid)%attribute_count
        if (present(dimids)) then
            if (size(dimids) < rank) then
                code = NF90_EINVAL
                return
            end if
            do i = 1, rank
                dimids(i) = files(ncid)%variables(varid)%dimension_ids(rank - i + 1) + 1
            end do
        end if
        code = NF90_NOERR
    end function nf90_inquire_variable

    integer function nf90_inquire_attribute(ncid, varid, name, xtype, len) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: name
        integer, intent(out), optional :: xtype, len
        integer :: attribute_id

        code = find_attribute(ncid, varid, name, attribute_id)
        if (code /= NF90_NOERR) return
        if (present(xtype)) xtype = attribute_type(ncid, varid, attribute_id)
        if (present(len)) len = attribute_length(ncid, varid, attribute_id)
    end function nf90_inquire_attribute

    integer function get_att_text(ncid, varid, name, value) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: name
        character(len=*), intent(out) :: value
        integer :: attribute_id, i, count

        code = find_attribute(ncid, varid, name, attribute_id)
        if (code /= NF90_NOERR) return
        if (attribute_type(ncid, varid, attribute_id) /= NF90_CHAR) then
            code = NF90_EINVAL
            return
        end if
        count = attribute_length(ncid, varid, attribute_id)
        value = ""
        do i = 1, min(count, len(value))
            value(i:i) = achar(attribute_byte(ncid, varid, attribute_id, i))
        end do
    end function get_att_text

    integer function get_att_i32_scalar(ncid, varid, name, value) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: name
        integer(int32), intent(out) :: value
        integer(int32) :: temporary(1)

        code = get_att_i32_rank1(ncid, varid, name, temporary)
        if (code == NF90_NOERR) value = temporary(1)
    end function get_att_i32_scalar

    integer function get_att_i32_rank1(ncid, varid, name, value) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: name
        integer(int32), intent(out) :: value(:)
        integer :: attribute_id, i, width

        code = find_attribute(ncid, varid, name, attribute_id)
        if (code /= NF90_NOERR) return
        if (size(value) /= attribute_length(ncid, varid, attribute_id)) then
            code = NF90_EINVAL
            return
        end if
        select case (attribute_type(ncid, varid, attribute_id))
        case (NF90_INT)
            width = 4
            do i = 1, size(value)
                value(i) = decode_attribute_i32(ncid, varid, attribute_id, i, width)
            end do
        case (NF90_BYTE)
            do i = 1, size(value)
                value(i) = attribute_byte(ncid, varid, attribute_id, i)
                if (value(i) > 127) value(i) = value(i) - 256
            end do
        case (NF90_SHORT)
            width = 2
            do i = 1, size(value)
                value(i) = decode_attribute_i32(ncid, varid, attribute_id, i, width)
            end do
        case default
            code = NF90_EINVAL
        end select
    end function get_att_i32_rank1

    integer function get_att_r64_scalar(ncid, varid, name, value) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: name
        real(real64), intent(out) :: value
        real(real64) :: temporary(1)

        code = get_att_r64_rank1(ncid, varid, name, temporary)
        if (code == NF90_NOERR) value = temporary(1)
    end function get_att_r64_scalar

    integer function get_att_r64_rank1(ncid, varid, name, value) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: name
        real(real64), intent(out) :: value(:)
        integer(int8) :: bytes4(4), bytes8(8)
        integer(int32) :: bits32
        integer(int64) :: bits64
        real(real32) :: value32
        integer :: attribute_id, i, j, offset

        code = find_attribute(ncid, varid, name, attribute_id)
        if (code /= NF90_NOERR) return
        if (size(value) /= attribute_length(ncid, varid, attribute_id)) then
            code = NF90_EINVAL
            return
        end if
        select case (attribute_type(ncid, varid, attribute_id))
        case (NF90_DOUBLE)
            do i = 1, size(value)
                offset = 8*(i - 1)
                do j = 1, 8
                    bytes8(j) = int(attribute_byte(ncid, varid, attribute_id, offset + j), int8)
                end do
                bits64 = decode_be_i64(bytes8)
                value(i) = transfer(bits64, value(i))
            end do
        case (NF90_FLOAT)
            do i = 1, size(value)
                offset = 4*(i - 1)
                do j = 1, 4
                    bytes4(j) = int(attribute_byte(ncid, varid, attribute_id, offset + j), int8)
                end do
                bits32 = decode_be_i32(bytes4)
                value32 = transfer(bits32, value32)
                value(i) = real(value32, real64)
            end do
        case (NF90_INT, NF90_SHORT, NF90_BYTE)
            do i = 1, size(value)
                value(i) = real(decode_attribute_i32(ncid, varid, attribute_id, i, &
                    type_width_compat(attribute_type(ncid, varid, attribute_id))), real64)
            end do
        case default
            code = NF90_EINVAL
        end select
    end function get_att_r64_rank1

    integer function put_att_text(ncid, varid, name, value) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: name, value
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%put_attribute_text(varid, name, value, status)
        code = finish_status(status)
    end function put_att_text

    integer function put_att_i32_scalar(ncid, varid, name, value) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: name
        integer(int32), intent(in) :: value

        code = put_att_i32_rank1(ncid, varid, name, [value])
    end function put_att_i32_scalar

    integer function put_att_i32_rank1(ncid, varid, name, values) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: name
        integer(int32), intent(in) :: values(:)
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%put_attribute_i32(varid, name, values, status)
        code = finish_status(status)
    end function put_att_i32_rank1

    integer function put_att_r64_scalar(ncid, varid, name, value) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: value

        code = put_att_r64_rank1(ncid, varid, name, [value])
    end function put_att_r64_scalar

    integer function put_att_r64_rank1(ncid, varid, name, values) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: values(:)
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%put_attribute_r64(varid, name, values, status)
        code = finish_status(status)
    end function put_att_r64_rank1

    integer function get_i32_scalar(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        integer(int32), intent(out) :: value
        type(fortio_status_t) :: status

        if (.not. prepare_get(ncid, varid, status)) then
            code = status%code
            return
        end if
        call files(ncid)%read_i32_scalar(files(ncid)%variables(varid)%name, value, status)
        code = finish_status(status)
    end function get_i32_scalar

    integer function get_char_scalar(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(out) :: value
        type(fortio_status_t) :: status

        if (.not. prepare_get(ncid, varid, status)) then
            code = status%code
            return
        end if
        call files(ncid)%read_char_scalar(files(ncid)%variables(varid)%name, value, status)
        code = finish_status(status)
    end function get_char_scalar

    integer function get_char_rank1(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(out) :: value(:)
        type(fortio_status_t) :: status

        if (.not. prepare_get(ncid, varid, status)) then
            code = status%code
            return
        end if
        call files(ncid)%read_char_1(files(ncid)%variables(varid)%name, value, status)
        code = finish_status(status)
    end function get_char_rank1

    integer function get_i32_rank1(ncid, varid, value, start, count) result(code)
        integer, intent(in) :: ncid, varid
        integer(int32), intent(out) :: value(:)
        integer, intent(in), optional :: start(:), count(:)
        integer(int32), allocatable :: temporary(:)
        type(fortio_status_t) :: status
        integer :: first(1), last(1)

        if (.not. prepare_get(ncid, varid, status)) then
            code = status%code
            return
        end if
        call files(ncid)%read_i32_1(files(ncid)%variables(varid)%name, temporary, status)
        if (status%ok()) call resolve_slice(shape(temporary), shape(value), start, count, &
                                            first, last, status)
        if (status%ok()) value = temporary(first(1):last(1))
        code = finish_status(status)
    end function get_i32_rank1

    integer function get_r64_scalar(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        real(real64), intent(out) :: value
        type(fortio_status_t) :: status

        if (.not. prepare_get(ncid, varid, status)) then
            code = status%code
            return
        end if
        call files(ncid)%read_r64_scalar(files(ncid)%variables(varid)%name, value, status)
        code = finish_status(status)
    end function get_r64_scalar

    integer function get_r64_rank1(ncid, varid, value, start, count) result(code)
        integer, intent(in) :: ncid, varid
        real(real64), intent(out) :: value(:)
        integer, intent(in), optional :: start(:), count(:)
        real(real64), allocatable :: temporary(:)
        type(fortio_status_t) :: status
        integer :: first(1), last(1)

        if (.not. prepare_get(ncid, varid, status)) then
            code = status%code
            return
        end if
        call files(ncid)%read_r64_1(files(ncid)%variables(varid)%name, temporary, status)
        if (status%ok()) call resolve_slice(shape(temporary), shape(value), start, count, &
                                            first, last, status)
        if (status%ok()) value = temporary(first(1):last(1))
        code = finish_status(status)
    end function get_r64_rank1

    integer function get_r64_rank2(ncid, varid, value, start, count) result(code)
        integer, intent(in) :: ncid, varid
        real(real64), intent(out) :: value(:, :)
        integer, intent(in), optional :: start(:), count(:)
        real(real64), allocatable :: temporary(:, :)
        type(fortio_status_t) :: status
        integer :: first(2), last(2)

        if (.not. prepare_get(ncid, varid, status)) then
            code = status%code
            return
        end if
        call files(ncid)%read_r64_2(files(ncid)%variables(varid)%name, temporary, status)
        if (status%ok()) call resolve_slice(shape(temporary), shape(value), start, count, &
                                            first, last, status)
        if (status%ok()) value = temporary(first(1):last(1), first(2):last(2))
        code = finish_status(status)
    end function get_r64_rank2

    integer function get_r64_rank3(ncid, varid, value, start, count) result(code)
        integer, intent(in) :: ncid, varid
        real(real64), intent(out) :: value(:, :, :)
        integer, intent(in), optional :: start(:), count(:)
        real(real64), allocatable :: temporary(:, :, :)
        type(fortio_status_t) :: status
        integer :: first(3), last(3)

        if (.not. prepare_get(ncid, varid, status)) then
            code = status%code
            return
        end if
        call files(ncid)%read_r64_3(files(ncid)%variables(varid)%name, temporary, status)
        if (status%ok()) call resolve_slice(shape(temporary), shape(value), start, count, &
                                            first, last, status)
        if (status%ok()) value = temporary(first(1):last(1), first(2):last(2), &
                                            first(3):last(3))
        code = finish_status(status)
    end function get_r64_rank3

    integer function get_r64_rank4(ncid, varid, value, start, count) result(code)
        integer, intent(in) :: ncid, varid
        real(real64), intent(out) :: value(:, :, :, :)
        integer, intent(in), optional :: start(:), count(:)
        real(real64), allocatable :: temporary(:, :, :, :)
        type(fortio_status_t) :: status
        integer :: first(4), last(4)

        if (.not. prepare_get(ncid, varid, status)) then
            code = status%code
            return
        end if
        call files(ncid)%read_r64_4(files(ncid)%variables(varid)%name, temporary, status)
        if (status%ok()) call resolve_slice(shape(temporary), shape(value), start, count, &
                                            first, last, status)
        if (status%ok()) value = temporary(first(1):last(1), first(2):last(2), &
                                            first(3):last(3), first(4):last(4))
        code = finish_status(status)
    end function get_r64_rank4

    integer function put_i32_scalar(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        integer(int32), intent(in) :: value
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%put_i32_scalar(varid, value, status)
        code = finish_status(status)
    end function put_i32_scalar

    integer function put_char_scalar(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: value
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%put_char_scalar(varid, value, status)
        code = finish_status(status)
    end function put_char_scalar

    integer function put_char_rank1(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: value(:)
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%put_char_1(varid, value, status)
        code = finish_status(status)
    end function put_char_rank1

    integer function put_i32_rank1(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        integer(int32), intent(in) :: value(:)
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%put_i32_1(varid, value, status)
        code = finish_status(status)
    end function put_i32_rank1

    integer function put_r64_scalar(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        real(real64), intent(in) :: value
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%put_r64_scalar(varid, value, status)
        code = finish_status(status)
    end function put_r64_scalar

    integer function put_r64_rank1(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        real(real64), intent(in) :: value(:)
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%put_r64_1(varid, value, status)
        code = finish_status(status)
    end function put_r64_rank1

    integer function put_r64_rank2(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        real(real64), intent(in) :: value(:, :)
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%put_r64_2(varid, value, status)
        code = finish_status(status)
    end function put_r64_rank2

    integer function put_r64_rank3(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        real(real64), intent(in) :: value(:, :, :)
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%put_r64_3(varid, value, status)
        code = finish_status(status)
    end function put_r64_rank3

    function nf90_strerror(code) result(message)
        integer, intent(in) :: code
        character(len=512) :: message

        select case (code)
        case (NF90_NOERR)
            message = "No error"
        case (NF90_EBADID)
            message = "Invalid file ID"
        case (NF90_ENOTVAR)
            message = "Variable not found"
        case (NF90_ENOTSUPPORT)
            message = "Operation or format feature is not supported"
        case (NF90_EEXIST)
            message = "File already exists"
        case default
            if (len_trim(last_error) > 0) then
                message = trim(last_error)
            else
                write(message, '("fortio error ", I0)') code
            end if
        end select
    end function nf90_strerror

    integer function first_free_slot()
        integer :: i

        first_free_slot = 0
        do i = 1, MAX_OPEN_FILES
            if (.not. in_use(i)) then
                first_free_slot = i
                return
            end if
        end do
    end function first_free_slot

    integer function writer_variable_id(ncid, name) result(varid)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        integer :: i

        varid = -1
        do i = 1, size(writers(ncid)%variables)
            if (writers(ncid)%variables(i)%name == trim(name)) then
                varid = i - 1
                return
            end if
        end do
    end function writer_variable_id

    logical function valid_id(ncid)
        integer, intent(in) :: ncid

        valid_id = ncid >= 1
        if (valid_id) valid_id = ncid <= MAX_OPEN_FILES
        if (valid_id) valid_id = in_use(ncid)
    end function valid_id

    logical function valid_writer(ncid)
        integer, intent(in) :: ncid

        valid_writer = valid_id(ncid)
        if (valid_writer) valid_writer = writing(ncid)
    end function valid_writer

    logical function valid_reader(ncid)
        integer, intent(in) :: ncid

        valid_reader = valid_id(ncid)
        if (valid_reader) valid_reader = .not. writing(ncid)
    end function valid_reader

    logical function prepare_get(ncid, varid, status)
        integer, intent(in) :: ncid, varid
        type(fortio_status_t), intent(out) :: status

        call status%clear()
        prepare_get = valid_id(ncid)
        if (.not. prepare_get) then
            call status%set(NF90_EBADID, "invalid file ID")
            return
        end if
        prepare_get = varid >= 1
        if (prepare_get) prepare_get = varid <= size(files(ncid)%variables)
        if (.not. prepare_get) call status%set(NF90_ENOTVAR, "invalid variable ID")
    end function prepare_get

    subroutine resolve_slice(source_shape, destination_shape, start, count, first, last, &
                             status)
        integer, intent(in) :: source_shape(:), destination_shape(:)
        integer, intent(in), optional :: start(:), count(:)
        integer, intent(out) :: first(:), last(:)
        type(fortio_status_t), intent(inout) :: status
        integer :: i

        call status%clear()
        if (size(source_shape) /= size(destination_shape)) then
            call status%set(NF90_EINVAL, "destination rank does not match variable")
            return
        end if
        first = 1
        if (present(start)) then
            if (size(start) /= size(source_shape)) then
                call status%set(NF90_EINVAL, "start rank does not match variable")
                return
            end if
            first = start
        end if
        if (present(count)) then
            if (size(count) /= size(source_shape)) then
                call status%set(NF90_EINVAL, "count rank does not match variable")
                return
            end if
            if (any(count /= destination_shape)) then
                call status%set(NF90_EINVAL, "count does not match destination shape")
                return
            end if
        else
            if (any(destination_shape /= source_shape)) then
                call status%set(NF90_EINVAL, &
                                "partial destination requires an explicit count")
                return
            end if
        end if
        last = first + destination_shape - 1
        do i = 1, size(first)
            if (first(i) < 1 .or. last(i) > source_shape(i)) then
                call status%set(NF90_EINVAL, "requested hyperslab is outside the variable")
                return
            end if
        end do
    end subroutine resolve_slice

    integer function find_attribute(ncid, varid, name, attribute_id) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: name
        integer, intent(out) :: attribute_id
        integer :: i

        attribute_id = 0
        if (.not. valid_reader(ncid)) then
            code = NF90_EBADID
            return
        end if
        if (varid == NF90_GLOBAL) then
            do i = 1, size(files(ncid)%global_attributes)
                if (files(ncid)%global_attributes(i)%name == trim(name)) then
                    attribute_id = i
                    code = NF90_NOERR
                    return
                end if
            end do
        else
            if (varid < 1 .or. varid > size(files(ncid)%variables)) then
                code = NF90_ENOTVAR
                return
            end if
            do i = 1, size(files(ncid)%variables(varid)%attributes)
                if (files(ncid)%variables(varid)%attributes(i)%name == trim(name)) then
                    attribute_id = i
                    code = NF90_NOERR
                    return
                end if
            end do
        end if
        code = NF90_ENOTATT
    end function find_attribute

    integer function attribute_type(ncid, varid, attribute_id) result(type_code)
        integer, intent(in) :: ncid, varid, attribute_id

        if (varid == NF90_GLOBAL) then
            type_code = files(ncid)%global_attributes(attribute_id)%type_code
        else
            type_code = files(ncid)%variables(varid)%attributes(attribute_id)%type_code
        end if
    end function attribute_type

    integer function attribute_length(ncid, varid, attribute_id) result(element_count)
        integer, intent(in) :: ncid, varid, attribute_id

        if (varid == NF90_GLOBAL) then
            element_count = files(ncid)%global_attributes(attribute_id)%element_count
        else
            element_count = files(ncid)%variables(varid)%attributes(attribute_id)%element_count
        end if
    end function attribute_length

    integer function attribute_byte(ncid, varid, attribute_id, position) result(value)
        integer, intent(in) :: ncid, varid, attribute_id, position

        if (varid == NF90_GLOBAL) then
            value = iand(int(files(ncid)%global_attributes(attribute_id)%bytes(position)), 255)
        else
            value = iand(int(files(ncid)%variables(varid)%attributes(attribute_id)%bytes(position)), 255)
        end if
    end function attribute_byte

    integer(int32) function decode_attribute_i32(ncid, varid, attribute_id, element, &
                                                 width) result(value)
        integer, intent(in) :: ncid, varid, attribute_id, element, width
        integer(int8) :: bytes4(4)
        integer :: i, offset

        bytes4 = 0_int8
        offset = width*(element - 1)
        do i = 1, width
            bytes4(4 - width + i) = int(attribute_byte(ncid, varid, attribute_id, &
                                                       offset + i), int8)
        end do
        value = decode_be_i32(bytes4)
        if (width == 2 .and. value >= 32768) value = value - 65536
        if (width == 1 .and. value >= 128) value = value - 256
    end function decode_attribute_i32

    pure integer function type_width_compat(type_code) result(width)
        integer, intent(in) :: type_code

        select case (type_code)
        case (NF90_BYTE, NF90_CHAR)
            width = 1
        case (NF90_SHORT)
            width = 2
        case (NF90_INT, NF90_FLOAT)
            width = 4
        case (NF90_DOUBLE)
            width = 8
        case default
            width = 0
        end select
    end function type_width_compat

    integer function finish_status(status)
        type(fortio_status_t), intent(in) :: status

        finish_status = status%code
        if (.not. status%ok()) last_error = status%message
    end function finish_status

end module netcdf
