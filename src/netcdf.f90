module netcdf
    use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real32, real64
    use fortio_bytes, only: decode_be_i32, decode_be_i64
    use fortio_netcdf_classic, only: classic_file_t, classic_dimension_t, &
        classic_attribute_t, classic_variable_t, NC_BYTE, NC_CHAR, NC_SHORT, &
        NC_INT, NC_FLOAT, NC_DOUBLE
    use fortio_hdf5_reader, only: hdf5_file_t, hdf5_attribute_t
    use fortio_netcdf_writer, only: classic_writer_t
    use fortio_status, only: fortio_status_t, FORTIO_SUCCESS, FORTIO_ENOTFOUND, &
        FORTIO_ESTATE, FORTIO_ENOTSUP, FORTIO_ESHAPE, FORTIO_EEXIST, &
        FORTIO_EIO, FORTIO_EFORMAT, FORTIO_ETYPE
    use fortio_posix, only: handle_table_lock, handle_table_unlock
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
    integer, parameter, public :: NF90_CLASSIC_MODEL = 256
    integer, parameter, public :: NF90_NETCDF4 = 4096
    integer, parameter, public :: NF90_UNLIMITED = 0
    integer, parameter, public :: NF90_GLOBAL = 0
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
    type :: netcdf4_file_t
        type(hdf5_file_t) :: hdf5
        type(classic_dimension_t), allocatable :: dimensions(:)
        type(classic_attribute_t), allocatable :: global_attributes(:)
        type(classic_variable_t), allocatable :: variables(:)
    end type netcdf4_file_t
    type(classic_file_t), save :: files(MAX_OPEN_FILES)
    type(netcdf4_file_t), save :: netcdf4_files(MAX_OPEN_FILES)
    type(classic_writer_t), save :: writers(MAX_OPEN_FILES)
    logical, save :: in_use(MAX_OPEN_FILES) = .false.
    logical, save :: writing(MAX_OPEN_FILES) = .false.
    logical, save :: writing_netcdf4(MAX_OPEN_FILES) = .false.
    logical, save :: netcdf4_reading(MAX_OPEN_FILES) = .false.

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
        module procedure put_i8_rank1
        module procedure put_i32_scalar, put_i32_rank1, put_i32_rank2
        module procedure put_r64_scalar, put_r64_rank1, put_r64_rank2, put_r64_rank3
        module procedure put_r64_rank4
    end interface nf90_put_var

    interface nf90_get_att
        module procedure get_att_text
        module procedure get_att_i32_scalar, get_att_i32_rank1
        module procedure get_att_r64_scalar, get_att_r64_rank1
    end interface nf90_get_att

    interface nf90_put_att
        module procedure put_att_text
        module procedure put_att_i32_scalar, put_att_i32_rank1
        module procedure put_att_i64_scalar
        module procedure put_att_r64_scalar, put_att_r64_rank1
    end interface nf90_put_att

    public :: nf90_open, nf90_create, nf90_close, nf90_def_dim, nf90_def_var
    public :: nf90_enddef, nf90_inq_varid, nf90_get_var, nf90_put_var, nf90_strerror
    public :: nf90_inq_dimid, nf90_inquire_dimension, nf90_inquire_variable
    public :: nf90_inquire_attribute, nf90_get_att
    public :: nf90_put_att, nf90_redef, nf90_def_grp, nf90_inq_ncid
    public :: nf90_def_var_deflate

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
            return
        end if
        slot = claim_free_slot()
        if (slot == 0) then
            code = NF90_EBADID
            return
        end if
        ! A slot can be reused after either reader backend.  Set the backend
        ! state explicitly before opening so a stale NetCDF-4 flag cannot
        ! route a classic file through the other metadata store.
        writing(slot) = .false.
        writing_netcdf4(slot) = .false.
        netcdf4_reading(slot) = .false.
        call files(slot)%open(path, status)
        if (.not. status%ok()) then
            call netcdf4_files(slot)%hdf5%open(path, status)
            if (status%ok()) call load_netcdf4_metadata(netcdf4_files(slot), status)
            if (.not. status%ok()) then
                code = status%code
                call release_slot(slot)
                return
            end if
            netcdf4_reading(slot) = .true.
        end if
        code = NF90_NOERR
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
            return
        end if
        slot = claim_free_slot()
        if (slot == 0) then
            code = NF90_EBADID
            return
        end if
        call writers(slot)%create(path, status)
        code = status%code
        if (.not. status%ok()) then
            call release_slot(slot)
            return
        end if
        writing(slot) = .true.
        writing_netcdf4(slot) = iand(cmode, NF90_NETCDF4) /= 0
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
            if (writing_netcdf4(ncid)) then
                call writers(ncid)%close_netcdf4(status)
            else
                call writers(ncid)%close(status)
            end if
        else if (netcdf4_reading(ncid)) then
            call netcdf4_files(ncid)%hdf5%close(status)
            if (allocated(netcdf4_files(ncid)%dimensions)) &
                deallocate(netcdf4_files(ncid)%dimensions)
            if (allocated(netcdf4_files(ncid)%variables)) &
                deallocate(netcdf4_files(ncid)%variables)
        else
            call files(ncid)%close(status)
        end if
        writing(ncid) = .false.
        writing_netcdf4(ncid) = .false.
        netcdf4_reading(ncid) = .false.
        call release_slot(ncid)
        code = status%code
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
        if (code == NF90_NOERR) dimid = dimid + 1
    end function nf90_def_dim

    integer function def_var_scalar(ncid, name, type_code, varid) result(code)
        integer, intent(in) :: ncid, type_code
        character(len=*), intent(in) :: name
        integer, intent(out) :: varid
        integer :: scalar_dimension_ids(0)

        code = def_var_array(ncid, name, type_code, scalar_dimension_ids, varid)
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
        call writers(ncid)%define_variable(name, type_code, dimension_ids - 1, &
            varid, status)
        code = finish_status(status)
        if (code == NF90_NOERR) varid = varid + 1
    end function def_var_array

    integer function nf90_def_var_deflate(ncid, varid, shuffle, deflate, &
            deflate_level) result(code)
        integer, intent(in) :: ncid, varid, shuffle, deflate, deflate_level
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        if (varid < 1 .or. varid > size(writers(ncid)%variables)) then
            code = NF90_ENOTVAR
            return
        end if
        if (shuffle < 0 .or. shuffle > 1 .or. deflate < 0 .or. deflate > 1) then
            code = NF90_EINVAL
            return
        end if
        if (deflate_level < 0 .or. deflate_level > 9) then
            code = NF90_EINVAL
            return
        end if
        if (.not. writing_netcdf4(ncid)) then
            code = NF90_ENOTSUPPORT
            return
        end if
        call writers(ncid)%set_deflate(varid - 1, shuffle == 1, deflate == 1, &
            deflate_level, status)
        code = finish_status(status)
    end function nf90_def_var_deflate

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
            if (netcdf4_reading(ncid)) then
                varid = netcdf4_variable_id(netcdf4_files(ncid), name)
            else
                varid = files(ncid)%variable_id(name)
            end if
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
        if (netcdf4_reading(ncid)) then
            dimid = netcdf4_dimension_id(netcdf4_files(ncid), name)
        else
            dimid = files(ncid)%dimension_id(name)
        end if
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
        if (netcdf4_reading(ncid)) then
            if (dimid < 1 .or. dimid > size(netcdf4_files(ncid)%dimensions)) then
                code = NF90_EBADDIM
                return
            end if
            if (present(name)) name = netcdf4_files(ncid)%dimensions(dimid)%name
            if (present(len)) len = int(netcdf4_files(ncid)%dimensions(dimid)%length)
            code = NF90_NOERR
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
        if (netcdf4_reading(ncid)) then
            code = inquire_netcdf4_variable(netcdf4_files(ncid), varid, name, xtype, &
                ndims, dimids, natts)
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
        integer :: writer_varid

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        writer_varid = compatibility_writer_varid(varid)
        call writers(ncid)%put_attribute_text(writer_varid, name, value, status)
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
        integer :: writer_varid

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        writer_varid = compatibility_writer_varid(varid)
        call writers(ncid)%put_attribute_i32(writer_varid, name, values, status)
        code = finish_status(status)
    end function put_att_i32_rank1

    integer function put_att_i64_scalar(ncid, varid, name, value) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: value
        type(fortio_status_t) :: status
        integer :: writer_varid

        if (value < -9007199254740992_int64 .or. value > 9007199254740992_int64) then
            code = NF90_EINVAL
            return
        end if
        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        writer_varid = compatibility_writer_varid(varid)
        call writers(ncid)%put_attribute_r64(writer_varid, name, &
            [real(value, real64)], status)
        code = finish_status(status)
    end function put_att_i64_scalar

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
        integer :: writer_varid

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        writer_varid = compatibility_writer_varid(varid)
        call writers(ncid)%put_attribute_r64(writer_varid, name, values, status)
        code = finish_status(status)
    end function put_att_r64_rank1

    pure integer function compatibility_writer_varid(varid) result(writer_varid)
        integer, intent(in) :: varid

        writer_varid = varid - 1
        if (varid == NF90_GLOBAL) writer_varid = -1
    end function compatibility_writer_varid

    integer function get_i32_scalar(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        integer(int32), intent(out) :: value
        type(fortio_status_t) :: status

        if (.not. prepare_get(ncid, varid, status)) then
            code = status%code
            return
        end if
        if (netcdf4_reading(ncid)) then
            call netcdf4_files(ncid)%hdf5%read_i32_scalar( &
                netcdf4_files(ncid)%variables(varid)%name, value, status)
        else
            call files(ncid)%read_i32_scalar(files(ncid)%variables(varid)%name, value, status)
        end if
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
        if (netcdf4_reading(ncid)) then
            call netcdf4_files(ncid)%hdf5%read_i32_1( &
                netcdf4_files(ncid)%variables(varid)%name, temporary, status)
        else
            call files(ncid)%read_i32_1(files(ncid)%variables(varid)%name, temporary, status)
        end if
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
        if (netcdf4_reading(ncid)) then
            call netcdf4_files(ncid)%hdf5%read_r64_scalar( &
                netcdf4_files(ncid)%variables(varid)%name, value, status)
        else
            call files(ncid)%read_r64_scalar(files(ncid)%variables(varid)%name, value, status)
        end if
        code = finish_status(status)
    end function get_r64_scalar

    integer function get_r64_rank1(ncid, varid, value, start, count, map) result(code)
        integer, intent(in) :: ncid, varid
        real(real64), intent(out) :: value(:)
        integer, intent(in), optional :: start(:), count(:), map(:)
        real(real64), allocatable :: temporary(:), mapped(:)
        type(fortio_status_t) :: status
        integer :: first(1), last(1)

        if (.not. prepare_get(ncid, varid, status)) then
            code = status%code
            return
        end if
        if (netcdf4_reading(ncid)) then
            call netcdf4_files(ncid)%hdf5%read_r64_1( &
                netcdf4_files(ncid)%variables(varid)%name, temporary, status)
        else
            call files(ncid)%read_r64_1(files(ncid)%variables(varid)%name, temporary, status)
        end if
        if (status%ok()) call resolve_slice(shape(temporary), shape(value), start, count, &
            first, last, status, map)
        if (status%ok()) then
            if (present(map)) then
                call map_r64_values(temporary(first(1):last(1)), &
                    last - first + 1, map, size(value), mapped, status)
                if (status%ok()) value = mapped
            else
                value = temporary(first(1):last(1))
            end if
        end if
        code = finish_status(status)
    end function get_r64_rank1

    integer function get_r64_rank2(ncid, varid, value, start, count, map) result(code)
        integer, intent(in) :: ncid, varid
        real(real64), intent(out) :: value(:, :)
        integer, intent(in), optional :: start(:), count(:), map(:)
        real(real64), allocatable :: temporary(:, :), selected(:, :), mapped(:)
        type(fortio_status_t) :: status
        integer :: first(2), last(2)

        if (.not. prepare_get(ncid, varid, status)) then
            code = status%code
            return
        end if
        if (.not. netcdf4_reading(ncid)) then
            if (.not. present(start) .and. .not. present(count) .and. .not. present(map)) then
                call files(ncid)%read_r64_2_into(files(ncid)%variables(varid)%name, &
                    value, status)
                code = finish_status(status)
                return
            end if
        end if
        if (netcdf4_reading(ncid)) then
            call netcdf4_files(ncid)%hdf5%read_r64_2( &
                netcdf4_files(ncid)%variables(varid)%name, temporary, status)
        else
            call files(ncid)%read_r64_2(files(ncid)%variables(varid)%name, temporary, status)
        end if
        if (status%ok()) call resolve_slice(shape(temporary), shape(value), start, count, &
            first, last, status, map)
        if (status%ok()) then
            if (present(map)) then
                selected = temporary(first(1):last(1), first(2):last(2))
                call map_r64_values(reshape(selected, [size(selected)]), &
                    shape(selected), map, size(value), mapped, status)
                if (status%ok()) value = reshape(mapped, shape(value))
            else
                value = temporary(first(1):last(1), first(2):last(2))
            end if
        end if
        code = finish_status(status)
    end function get_r64_rank2

    integer function get_r64_rank3(ncid, varid, value, start, count, map) result(code)
        integer, intent(in) :: ncid, varid
        real(real64), intent(out) :: value(:, :, :)
        integer, intent(in), optional :: start(:), count(:), map(:)
        real(real64), allocatable :: temporary(:, :, :), selected(:, :, :), mapped(:)
        type(fortio_status_t) :: status
        integer :: first(3), last(3)

        if (.not. prepare_get(ncid, varid, status)) then
            code = status%code
            return
        end if
        if (netcdf4_reading(ncid)) then
            call netcdf4_files(ncid)%hdf5%read_r64_3( &
                netcdf4_files(ncid)%variables(varid)%name, temporary, status)
        else
            call files(ncid)%read_r64_3(files(ncid)%variables(varid)%name, temporary, status)
        end if
        if (status%ok()) call resolve_slice(shape(temporary), shape(value), start, count, &
            first, last, status, map)
        if (status%ok()) then
            if (present(map)) then
                selected = temporary(first(1):last(1), first(2):last(2), &
                    first(3):last(3))
                call map_r64_values(reshape(selected, [size(selected)]), &
                    shape(selected), map, size(value), mapped, status)
                if (status%ok()) value = reshape(mapped, shape(value))
            else
                value = temporary(first(1):last(1), first(2):last(2), &
                    first(3):last(3))
            end if
        end if
        code = finish_status(status)
    end function get_r64_rank3

    integer function get_r64_rank4(ncid, varid, value, start, count, map) result(code)
        integer, intent(in) :: ncid, varid
        real(real64), intent(out) :: value(:, :, :, :)
        integer, intent(in), optional :: start(:), count(:), map(:)
        real(real64), allocatable :: temporary(:, :, :, :), selected(:, :, :, :)
        real(real64), allocatable :: mapped(:)
        type(fortio_status_t) :: status
        integer :: first(4), last(4)

        if (.not. prepare_get(ncid, varid, status)) then
            code = status%code
            return
        end if
        if (netcdf4_reading(ncid)) then
            call netcdf4_files(ncid)%hdf5%read_r64_4( &
                netcdf4_files(ncid)%variables(varid)%name, temporary, status)
        else
            call files(ncid)%read_r64_4(files(ncid)%variables(varid)%name, temporary, status)
        end if
        if (status%ok()) call resolve_slice(shape(temporary), shape(value), start, count, &
            first, last, status, map)
        if (status%ok()) then
            if (present(map)) then
                selected = temporary(first(1):last(1), first(2):last(2), &
                    first(3):last(3), first(4):last(4))
                call map_r64_values(reshape(selected, [size(selected)]), &
                    shape(selected), map, size(value), mapped, status)
                if (status%ok()) value = reshape(mapped, shape(value))
            else
                value = temporary(first(1):last(1), first(2):last(2), &
                    first(3):last(3), first(4):last(4))
            end if
        end if
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
        call writers(ncid)%put_i32_scalar(compatibility_writer_varid(varid), &
            value, status)
        code = finish_status(status)
    end function put_i32_scalar

    integer function put_i8_rank1(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        integer(int8), intent(in) :: value(:)
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%put_i8_1(compatibility_writer_varid(varid), value, status)
        code = finish_status(status)
    end function put_i8_rank1

    integer function put_char_scalar(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        character(len=*), intent(in) :: value
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%put_char_scalar(compatibility_writer_varid(varid), &
            value, status)
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
        call writers(ncid)%put_char_1(compatibility_writer_varid(varid), value, status)
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
        call writers(ncid)%put_i32_1(compatibility_writer_varid(varid), value, status)
        code = finish_status(status)
    end function put_i32_rank1

    integer function put_i32_rank2(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        integer(int32), intent(in) :: value(:, :)
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%put_i32_2(compatibility_writer_varid(varid), value, status)
        code = finish_status(status)
    end function put_i32_rank2

    integer function put_r64_scalar(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        real(real64), intent(in) :: value
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%put_r64_scalar(compatibility_writer_varid(varid), &
            value, status)
        code = finish_status(status)
    end function put_r64_scalar

    integer function put_r64_rank1(ncid, varid, value, start, count) result(code)
        integer, intent(in) :: ncid, varid
        real(real64), intent(in) :: value(:)
        integer, intent(in), optional :: start(:), count(:)
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        if (present(start) .neqv. present(count)) then
            code = NF90_EINVAL
            return
        end if
        if (present(start)) then
            call writers(ncid)%put_r64_slice(compatibility_writer_varid(varid), &
                value, start, count, status)
        else
            call writers(ncid)%put_r64_1(compatibility_writer_varid(varid), &
                value, status)
        end if
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
        call writers(ncid)%put_r64_2(compatibility_writer_varid(varid), value, status)
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
        call writers(ncid)%put_r64_3(compatibility_writer_varid(varid), value, status)
        code = finish_status(status)
    end function put_r64_rank3

    integer function put_r64_rank4(ncid, varid, value) result(code)
        integer, intent(in) :: ncid, varid
        real(real64), intent(in) :: value(:, :, :, :)
        type(fortio_status_t) :: status

        if (.not. valid_writer(ncid)) then
            code = NF90_EBADID
            return
        end if
        call writers(ncid)%put_r64_4(compatibility_writer_varid(varid), value, status)
        code = finish_status(status)
    end function put_r64_rank4

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
        case (FORTIO_EIO)
            message = "I/O error"
        case (FORTIO_EFORMAT)
            message = "Invalid or unsupported file format"
        case (FORTIO_ETYPE)
            message = "Incompatible data type"
        case default
            write(message, '("fortio error ", I0)') code
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

    integer function claim_free_slot() result(slot)
        call handle_table_lock()
        slot = first_free_slot()
        if (slot /= 0) in_use(slot) = .true.
        call handle_table_unlock()
    end function claim_free_slot

    subroutine release_slot(slot)
        integer, intent(in) :: slot

        call handle_table_lock()
        in_use(slot) = .false.
        call handle_table_unlock()
    end subroutine release_slot

    integer function writer_variable_id(ncid, name) result(varid)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: name
        integer :: i

        varid = -1
        do i = 1, size(writers(ncid)%variables)
            if (writers(ncid)%variables(i)%name == trim(name)) then
                varid = i
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
        if (prepare_get) then
            if (netcdf4_reading(ncid)) then
                prepare_get = varid <= size(netcdf4_files(ncid)%variables)
            else
                prepare_get = varid <= size(files(ncid)%variables)
            end if
        end if
        if (.not. prepare_get) call status%set(NF90_ENOTVAR, "invalid variable ID")
    end function prepare_get

    subroutine load_netcdf4_metadata(file, status)
        type(netcdf4_file_t), intent(inout) :: file
        type(fortio_status_t), intent(inout) :: status
        character(len=:), allocatable :: names(:)
        logical, allocatable :: group_flags(:)
        type(hdf5_attribute_t), allocatable :: attributes(:)
        integer(int64), allocatable :: dimensions(:)
        logical :: is_group
        integer :: i, dimension_id, maximum_dimension_id
        integer :: type_class, element_size

        call status%clear()
        call file%hdf5%list_children("", names, group_flags, status)
        if (.not. status%ok()) return
        call file%hdf5%get_attributes("", attributes, status)
        if (.not. status%ok()) return
        call convert_netcdf4_attributes(attributes, file%global_attributes)
        maximum_dimension_id = -1
        do i = 1, size(names)
            if (group_flags(i)) cycle
            call file%hdf5%get_attributes(names(i), attributes, status)
            if (.not. status%ok()) return
            dimension_id = netcdf4_coordinate_id(attributes, "_Netcdf4Dimid")
            maximum_dimension_id = max(maximum_dimension_id, dimension_id)
        end do
        allocate(file%dimensions(maximum_dimension_id + 1))
        allocate(file%variables(0))
        do i = 1, size(names)
            if (group_flags(i)) cycle
            call file%hdf5%get_attributes(names(i), attributes, status)
            if (.not. status%ok()) return
            dimension_id = netcdf4_coordinate_id(attributes, "_Netcdf4Dimid")
            call file%hdf5%describe(names(i), is_group, type_class, dimensions, status, &
                element_size)
            if (.not. status%ok()) return
            if (dimension_id >= 0) then
                file%dimensions(dimension_id + 1)%name = names(i)
                if (size(dimensions) /= 1) then
                    call status%set(NF90_EINVAL, "NetCDF-4 dimension scale is not rank one")
                    return
                end if
                file%dimensions(dimension_id + 1)%length = dimensions(1)
                if (netcdf4_is_coordinate_variable(attributes, names(i))) then
                    call append_netcdf4_variable(file, names(i), type_class, element_size, &
                        attributes, status, dimension_id)
                    if (.not. status%ok()) return
                end if
            else
                call append_netcdf4_variable(file, names(i), type_class, element_size, &
                    attributes, status)
                if (.not. status%ok()) return
            end if
        end do
    end subroutine load_netcdf4_metadata

    integer function netcdf4_coordinate_id(attributes, name) result(id)
        type(hdf5_attribute_t), intent(in) :: attributes(:)
        character(len=*), intent(in) :: name
        integer :: i

        id = -1
        do i = 1, size(attributes)
            if (attributes(i)%name /= name) cycle
            if (.not. allocated(attributes(i)%values_i32)) return
            if (size(attributes(i)%values_i32) /= 1) return
            id = int(attributes(i)%values_i32(1))
            return
        end do
    end function netcdf4_coordinate_id

    logical function netcdf4_is_coordinate_variable(attributes, name)
        type(hdf5_attribute_t), intent(in) :: attributes(:)
        character(len=*), intent(in) :: name
        integer :: i

        netcdf4_is_coordinate_variable = .false.
        do i = 1, size(attributes)
            if (attributes(i)%name /= "NAME") cycle
            if (.not. allocated(attributes(i)%value_text)) return
            netcdf4_is_coordinate_variable = attributes(i)%value_text == trim(name)
            return
        end do
    end function netcdf4_is_coordinate_variable

    subroutine append_netcdf4_variable(file, name, type_class, element_size, attributes, &
            status, coordinate_dimension_id)
        type(netcdf4_file_t), intent(inout) :: file
        character(len=*), intent(in) :: name
        integer, intent(in) :: type_class, element_size
        type(hdf5_attribute_t), intent(in) :: attributes(:)
        type(fortio_status_t), intent(inout) :: status
        integer, intent(in), optional :: coordinate_dimension_id
        type(classic_variable_t), allocatable :: temporary(:)
        integer :: count, i

        count = size(file%variables)
        allocate(temporary(count + 1))
        if (count > 0) temporary(:count) = file%variables
        temporary(count + 1)%name = name
        temporary(count + 1)%type_code = netcdf4_type_code(type_class, element_size)
        if (temporary(count + 1)%type_code == 0) then
            call status%set(NF90_ENOTSUPPORT, "NetCDF-4 variable datatype is not required")
            return
        end if
        if (present(coordinate_dimension_id)) then
            temporary(count + 1)%dimension_ids = [coordinate_dimension_id]
        else
            allocate(temporary(count + 1)%dimension_ids(0))
            do i = 1, size(attributes)
                if (attributes(i)%name /= "_Netcdf4Coordinates") cycle
                if (.not. allocated(attributes(i)%values_i32)) cycle
                temporary(count + 1)%dimension_ids = int(attributes(i)%values_i32)
                exit
            end do
        end if
        call convert_netcdf4_attributes(attributes, temporary(count + 1)%attributes)
        temporary(count + 1)%attribute_count = size(temporary(count + 1)%attributes)
        call move_alloc(temporary, file%variables)
    end subroutine append_netcdf4_variable

    subroutine convert_netcdf4_attributes(source, destination)
        type(hdf5_attribute_t), intent(in) :: source(:)
        type(classic_attribute_t), allocatable, intent(out) :: destination(:)
        type(classic_attribute_t), allocatable :: temporary(:)
        type(classic_attribute_t) :: attribute
        integer :: count, i

        allocate(destination(0))
        do i = 1, size(source)
            if (is_internal_netcdf4_attribute(source(i)%name)) cycle
            call convert_netcdf4_attribute(source(i), attribute)
            if (.not. allocated(attribute%bytes)) cycle
            count = size(destination)
            allocate(temporary(count + 1))
            if (count > 0) temporary(:count) = destination
            temporary(count + 1) = attribute
            call move_alloc(temporary, destination)
        end do
    end subroutine convert_netcdf4_attributes

    logical function is_internal_netcdf4_attribute(name)
        character(len=*), intent(in) :: name

        select case (trim(name))
        case ("CLASS", "NAME", "DIMENSION_LIST", "REFERENCE_LIST", &
                "_Netcdf4Dimid", "_Netcdf4Coordinates")
            is_internal_netcdf4_attribute = .true.
        case default
            is_internal_netcdf4_attribute = .false.
        end select
    end function is_internal_netcdf4_attribute

    subroutine convert_netcdf4_attribute(source, destination)
        type(hdf5_attribute_t), intent(in) :: source
        type(classic_attribute_t), intent(out) :: destination
        integer(int64) :: bits
        integer :: i

        destination%name = source%name
        if (allocated(source%value_text)) then
            destination%type_code = NF90_CHAR
            destination%element_count = len(source%value_text)
            allocate(destination%bytes(destination%element_count))
            do i = 1, destination%element_count
                destination%bytes(i) = int(iachar(source%value_text(i:i)), int8)
            end do
        else if (allocated(source%values_i32)) then
            destination%type_code = NF90_INT
            destination%element_count = size(source%values_i32)
            allocate(destination%bytes(4*destination%element_count))
            do i = 1, destination%element_count
                call encode_be_i32(source%values_i32(i), destination%bytes(4*i - 3:4*i))
            end do
        else if (allocated(source%values_i64)) then
            destination%type_code = NF90_INT64
            destination%element_count = size(source%values_i64)
            allocate(destination%bytes(8*destination%element_count))
            do i = 1, destination%element_count
                call encode_be_i64(source%values_i64(i), destination%bytes(8*i - 7:8*i))
            end do
        else if (allocated(source%values_r64)) then
            destination%type_code = NF90_DOUBLE
            destination%element_count = size(source%values_r64)
            allocate(destination%bytes(8*destination%element_count))
            do i = 1, destination%element_count
                bits = transfer(source%values_r64(i), bits)
                call encode_be_i64(bits, destination%bytes(8*i - 7:8*i))
            end do
        end if
    end subroutine convert_netcdf4_attribute

    subroutine encode_be_i32(value, bytes)
        integer(int32), intent(in) :: value
        integer(int8), intent(out) :: bytes(4)
        integer(int64) :: unsigned
        integer :: i

        unsigned = iand(int(value, int64), int(z'ffffffff', int64))
        do i = 1, 4
            bytes(i) = int(iand(ishft(unsigned, -8*(4 - i)), 255_int64), int8)
        end do
    end subroutine encode_be_i32

    subroutine encode_be_i64(value, bytes)
        integer(int64), intent(in) :: value
        integer(int8), intent(out) :: bytes(8)
        integer :: i

        do i = 1, 8
            bytes(i) = int(iand(ishft(value, -8*(8 - i)), 255_int64), int8)
        end do
    end subroutine encode_be_i64

    pure integer function netcdf4_type_code(type_class, element_size) result(type_code)
        integer, intent(in) :: type_class, element_size

        type_code = 0
        if (type_class == 0 .and. element_size == 4) type_code = NF90_INT
        if (type_class == 1 .and. element_size == 4) type_code = NF90_FLOAT
        if (type_class == 1 .and. element_size == 8) type_code = NF90_DOUBLE
    end function netcdf4_type_code

    integer function netcdf4_variable_id(file, name) result(variable_id)
        type(netcdf4_file_t), intent(in) :: file
        character(len=*), intent(in) :: name
        integer :: i

        variable_id = 0
        do i = 1, size(file%variables)
            if (file%variables(i)%name == trim(name)) then
                variable_id = i
                return
            end if
        end do
    end function netcdf4_variable_id

    integer function netcdf4_dimension_id(file, name) result(dimension_id)
        type(netcdf4_file_t), intent(in) :: file
        character(len=*), intent(in) :: name
        integer :: i

        dimension_id = 0
        do i = 1, size(file%dimensions)
            if (file%dimensions(i)%name == trim(name)) then
                dimension_id = i
                return
            end if
        end do
    end function netcdf4_dimension_id

    integer function inquire_netcdf4_variable(file, varid, name, xtype, ndims, dimids, &
            natts) result(code)
        type(netcdf4_file_t), intent(in) :: file
        integer, intent(in) :: varid
        character(len=*), intent(out), optional :: name
        integer, intent(out), optional :: xtype, ndims, dimids(:), natts
        integer :: rank, i

        if (varid < 1 .or. varid > size(file%variables)) then
            code = NF90_ENOTVAR
            return
        end if
        rank = size(file%variables(varid)%dimension_ids)
        if (present(name)) name = file%variables(varid)%name
        if (present(xtype)) xtype = file%variables(varid)%type_code
        if (present(ndims)) ndims = rank
        if (present(natts)) natts = 0
        if (present(dimids)) then
            if (size(dimids) < rank) then
                code = NF90_EINVAL
                return
            end if
            do i = 1, rank
                dimids(i) = file%variables(varid)%dimension_ids(rank - i + 1) + 1
            end do
        end if
        code = NF90_NOERR
    end function inquire_netcdf4_variable

    subroutine resolve_slice(source_shape, destination_shape, start, count, first, last, &
            status, map)
        integer, intent(in) :: source_shape(:), destination_shape(:)
        integer, intent(in), optional :: start(:), count(:), map(:)
        integer, intent(out) :: first(:), last(:)
        type(fortio_status_t), intent(inout) :: status
        integer :: requested(size(source_shape))
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
            requested = count
            if (present(map)) then
                if (product(count) /= product(destination_shape)) then
                    call status%set(NF90_EINVAL, &
                        "mapped count does not match destination size")
                    return
                end if
            else
                if (any(count /= destination_shape)) then
                    call status%set(NF90_EINVAL, "count does not match destination shape")
                    return
                end if
            end if
        else
            requested = source_shape
            if (present(map)) then
                if (product(destination_shape) /= product(source_shape)) then
                    call status%set(NF90_EINVAL, &
                        "mapped destination size does not match variable")
                    return
                end if
            else
                if (any(destination_shape /= source_shape)) then
                    call status%set(NF90_EINVAL, &
                        "partial destination requires an explicit count")
                    return
                end if
            end if
        end if
        if (present(map)) then
            if (size(map) /= size(source_shape)) then
                call status%set(NF90_EINVAL, "map rank does not match variable")
                return
            end if
            if (any(map < 1)) then
                call status%set(NF90_ENOTSUPPORT, "non-positive map strides are unsupported")
                return
            end if
        end if
        last = first + requested - 1
        do i = 1, size(first)
            if (first(i) < 1 .or. last(i) > source_shape(i)) then
                call status%set(NF90_EINVAL, "requested hyperslab is outside the variable")
                return
            end if
        end do
    end subroutine resolve_slice

    subroutine map_r64_values(source, counts, map, output_size, output, status)
        real(real64), intent(in) :: source(:)
        integer, intent(in) :: counts(:), map(:), output_size
        real(real64), allocatable, intent(out) :: output(:)
        type(fortio_status_t), intent(inout) :: status
        integer :: coordinate, dimension, linear, output_index, remaining

        call status%clear()
        if (size(counts) /= size(map)) then
            call status%set(NF90_EINVAL, "map rank does not match count")
            return
        end if
        if (product(counts) /= size(source)) then
            call status%set(NF90_EINVAL, "mapped source size does not match count")
            return
        end if
        allocate(output(output_size), source=0.0_real64)
        do linear = 1, size(source)
            remaining = linear - 1
            output_index = 1
            do dimension = 1, size(counts)
                coordinate = mod(remaining, counts(dimension))
                remaining = remaining/counts(dimension)
                output_index = output_index + coordinate*map(dimension)
            end do
            if (output_index < 1 .or. output_index > output_size) then
                call status%set(NF90_EINVAL, "map addresses outside destination")
                return
            end if
            output(output_index) = source(linear)
        end do
    end subroutine map_r64_values

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
            if (netcdf4_reading(ncid)) then
                do i = 1, size(netcdf4_files(ncid)%global_attributes)
                    if (same_attribute_name( &
                        netcdf4_files(ncid)%global_attributes(i)%name, name)) then
                        attribute_id = i
                        code = NF90_NOERR
                        return
                    end if
                end do
            else
                do i = 1, size(files(ncid)%global_attributes)
                    if (same_attribute_name(files(ncid)%global_attributes(i)%name, &
                        name)) then
                        attribute_id = i
                        code = NF90_NOERR
                        return
                    end if
                end do
            end if
        else
            if (varid < 1) then
                code = NF90_ENOTVAR
                return
            end if
            if (netcdf4_reading(ncid)) then
                if (varid > size(netcdf4_files(ncid)%variables)) then
                    code = NF90_ENOTVAR
                    return
                end if
                do i = 1, size(netcdf4_files(ncid)%variables(varid)%attributes)
                    if (same_attribute_name( &
                        netcdf4_files(ncid)%variables(varid)%attributes(i)%name, &
                        name)) then
                        attribute_id = i
                        code = NF90_NOERR
                        return
                    end if
                end do
            else
                if (varid > size(files(ncid)%variables)) then
                    code = NF90_ENOTVAR
                    return
                end if
                do i = 1, size(files(ncid)%variables(varid)%attributes)
                    if (same_attribute_name(files(ncid)%variables(varid)%attributes(i)%name, name)) then
                        attribute_id = i
                        code = NF90_NOERR
                        return
                    end if
                end do
            end if
        end if
        code = NF90_ENOTATT
    end function find_attribute

    logical function same_attribute_name(left, right) result(equal)
        character(len=*), intent(in) :: left, right
        integer :: i, left_length, right_length

        left_length = len_trim(left)
        right_length = len_trim(right)
        equal = left_length == right_length
    end function same_attribute_name

    integer function attribute_type(ncid, varid, attribute_id) result(type_code)
        integer, intent(in) :: ncid, varid, attribute_id

        if (netcdf4_reading(ncid)) then
            if (varid == NF90_GLOBAL) then
                type_code = netcdf4_files(ncid)%global_attributes(attribute_id)%type_code
            else
                type_code = netcdf4_files(ncid)%variables(varid)%attributes(attribute_id)%type_code
            end if
        else if (varid == NF90_GLOBAL) then
            type_code = files(ncid)%global_attributes(attribute_id)%type_code
        else
            type_code = files(ncid)%variables(varid)%attributes(attribute_id)%type_code
        end if
    end function attribute_type

    integer function attribute_length(ncid, varid, attribute_id) result(element_count)
        integer, intent(in) :: ncid, varid, attribute_id

        if (netcdf4_reading(ncid)) then
            if (varid == NF90_GLOBAL) then
                element_count = &
                    netcdf4_files(ncid)%global_attributes(attribute_id)%element_count
            else
                element_count = &
                    netcdf4_files(ncid)%variables(varid)%attributes(attribute_id)%element_count
            end if
        else if (varid == NF90_GLOBAL) then
            element_count = files(ncid)%global_attributes(attribute_id)%element_count
        else
            element_count = files(ncid)%variables(varid)%attributes(attribute_id)%element_count
        end if
    end function attribute_length

    integer function attribute_byte(ncid, varid, attribute_id, position) result(value)
        integer, intent(in) :: ncid, varid, attribute_id, position

        if (netcdf4_reading(ncid)) then
            if (varid == NF90_GLOBAL) then
                value = iand(int( &
                    netcdf4_files(ncid)%global_attributes(attribute_id)%bytes(position)), 255)
            else
                value = iand(int(netcdf4_files(ncid)%variables(varid)% &
                    attributes(attribute_id)%bytes(position)), 255)
            end if
        else if (varid == NF90_GLOBAL) then
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
    end function finish_status

end module netcdf
