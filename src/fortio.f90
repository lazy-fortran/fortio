module fortio
    use, intrinsic :: iso_fortran_env, only: int8, int32, real64
    use fortio_bytes, only: byte_reader_t
    use fortio_hdf5_reader, only: hdf5_file_t
    use fortio_netcdf_classic, only: classic_file_t
    use fortio_status
    implicit none
    private

    integer, parameter :: FORMAT_NONE = 0
    integer, parameter :: FORMAT_NETCDF_CLASSIC = 1
    integer, parameter :: FORMAT_HDF5 = 2

    type, public :: fortio_file_t
        private
        integer :: format = FORMAT_NONE
        type(classic_file_t) :: classic
        type(hdf5_file_t) :: hdf5
    contains
        procedure :: open => file_open
        procedure :: close => file_close
        procedure :: read_i32_scalar => file_read_i32_scalar
        procedure :: read_i32_1 => file_read_i32_1
        procedure :: read_r64_scalar => file_read_r64_scalar
        procedure :: read_r64_1 => file_read_r64_1
        procedure :: read_r64_2 => file_read_r64_2
        procedure :: read_r64_3 => file_read_r64_3
        generic :: read => read_i32_scalar, read_i32_1, read_r64_scalar, read_r64_1, &
                           read_r64_2, read_r64_3
        final :: file_finalize
    end type fortio_file_t

    public :: fortio_status_t
    public :: FORTIO_SUCCESS, FORTIO_EIO, FORTIO_EFORMAT, FORTIO_ENOTFOUND
    public :: FORTIO_ETYPE, FORTIO_ESHAPE, FORTIO_ENOTSUP, FORTIO_ESTATE
    public :: FORTIO_EEXIST

contains

    subroutine file_open(this, path, status)
        class(fortio_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status
        type(byte_reader_t) :: detector
        integer(int8) :: signature(8)
        type(fortio_status_t) :: close_status

        call detector%open(path, status)
        if (.not. status%ok()) return
        call detector%read_bytes(signature, status)
        call detector%close(close_status)
        if (.not. status%ok()) return
        if (is_hdf5_signature(signature)) then
            call this%hdf5%open(path, status)
            if (status%ok()) this%format = FORMAT_HDF5
        else
            call this%classic%open(path, status)
            if (status%ok()) this%format = FORMAT_NETCDF_CLASSIC
        end if
    end subroutine file_open

    subroutine file_close(this, status)
        class(fortio_file_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status

        select case (this%format)
        case (FORMAT_NETCDF_CLASSIC)
            call this%classic%close(status)
        case (FORMAT_HDF5)
            call this%hdf5%close(status)
        case default
            call status%clear()
        end select
        this%format = FORMAT_NONE
    end subroutine file_close

    subroutine file_read_i32_scalar(this, path, value, status)
        class(fortio_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int32), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status

        select case (this%format)
        case (FORMAT_NETCDF_CLASSIC)
            call this%classic%read_i32_scalar(path, value, status)
        case (FORMAT_HDF5)
            call this%hdf5%read_i32_scalar(path, value, status)
        case default
            call status%set(FORTIO_ESTATE, "no supported file is open")
        end select
    end subroutine file_read_i32_scalar

    subroutine file_read_i32_1(this, path, value, status)
        class(fortio_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int32), allocatable, intent(out) :: value(:)
        type(fortio_status_t), intent(inout) :: status

        select case (this%format)
        case (FORMAT_NETCDF_CLASSIC)
            call this%classic%read_i32_1(path, value, status)
        case (FORMAT_HDF5)
            call this%hdf5%read_i32_1(path, value, status)
        case default
            call status%set(FORTIO_ESTATE, "no supported file is open")
        end select
    end subroutine file_read_i32_1

    subroutine file_read_r64_scalar(this, path, value, status)
        class(fortio_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status

        select case (this%format)
        case (FORMAT_NETCDF_CLASSIC)
            call this%classic%read_r64_scalar(path, value, status)
        case (FORMAT_HDF5)
            call this%hdf5%read_r64_scalar(path, value, status)
        case default
            call status%set(FORTIO_ESTATE, "no supported file is open")
        end select
    end subroutine file_read_r64_scalar

    subroutine file_read_r64_1(this, path, value, status)
        class(fortio_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), allocatable, intent(out) :: value(:)
        type(fortio_status_t), intent(inout) :: status

        select case (this%format)
        case (FORMAT_NETCDF_CLASSIC)
            call this%classic%read_r64_1(path, value, status)
        case (FORMAT_HDF5)
            call this%hdf5%read_r64_1(path, value, status)
        case default
            call status%set(FORTIO_ESTATE, "no supported file is open")
        end select
    end subroutine file_read_r64_1

    subroutine file_read_r64_2(this, path, value, status)
        class(fortio_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), allocatable, intent(out) :: value(:, :)
        type(fortio_status_t), intent(inout) :: status

        select case (this%format)
        case (FORMAT_NETCDF_CLASSIC)
            call this%classic%read_r64_2(path, value, status)
        case (FORMAT_HDF5)
            call this%hdf5%read_r64_2(path, value, status)
        case default
            call status%set(FORTIO_ESTATE, "no supported file is open")
        end select
    end subroutine file_read_r64_2

    subroutine file_read_r64_3(this, path, value, status)
        class(fortio_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), allocatable, intent(out) :: value(:, :, :)
        type(fortio_status_t), intent(inout) :: status

        select case (this%format)
        case (FORMAT_NETCDF_CLASSIC)
            call this%classic%read_r64_3(path, value, status)
        case (FORMAT_HDF5)
            call this%hdf5%read_r64_3(path, value, status)
        case default
            call status%set(FORTIO_ESTATE, "no supported file is open")
        end select
    end subroutine file_read_r64_3

    pure logical function is_hdf5_signature(bytes)
        integer(int8), intent(in) :: bytes(8)

        is_hdf5_signature = all(bytes == [int(z'89', int8), int(z'48', int8), &
            int(z'44', int8), int(z'46', int8), int(z'0d', int8), int(z'0a', int8), &
            int(z'1a', int8), int(z'0a', int8)])
    end function is_hdf5_signature

    subroutine file_finalize(this)
        type(fortio_file_t), intent(inout) :: this
        type(fortio_status_t) :: ignored

        call this%close(ignored)
    end subroutine file_finalize

end module fortio
