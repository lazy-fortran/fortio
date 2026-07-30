module fortio
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use fortio_netcdf_classic, only: classic_file_t
    use fortio_status
    implicit none
    private

    integer, parameter :: FORMAT_NONE = 0
    integer, parameter :: FORMAT_NETCDF_CLASSIC = 1

    type, public :: fortio_file_t
        private
        integer :: format = FORMAT_NONE
        type(classic_file_t) :: classic
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

contains

    subroutine file_open(this, path, status)
        class(fortio_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status

        call this%classic%open(path, status)
        if (status%ok()) this%format = FORMAT_NETCDF_CLASSIC
    end subroutine file_open

    subroutine file_close(this, status)
        class(fortio_file_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status

        select case (this%format)
        case (FORMAT_NETCDF_CLASSIC)
            call this%classic%close(status)
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

        if (.not. require_classic(this, status)) return
        call this%classic%read_i32_scalar(path, value, status)
    end subroutine file_read_i32_scalar

    subroutine file_read_i32_1(this, path, value, status)
        class(fortio_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int32), allocatable, intent(out) :: value(:)
        type(fortio_status_t), intent(inout) :: status

        if (.not. require_classic(this, status)) return
        call this%classic%read_i32_1(path, value, status)
    end subroutine file_read_i32_1

    subroutine file_read_r64_scalar(this, path, value, status)
        class(fortio_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status

        if (.not. require_classic(this, status)) return
        call this%classic%read_r64_scalar(path, value, status)
    end subroutine file_read_r64_scalar

    subroutine file_read_r64_1(this, path, value, status)
        class(fortio_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), allocatable, intent(out) :: value(:)
        type(fortio_status_t), intent(inout) :: status

        if (.not. require_classic(this, status)) return
        call this%classic%read_r64_1(path, value, status)
    end subroutine file_read_r64_1

    subroutine file_read_r64_2(this, path, value, status)
        class(fortio_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), allocatable, intent(out) :: value(:, :)
        type(fortio_status_t), intent(inout) :: status

        if (.not. require_classic(this, status)) return
        call this%classic%read_r64_2(path, value, status)
    end subroutine file_read_r64_2

    subroutine file_read_r64_3(this, path, value, status)
        class(fortio_file_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), allocatable, intent(out) :: value(:, :, :)
        type(fortio_status_t), intent(inout) :: status

        if (.not. require_classic(this, status)) return
        call this%classic%read_r64_3(path, value, status)
    end subroutine file_read_r64_3

    logical function require_classic(this, status)
        class(fortio_file_t), intent(in) :: this
        type(fortio_status_t), intent(inout) :: status

        require_classic = this%format == FORMAT_NETCDF_CLASSIC
        if (.not. require_classic) then
            call status%set(FORTIO_ESTATE, "no supported file is open")
        end if
    end function require_classic

    subroutine file_finalize(this)
        type(fortio_file_t), intent(inout) :: this
        type(fortio_status_t) :: ignored

        call this%close(ignored)
    end subroutine file_finalize

end module fortio
