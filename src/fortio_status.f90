module fortio_status
    !! Status codes and diagnostic messages returned by the native API.
    implicit none
    private

    integer, parameter, public :: FORTIO_SUCCESS = 0
    integer, parameter, public :: FORTIO_EIO = -1
    integer, parameter, public :: FORTIO_EFORMAT = -2
    integer, parameter, public :: FORTIO_ENOTFOUND = -3
    integer, parameter, public :: FORTIO_ETYPE = -4
    integer, parameter, public :: FORTIO_ESHAPE = -5
    integer, parameter, public :: FORTIO_ENOTSUP = -6
    integer, parameter, public :: FORTIO_ESTATE = -7
    integer, parameter, public :: FORTIO_EEXIST = -8

    type, public :: fortio_status_t
        !! Result of an operation; `ok()` is true only for `FORTIO_SUCCESS`.
        integer :: code = FORTIO_SUCCESS
        character(len=:), allocatable :: message
    contains
        procedure :: ok => status_ok
        procedure :: clear => status_clear
        procedure :: set => status_set
    end type fortio_status_t

contains

    pure logical function status_ok(this)
        class(fortio_status_t), intent(in) :: this

        status_ok = this%code == FORTIO_SUCCESS
    end function status_ok

    subroutine status_clear(this)
        class(fortio_status_t), intent(inout) :: this

        this%code = FORTIO_SUCCESS
        this%message = ""
    end subroutine status_clear

    subroutine status_set(this, code, message)
        class(fortio_status_t), intent(inout) :: this
        integer, intent(in) :: code
        character(len=*), intent(in) :: message

        this%code = code
        this%message = message
    end subroutine status_set

end module fortio_status
