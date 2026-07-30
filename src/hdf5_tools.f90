module hdf5_tools
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use fortio, only: fortio_file_t, fortio_status_t
    implicit none
    private

    integer, parameter, public :: HID_T = int64
    integer, parameter :: MAX_OPEN_FILES = 64
    type(fortio_file_t), save :: files(MAX_OPEN_FILES)
    logical, save :: in_use(MAX_OPEN_FILES) = .false.

    interface h5_get
        module procedure h5_get_int
        module procedure h5_get_int_1
        module procedure h5_get_double_0
        module procedure h5_get_double_1
        module procedure h5_get_double_2
    end interface h5_get

    public :: h5_init, h5_deinit, h5_open, h5_close, h5_get

contains

    subroutine h5_init()
    end subroutine h5_init

    subroutine h5_deinit()
        type(fortio_status_t) :: status
        integer :: slot

        do slot = 1, MAX_OPEN_FILES
            if (in_use(slot)) call files(slot)%close(status)
            in_use(slot) = .false.
        end do
    end subroutine h5_deinit

    subroutine h5_open(filename, h5id)
        character(len=*), intent(in) :: filename
        integer(HID_T), intent(out) :: h5id
        type(fortio_status_t) :: status
        integer :: slot

        slot = first_free_slot()
        if (slot == 0) error stop "fortio hdf5_tools open-file table is full"
        call files(slot)%open(trim(filename), status)
        call require_ok(status)
        in_use(slot) = .true.
        h5id = int(slot, HID_T)
    end subroutine h5_open

    subroutine h5_close(h5id)
        integer(HID_T), intent(inout) :: h5id
        type(fortio_status_t) :: status
        integer :: slot

        slot = require_id(h5id)
        call files(slot)%close(status)
        call require_ok(status)
        in_use(slot) = .false.
        h5id = -1_HID_T
    end subroutine h5_close

    subroutine h5_get_int(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, intent(out) :: value
        type(fortio_status_t) :: status
        integer(int32) :: temporary

        call files(require_id(h5id))%read(trim(dataset), temporary, status)
        call require_ok(status)
        value = int(temporary, kind(value))
    end subroutine h5_get_int

    subroutine h5_get_int_1(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, intent(out) :: value(:)
        type(fortio_status_t) :: status
        integer(int32), allocatable :: temporary(:)

        call files(require_id(h5id))%read(trim(dataset), temporary, status)
        call require_ok(status)
        if (any(shape(value) /= shape(temporary))) error stop "HDF5 dataset shape mismatch"
        value = int(temporary, kind(value))
    end subroutine h5_get_int_1

    subroutine h5_get_double_0(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), intent(out) :: value
        type(fortio_status_t) :: status

        call files(require_id(h5id))%read(trim(dataset), value, status)
        call require_ok(status)
    end subroutine h5_get_double_0

    subroutine h5_get_double_1(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), intent(out) :: value(:)
        type(fortio_status_t) :: status
        real(real64), allocatable :: temporary(:)

        call files(require_id(h5id))%read(trim(dataset), temporary, status)
        call require_ok(status)
        if (any(shape(value) /= shape(temporary))) error stop "HDF5 dataset shape mismatch"
        value = temporary
    end subroutine h5_get_double_1

    subroutine h5_get_double_2(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), intent(out) :: value(:, :)
        type(fortio_status_t) :: status
        real(real64), allocatable :: temporary(:, :)

        call files(require_id(h5id))%read(trim(dataset), temporary, status)
        call require_ok(status)
        if (any(shape(value) /= shape(temporary))) error stop "HDF5 dataset shape mismatch"
        value = temporary
    end subroutine h5_get_double_2

    integer function first_free_slot() result(slot)
        do slot = 1, MAX_OPEN_FILES
            if (.not. in_use(slot)) return
        end do
        slot = 0
    end function first_free_slot

    integer function require_id(h5id) result(slot)
        integer(HID_T), intent(in) :: h5id

        if (h5id < 1_HID_T .or. h5id > int(MAX_OPEN_FILES, HID_T)) &
            error stop "invalid fortio HDF5 identifier"
        slot = int(h5id)
        if (.not. in_use(slot)) error stop "closed fortio HDF5 identifier"
    end function require_id

    subroutine require_ok(status)
        type(fortio_status_t), intent(in) :: status

        if (.not. status%ok()) error stop status%message
    end subroutine require_ok

end module hdf5_tools
