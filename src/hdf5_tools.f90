module hdf5_tools
    use, intrinsic :: iso_c_binding, only: c_int, c_null_char, c_size_t
    use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
    use fortio, only: fortio_file_t, hdf5_attribute_t
    use fortio_hdf5_writer, only: hdf5_writer_t
    use fortio_status, only: fortio_status_t, FORTIO_ENOTSUP
    use fortio_posix, only: handle_table_lock, handle_table_unlock, posix_path_exists, &
        write_session_lock, write_session_unlock
    implicit none
    private

    integer, parameter, public :: HID_T = int64
    integer, parameter, public :: HSIZE_T = c_size_t
    integer, parameter, public :: SIZE_T = c_size_t
    integer, parameter, public :: dcp = real64
    integer(HID_T), parameter, public :: H5T_NATIVE_INTEGER = 1_HID_T
    integer(HID_T), parameter, public :: H5T_NATIVE_DOUBLE = 2_HID_T
    integer, parameter :: MAX_OPEN_FILES = 64
    integer, parameter :: MODE_READ = 1, MODE_WRITE = 2, MODE_UNLIMITED = 3
    integer, parameter :: UNLIMITED_INTEGER = 1, UNLIMITED_DOUBLE = 2
    type :: unlimited_buffer_t
        character(len=1024) :: file_path = ""
        character(len=1024) :: path = ""
        integer :: type_code = 0
        integer :: used = 0
        integer :: row_count = 0
        integer :: column_limit = 0
        integer(int32), allocatable :: values_i32(:)
        real(real64), allocatable :: values_r64(:)
        real(real64), allocatable :: matrix_r64(:, :)
    end type unlimited_buffer_t
    type(fortio_file_t), save :: files(MAX_OPEN_FILES)
    type(hdf5_writer_t), save :: writers(MAX_OPEN_FILES)
    logical, save :: in_use(MAX_OPEN_FILES) = .false.
    integer, save :: handle_mode(MAX_OPEN_FILES) = 0
    integer, save :: root_slot(MAX_OPEN_FILES) = 0
    logical, save :: root_handle(MAX_OPEN_FILES) = .false.
    logical, save :: read_shadow_open(MAX_OPEN_FILES) = .false.
    integer(c_int), save :: write_lock_token(MAX_OPEN_FILES) = -1_c_int
    character(len=1024), save :: handle_prefix(MAX_OPEN_FILES) = ""
    type(unlimited_buffer_t), save :: unlimited_buffers(MAX_OPEN_FILES)
    logical, save, public :: h5overwrite = .false.
    ! When enabled, h5_close retains same-process writer state and h5_deinit
    ! emits each final file image once.  This is useful for applications such
    ! as MEPHIT that perform many close/reopen updates to one output file.
    logical, save, public :: h5_defer_close = .false.

    interface h5_get
        module procedure h5_get_int
        module procedure h5_get_int_1
        module procedure h5_get_int_2, h5_get_int_3
        module procedure h5_get_i64_1
        module procedure h5_get_double_0
        module procedure h5_get_double_1
        module procedure h5_get_double_2
        module procedure h5_get_double_3, h5_get_double_4, h5_get_double_5
        module procedure h5_get_string
        module procedure h5_get_logical
        module procedure h5_get_complex_1, h5_get_complex_2, h5_get_complex_3
    end interface h5_get

    interface h5_add
        module procedure h5_add_int
        module procedure h5_add_int_1_bounds, h5_add_int_1_nobounds
        module procedure h5_add_int_2_bounds, h5_add_int_2_nobounds
        module procedure h5_add_int_3_bounds
        module procedure h5_add_i64_1_bounds, h5_add_i64_1_nobounds
        module procedure h5_add_double_0
        module procedure h5_add_double_1, h5_add_double_1_nobounds
        module procedure h5_add_double_2, h5_add_double_3
        module procedure h5_add_double_4, h5_add_double_5
        module procedure h5_add_logical
        module procedure h5_add_string
        module procedure h5_add_complex_1, h5_add_complex_2, h5_add_complex_3
    end interface h5_add

    interface h5_get_bounds
        module procedure h5_get_bounds_1
        module procedure h5_get_bounds_2
        module procedure h5_get_bounds_3
    end interface h5_get_bounds

    interface h5_append
        module procedure h5_append_int, h5_append_double_0, h5_append_double_1
    end interface h5_append

    public :: h5_init, h5_deinit, h5_open, h5_close, h5_get
    public :: h5_create, h5_open_rw, h5_define_group, h5_open_group, h5_close_group
    public :: h5_add
    public :: h5_get_bounds
    public :: h5_exists, h5_obj_exists
    public :: h5_isvalid, h5_create_parent_groups
    public :: h5_delete, h5_define_unlimited_array, h5_define_unlimited_matrix, h5_append
    public :: h5_append_double_0, h5_append_double_1
    public :: h5_add_int, h5_add_double_0, h5_add_double_1, h5_add_string
    public :: h5_add_float_1
    public :: h5_add_complex_1, h5_get_double_1, h5_get_bounds_1
    public :: h5_get_dataset_info
    public :: h5_copy

contains

    subroutine h5_get_dataset_info(h5id, dataset, dimensions, type_class, &
            type_size, hdferr)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer(HSIZE_T), intent(out) :: dimensions(:)
        integer, intent(out) :: type_class
        integer(SIZE_T), intent(out) :: type_size
        integer, intent(out) :: hdferr
        type(fortio_status_t) :: status
        integer(int64), allocatable :: file_dimensions(:)
        logical :: is_group
        integer :: element_size, slot

        hdferr = -1
        slot = require_readable(h5id)
        call files(root_slot(slot))%describe(joined_path(slot, dataset), is_group, &
            type_class, file_dimensions, status, element_size)
        if (.not. status%ok()) return
        if (is_group) return
        if (size(file_dimensions) /= size(dimensions)) return
        dimensions = int(file_dimensions(size(file_dimensions):1:-1), HSIZE_T)
        type_size = int(element_size, SIZE_T)
        hdferr = 0
    end subroutine h5_get_dataset_info

    subroutine h5_init()
    end subroutine h5_init

    subroutine h5_deinit()
        integer :: slot
        type(fortio_status_t) :: status

        do slot = 1, MAX_OPEN_FILES
            if (.not. in_use(slot) .or. .not. root_handle(slot)) cycle
            if (h5_defer_close .and. handle_mode(slot) == MODE_WRITE) then
                call flush_unlimited_buffers(slot)
                if (read_shadow_open(slot)) then
                    call files(slot)%close(status)
                    call require_ok(status)
                    read_shadow_open(slot) = .false.
                end if
                call writers(slot)%suspend(status)
                call require_ok(status)
            else
                call close_root(slot)
            end if
            if (handle_mode(slot) == MODE_WRITE) &
                call write_session_unlock(write_lock_token(slot))
            call invalidate_root(slot)
        end do
        if (h5_defer_close) then
            do slot = 1, MAX_OPEN_FILES
                if (in_use(slot)) cycle
                if (.not. allocated(writers(slot)%path)) cycle
                if (writers(slot)%opened) cycle
                call writers(slot)%flush(status)
                call require_ok(status)
            end do
        end if
        call clear_nonpersistent_handles()
    end subroutine h5_deinit

    subroutine h5_open(filename, h5id)
        character(len=*), intent(in) :: filename
        integer(HID_T), intent(out) :: h5id
        type(fortio_status_t) :: status
        integer :: slot

        slot = allocate_handle()
        call files(slot)%open(trim(filename), status)
        call require_ok(status)
        call set_root_handle(slot, MODE_READ)
        h5id = int(slot, HID_T)
    end subroutine h5_open

    subroutine h5_create(filename, h5id, opt_fileformat_version)
        character(len=*), intent(in) :: filename
        integer(HID_T), intent(out) :: h5id
        integer, intent(in), optional :: opt_fileformat_version
        type(fortio_status_t) :: status
        integer :: slot
        integer(c_int) :: lock_token

        lock_token = write_session_lock(trim(filename)//c_null_char)
        slot = allocate_handle()
        call writers(slot)%create(trim(filename), status)
        call require_ok(status)
        call set_root_handle(slot, MODE_WRITE)
        write_lock_token(slot) = lock_token
        h5id = int(slot, HID_T)
    end subroutine h5_create

    subroutine h5_open_rw(filename, h5id, opt_fileformat_version)
        character(len=*), intent(in) :: filename
        integer(HID_T), intent(out) :: h5id
        integer, intent(in), optional :: opt_fileformat_version
        type(fortio_status_t) :: status
        integer :: slot
        integer(c_int) :: lock_token

        lock_token = write_session_lock(trim(filename)//c_null_char)
        ! A newly created deferred writer has no on-disk file until h5_deinit
        ! flushes it, so check retained state before the filesystem in that
        ! mode.  Preserve ordinary open_rw behavior for externally removed
        ! files when close deferral is disabled.
        slot = 0
        if (h5_defer_close) slot = persistent_writer_slot(trim(filename))
        if (slot > 0) then
            call writers(slot)%reopen(trim(filename), status)
            call require_ok(status)
            call set_root_handle(slot, MODE_WRITE)
            write_lock_token(slot) = lock_token
            h5id = int(slot, HID_T)
            return
        end if
        if (posix_path_exists(trim(filename)//c_null_char) == 0_c_int) then
            slot = allocate_handle()
            call writers(slot)%create(trim(filename), status)
            call require_ok(status)
            call set_root_handle(slot, MODE_WRITE)
            write_lock_token(slot) = lock_token
            h5id = int(slot, HID_T)
            return
        end if
        ! A writer that was closed in this process already owns the complete
        ! file image. Reuse it instead of reading and copying the whole file
        ! for every close/reopen update.
        slot = persistent_writer_slot(trim(filename))
        if (slot > 0) then
            call files(slot)%open(trim(filename), status)
            call require_ok(status)
            call writers(slot)%reopen(trim(filename), status)
            call require_ok(status)
            call set_root_handle(slot, MODE_WRITE)
            write_lock_token(slot) = lock_token
            read_shadow_open(slot) = .true.
            h5id = int(slot, HID_T)
            return
        end if
        slot = allocate_handle()
        call files(slot)%open(trim(filename), status)
        call require_ok(status)
        call writers(slot)%create(trim(filename), status)
        call require_ok(status)
        call set_root_handle(slot, MODE_WRITE)
        write_lock_token(slot) = lock_token
        call copy_object(slot, "", slot, "")
        read_shadow_open(slot) = .true.
        h5id = int(slot, HID_T)
    end subroutine h5_open_rw

    integer function persistent_writer_slot(path) result(slot)
        character(len=*), intent(in) :: path
        integer :: candidate

        slot = 0
        call handle_table_lock()
        do candidate = 1, MAX_OPEN_FILES
            if (in_use(candidate)) cycle
            if (.not. allocated(writers(candidate)%path)) cycle
            if (writers(candidate)%opened) cycle
            if (trim(writers(candidate)%path) /= trim(path)) cycle
            in_use(candidate) = .true.
            slot = candidate
            exit
        end do
        call handle_table_unlock()
    end function persistent_writer_slot

    subroutine h5_close(h5id)
        integer(HID_T), intent(inout) :: h5id
        integer :: slot
        logical :: writing
        integer(c_int) :: lock_token
        type(fortio_status_t) :: status

        slot = require_id(h5id)
        if (.not. root_handle(slot)) error stop "h5_close requires a file identifier"
        writing = handle_mode(slot) == MODE_WRITE
        lock_token = write_lock_token(slot)
        if (writing .and. h5_defer_close) then
            call flush_unlimited_buffers(slot)
            if (read_shadow_open(slot)) then
                call files(slot)%close(status)
                call require_ok(status)
                read_shadow_open(slot) = .false.
            end if
            call writers(slot)%suspend(status)
            call require_ok(status)
        else
            call close_root(slot)
        end if
        call invalidate_root(slot)
        if (writing) call write_session_unlock(lock_token)
        h5id = -1_HID_T
    end subroutine h5_close

    subroutine h5_define_group(h5id, grpname, h5grpid)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: grpname
        integer(HID_T), intent(out) :: h5grpid
        type(fortio_status_t) :: status
        integer :: group_slot, slot, root
        character(len=:), allocatable :: path

        slot = require_mode(h5id, MODE_WRITE)
        root = root_slot(slot)
        path = joined_path(slot, grpname)
        call writers(root)%define_group(path, status)
        call require_ok(status)
        group_slot = allocate_handle()
        call set_group_handle(group_slot, root, MODE_WRITE, path)
        h5grpid = int(group_slot, HID_T)
    end subroutine h5_define_group

    subroutine h5_open_group(h5id, grpname, h5id_grp)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: grpname
        integer(HID_T), intent(out) :: h5id_grp
        integer :: group_slot, slot

        slot = require_id(h5id)
        group_slot = allocate_handle()
        call set_group_handle(group_slot, root_slot(slot), handle_mode(slot), &
            joined_path(slot, grpname))
        h5id_grp = int(group_slot, HID_T)
    end subroutine h5_open_group

    subroutine h5_close_group(h5id_grp)
        integer(HID_T), intent(inout) :: h5id_grp
        integer :: slot

        slot = require_id(h5id_grp)
        if (root_handle(slot)) error stop "h5_close_group requires a group identifier"
        call clear_handle(slot)
        h5id_grp = -1_HID_T
    end subroutine h5_close_group

    subroutine h5_delete(h5id, dataset)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        type(fortio_status_t) :: status
        integer :: slot

        slot = require_mode(h5id, MODE_WRITE)
        call writers(root_slot(slot))%remove_dataset(joined_path(slot, dataset), status)
        call require_ok(status)
    end subroutine h5_delete

    subroutine h5_copy(source_id, source_path, destination_id, destination_path)
        integer(HID_T), intent(in) :: source_id, destination_id
        character(len=*), intent(in) :: source_path, destination_path
        integer :: source_slot, destination_slot

        source_slot = require_mode(source_id, MODE_READ)
        destination_slot = require_mode(destination_id, MODE_WRITE)
        call copy_object(root_slot(source_slot), joined_path(source_slot, source_path), &
            root_slot(destination_slot), &
            joined_path(destination_slot, destination_path))
    end subroutine h5_copy

    recursive subroutine copy_object(source_root, source_path, destination_root, &
            destination_path)
        integer, intent(in) :: source_root, destination_root
        character(len=*), intent(in) :: source_path, destination_path
        type(fortio_status_t) :: status
        character(len=:), allocatable :: names(:)
        character(len=:), allocatable :: child_source, child_destination
        logical, allocatable :: group_flags(:)
        logical :: is_group
        integer(int64), allocatable :: dimensions(:)
        integer :: type_class, element_size, i

        call files(source_root)%describe(source_path, is_group, type_class, dimensions, status, &
            element_size)
        call require_ok(status)
        if (.not. is_group) then
            call copy_dataset(source_root, source_path, destination_root, &
                destination_path, type_class, element_size, size(dimensions))
            return
        end if
        if (len_trim(destination_path) > 0 .and. trim(destination_path) /= "/") then
            call writers(destination_root)%define_group(destination_path, status)
            call require_ok(status)
        end if
        call files(source_root)%list_children(source_path, names, group_flags, status)
        call require_ok(status)
        do i = 1, size(names)
            child_source = append_path(source_path, trim(names(i)))
            child_destination = append_path(destination_path, trim(names(i)))
            call copy_object(source_root, child_source, destination_root, child_destination)
        end do
    end subroutine copy_object

    subroutine copy_dataset(source_root, source_path, destination_root, destination_path, &
            type_class, element_size, rank)
        integer, intent(in) :: source_root, destination_root, type_class, element_size, rank
        character(len=*), intent(in) :: source_path, destination_path
        type(fortio_status_t) :: status
        integer(int32) :: i0
        integer(int32), allocatable :: i1(:), i2(:, :), i3(:, :, :)
        integer(int64), allocatable :: i64_1(:)
        real(real64) :: r0
        real(real64), allocatable :: r1(:), r2(:, :), r3(:, :, :)
        real(real64), allocatable :: r4(:, :, :, :), r5(:, :, :, :, :)
        real(real32), allocatable :: r32_1(:)
        complex(real64), allocatable :: c1(:), c2(:, :), c3(:, :, :)
        character(len=:), allocatable :: text
        type(hdf5_attribute_t), allocatable :: attributes(:)
        integer :: attribute_index

        select case (type_class)
        case (0)
            if (element_size == 8 .and. rank == 1) then
                call files(source_root)%read(source_path, i64_1, status)
                if (status%ok()) call writers(destination_root)%add_i64_1( &
                    destination_path, i64_1, status)
                call require_ok(status)
                return
            end if
            select case (rank)
            case (0)
                call files(source_root)%read_i32_scalar(source_path, i0, status)
                if (status%ok()) call writers(destination_root)%add_i32_scalar( &
                    destination_path, i0, status)
            case (1)
                call files(source_root)%read_i32_1(source_path, i1, status)
                if (status%ok()) call writers(destination_root)%add_i32_1( &
                    destination_path, i1, status)
            case (2)
                call files(source_root)%read_i32_2(source_path, i2, status)
                if (status%ok()) call writers(destination_root)%add_i32_2( &
                    destination_path, i2, status)
            case (3)
                call files(source_root)%read_i32_3(source_path, i3, status)
                if (status%ok()) call writers(destination_root)%add_i32_3( &
                    destination_path, i3, status)
            case default
                call status%set(FORTIO_ENOTSUP, "integer HDF5 copy rank is not supported")
            end select
        case (1)
            if (element_size == 4 .and. rank == 1) then
                call files(source_root)%read_r64_1(source_path, r1, status)
                if (status%ok()) then
                    r32_1 = real(r1, real32)
                    call writers(destination_root)%add_r32_1(destination_path, r32_1, status)
                end if
            else
                select case (rank)
                case (0)
                    call files(source_root)%read_r64_scalar(source_path, r0, status)
                    if (status%ok()) call writers(destination_root)%add_r64_scalar( &
                        destination_path, r0, status)
                case (1)
                    call files(source_root)%read_r64_1(source_path, r1, status)
                    if (status%ok()) call writers(destination_root)%add_r64_1( &
                        destination_path, r1, status)
                case (2)
                    call files(source_root)%read_r64_2(source_path, r2, status)
                    if (status%ok()) call writers(destination_root)%add_r64_2( &
                        destination_path, r2, status)
                case (3)
                    call files(source_root)%read_r64_3(source_path, r3, status)
                    if (status%ok()) call writers(destination_root)%add_r64_3( &
                        destination_path, r3, status)
                case (4)
                    call files(source_root)%read_r64_4(source_path, r4, status)
                    if (status%ok()) call writers(destination_root)%add_r64_4( &
                        destination_path, r4, status)
                case (5)
                    call files(source_root)%read_r64_5(source_path, r5, status)
                    if (status%ok()) call writers(destination_root)%add_r64_5( &
                        destination_path, r5, status)
                case default
                    call status%set(FORTIO_ENOTSUP, "real HDF5 copy rank is not supported")
                end select
            end if
        case (3)
            call files(source_root)%read_text_scalar(source_path, text, status)
            if (status%ok()) call writers(destination_root)%add_text_scalar( &
                destination_path, text, status)
        case (6)
            select case (rank)
            case (1)
                call files(source_root)%read_c64_1(source_path, c1, status)
                if (status%ok()) call writers(destination_root)%add_c64_1( &
                    destination_path, c1, status)
            case (2)
                call files(source_root)%read_c64_2(source_path, c2, status)
                if (status%ok()) call writers(destination_root)%add_c64_2( &
                    destination_path, c2, status)
            case (3)
                call files(source_root)%read_c64_3(source_path, c3, status)
                if (status%ok()) call writers(destination_root)%add_c64_3( &
                    destination_path, c3, status)
            case default
                call status%set(FORTIO_ENOTSUP, "complex HDF5 copy rank is not supported")
            end select
        case default
            call status%set(FORTIO_ENOTSUP, "HDF5 copy datatype is not supported")
        end select
        call require_ok(status)
        call files(source_root)%get_attributes(source_path, attributes, status)
        call require_ok(status)
        do attribute_index = 1, size(attributes)
            if (allocated(attributes(attribute_index)%values_i32)) then
                call writers(destination_root)%add_i32_attribute(destination_path, &
                    attributes(attribute_index)%name, &
                    attributes(attribute_index)%values_i32, status)
            else if (allocated(attributes(attribute_index)%values_r64)) then
                if (size(attributes(attribute_index)%values_r64) /= 1) then
                    call status%set(FORTIO_ENOTSUP, &
                        "real vector HDF5 attributes are not required")
                else
                    call writers(destination_root)%add_r64_attribute(destination_path, &
                        attributes(attribute_index)%name, &
                        attributes(attribute_index)%values_r64(1), status)
                end if
            else if (allocated(attributes(attribute_index)%value_text)) then
                call writers(destination_root)%add_text_attribute(destination_path, &
                    attributes(attribute_index)%name, &
                    attributes(attribute_index)%value_text, status)
            end if
            call require_ok(status)
        end do
    end subroutine copy_dataset

    subroutine h5_define_unlimited_array(h5id, dataset, type_id, dataset_id)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer(HID_T), intent(in) :: type_id
        integer(HID_T), intent(out) :: dataset_id
        integer :: root, slot

        slot = require_mode(h5id, MODE_WRITE)
        root = root_slot(slot)
        slot = allocate_handle()
        handle_mode(slot) = MODE_UNLIMITED
        root_slot(slot) = root
        root_handle(slot) = .false.
        write_lock_token(slot) = -1_c_int
        unlimited_buffers(slot)%path = joined_path(int(h5id), dataset)
        unlimited_buffers(slot)%file_path = writers(root)%path
        unlimited_buffers(slot)%type_code = int(type_id)
        select case (unlimited_buffers(slot)%type_code)
        case (UNLIMITED_INTEGER)
            allocate(unlimited_buffers(slot)%values_i32(16), source=0_int32)
        case (UNLIMITED_DOUBLE)
            allocate(unlimited_buffers(slot)%values_r64(16), source=0.0_real64)
        case default
            call clear_handle(slot)
            error stop "unsupported fortio unlimited-array type"
        end select
        dataset_id = int(slot, HID_T)
    end subroutine h5_define_unlimited_array

    subroutine h5_define_unlimited_matrix(h5id, dataset, type_id, dimensions, dataset_id)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer(HID_T), intent(in) :: type_id
        integer, intent(in) :: dimensions(:)
        integer(HID_T), intent(out) :: dataset_id
        integer :: root, slot, unlimited_dimension

        if (size(dimensions) /= 2 .or. count(dimensions == -1) /= 1) &
            error stop "fortio supports one unlimited dimension in a rank-2 matrix"
        if (type_id /= H5T_NATIVE_DOUBLE) &
            error stop "fortio supports only double unlimited matrices"
        unlimited_dimension = findloc(dimensions, -1, dim=1)
        slot = require_mode(h5id, MODE_WRITE)
        root = root_slot(slot)
        slot = allocate_handle()
        handle_mode(slot) = MODE_UNLIMITED
        root_slot(slot) = root
        root_handle(slot) = .false.
        unlimited_buffers(slot)%path = joined_path(int(h5id), dataset)
        unlimited_buffers(slot)%file_path = writers(root)%path
        unlimited_buffers(slot)%type_code = UNLIMITED_DOUBLE
        if (unlimited_dimension == 1) then
            if (dimensions(2) < 1) &
                error stop "fortio unlimited matrix column count must be positive"
            unlimited_buffers(slot)%column_limit = dimensions(2)
        else
            if (dimensions(1) < 1) &
                error stop "fortio unlimited matrix row count must be positive"
            unlimited_buffers(slot)%row_count = dimensions(1)
            allocate(unlimited_buffers(slot)%matrix_r64(dimensions(1), 16), source=0.0_real64)
        end if
        dataset_id = int(slot, HID_T)
    end subroutine h5_define_unlimited_matrix

    subroutine h5_append_int(dataset_id, value, position)
        integer(HID_T), intent(in) :: dataset_id
        integer, intent(in) :: value, position
        integer(int32), allocatable :: temporary(:)
        integer :: slot

        slot = require_mode(dataset_id, MODE_UNLIMITED)
        if (unlimited_buffers(slot)%type_code /= UNLIMITED_INTEGER) &
            error stop "integer appended to non-integer HDF5 dataset"
        if (position < 1) error stop "HDF5 append position must be positive"
        if (position > size(unlimited_buffers(slot)%values_i32)) then
            allocate(temporary(max(position, 2*size(unlimited_buffers(slot)%values_i32))), &
                source=0_int32)
            temporary(:size(unlimited_buffers(slot)%values_i32)) = &
                unlimited_buffers(slot)%values_i32
            call move_alloc(temporary, unlimited_buffers(slot)%values_i32)
        end if
        unlimited_buffers(slot)%values_i32(position) = int(value, int32)
        unlimited_buffers(slot)%used = max(unlimited_buffers(slot)%used, position)
    end subroutine h5_append_int

    subroutine h5_append_double_0(dataset_id, value, position)
        integer(HID_T), intent(in) :: dataset_id
        real(real64), intent(in) :: value
        integer, intent(in) :: position
        real(real64), allocatable :: temporary(:)
        integer :: slot

        slot = require_mode(dataset_id, MODE_UNLIMITED)
        if (unlimited_buffers(slot)%type_code /= UNLIMITED_DOUBLE) &
            error stop "real appended to non-real HDF5 dataset"
        if (position < 1) error stop "HDF5 append position must be positive"
        if (position > size(unlimited_buffers(slot)%values_r64)) then
            allocate(temporary(max(position, 2*size(unlimited_buffers(slot)%values_r64))), &
                source=0.0_real64)
            temporary(:size(unlimited_buffers(slot)%values_r64)) = &
                unlimited_buffers(slot)%values_r64
            call move_alloc(temporary, unlimited_buffers(slot)%values_r64)
        end if
        unlimited_buffers(slot)%values_r64(position) = value
        unlimited_buffers(slot)%used = max(unlimited_buffers(slot)%used, position)
    end subroutine h5_append_double_0

    subroutine h5_append_double_1(dataset_id, values, position)
        integer(HID_T), intent(in) :: dataset_id
        real(real64), intent(in) :: values(:)
        integer, intent(in) :: position
        real(real64), allocatable :: temporary(:, :)
        integer :: slot

        slot = require_mode(dataset_id, MODE_UNLIMITED)
        if (unlimited_buffers(slot)%row_count == 0 .and. &
            unlimited_buffers(slot)%column_limit == 0) &
            error stop "array appended to non-matrix HDF5 dataset"
        if (unlimited_buffers(slot)%row_count == 0) then
            unlimited_buffers(slot)%row_count = size(values)
            allocate(unlimited_buffers(slot)%matrix_r64(size(values), &
                unlimited_buffers(slot)%column_limit), source=0.0_real64)
        end if
        if (size(values) /= unlimited_buffers(slot)%row_count) &
            error stop "HDF5 appended array has the wrong size"
        if (position < 1) error stop "HDF5 append position must be positive"
        if (unlimited_buffers(slot)%column_limit > 0 .and. &
            position > unlimited_buffers(slot)%column_limit) &
            error stop "HDF5 append position exceeds the fixed matrix dimension"
        if (position > size(unlimited_buffers(slot)%matrix_r64, 2)) then
            allocate(temporary(unlimited_buffers(slot)%row_count, &
                max(position, 2*size(unlimited_buffers(slot)%matrix_r64, 2))), &
                source=0.0_real64)
            temporary(:, :size(unlimited_buffers(slot)%matrix_r64, 2)) = &
                unlimited_buffers(slot)%matrix_r64
            call move_alloc(temporary, unlimited_buffers(slot)%matrix_r64)
        end if
        unlimited_buffers(slot)%matrix_r64(:, position) = values
        unlimited_buffers(slot)%used = max(unlimited_buffers(slot)%used, position)
    end subroutine h5_append_double_1

    subroutine h5_get_int(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, intent(out) :: value
        type(fortio_status_t) :: status
        integer(int32) :: temporary
        integer(int32), allocatable :: temporary_vector(:)
        integer(int64), allocatable :: dimensions(:)
        logical :: is_group
        integer :: element_size, slot, type_class
        character(len=:), allocatable :: path

        slot = require_readable(h5id)
        path = joined_path(slot, dataset)
        call files(root_slot(slot))%describe(path, is_group, type_class, dimensions, &
            status, element_size)
        call require_ok(status)
        if (is_group) error stop "HDF5 integer scalar read found a group"
        select case (size(dimensions))
        case (0)
            ! Native HDF5 scalar dataspace.
            call files(root_slot(slot))%read(path, temporary, status)
            call require_ok(status)
            value = int(temporary, kind(value))
        case (1)
            ! The pre-Fortio hdf5_tools writer represented legacy scalar
            ! values as a rank-1 dataset with one element.  Keep accepting
            ! that valid HDF5 representation through the scalar adapter.
            if (dimensions(1) /= 1_int64) &
                error stop "HDF5 integer scalar read found a non-unit vector"
            call files(root_slot(slot))%read(path, temporary_vector, status)
            call require_ok(status)
            value = int(temporary_vector(1), kind(value))
        case default
            error stop "HDF5 integer scalar read found a non-scalar dataset"
        end select
    end subroutine h5_get_int

    subroutine h5_get_int_1(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, intent(out) :: value(:)
        type(fortio_status_t) :: status
        integer(int32), allocatable :: temporary(:)
        integer :: slot

        slot = require_readable(h5id)
        call files(root_slot(slot))%read(joined_path(slot, dataset), temporary, status)
        call require_ok(status)
        if (any(shape(value) /= shape(temporary))) error stop "HDF5 dataset shape mismatch"
        value = int(temporary, kind(value))
    end subroutine h5_get_int_1

    subroutine h5_get_i64_1(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer(int64), intent(out) :: value(:)
        integer(int64), allocatable :: temporary(:)
        type(fortio_status_t) :: status
        integer :: slot

        slot = require_readable(h5id)
        call files(root_slot(slot))%read(joined_path(slot, dataset), temporary, status)
        call require_ok(status)
        if (any(shape(value) /= shape(temporary))) error stop "HDF5 dataset shape mismatch"
        value = temporary
    end subroutine h5_get_i64_1

    subroutine h5_get_int_2(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, intent(out) :: value(:, :)
        type(fortio_status_t) :: status
        integer(int32), allocatable :: temporary(:, :)
        integer :: slot

        slot = require_readable(h5id)
        call files(root_slot(slot))%read(joined_path(slot, dataset), temporary, status)
        call require_ok(status)
        if (any(shape(value) /= shape(temporary))) error stop "HDF5 dataset shape mismatch"
        value = int(temporary, kind(value))
    end subroutine h5_get_int_2

    subroutine h5_get_int_3(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, intent(out) :: value(:, :, :)
        type(fortio_status_t) :: status
        integer(int32), allocatable :: temporary(:, :, :)
        integer :: slot

        slot = require_readable(h5id)
        call files(root_slot(slot))%read(joined_path(slot, dataset), temporary, status)
        call require_ok(status)
        if (any(shape(value) /= shape(temporary))) error stop "HDF5 dataset shape mismatch"
        value = int(temporary, kind(value))
    end subroutine h5_get_int_3

    subroutine h5_get_double_0(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), intent(out) :: value
        type(fortio_status_t) :: status
        integer :: slot

        slot = require_readable(h5id)
        call files(root_slot(slot))%read(joined_path(slot, dataset), value, status)
        call require_ok(status)
    end subroutine h5_get_double_0

    subroutine h5_get_double_1(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), intent(out) :: value(:)
        type(fortio_status_t) :: status
        real(real64), allocatable :: temporary(:)
        integer :: slot

        slot = require_readable(h5id)
        call files(root_slot(slot))%read(joined_path(slot, dataset), temporary, status)
        call require_ok(status)
        if (any(shape(value) /= shape(temporary))) error stop "HDF5 dataset shape mismatch"
        value = temporary
    end subroutine h5_get_double_1

    subroutine h5_get_double_2(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), contiguous, target, intent(out) :: value(:, :)
        type(fortio_status_t) :: status
        integer :: slot
        character(len=2048) :: path

        slot = require_readable(h5id)
        call joined_path_into(slot, dataset, path)
        call files(root_slot(slot))%read_into_r64_2(trim(path), value, status)
        call require_ok(status)
    end subroutine h5_get_double_2

    subroutine h5_get_double_3(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), intent(out) :: value(:, :, :)
        type(fortio_status_t) :: status
        real(real64), allocatable :: temporary(:, :, :)
        integer :: slot

        slot = require_readable(h5id)
        call files(root_slot(slot))%read(joined_path(slot, dataset), temporary, status)
        call require_ok(status)
        if (any(shape(value) /= shape(temporary))) error stop "HDF5 dataset shape mismatch"
        value = temporary
    end subroutine h5_get_double_3

    subroutine h5_get_double_4(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), intent(out) :: value(:, :, :, :)
        type(fortio_status_t) :: status
        real(real64), allocatable :: temporary(:, :, :, :)
        integer :: slot

        slot = require_readable(h5id)
        call files(root_slot(slot))%read(joined_path(slot, dataset), temporary, status)
        call require_ok(status)
        if (any(shape(value) /= shape(temporary))) error stop "HDF5 dataset shape mismatch"
        value = temporary
    end subroutine h5_get_double_4

    subroutine h5_get_double_5(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), intent(out) :: value(:, :, :, :, :)
        type(fortio_status_t) :: status
        real(real64), allocatable :: temporary(:, :, :, :, :)
        integer :: slot

        slot = require_readable(h5id)
        call files(root_slot(slot))%read(joined_path(slot, dataset), temporary, status)
        call require_ok(status)
        if (any(shape(value) /= shape(temporary))) error stop "HDF5 dataset shape mismatch"
        value = temporary
    end subroutine h5_get_double_5

    subroutine h5_get_complex_1(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        complex(dcp), intent(out) :: value(:)
        complex(dcp), allocatable :: temporary(:)
        type(fortio_status_t) :: status
        integer :: slot

        slot = require_readable(h5id)
        call files(root_slot(slot))%read(joined_path(slot, dataset), temporary, status)
        call require_ok(status)
        if (any(shape(value) /= shape(temporary))) error stop "HDF5 dataset shape mismatch"
        value = temporary
    end subroutine h5_get_complex_1

    subroutine h5_get_complex_2(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        complex(dcp), intent(out) :: value(:, :)
        complex(dcp), allocatable :: temporary(:, :)
        type(fortio_status_t) :: status
        integer :: slot

        slot = require_readable(h5id)
        call files(root_slot(slot))%read(joined_path(slot, dataset), temporary, status)
        call require_ok(status)
        if (any(shape(value) /= shape(temporary))) error stop "HDF5 dataset shape mismatch"
        value = temporary
    end subroutine h5_get_complex_2

    subroutine h5_get_complex_3(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        complex(dcp), intent(out) :: value(:, :, :)
        complex(dcp), allocatable :: temporary(:, :, :)
        type(fortio_status_t) :: status
        integer :: slot

        slot = require_readable(h5id)
        call files(root_slot(slot))%read(joined_path(slot, dataset), temporary, status)
        call require_ok(status)
        if (any(shape(value) /= shape(temporary))) error stop "HDF5 dataset shape mismatch"
        value = temporary
    end subroutine h5_get_complex_3

    subroutine h5_get_string(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        character(len=*), intent(out) :: value
        character(len=:), allocatable :: temporary
        type(fortio_status_t) :: status
        integer :: i, slot

        slot = require_readable(h5id)
        call files(root_slot(slot))%read_text_scalar(joined_path(slot, dataset), &
            temporary, status)
        call require_ok(status)
        value = ""
        do i = 1, min(len(value), len(temporary))
            if (temporary(i:i) == achar(0)) exit
            value(i:i) = temporary(i:i)
        end do
    end subroutine h5_get_string

    subroutine h5_get_logical(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        logical, intent(out) :: value
        integer :: temporary

        call h5_get_int(h5id, dataset, temporary)
        value = temporary /= 0
    end subroutine h5_get_logical

    logical function h5_exists(h5id, name_obj) result(exists)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: name_obj
        type(fortio_status_t) :: status
        integer :: slot

        if (.not. h5_isvalid(h5id)) error stop "invalid fortio HDF5 identifier"
        slot = int(h5id)
        if (handle_mode(slot) == MODE_READ) then
            call files(root_slot(slot))%exists(joined_path(slot, name_obj), exists, status)
            call require_ok(status)
        else if (handle_mode(slot) == MODE_WRITE) then
            exists = writers(root_slot(slot))%object_exists(joined_path(slot, name_obj))
        else
            error stop "fortio HDF5 identifier has wrong mode"
        end if
    end function h5_exists

    subroutine h5_obj_exists(h5id, name_obj, exists)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: name_obj
        logical, intent(out) :: exists

        exists = h5_exists(h5id, name_obj)
    end subroutine h5_obj_exists

    logical function h5_isvalid(h5id) result(valid)
        integer(HID_T), intent(in) :: h5id
        integer :: slot

        valid = .false.
        if (h5id < 1_HID_T) return
        if (h5id > int(MAX_OPEN_FILES, HID_T)) return
        slot = int(h5id)
        valid = in_use(slot)
    end function h5_isvalid

    subroutine h5_create_parent_groups(h5id, dataset)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        type(fortio_status_t) :: status
        character(len=:), allocatable :: path
        integer :: separator, slot

        slot = require_mode(h5id, MODE_WRITE)
        path = trim(dataset)
        do while (len(path) > 0)
            if (path(len(path):len(path)) /= "/") exit
            path = path(:len(path) - 1)
        end do
        if (len(path) == 0) return
        if (len_trim(dataset) > 0) then
            if (dataset(len_trim(dataset):len_trim(dataset)) /= "/") then
                separator = scan(path, "/", back=.true.)
                if (separator <= 1) return
                path = path(:separator - 1)
            end if
        end if
        call writers(root_slot(slot))%define_group(joined_path(slot, path), status)
        call require_ok(status)
    end subroutine h5_create_parent_groups

    subroutine h5_get_bounds_1(h5id, dataset, lb1, ub1)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, intent(inout) :: lb1, ub1
        integer :: lower(1), upper(1)

        call read_bounds(h5id, dataset, lower, upper)
        lb1 = lower(1)
        ub1 = upper(1)
    end subroutine h5_get_bounds_1

    subroutine h5_get_bounds_2(h5id, dataset, lb1, lb2, ub1, ub2)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, intent(out) :: lb1, lb2, ub1, ub2
        integer :: lower(2), upper(2)

        call read_bounds(h5id, dataset, lower, upper)
        lb1 = lower(1)
        lb2 = lower(2)
        ub1 = upper(1)
        ub2 = upper(2)
    end subroutine h5_get_bounds_2

    subroutine h5_get_bounds_3(h5id, dataset, lb1, lb2, lb3, ub1, ub2, ub3)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, intent(out) :: lb1, lb2, lb3, ub1, ub2, ub3
        integer :: lower(3), upper(3)

        call read_bounds(h5id, dataset, lower, upper)
        lb1 = lower(1)
        lb2 = lower(2)
        lb3 = lower(3)
        ub1 = upper(1)
        ub2 = upper(2)
        ub3 = upper(3)
    end subroutine h5_get_bounds_3

    subroutine read_bounds(h5id, dataset, lower, upper)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, intent(out) :: lower(:), upper(:)
        integer(int32), allocatable :: values(:)
        type(fortio_status_t) :: status
        logical :: found
        integer :: slot

        slot = require_readable(h5id)
        lower = 0
        upper = 0
        call files(root_slot(slot))%read_i32_attribute(joined_path(slot, dataset), &
            "lbounds", values, found, status)
        call require_ok(status)
        if (found) then
            if (size(values) /= size(lower)) error stop "HDF5 lower-bound rank mismatch"
            lower = int(values, kind(lower))
        end if
        call files(root_slot(slot))%read_i32_attribute(joined_path(slot, dataset), &
            "ubounds", values, found, status)
        call require_ok(status)
        if (found) then
            if (size(values) /= size(upper)) error stop "HDF5 upper-bound rank mismatch"
            upper = int(values, kind(upper))
        end if
    end subroutine read_bounds

    subroutine h5_add_int(h5id, dataset, value, comment, unit)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, intent(in) :: value
        character(len=*), intent(in), optional :: comment, unit
        type(fortio_status_t) :: status
        integer :: slot

        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        call writers(root_slot(slot))%add_i32_scalar(joined_path(slot, dataset), &
            int(value, int32), status)
        call require_ok(status)
        call add_common_attributes(slot, dataset, comment, unit)
    end subroutine h5_add_int

    subroutine h5_add_logical(h5id, dataset, value, comment, unit)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        logical, intent(in) :: value
        character(len=*), intent(in), optional :: comment, unit

        call h5_add_int(h5id, dataset, merge(1, 0, value), comment, unit)
    end subroutine h5_add_logical

    subroutine h5_add_string(h5id, dataset, value, comment, unit)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset, value
        character(len=*), intent(in), optional :: comment, unit
        type(fortio_status_t) :: status
        integer :: slot

        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        call writers(root_slot(slot))%add_text_scalar(joined_path(slot, dataset), value, status)
        call require_ok(status)
        call add_common_attributes(slot, dataset, comment, unit)
    end subroutine h5_add_string

    subroutine h5_add_int_1_bounds(h5id, dataset, value, lbounds, ubounds, comment, unit)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, intent(in) :: value(:), lbounds(:), ubounds(:)
        character(len=*), intent(in), optional :: comment, unit

        call require_bounds(shape(value), lbounds, ubounds)
        call add_int_1(h5id, dataset, value)
        call add_bounds_attributes(require_mode(h5id, MODE_WRITE), dataset, lbounds, ubounds)
        call add_common_attributes(require_mode(h5id, MODE_WRITE), dataset, comment, unit)
    end subroutine h5_add_int_1_bounds

    subroutine h5_add_int_1_nobounds(h5id, dataset, value, comment, unit, default)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, allocatable, intent(in) :: value(:)
        character(len=*), intent(in), optional :: comment, unit
        integer, intent(in), optional :: default

        if (allocated(value)) then
            call add_int_1(h5id, dataset, value)
            call add_bounds_attributes(require_mode(h5id, MODE_WRITE), dataset, &
                lbound(value), ubound(value))
            call add_common_attributes(require_mode(h5id, MODE_WRITE), dataset, comment, unit)
        else
            if (present(default)) then
                call h5_add_int(h5id, dataset, default)
            else
                call h5_add_int(h5id, dataset, 0)
            end if
            call add_common_attributes(require_mode(h5id, MODE_WRITE), dataset, &
                "value not allocated")
        end if
    end subroutine h5_add_int_1_nobounds

    subroutine h5_add_i64_1_bounds(h5id, dataset, value, lbounds, ubounds, comment, unit)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer(int64), intent(in) :: value(:)
        integer, intent(in) :: lbounds(:), ubounds(:)
        character(len=*), intent(in), optional :: comment, unit
        type(fortio_status_t) :: status
        integer :: slot

        call require_bounds(shape(value), lbounds, ubounds)
        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        call writers(root_slot(slot))%add_i64_1(joined_path(slot, dataset), value, status)
        call require_ok(status)
        call add_bounds_attributes(slot, dataset, lbounds, ubounds)
        call add_common_attributes(slot, dataset, comment, unit)
    end subroutine h5_add_i64_1_bounds

    subroutine h5_add_i64_1_nobounds(h5id, dataset, value, comment, unit)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer(int64), intent(in) :: value(:)
        character(len=*), intent(in), optional :: comment, unit
        type(fortio_status_t) :: status
        integer :: slot

        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        call writers(root_slot(slot))%add_i64_1(joined_path(slot, dataset), value, status)
        call require_ok(status)
        call add_bounds_attributes(slot, dataset, lbound(value), ubound(value))
        call add_common_attributes(slot, dataset, comment, unit)
    end subroutine h5_add_i64_1_nobounds

    subroutine add_int_1(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, intent(in) :: value(:)
        type(fortio_status_t) :: status
        integer(int32), allocatable :: converted(:)
        integer :: slot

        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        converted = int(value, int32)
        call writers(root_slot(slot))%add_i32_1(joined_path(slot, dataset), converted, status)
        call require_ok(status)
    end subroutine add_int_1

    subroutine h5_add_int_2_bounds(h5id, dataset, value, lbounds, ubounds, comment, unit)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, intent(in) :: value(:, :), lbounds(:), ubounds(:)
        character(len=*), intent(in), optional :: comment, unit

        call require_bounds(shape(value), lbounds, ubounds)
        call add_int_2(h5id, dataset, value)
        call add_bounds_attributes(require_mode(h5id, MODE_WRITE), dataset, lbounds, ubounds)
        call add_common_attributes(require_mode(h5id, MODE_WRITE), dataset, comment, unit)
    end subroutine h5_add_int_2_bounds

    subroutine h5_add_int_2_nobounds(h5id, dataset, value, comment, unit, default)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, allocatable, intent(in) :: value(:, :)
        character(len=*), intent(in), optional :: comment, unit
        integer, intent(in), optional :: default

        if (allocated(value)) then
            call add_int_2(h5id, dataset, value)
            call add_bounds_attributes(require_mode(h5id, MODE_WRITE), dataset, &
                lbound(value), ubound(value))
            call add_common_attributes(require_mode(h5id, MODE_WRITE), dataset, comment, unit)
        else
            if (present(default)) then
                call h5_add_int(h5id, dataset, default)
            else
                call h5_add_int(h5id, dataset, 0)
            end if
            call add_common_attributes(require_mode(h5id, MODE_WRITE), dataset, &
                "value not allocated")
        end if
    end subroutine h5_add_int_2_nobounds

    subroutine add_int_2(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, intent(in) :: value(:, :)
        type(fortio_status_t) :: status
        integer(int32), allocatable :: converted(:, :)
        integer :: slot

        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        converted = int(value, int32)
        call writers(root_slot(slot))%add_i32_2(joined_path(slot, dataset), converted, status)
        call require_ok(status)
    end subroutine add_int_2

    subroutine h5_add_int_3_bounds(h5id, dataset, value, lbounds, ubounds, comment, unit)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        integer, intent(in) :: value(:, :, :), lbounds(:), ubounds(:)
        character(len=*), intent(in), optional :: comment, unit
        type(fortio_status_t) :: status
        integer(int32), allocatable :: converted(:, :, :)
        integer :: slot

        call require_bounds(shape(value), lbounds, ubounds)
        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        converted = int(value, int32)
        call writers(root_slot(slot))%add_i32_3(joined_path(slot, dataset), converted, status)
        call require_ok(status)
        call add_bounds_attributes(slot, dataset, lbounds, ubounds)
        call add_common_attributes(slot, dataset, comment, unit)
    end subroutine h5_add_int_3_bounds

    subroutine h5_add_double_0(h5id, dataset, value, comment, unit, accuracy)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), intent(in) :: value
        character(len=*), intent(in), optional :: comment, unit
        real(real64), intent(in), optional :: accuracy
        type(fortio_status_t) :: status
        integer :: slot

        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        call writers(root_slot(slot))%add_r64_scalar(joined_path(slot, dataset), value, status)
        call require_ok(status)
        call add_common_attributes(slot, dataset, comment, unit)
        call add_accuracy_attribute(slot, dataset, accuracy)
    end subroutine h5_add_double_0

    subroutine h5_add_complex_1(h5id, dataset, value, lbounds, ubounds, &
            comment, unit, accuracy)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        complex(dcp), intent(in) :: value(:)
        integer, intent(in) :: lbounds(:), ubounds(:)
        character(len=*), intent(in), optional :: comment, unit
        real(real64), intent(in), optional :: accuracy
        type(fortio_status_t) :: status
        integer :: slot

        call require_bounds(shape(value), lbounds, ubounds)
        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        call writers(root_slot(slot))%add_c64_1(joined_path(slot, dataset), value, status)
        call require_ok(status)
        call add_bounds_attributes(slot, dataset, lbounds, ubounds)
        call add_common_attributes(slot, dataset, comment, unit)
        call add_accuracy_attribute(slot, dataset, accuracy)
    end subroutine h5_add_complex_1

    subroutine h5_add_complex_2(h5id, dataset, value, lbounds, ubounds, &
            comment, unit, accuracy)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        complex(dcp), intent(in) :: value(:, :)
        integer, intent(in) :: lbounds(:), ubounds(:)
        character(len=*), intent(in), optional :: comment, unit
        real(real64), intent(in), optional :: accuracy
        type(fortio_status_t) :: status
        integer :: slot

        call require_bounds(shape(value), lbounds, ubounds)
        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        call writers(root_slot(slot))%add_c64_2(joined_path(slot, dataset), value, status)
        call require_ok(status)
        call add_bounds_attributes(slot, dataset, lbounds, ubounds)
        call add_common_attributes(slot, dataset, comment, unit)
        call add_accuracy_attribute(slot, dataset, accuracy)
    end subroutine h5_add_complex_2

    subroutine h5_add_complex_3(h5id, dataset, value, lbounds, ubounds, &
            comment, unit, accuracy)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        complex(dcp), intent(in) :: value(:, :, :)
        integer, intent(in) :: lbounds(:), ubounds(:)
        character(len=*), intent(in), optional :: comment, unit
        real(real64), intent(in), optional :: accuracy
        type(fortio_status_t) :: status
        integer :: slot

        call require_bounds(shape(value), lbounds, ubounds)
        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        call writers(root_slot(slot))%add_c64_3(joined_path(slot, dataset), value, status)
        call require_ok(status)
        call add_bounds_attributes(slot, dataset, lbounds, ubounds)
        call add_common_attributes(slot, dataset, comment, unit)
        call add_accuracy_attribute(slot, dataset, accuracy)
    end subroutine h5_add_complex_3

    subroutine h5_add_double_1(h5id, dataset, value, lbounds, ubounds, &
            comment, unit, accuracy)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), intent(in) :: value(:)
        integer, intent(in) :: lbounds(:), ubounds(:)
        character(len=*), intent(in), optional :: comment, unit
        real(real64), intent(in), optional :: accuracy

        call require_bounds(shape(value), lbounds, ubounds)
        call add_double_1(h5id, dataset, value)
        call add_bounds_attributes(require_mode(h5id, MODE_WRITE), dataset, lbounds, ubounds)
        call add_common_attributes(require_mode(h5id, MODE_WRITE), dataset, comment, unit)
        call add_accuracy_attribute(require_mode(h5id, MODE_WRITE), dataset, accuracy)
    end subroutine h5_add_double_1

    subroutine h5_add_float_1(h5id, dataset, value, lbounds, ubounds, comment, unit)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real32), intent(in) :: value(:)
        integer, intent(in) :: lbounds(:), ubounds(:)
        character(len=*), intent(in), optional :: comment, unit
        type(fortio_status_t) :: status
        integer :: slot

        call require_bounds(shape(value), lbounds, ubounds)
        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        call writers(root_slot(slot))%add_r32_1(joined_path(slot, dataset), value, status)
        call require_ok(status)
        call add_bounds_attributes(slot, dataset, lbounds, ubounds)
        call add_common_attributes(slot, dataset, comment, unit)
    end subroutine h5_add_float_1

    subroutine h5_add_double_1_nobounds(h5id, dataset, value, comment, unit, default, &
            accuracy)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), allocatable, intent(in) :: value(:)
        character(len=*), intent(in), optional :: comment, unit
        real(real64), intent(in), optional :: default, accuracy

        if (allocated(value)) then
            call add_double_1(h5id, dataset, value)
            call add_bounds_attributes(require_mode(h5id, MODE_WRITE), dataset, &
                lbound(value), ubound(value))
            call add_common_attributes(require_mode(h5id, MODE_WRITE), dataset, comment, unit)
            call add_accuracy_attribute(require_mode(h5id, MODE_WRITE), dataset, accuracy)
        else
            if (present(default)) then
                call h5_add_double_0(h5id, dataset, default)
            else
                call h5_add_double_0(h5id, dataset, 0.0_real64)
            end if
            call add_common_attributes(require_mode(h5id, MODE_WRITE), dataset, &
                "value not allocated")
        end if
    end subroutine h5_add_double_1_nobounds

    subroutine add_double_1(h5id, dataset, value)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), intent(in) :: value(:)
        type(fortio_status_t) :: status
        integer :: slot

        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        call writers(root_slot(slot))%add_r64_1(joined_path(slot, dataset), value, status)
        call require_ok(status)
    end subroutine add_double_1

    subroutine h5_add_double_2(h5id, dataset, value, lbounds, ubounds, comment, unit, accuracy)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), intent(in) :: value(:, :)
        integer, intent(in) :: lbounds(:), ubounds(:)
        character(len=*), intent(in), optional :: comment, unit
        real(real64), intent(in), optional :: accuracy
        type(fortio_status_t) :: status
        integer :: slot

        call require_bounds(shape(value), lbounds, ubounds)
        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        call writers(root_slot(slot))%add_r64_2(joined_path(slot, dataset), value, status)
        call require_ok(status)
        call add_bounds_attributes(slot, dataset, lbounds, ubounds)
        call add_common_attributes(slot, dataset, comment, unit)
        call add_accuracy_attribute(slot, dataset, accuracy)
    end subroutine h5_add_double_2

    subroutine h5_add_double_3(h5id, dataset, value, lbounds, ubounds, comment, unit, accuracy)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), intent(in) :: value(:, :, :)
        integer, intent(in) :: lbounds(:), ubounds(:)
        character(len=*), intent(in), optional :: comment, unit
        real(real64), intent(in), optional :: accuracy
        type(fortio_status_t) :: status
        integer :: slot

        call require_bounds(shape(value), lbounds, ubounds)
        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        call writers(root_slot(slot))%add_r64_3(joined_path(slot, dataset), value, status)
        call require_ok(status)
        call add_bounds_attributes(slot, dataset, lbounds, ubounds)
        call add_common_attributes(slot, dataset, comment, unit)
        call add_accuracy_attribute(slot, dataset, accuracy)
    end subroutine h5_add_double_3

    subroutine h5_add_double_4(h5id, dataset, value, lbounds, ubounds, comment, unit, accuracy)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), intent(in) :: value(:, :, :, :)
        integer, intent(in) :: lbounds(:), ubounds(:)
        character(len=*), intent(in), optional :: comment, unit
        real(real64), intent(in), optional :: accuracy
        type(fortio_status_t) :: status
        integer :: slot

        call require_bounds(shape(value), lbounds, ubounds)
        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        call writers(root_slot(slot))%add_r64_4(joined_path(slot, dataset), value, status)
        call require_ok(status)
        call add_bounds_attributes(slot, dataset, lbounds, ubounds)
        call add_common_attributes(slot, dataset, comment, unit)
        call add_accuracy_attribute(slot, dataset, accuracy)
    end subroutine h5_add_double_4

    subroutine h5_add_double_5(h5id, dataset, value, lbounds, ubounds, comment, unit, accuracy)
        integer(HID_T), intent(in) :: h5id
        character(len=*), intent(in) :: dataset
        real(real64), intent(in) :: value(:, :, :, :, :)
        integer, intent(in) :: lbounds(:), ubounds(:)
        character(len=*), intent(in), optional :: comment, unit
        real(real64), intent(in), optional :: accuracy
        type(fortio_status_t) :: status
        integer :: slot

        call require_bounds(shape(value), lbounds, ubounds)
        slot = require_mode(h5id, MODE_WRITE)
        call prepare_overwrite(slot, dataset)
        call writers(root_slot(slot))%add_r64_5(joined_path(slot, dataset), value, status)
        call require_ok(status)
        call add_bounds_attributes(slot, dataset, lbounds, ubounds)
        call add_common_attributes(slot, dataset, comment, unit)
        call add_accuracy_attribute(slot, dataset, accuracy)
    end subroutine h5_add_double_5

    integer function allocate_handle() result(slot)
        call handle_table_lock()
        slot = first_free_slot()
        if (slot == 0) then
            call handle_table_unlock()
            error stop "fortio hdf5_tools open-handle table is full"
        end if
        in_use(slot) = .true.
        call handle_table_unlock()
    end function allocate_handle

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

    integer function require_mode(h5id, mode) result(slot)
        integer(HID_T), intent(in) :: h5id
        integer, intent(in) :: mode

        slot = require_id(h5id)
        if (mode == MODE_UNLIMITED) call attach_unlimited_buffer(slot)
        if (handle_mode(slot) /= mode) error stop "fortio HDF5 identifier has wrong mode"
    end function require_mode

    integer function require_readable(h5id) result(slot)
        integer(HID_T), intent(in) :: h5id
        integer :: root

        slot = require_id(h5id)
        root = root_slot(slot)
        if (handle_mode(slot) == MODE_READ) return
        if (handle_mode(slot) == MODE_WRITE) then
            if (read_shadow_open(root)) return
        end if
        error stop "fortio HDF5 identifier is not readable"
    end function require_readable

    subroutine attach_unlimited_buffer(slot)
        integer, intent(in) :: slot
        integer :: candidate, root

        root = root_slot(slot)
        if (root > 0) then
            if (in_use(root)) return
        end if

        root = 0
        call handle_table_lock()
        do candidate = 1, MAX_OPEN_FILES
            if (.not. in_use(candidate)) cycle
            if (.not. root_handle(candidate)) cycle
            if (handle_mode(candidate) /= MODE_WRITE) cycle
            if (writers(candidate)%path /= trim(unlimited_buffers(slot)%file_path)) cycle
            root = candidate
            root_slot(slot) = candidate
            exit
        end do
        call handle_table_unlock()
        if (root == 0) error stop "unlimited HDF5 dataset file is not open"
    end subroutine attach_unlimited_buffer

    subroutine set_root_handle(slot, mode)
        integer, intent(in) :: slot, mode

        call handle_table_lock()
        handle_mode(slot) = mode
        root_slot(slot) = slot
        root_handle(slot) = .true.
        read_shadow_open(slot) = .false.
        handle_prefix(slot) = ""
        call handle_table_unlock()
    end subroutine set_root_handle

    subroutine set_group_handle(slot, root, mode, prefix)
        integer, intent(in) :: slot, root, mode
        character(len=*), intent(in) :: prefix
        character(len=:), allocatable :: clean_prefix

        call handle_table_lock()
        clean_prefix = trim(adjustl(prefix))
        do while (len(clean_prefix) > 0)
            if (clean_prefix(1:1) /= "/") exit
            clean_prefix = clean_prefix(2:)
        end do
        do while (len(clean_prefix) > 0)
            if (clean_prefix(len(clean_prefix):len(clean_prefix)) /= "/") exit
            clean_prefix = clean_prefix(:len(clean_prefix) - 1)
        end do

        handle_mode(slot) = mode
        root_slot(slot) = root
        root_handle(slot) = .false.
        handle_prefix(slot) = clean_prefix
        call handle_table_unlock()
    end subroutine set_group_handle

    subroutine close_root(slot)
        integer, intent(in) :: slot
        type(fortio_status_t) :: status

        if (handle_mode(slot) == MODE_READ) then
            call files(slot)%close(status)
        else if (handle_mode(slot) == MODE_WRITE) then
            call flush_unlimited_buffers(slot)
            if (read_shadow_open(slot)) then
                call files(slot)%close(status)
                call require_ok(status)
                read_shadow_open(slot) = .false.
            end if
            call writers(slot)%close(status)
        else
            error stop "invalid fortio HDF5 handle mode"
        end if
        call require_ok(status)
    end subroutine close_root

    subroutine flush_unlimited_buffers(root)
        integer, intent(in) :: root
        type(fortio_status_t) :: status
        integer :: slot

        do slot = 1, MAX_OPEN_FILES
            if (.not. in_use(slot)) cycle
            if (root_slot(slot) /= root .or. handle_mode(slot) /= MODE_UNLIMITED) cycle
            if (writers(root)%object_exists(trim(unlimited_buffers(slot)%path))) &
                call writers(root)%remove_dataset(trim(unlimited_buffers(slot)%path), status)
            select case (unlimited_buffers(slot)%type_code)
            case (UNLIMITED_INTEGER)
                call writers(root)%add_i32_1(trim(unlimited_buffers(slot)%path), &
                    unlimited_buffers(slot)%values_i32(:unlimited_buffers(slot)%used), status)
            case (UNLIMITED_DOUBLE)
                if (allocated(unlimited_buffers(slot)%matrix_r64)) then
                    call writers(root)%add_r64_2(trim(unlimited_buffers(slot)%path), &
                        unlimited_buffers(slot)%matrix_r64(:, :unlimited_buffers(slot)%used), &
                        status)
                else
                    call writers(root)%add_r64_1(trim(unlimited_buffers(slot)%path), &
                        unlimited_buffers(slot)%values_r64(:unlimited_buffers(slot)%used), &
                        status)
                end if
            end select
            call require_ok(status)
        end do
    end subroutine flush_unlimited_buffers

    subroutine invalidate_root(root)
        integer, intent(in) :: root
        integer :: slot

        call handle_table_lock()
        do slot = 1, MAX_OPEN_FILES
            if (in_use(slot)) then
                if (root_slot(slot) /= root) cycle
                if (handle_mode(slot) == MODE_UNLIMITED) then
                    root_slot(slot) = 0
                else
                    call clear_handle(slot)
                end if
            end if
        end do
        call handle_table_unlock()
    end subroutine invalidate_root

    subroutine clear_all_handles()
        integer :: slot

        do slot = 1, MAX_OPEN_FILES
            call clear_handle(slot)
        end do
    end subroutine clear_all_handles

    subroutine clear_nonpersistent_handles()
        integer :: slot

        do slot = 1, MAX_OPEN_FILES
            if (handle_mode(slot) /= MODE_UNLIMITED) call clear_handle(slot)
        end do
    end subroutine clear_nonpersistent_handles

    subroutine clear_handle(slot)
        integer, intent(in) :: slot

        call handle_table_lock()
        in_use(slot) = .false.
        handle_mode(slot) = 0
        root_slot(slot) = 0
        root_handle(slot) = .false.
        read_shadow_open(slot) = .false.
        write_lock_token(slot) = -1_c_int
        handle_prefix(slot) = ""
        unlimited_buffers(slot)%file_path = ""
        unlimited_buffers(slot)%path = ""
        unlimited_buffers(slot)%type_code = 0
        unlimited_buffers(slot)%used = 0
        unlimited_buffers(slot)%row_count = 0
        unlimited_buffers(slot)%column_limit = 0
        if (allocated(unlimited_buffers(slot)%values_i32)) &
            deallocate(unlimited_buffers(slot)%values_i32)
        if (allocated(unlimited_buffers(slot)%values_r64)) &
            deallocate(unlimited_buffers(slot)%values_r64)
        if (allocated(unlimited_buffers(slot)%matrix_r64)) &
            deallocate(unlimited_buffers(slot)%matrix_r64)
        call handle_table_unlock()
    end subroutine clear_handle

    function joined_path(slot, name) result(path)
        integer, intent(in) :: slot
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: path
        character(len=:), allocatable :: clean_name

        clean_name = trim(adjustl(name))
        do while (len(clean_name) > 0)
            if (clean_name(1:1) /= "/") exit
            clean_name = clean_name(2:)
        end do
        if (len_trim(handle_prefix(slot)) == 0) then
            path = clean_name
        else if (len(clean_name) == 0) then
            path = trim(handle_prefix(slot))
        else
            path = trim(handle_prefix(slot))//"/"//clean_name
        end if
    end function joined_path

    subroutine joined_path_into(slot, name, path)
        integer, intent(in) :: slot
        character(len=*), intent(in) :: name
        character(len=*), intent(out) :: path
        character(len=len(path)) :: clean_name
        integer :: first

        clean_name = trim(adjustl(name))
        first = 1
        do while (first <= len_trim(clean_name))
            if (clean_name(first:first) /= "/") exit
            first = first + 1
        end do
        if (len_trim(handle_prefix(slot)) == 0) then
            path = clean_name(first:)
        else if (first > len_trim(clean_name)) then
            path = trim(handle_prefix(slot))
        else
            path = trim(handle_prefix(slot))//"/"//clean_name(first:)
        end if
    end subroutine joined_path_into

    function append_path(prefix, name) result(path)
        character(len=*), intent(in) :: prefix, name
        character(len=:), allocatable :: path
        character(len=:), allocatable :: clean_prefix, clean_name

        clean_prefix = trim(adjustl(prefix))
        clean_name = trim(adjustl(name))
        if (clean_prefix == "." .or. clean_prefix == "/") clean_prefix = ""
        do while (len(clean_name) > 0)
            if (clean_name(1:1) /= "/") exit
            clean_name = clean_name(2:)
        end do
        if (len(clean_prefix) == 0) then
            path = clean_name
        else
            path = trim(clean_prefix)//"/"//clean_name
        end if
    end function append_path

    subroutine require_bounds(actual_shape, lbounds, ubounds)
        integer, intent(in) :: actual_shape(:), lbounds(:), ubounds(:)

        if (size(lbounds) /= size(actual_shape)) error stop "HDF5 lower-bound rank mismatch"
        if (size(ubounds) /= size(actual_shape)) error stop "HDF5 upper-bound rank mismatch"
        if (any(ubounds - lbounds + 1 /= actual_shape)) error stop "HDF5 bounds shape mismatch"
    end subroutine require_bounds

    subroutine prepare_overwrite(slot, dataset)
        integer, intent(in) :: slot
        character(len=*), intent(in) :: dataset
        type(fortio_status_t) :: status

        if (.not. h5overwrite) return
        call writers(root_slot(slot))%remove_dataset(joined_path(slot, dataset), status)
        call require_ok(status)
    end subroutine prepare_overwrite

    subroutine add_common_attributes(slot, dataset, comment, unit)
        integer, intent(in) :: slot
        character(len=*), intent(in) :: dataset
        character(len=*), intent(in), optional :: comment, unit
        type(fortio_status_t) :: status
        character(len=:), allocatable :: path

        path = joined_path(slot, dataset)
        if (present(comment)) then
            call writers(root_slot(slot))%add_text_attribute(path, "comment", comment, status)
            call require_ok(status)
        end if
        if (present(unit)) then
            call writers(root_slot(slot))%add_text_attribute(path, "unit", unit, status)
            call require_ok(status)
        end if
    end subroutine add_common_attributes

    subroutine add_bounds_attributes(slot, dataset, lbounds, ubounds)
        integer, intent(in) :: slot
        character(len=*), intent(in) :: dataset
        integer, intent(in) :: lbounds(:), ubounds(:)
        type(fortio_status_t) :: status
        integer(int32), allocatable :: converted(:)
        character(len=:), allocatable :: path

        path = joined_path(slot, dataset)
        converted = int(lbounds, int32)
        call writers(root_slot(slot))%add_i32_attribute(path, "lbounds", converted, status)
        call require_ok(status)
        converted = int(ubounds, int32)
        call writers(root_slot(slot))%add_i32_attribute(path, "ubounds", converted, status)
        call require_ok(status)
    end subroutine add_bounds_attributes

    subroutine add_accuracy_attribute(slot, dataset, accuracy)
        integer, intent(in) :: slot
        character(len=*), intent(in) :: dataset
        real(real64), intent(in), optional :: accuracy
        type(fortio_status_t) :: status

        if (.not. present(accuracy)) return
        call writers(root_slot(slot))%add_r64_attribute(joined_path(slot, dataset), &
            "accuracy", accuracy, status)
        call require_ok(status)
    end subroutine add_accuracy_attribute

    subroutine require_ok(status)
        type(fortio_status_t), intent(in) :: status

        if (.not. status%ok()) error stop status%message
    end subroutine require_ok

end module hdf5_tools
