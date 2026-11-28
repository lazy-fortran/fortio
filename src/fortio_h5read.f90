!> General read access to HDF5 files, following the HDF5 File Format Specification 3.0.
!>
!> The file is memory-mapped and parsed in place. Supported: superblocks 0 to 3, object
!> headers 1 and 2 with continuation blocks, shared (committed) datatypes, groups in
!> symbol-table, compact and dense (fractal heap and version-2 B-tree) storage, every
!> datatype class, fill values, compact, contiguous and chunked layouts with all five
!> chunk indexes and version-1 B-trees, the deflate, shuffle and Fletcher-32 filters,
!> compact and dense attributes, and variable-length data in the global heap.
!> Paths are absolute or relative to the root group, components separated by `/`.
!> Data are returned in the file's element order (last dimension fastest).
module fortio_h5read
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_int64_t, c_associated, c_null_ptr, &
        c_f_pointer, c_null_char
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real32, real64
    use fortio_status, only: fortio_status_t, FORTIO_EFORMAT, FORTIO_EIO, FORTIO_ENOTFOUND, &
        FORTIO_ENOTSUP, FORTIO_ETYPE, FORTIO_ESTATE
    use fortio_string, only: string_t, string, assignment(=), operator(//), operator(==), len
    use fortio_posix, only: posix_open_read, posix_close, mapped_open, mapped_close, mapped_view
    use fortio_compression, only: decompress_zlib
    use fortio_deflate, only: unshuffle_bytes
    implicit none
    private

    !> Datatype classes of the HDF5 format.
    integer, parameter, public :: H5_INTEGER = 0, H5_FLOAT = 1, H5_TIME = 2, H5_STRING = 3, &
        H5_BITFIELD = 4, H5_OPAQUE = 5, H5_COMPOUND = 6, H5_REFERENCE = 7, H5_ENUM = 8, &
        H5_VLEN = 9, H5_ARRAY = 10

    integer(int64), parameter :: UNDEFINED = -1_int64

    type, public :: h5_type_t
        integer :: class = -1
        integer :: size = 0
        logical :: little_endian = .true.
        logical :: signed = .true.
        !> String padding: 0 null-terminated, 1 null-padded, 2 space-padded.
        integer :: padding = 0
        !> True for a variable-length string (class H5_VLEN).
        logical :: vlen_string = .false.
        type(string_t), allocatable :: member_names(:)
        integer, allocatable :: member_offsets(:)
        type(h5_type_t), allocatable :: members(:)
        !> Base type of vlen, array and enum types.
        type(h5_type_t), allocatable :: base(:)
        integer(int64), allocatable :: array_dims(:)
    contains
        procedure :: is_complex => type_is_complex
    end type h5_type_t

    type :: message_t
        integer :: kind = 0
        integer :: flags = 0
        integer(int64) :: address = 0
        integer(int64) :: size = 0
    end type message_t

    type :: object_t
        type(message_t), allocatable :: messages(:)
        integer :: count = 0
    end type object_t

    type :: chunk_ref_t
        integer(int64) :: address = UNDEFINED
        integer(int64) :: size = 0
        integer :: mask = 0
        integer(int64), allocatable :: offset(:)
    end type chunk_ref_t

    type :: group_cache_t
        integer(int64) :: address = UNDEFINED
        type(string_t), allocatable :: names(:)
        integer(int64), allocatable :: targets(:)
    end type group_cache_t

    type :: attribute_t
        type(string_t) :: name
        type(h5_type_t) :: type
        integer(int64), allocatable :: dims(:)
        integer(int8), allocatable :: data(:)
    end type attribute_t

    type :: dataset_t
        type(h5_type_t) :: type
        integer(int64), allocatable :: dims(:)
        integer(int64), allocatable :: maxdims(:)
        integer :: layout_class = -1
        integer :: layout_version = 0
        integer(int64) :: address = UNDEFINED
        integer(int64) :: storage_size = 0
        integer(int64) :: compact_address = 0
        integer(int64), allocatable :: chunk(:)
        integer :: index_type = 0
        integer :: layout_flags = 0
        integer :: page_bits = 0
        integer(int64) :: single_size = 0
        integer :: single_mask = 0
        integer, allocatable :: filters(:)
        integer(int8), allocatable :: fill(:)
    end type dataset_t

    type, public :: h5_reader_t
        private
        integer(c_int) :: descriptor = -1_c_int
        type(c_ptr) :: mapping = c_null_ptr
        integer(int8), pointer, contiguous :: m(:) => null()
        integer(int64) :: length = 0
        integer :: offset_size = 8
        integer :: length_size = 8
        integer(int64) :: base = 0
        integer(int64) :: root = UNDEFINED
        logical :: bad = .false.
        type(string_t) :: error
        type(group_cache_t), allocatable :: groups(:)
        integer :: ngroups = 0
        integer(int64) :: gheap_address = UNDEFINED
        integer(int64), allocatable :: gheap_index(:), gheap_offset(:), gheap_size(:)
    contains
        procedure :: open => reader_open
        procedure :: close => reader_close
        procedure :: exists => reader_exists
        procedure :: is_group => reader_is_group
        procedure :: children => reader_children
        procedure :: shape => reader_shape
        procedure :: datatype => reader_datatype
        procedure :: read_raw => reader_read_raw
        procedure :: attribute_names => reader_attribute_names
        procedure :: has_attribute => reader_has_attribute
        procedure :: attribute_shape => reader_attribute_shape
        procedure :: attribute_type => reader_attribute_type
        procedure :: attribute_raw => reader_attribute_raw
        procedure, private :: read_real64 => reader_read_real64
        procedure, private :: read_int64 => reader_read_int64
        procedure, private :: read_int32 => reader_read_int32
        procedure, private :: read_complex => reader_read_complex
        procedure, private :: read_strings => reader_read_strings
        generic :: read => read_real64, read_int64, read_int32, read_complex, read_strings
        procedure, private :: attribute_real64 => reader_attribute_real64
        procedure, private :: attribute_int64 => reader_attribute_int64
        procedure, private :: attribute_strings => reader_attribute_strings
        procedure, private :: attribute_string => reader_attribute_string
        generic :: read_attribute => attribute_real64, attribute_int64, attribute_strings, &
            attribute_string
        ! internals
        procedure, private :: fail
        procedure, private :: u
        procedure, private :: bytes
        procedure, private :: signature
        procedure, private :: resolve
        procedure, private :: parse_object
        procedure, private :: object_messages
        procedure, private :: links
        procedure, private :: parse_type
        procedure, private :: parse_space
        procedure, private :: shared_target
        procedure, private :: dataset
        procedure, private :: gather_chunks
        procedure, private :: attributes
        procedure, private :: parse_attribute
        procedure, private :: fheap_object
        procedure, private :: bt2_records
        procedure, private :: gheap_object
        procedure, private :: finish
    end type h5_reader_t

    public :: decode_real64, decode_int64, decode_strings

contains

    ! ------------------------------------------------------------------ errors and bytes

    subroutine fail(this, message)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: message

        if (this%bad) return
        this%bad = .true.
        this%error = message
    end subroutine fail

    !> Move a pending error into `status`; true if there was none.
    logical function finish(this, status)
        class(h5_reader_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status

        finish = .not. this%bad
        if (this%bad) then
            call status%set(FORTIO_EFORMAT, this%error%str())
            this%bad = .false.
        else
            call status%clear()
        end if
    end function finish

    !> Unsigned little-endian integer of `n` bytes at file offset `a`; all ones gives -1.
    integer(int64) function u(this, a, n)
        class(h5_reader_t), intent(inout) :: this
        integer(int64), intent(in) :: a
        integer, intent(in) :: n
        integer :: i
        logical :: ones

        u = 0
        if (n == 0) return
        if (a < 0 .or. a + n > this%length) then
            call this%fail("HDF5 structure points outside the file")
            return
        end if
        ones = .true.
        do i = n, 1, -1
            u = ior(shiftl(u, 8), int(iand(int(this%m(a + i - 1), int32), 255), int64))
            if (this%m(a + i - 1) /= -1_int8) ones = .false.
        end do
        if (n == 8 .and. ones) u = UNDEFINED
        if (n < 8 .and. ones .and. n >= 4) then
            ! an undefined address of a 4-byte offset size
            if (n == this%offset_size) u = UNDEFINED
        end if
    end function u

    function bytes(this, a, n) result(b)
        class(h5_reader_t), intent(inout) :: this
        integer(int64), intent(in) :: a, n
        integer(int8), allocatable :: b(:)

        if (a < 0 .or. n < 0 .or. a + n > this%length) then
            call this%fail("HDF5 data points outside the file")
            allocate (b(0))
            return
        end if
        b = this%m(a:a + n - 1)
    end function bytes

    logical function signature(this, a, sig)
        class(h5_reader_t), intent(inout) :: this
        integer(int64), intent(in) :: a
        character(len=4), intent(in) :: sig
        integer :: i

        signature = .false.
        if (a < 0 .or. a + 4 > this%length) then
            call this%fail("HDF5 structure points outside the file")
            return
        end if
        do i = 1, 4
            if (this%m(a + i - 1) /= int(iachar(sig(i:i)), int8)) then
                call this%fail("HDF5 signature " // sig // " expected")
                return
            end if
        end do
        signature = .true.
    end function signature

    integer function log2_floor(x)
        integer(int64), intent(in) :: x
        integer(int64) :: y

        log2_floor = -1
        y = x
        do while (y > 0)
            y = shiftr(y, 1)
            log2_floor = log2_floor + 1
        end do
    end function log2_floor

    !> Bytes needed for an unsigned value up to `x` in version-2 B-tree pointers.
    integer function limit_size(x)
        integer(int64), intent(in) :: x

        limit_size = log2_floor(x) / 8 + 1
    end function limit_size

    ! ------------------------------------------------------------------ open and close

    subroutine reader_open(this, path, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status
        type(c_ptr) :: address
        integer(c_int64_t) :: length
        integer(int8), pointer, contiguous :: flat(:)
        integer(int64) :: sb, a
        integer :: version
        integer(int8), parameter :: magic(8) = int([137, 72, 68, 70, 13, 10, 26, 10] &
            - merge(256, 0, [137, 72, 68, 70, 13, 10, 26, 10] > 127), int8)
        integer :: i
        logical :: found

        call this%close(status)
        this%descriptor = posix_open_read(path // c_null_char)
        if (this%descriptor < 0) then
            call status%set(FORTIO_EIO, "cannot open " // path)
            return
        end if
        this%mapping = mapped_open(this%descriptor)
        if (.not. c_associated(this%mapping)) then
            call status%set(FORTIO_EIO, "cannot map " // path)
            call this%close(status)
            call status%set(FORTIO_EIO, "cannot map " // path)
            return
        end if
        address = mapped_view(this%mapping, length)
        call c_f_pointer(address, flat, [length])
        this%m(0:length - 1) => flat
        this%length = length
        ! the superblock is at 0, 512, 1024, 2048, ...
        found = .false.
        sb = 0
        do
            if (sb + 8 > this%length) exit
            found = .true.
            do i = 1, 8
                if (this%m(sb + i - 1) /= magic(i)) found = .false.
            end do
            if (found) exit
            if (sb == 0) then
                sb = 512
            else
                sb = 2 * sb
            end if
        end do
        if (.not. found) then
            call this%close(status)
            call status%set(FORTIO_EFORMAT, path // " is not an HDF5 file")
            return
        end if
        version = int(this%u(sb + 8, 1))
        select case (version)
        case (0, 1)
            this%offset_size = int(this%u(sb + 13, 1))
            this%length_size = int(this%u(sb + 14, 1))
            a = sb + 24
            if (version == 1) a = a + 4
            this%base = this%u(a, this%offset_size)
            a = a + 4 * this%offset_size
            ! root symbol table entry: name offset, object header address
            this%root = this%u(a + this%offset_size, this%offset_size)
        case (2, 3)
            this%offset_size = int(this%u(sb + 9, 1))
            this%length_size = int(this%u(sb + 10, 1))
            a = sb + 12
            this%base = this%u(a, this%offset_size)
            this%root = this%u(a + 3 * this%offset_size, this%offset_size)
        case default
            call this%close(status)
            call status%set(FORTIO_ENOTSUP, "HDF5 superblock version is not supported")
            return
        end select
        if (this%base == UNDEFINED) this%base = sb
        if (.not. this%finish(status)) call this%close(status)
    end subroutine reader_open

    subroutine reader_close(this, status)
        class(h5_reader_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status
        integer(c_int) :: code

        if (c_associated(this%mapping)) code = mapped_close(this%mapping)
        if (this%descriptor >= 0) code = posix_close(this%descriptor)
        this%mapping = c_null_ptr
        this%descriptor = -1_c_int
        this%m => null()
        this%length = 0
        this%ngroups = 0
        if (allocated(this%groups)) deallocate (this%groups)
        this%gheap_address = UNDEFINED
        this%bad = .false.
        call status%clear()
    end subroutine reader_close

    ! ------------------------------------------------------------------ object headers

    function parse_object(this, address) result(obj)
        class(h5_reader_t), intent(inout) :: this
        integer(int64), intent(in) :: address
        type(object_t) :: obj
        integer(int64) :: a, chunk_size, cont(2, 64)
        integer :: version, nmessages, flags, ncont, k

        allocate (obj%messages(16))
        obj%count = 0
        ncont = 0
        a = this%base + address
        if (a + 4 > this%length) then
            call this%fail("object header outside the file")
            return
        end if
        if (this%m(a) == int(iachar("O"), int8)) then
            if (.not. this%signature(a, "OHDR")) return
            version = int(this%u(a + 4, 1))
            if (version /= 2) then
                call this%fail("unsupported object header version")
                return
            end if
            flags = int(this%u(a + 5, 1))
            a = a + 6
            if (btest(flags, 5)) a = a + 16
            if (btest(flags, 4)) a = a + 4
            chunk_size = this%u(a, 2**iand(flags, 3))
            a = a + 2**iand(flags, 3)
            call v2_messages(a, chunk_size, flags)
            k = 0
            do while (k < ncont)
                k = k + 1
                a = this%base + cont(1, k)
                if (.not. this%signature(a, "OCHK")) return
                call v2_messages(a + 4, cont(2, k) - 8, flags)
            end do
        else
            version = int(this%u(a, 1))
            if (version /= 1) then
                call this%fail("unsupported object header version")
                return
            end if
            nmessages = int(this%u(a + 2, 2))
            chunk_size = this%u(a + 8, 4)
            call v1_messages(a + 16, chunk_size)
            k = 0
            do while (k < ncont)
                k = k + 1
                call v1_messages(this%base + cont(1, k), cont(2, k))
            end do
        end if

    contains

        subroutine add(kind, mflags, maddr, msize)
            integer, intent(in) :: kind, mflags
            integer(int64), intent(in) :: maddr, msize
            type(message_t), allocatable :: grown(:)

            if (kind == 16) then
                if (ncont == size(cont, 2)) then
                    call this%fail("too many object header continuation blocks")
                    return
                end if
                ncont = ncont + 1
                cont(1, ncont) = this%u(maddr, this%offset_size)
                cont(2, ncont) = this%u(maddr + this%offset_size, this%length_size)
                return
            end if
            if (kind == 0) return
            if (obj%count == size(obj%messages)) then
                allocate (grown(2 * obj%count))
                grown(1:obj%count) = obj%messages(1:obj%count)
                call move_alloc(grown, obj%messages)
            end if
            obj%count = obj%count + 1
            obj%messages(obj%count) = message_t(kind, mflags, maddr, msize)
        end subroutine add

        subroutine v2_messages(start, nbytes, hflags)
            integer(int64), intent(in) :: start, nbytes
            integer, intent(in) :: hflags
            integer(int64) :: p, last, msize
            integer :: kind, mflags, header

            header = 4
            if (btest(hflags, 2)) header = 6
            p = start
            last = start + nbytes
            do while (p + header <= last)
                kind = int(this%u(p, 1))
                msize = this%u(p + 1, 2)
                mflags = int(this%u(p + 3, 1))
                if (this%bad) return
                call add(kind, mflags, p + header, msize)
                p = p + header + msize
            end do
        end subroutine v2_messages

        subroutine v1_messages(start, nbytes)
            integer(int64), intent(in) :: start, nbytes
            integer(int64) :: p, last, msize
            integer :: kind, mflags

            p = start
            last = start + nbytes
            do while (p + 8 <= last)
                kind = int(this%u(p, 2))
                msize = this%u(p + 2, 2)
                mflags = int(this%u(p + 4, 1))
                if (this%bad) return
                call add(kind, mflags, p + 8, msize)
                p = p + 8 + msize
            end do
        end subroutine v1_messages

    end function parse_object

    !> Messages of `kind` in the object, in header order.
    function object_messages(this, obj, kind) result(found)
        class(h5_reader_t), intent(inout) :: this
        type(object_t), intent(in) :: obj
        integer, intent(in) :: kind
        type(message_t), allocatable :: found(:)

        if (this%bad) then
            allocate (found(0))
            return
        end if
        found = pack(obj%messages(1:obj%count), obj%messages(1:obj%count)%kind == kind)
    end function object_messages

    !> Address of the message body that a shared message refers to.
    function shared_target(this, msg, kind) result(target_msg)
        class(h5_reader_t), intent(inout) :: this
        type(message_t), intent(in) :: msg
        integer, intent(in) :: kind
        type(message_t) :: target_msg
        type(object_t) :: other
        type(message_t), allocatable :: found(:)
        integer :: version, stype
        integer(int64) :: a

        target_msg = msg
        if (.not. btest(msg%flags, 1)) return
        version = int(this%u(msg%address, 1))
        stype = int(this%u(msg%address + 1, 1))
        if (version == 1) then
            a = this%u(msg%address + 8, this%offset_size)
        else if (stype == 2 .or. version == 2) then
            a = this%u(msg%address + 2, this%offset_size)
        else
            call this%fail("messages in the shared-message heap are not supported")
            return
        end if
        other = this%parse_object(a)
        found = this%object_messages(other, kind)
        if (size(found) == 0) then
            call this%fail("shared message target has no message of its type")
            return
        end if
        target_msg = found(1)
    end function shared_target

    ! ------------------------------------------------------------------ groups and links

    !> Names and object addresses of the links in the group at `address`.
    subroutine links(this, address, names, targets)
        class(h5_reader_t), intent(inout) :: this
        integer(int64), intent(in) :: address
        type(string_t), allocatable, intent(out) :: names(:)
        integer(int64), allocatable, intent(out) :: targets(:)
        type(object_t) :: obj
        type(message_t), allocatable :: found(:)
        type(group_cache_t), allocatable :: grown(:)
        integer(int64), allocatable :: records(:)
        integer :: i, n, record_size, nrec
        integer(int64) :: heap, btree, oa, ol

        do i = 1, this%ngroups
            if (this%groups(i)%address == address) then
                names = this%groups(i)%names
                targets = this%groups(i)%targets
                return
            end if
        end do
        allocate (names(0), targets(0))
        obj = this%parse_object(address)
        found = this%object_messages(obj, 17)
        if (size(found) > 0) call symbol_table(this%u(found(1)%address, this%offset_size), &
            this%u(found(1)%address + this%offset_size, this%offset_size))
        found = this%object_messages(obj, 6)
        do i = 1, size(found)
            call link_message(found(i)%address)
        end do
        found = this%object_messages(obj, 2)
        if (size(found) > 0) then
            n = int(this%u(found(1)%address + 1, 1))
            heap = this%u(found(1)%address + 2 + merge(8, 0, btest(n, 0)), this%offset_size)
            btree = this%u(found(1)%address + 2 + merge(8, 0, btest(n, 0)) + this%offset_size, &
                this%offset_size)
            if (heap /= UNDEFINED .and. btree /= UNDEFINED) then
                call this%bt2_records(btree, records, record_size, nrec)
                ! type-5 records: 4-byte name hash, then the heap ID
                do i = 1, nrec
                    call this%fheap_object(heap, records(i) + 4, oa, ol)
                    if (this%bad) return
                    call link_message(oa)
                end do
            end if
        end if
        if (this%bad) return
        if (.not. allocated(this%groups)) allocate (this%groups(8))
        if (this%ngroups == size(this%groups)) then
            allocate (grown(2 * this%ngroups))
            grown(1:this%ngroups) = this%groups(1:this%ngroups)
            call move_alloc(grown, this%groups)
        end if
        this%ngroups = this%ngroups + 1
        this%groups(this%ngroups)%address = address
        this%groups(this%ngroups)%names = names
        this%groups(this%ngroups)%targets = targets

    contains

        subroutine push(name, target_address)
            type(string_t), intent(in) :: name
            integer(int64), intent(in) :: target_address

            names = [names, name]
            targets = [targets, target_address]
        end subroutine push

        subroutine link_message(a)
            integer(int64), intent(in) :: a
            integer(int64) :: p, nlen
            integer :: flags, ltype, width
            character(len=:), allocatable :: name

            flags = int(this%u(a + 1, 1))
            p = a + 2
            ltype = 0
            if (btest(flags, 3)) then
                ltype = int(this%u(p, 1))
                p = p + 1
            end if
            if (btest(flags, 2)) p = p + 8
            if (btest(flags, 4)) p = p + 1
            width = 2**iand(flags, 3)
            nlen = this%u(p, width)
            p = p + width
            name = text_of(this%bytes(p, nlen))
            p = p + nlen
            if (ltype == 0) call push(string(name), this%u(p, this%offset_size))
        end subroutine link_message


        recursive subroutine symbol_table(btree_address, heap_address)
            integer(int64), intent(in) :: btree_address, heap_address
            integer(int64) :: data_segment

            if (.not. this%signature(this%base + heap_address, "HEAP")) return
            data_segment = this%u(this%base + heap_address + 8 + 2 * this%length_size, &
                this%offset_size)
            call group_node(btree_address, data_segment)
        end subroutine symbol_table

        recursive subroutine group_node(node, data_segment)
            integer(int64), intent(in) :: node, data_segment
            integer(int64) :: a, child, entry, name_offset
            integer :: level, used, k, nsym, j

            a = this%base + node
            if (.not. this%signature(a, "TREE")) return
            level = int(this%u(a + 5, 1))
            used = int(this%u(a + 6, 2))
            a = a + 8 + 2 * this%offset_size
            do k = 0, used - 1
                child = this%u(a + this%length_size + k * (this%length_size + this%offset_size), &
                    this%offset_size)
                if (level > 0) then
                    call group_node(child, data_segment)
                    cycle
                end if
                if (.not. this%signature(this%base + child, "SNOD")) return
                nsym = int(this%u(this%base + child + 6, 2))
                do j = 0, nsym - 1
                    entry = this%base + child + 8 + j * (2 * this%offset_size + 24)
                    name_offset = this%u(entry, this%offset_size)
                    call push(string(c_string(this%base + data_segment + name_offset)), &
                        this%u(entry + this%offset_size, this%offset_size))
                end do
            end do
        end subroutine group_node

        function c_string(a) result(text)
            integer(int64), intent(in) :: a
            character(len=:), allocatable :: text
            integer(int64) :: e

            e = a
            do while (e < this%length)
                if (this%m(e) == 0_int8) exit
                e = e + 1
            end do
            text = text_of(this%bytes(a, e - a))
        end function c_string

    end subroutine links

    !> Object address of `path`, or UNDEFINED.
    integer(int64) function resolve(this, path)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(string_t) :: p
        type(string_t), allocatable :: parts(:), names(:)
        integer(int64), allocatable :: targets(:)
        integer :: i, j

        resolve = this%root
        p = path
        parts = p%split("/")
        do i = 1, size(parts)
            if (parts(i) == "" .or. parts(i) == ".") cycle
            call this%links(resolve, names, targets)
            if (this%bad) then
                resolve = UNDEFINED
                return
            end if
            do j = 1, size(names)
                if (names(j) == parts(i)) exit
            end do
            if (j > size(names)) then
                resolve = UNDEFINED
                return
            end if
            resolve = targets(j)
        end do
    end function resolve

    ! ------------------------------------------------------------------ datatypes

    recursive function parse_type(this, a, used) result(t)
        class(h5_reader_t), intent(inout) :: this
        integer(int64), intent(in) :: a
        integer(int64), intent(out), optional :: used
        type(h5_type_t) :: t
        integer :: bits, version, n, i, ndims, width, nlen
        integer(int64) :: p, start, sub
        character(len=:), allocatable :: name

        start = a
        version = int(shiftr(this%u(a, 1), 4))
        t%class = int(iand(this%u(a, 1), 15_int64))
        bits = int(this%u(a + 1, 3))
        t%size = int(this%u(a + 4, 4))
        p = a + 8
        select case (t%class)
        case (H5_INTEGER, H5_BITFIELD)
            t%little_endian = .not. btest(bits, 0)
            t%signed = btest(bits, 3)
            p = p + 4
        case (H5_FLOAT)
            t%little_endian = .not. btest(bits, 0)
            if (btest(bits, 6)) call this%fail("VAX floating point is not supported")
            p = p + 12
        case (H5_TIME)
            p = p + 2
        case (H5_STRING)
            t%padding = iand(bits, 15)
        case (H5_OPAQUE)
            p = p + iand(bits, 255)
        case (H5_COMPOUND)
            n = iand(bits, 65535)
            allocate (t%member_names(n), t%member_offsets(n), t%members(n))
            do i = 1, n
                name = name_at(p)
                t%member_names(i) = name
                if (version >= 3) then
                    p = p + len(name) + 1
                    width = 1
                    if (t%size >= 256) width = 2
                    if (t%size >= 65536) width = 3
                    if (t%size >= 16777216) width = 4
                    t%member_offsets(i) = int(this%u(p, width))
                    p = p + width
                else
                    p = p + pad8(len(name) + 1)
                    t%member_offsets(i) = int(this%u(p, 4))
                    p = p + 4
                    if (version == 1) p = p + 28
                end if
                t%members(i) = this%parse_type(p, sub)
                p = p + sub
            end do
        case (H5_REFERENCE)
            t%padding = iand(bits, 15)
        case (H5_ENUM)
            n = iand(bits, 65535)
            allocate (t%base(1))
            t%base(1) = this%parse_type(p, sub)
            p = p + sub
            do i = 1, n
                nlen = len(name_at(p)) + 1
                if (version >= 3) then
                    p = p + nlen
                else
                    p = p + pad8(nlen)
                end if
            end do
            p = p + int(n, int64) * t%base(1)%size
        case (H5_VLEN)
            t%vlen_string = iand(bits, 15) == 1
            t%padding = iand(shiftr(bits, 4), 15)
            allocate (t%base(1))
            t%base(1) = this%parse_type(p, sub)
            p = p + sub
        case (H5_ARRAY)
            ndims = int(this%u(p, 1))
            allocate (t%array_dims(ndims))
            if (version < 3) then
                p = p + 4
            else
                p = p + 1
            end if
            do i = 1, ndims
                t%array_dims(i) = this%u(p, 4)
                p = p + 4
            end do
            if (version < 3) p = p + 4 * ndims
            allocate (t%base(1))
            t%base(1) = this%parse_type(p, sub)
            p = p + sub
        case default
            call this%fail("unknown HDF5 datatype class")
        end select
        if (present(used)) used = p - start

    contains

        function name_at(q) result(text)
            integer(int64), intent(in) :: q
            character(len=:), allocatable :: text
            integer(int64) :: e

            e = q
            do while (e < this%length)
                if (this%m(e) == 0_int8) exit
                e = e + 1
            end do
            text = text_of(this%bytes(q, e - q))
        end function name_at

    end function parse_type

    pure integer(int64) function pad8(n)
        integer, intent(in) :: n

        pad8 = int(8 * ((n + 7) / 8), int64)
    end function pad8

    !> True for a compound of two floating-point members, the usual complex encoding.
    elemental logical function type_is_complex(this)
        class(h5_type_t), intent(in) :: this

        type_is_complex = .false.
        if (this%class /= H5_COMPOUND) return
        if (.not. allocated(this%members)) return
        if (size(this%members) /= 2) return
        type_is_complex = this%members(1)%class == H5_FLOAT .and. &
            this%members(2)%class == H5_FLOAT .and. this%members(1)%size == this%members(2)%size
    end function type_is_complex

    !> Dimensions of a dataspace message; scalar gives size 0, null gives -1 in dims(1).
    function parse_space(this, a, maxdims) result(dims)
        class(h5_reader_t), intent(inout) :: this
        integer(int64), intent(in) :: a
        integer(int64), allocatable, intent(out), optional :: maxdims(:)
        integer(int64), allocatable :: dims(:)
        integer :: version, rank, flags, stype, i
        integer(int64) :: p

        version = int(this%u(a, 1))
        rank = int(this%u(a + 1, 1))
        flags = int(this%u(a + 2, 1))
        if (version == 1) then
            p = a + 8
            stype = 1
            if (rank == 0) stype = 0
        else
            stype = int(this%u(a + 3, 1))
            p = a + 4
        end if
        if (stype == 2) then
            dims = [-1_int64]
            if (present(maxdims)) maxdims = dims
            return
        end if
        allocate (dims(rank))
        do i = 1, rank
            dims(i) = this%u(p, this%length_size)
            p = p + this%length_size
        end do
        if (.not. present(maxdims)) return
        maxdims = dims
        if (btest(flags, 0)) then
            do i = 1, rank
                maxdims(i) = this%u(p, this%length_size)
                p = p + this%length_size
            end do
        end if
    end function parse_space

    ! ------------------------------------------------------------------ datasets

    function dataset(this, address) result(d)
        class(h5_reader_t), intent(inout) :: this
        integer(int64), intent(in) :: address
        type(dataset_t) :: d
        type(object_t) :: obj
        type(message_t), allocatable :: found(:)
        type(message_t) :: msg
        integer(int64) :: a, p
        integer :: version, rank, i, nfilters, id, nvalues, namelen, flags, width

        obj = this%parse_object(address)
        found = this%object_messages(obj, 1)
        if (size(found) == 0) then
            call this%fail("object is not a dataset")
            return
        end if
        d%dims = this%parse_space(found(1)%address, d%maxdims)
        found = this%object_messages(obj, 3)
        if (size(found) == 0) then
            call this%fail("dataset without a datatype")
            return
        end if
        msg = this%shared_target(found(1), 3)
        d%type = this%parse_type(msg%address)
        allocate (d%filters(0))
        found = this%object_messages(obj, 11)
        if (size(found) > 0) then
            a = found(1)%address
            version = int(this%u(a, 1))
            nfilters = int(this%u(a + 1, 1))
            p = a + 2
            if (version == 1) p = a + 8
            do i = 1, nfilters
                id = int(this%u(p, 2))
                namelen = 0
                if (version == 1 .or. id >= 256) then
                    namelen = int(this%u(p + 2, 2))
                    p = p + 2
                end if
                flags = int(this%u(p + 2, 2))
                nvalues = int(this%u(p + 4, 2))
                p = p + 6
                if (version == 1) then
                    p = p + pad8(namelen)
                else
                    p = p + namelen
                end if
                p = p + 4 * nvalues
                if (version == 1 .and. mod(nvalues, 2) == 1) p = p + 4
                d%filters = [d%filters, id]
                if (id /= 1 .and. id /= 2 .and. id /= 3 .and. .not. btest(flags, 0)) then
                    call this%fail("HDF5 filter is not supported")
                end if
            end do
        end if
        allocate (d%fill(0))
        found = this%object_messages(obj, 5)
        if (size(found) > 0) then
            a = found(1)%address
            version = int(this%u(a, 1))
            if (version <= 2) then
                if (this%u(a + 3, 1) /= 0 .or. version == 1) then
                    d%fill = this%bytes(a + 8, this%u(a + 4, 4))
                end if
            else
                flags = int(this%u(a + 1, 1))
                if (btest(flags, 5)) d%fill = this%bytes(a + 6, this%u(a + 2, 4))
            end if
        else
            found = this%object_messages(obj, 4)
            if (size(found) > 0) d%fill = this%bytes(found(1)%address + 4, &
                this%u(found(1)%address, 4))
        end if
        found = this%object_messages(obj, 8)
        if (size(found) == 0) then
            call this%fail("dataset without a layout")
            return
        end if
        a = found(1)%address
        version = int(this%u(a, 1))
        d%layout_version = version
        if (version < 3) then
            rank = int(this%u(a + 1, 1))
            d%layout_class = int(this%u(a + 2, 1))
            p = a + 8
            if (d%layout_class /= 0) then
                d%address = this%u(p, this%offset_size)
                p = p + this%offset_size
            end if
            allocate (d%chunk(rank))
            do i = 1, rank
                d%chunk(i) = this%u(p, 4)
                p = p + 4
            end do
            if (d%layout_class == 2) then
                d%chunk = d%chunk(1:rank - 1)
            else if (d%layout_class == 0) then
                d%storage_size = this%u(p, 4)
                d%compact_address = p + 4
            end if
            return
        end if
        d%layout_class = int(this%u(a + 1, 1))
        p = a + 2
        select case (d%layout_class)
        case (0)
            d%storage_size = this%u(p, 2)
            d%compact_address = p + 2
        case (1)
            d%address = this%u(p, this%offset_size)
            d%storage_size = this%u(p + this%offset_size, this%length_size)
        case (2)
            if (version == 3) then
                rank = int(this%u(p, 1))
                d%address = this%u(p + 1, this%offset_size)
                p = p + 1 + this%offset_size
                allocate (d%chunk(rank - 1))
                do i = 1, rank - 1
                    d%chunk(i) = this%u(p, 4)
                    p = p + 4
                end do
                d%index_type = 0
            else
                d%layout_flags = int(this%u(p, 1))
                rank = int(this%u(p + 1, 1))
                width = int(this%u(p + 2, 1))
                p = p + 3
                allocate (d%chunk(rank - 1))
                do i = 1, rank - 1
                    d%chunk(i) = this%u(p, width)
                    p = p + width
                end do
                p = p + width
                d%index_type = int(this%u(p, 1))
                p = p + 1
                select case (d%index_type)
                case (1)
                    if (btest(d%layout_flags, 1)) then
                        d%single_size = this%u(p, this%length_size)
                        d%single_mask = int(this%u(p + this%length_size, 4))
                        p = p + this%length_size + 4
                    end if
                case (3)
                    d%page_bits = int(this%u(p, 1))
                    p = p + 1
                case (4)
                    p = p + 5
                case (5)
                    p = p + 6
                end select
                d%address = this%u(p, this%offset_size)
            end if
        case default
            call this%fail("virtual HDF5 datasets are not supported")
        end select
    end function dataset

    !> All allocated chunks with their element offsets.
    subroutine gather_chunks(this, d, chunks, nchunks)
        class(h5_reader_t), intent(inout) :: this
        type(dataset_t), intent(in) :: d
        type(chunk_ref_t), allocatable, intent(out) :: chunks(:)
        integer, intent(out) :: nchunks
        integer :: rank, i, record_size, nrec, size_bytes
        integer(int64) :: nchunk_total, k, chunk_bytes
        integer(int64), allocatable :: grid(:)
        integer(int64), allocatable :: records(:)
        ! extensible-array state shared with its block readers
        integer(int64) :: ea_nelem, db_addr
        integer(int64), allocatable :: counts(:)
        integer, allocatable :: order(:)
        integer :: entry_size, page_bits, off_bytes
        logical :: filtered

        rank = size(d%chunk)
        allocate (chunks(16))
        nchunks = 0
        if (d%address == UNDEFINED) return
        allocate (grid(rank))
        do i = 1, rank
            ! fixed and implicit indexes number chunks over the maximum dimensions
            if (d%maxdims(i) == UNDEFINED) then
                grid(i) = (d%dims(i) + d%chunk(i) - 1) / d%chunk(i)
            else
                grid(i) = (d%maxdims(i) + d%chunk(i) - 1) / d%chunk(i)
            end if
        end do
        chunk_bytes = product(d%chunk) * d%type%size
        select case (d%index_type)
        case (0)
            call v1_node(d%address)
        case (1)
            if (btest(d%layout_flags, 1)) then
                call add(d%address, d%single_size, d%single_mask, [(0_int64, i=1, rank)])
            else
                call add(d%address, chunk_bytes, 0, [(0_int64, i=1, rank)])
            end if
        case (2)
            nchunk_total = product(grid)
            do k = 0, nchunk_total - 1
                call add(d%address + k * chunk_bytes, chunk_bytes, 0, scaled(k, grid) * d%chunk)
            end do
        case (3)
            call fixed_array(d%address)
        case (4)
            call extensible_array(d%address)
        case (5)
            call this%bt2_records(d%address, records, record_size, nrec)
            size_bytes = record_size - this%offset_size - 8 * rank
            if (size_bytes > 0) size_bytes = size_bytes - 4
            do k = 1, nrec
                call bt2_chunk(records(k), size_bytes)
            end do
        case default
            call this%fail("unknown chunk index type")
        end select

    contains

        subroutine add(address, nbytes, mask, offset)
            integer(int64), intent(in) :: address, nbytes
            integer, intent(in) :: mask
            integer(int64), intent(in) :: offset(:)
            type(chunk_ref_t), allocatable :: grown(:)

            if (address == UNDEFINED .or. address == 0) return
            if (nchunks == size(chunks)) then
                allocate (grown(2 * nchunks))
                grown(1:nchunks) = chunks(1:nchunks)
                call move_alloc(grown, chunks)
            end if
            nchunks = nchunks + 1
            chunks(nchunks)%address = address
            chunks(nchunks)%size = nbytes
            chunks(nchunks)%mask = mask
            chunks(nchunks)%offset = offset
        end subroutine add

        !> Chunk grid coordinates of linear index `k`, last dimension fastest.
        function scaled(k, counts) result(c)
            integer(int64), intent(in) :: k
            integer(int64), intent(in) :: counts(:)
            integer(int64) :: c(size(counts)), rest
            integer :: j

            rest = k
            do j = size(counts), 1, -1
                c(j) = mod(rest, counts(j))
                rest = rest / counts(j)
            end do
        end function scaled

        recursive subroutine v1_node(node)
            integer(int64), intent(in) :: node
            integer(int64) :: a, key, child
            integer :: level, used, j, entry_size
            integer(int64), allocatable :: offset(:)

            a = this%base + node
            if (.not. this%signature(a, "TREE")) return
            level = int(this%u(a + 5, 1))
            used = int(this%u(a + 6, 2))
            a = a + 8 + 2 * this%offset_size
            entry_size = 8 + 8 * (rank + 1) + this%offset_size
            allocate (offset(rank))
            do j = 0, used - 1
                key = a + j * entry_size
                child = this%u(key + 8 + 8 * (rank + 1), this%offset_size)
                if (level > 0) then
                    call v1_node(child)
                else
                    do i = 1, rank
                        offset(i) = this%u(key + 8 + 8 * (i - 1), 8)
                    end do
                    call add(child, this%u(key, 4), int(this%u(key + 4, 4)), offset)
                end if
                if (this%bad) return
            end do
        end subroutine v1_node

        subroutine entry(p, entry_size, filtered, k, counts, order)
            integer(int64), intent(in) :: p
            integer, intent(in) :: entry_size
            logical, intent(in) :: filtered
            integer(int64), intent(in) :: k
            integer(int64), intent(in) :: counts(:)
            integer, intent(in), optional :: order(:)
            integer(int64) :: address, nbytes, c(size(counts)), cfile(size(counts))
            integer :: mask, size_len

            address = this%u(p, this%offset_size)
            nbytes = chunk_bytes
            mask = 0
            if (filtered) then
                size_len = entry_size - this%offset_size - 4
                nbytes = this%u(p + this%offset_size, size_len)
                mask = int(this%u(p + this%offset_size + size_len, 4))
            end if
            c = scaled(k, counts)
            if (present(order)) then
                ! counts are in index order; order(j) is the file dimension of index dimension j
                cfile(order) = c
            else
                cfile = c
            end if
            call add(address, nbytes, mask, cfile * d%chunk)
        end subroutine entry

        subroutine fixed_array(header)
            integer(int64), intent(in) :: header
            integer(int64) :: a, nentries, block, p, k, page_entries, npages, prefix, pg
            integer :: entry_size, page_bits
            logical :: filtered

            a = this%base + header
            if (.not. this%signature(a, "FAHD")) return
            filtered = this%u(a + 5, 1) == 1
            entry_size = int(this%u(a + 6, 1))
            page_bits = int(this%u(a + 7, 1))
            nentries = this%u(a + 8, this%length_size)
            block = this%base + this%u(a + 8 + this%length_size, this%offset_size)
            if (.not. this%signature(block, "FADB")) return
            prefix = 6 + this%offset_size
            page_entries = shiftl(1_int64, page_bits)
            if (nentries <= page_entries) then
                p = block + prefix
                do k = 0, nentries - 1
                    call entry(p + k * entry_size, entry_size, filtered, k, grid)
                end do
                return
            end if
            npages = (nentries + page_entries - 1) / page_entries
            ! paged: bitmap, block checksum, then each page followed by its checksum
            p = block + prefix + (npages + 7) / 8 + 4
            do pg = 0, npages - 1
                do k = pg * page_entries, min(nentries, (pg + 1) * page_entries) - 1
                    if (.not. btest(this%u(block + prefix + pg / 8, 1), int(7 - mod(pg, 8)))) exit
                    call entry(p + (k - pg * page_entries) * entry_size, entry_size, filtered, &
                        k, grid)
                end do
                p = p + page_entries * entry_size + 4
            end do
        end subroutine fixed_array

        !> Extensible array: one unlimited dimension, which is the slowest in the index.
        subroutine extensible_array(header)
            integer(int64), intent(in) :: header
            integer(int64) :: a, ib, p, k, start, dblk_nelem, sb_addr
            integer :: max_bits, idx_elems, dblk_min, sblk_min_ptrs, nsblks, iblock_nsblks, &
                ndblk_addrs, s, j, ndblks, unlim, jj

            a = this%base + header
            if (.not. this%signature(a, "EAHD")) return
            filtered = this%u(a + 5, 1) == 1
            entry_size = int(this%u(a + 6, 1))
            max_bits = int(this%u(a + 7, 1))
            idx_elems = int(this%u(a + 8, 1))
            dblk_min = int(this%u(a + 9, 1))
            sblk_min_ptrs = int(this%u(a + 10, 1))
            page_bits = int(this%u(a + 11, 1))
            ea_nelem = this%u(a + 12 + 5 * this%length_size, this%length_size)
            ib = this%base + this%u(a + 12 + 6 * this%length_size, this%offset_size)
            if (ib == this%base + UNDEFINED) return
            if (.not. this%signature(ib, "EAIB")) return
            ! the index counts chunks with the unlimited dimension moved to the front
            unlim = 1
            do jj = 1, rank
                if (d%maxdims(jj) == UNDEFINED) unlim = jj
            end do
            order = [unlim, pack([(jj, jj=1, rank)], [(jj /= unlim, jj=1, rank)])]
            counts = grid(order)
            counts(1) = huge(1_int64)
            nsblks = 1 + (max_bits - log2_floor(int(dblk_min, int64)))
            iblock_nsblks = 2 * log2_floor(int(sblk_min_ptrs, int64))
            ndblk_addrs = 2 * (sblk_min_ptrs - 1)
            off_bytes = (max_bits + 7) / 8
            p = ib + 6 + this%offset_size
            do k = 0, min(int(idx_elems, int64), ea_nelem) - 1
                call entry(p + k * entry_size, entry_size, filtered, k, counts, order)
            end do
            p = p + int(idx_elems, int64) * entry_size
            start = idx_elems
            ! data blocks addressed from the index block, then secondary blocks
            j = 0
            do s = 0, nsblks - 1
                ndblks = 2**(s / 2)
                dblk_nelem = int(2**((s + 1) / 2), int64) * dblk_min
                if (s < iblock_nsblks) then
                    do k = 1, ndblks
                        db_addr = this%u(p + j * this%offset_size, this%offset_size)
                        j = j + 1
                        if (start >= ea_nelem) return
                        if (db_addr /= UNDEFINED) call data_block(db_addr, start, dblk_nelem)
                        start = start + dblk_nelem
                    end do
                else
                    if (s == iblock_nsblks) p = p + int(ndblk_addrs, int64) * this%offset_size
                    sb_addr = this%u(p + (s - iblock_nsblks) * this%offset_size, this%offset_size)
                    if (start >= ea_nelem) return
                    if (sb_addr /= UNDEFINED) then
                        call secondary_block(sb_addr, ndblks, start, dblk_nelem)
                    end if
                    start = start + ndblks * dblk_nelem
                end if
            end do
        end subroutine extensible_array

        subroutine data_block(block, first, count)
            integer(int64), intent(in) :: block, first, count
            integer(int64) :: q, i0, page_entries, npages, pg

            if (.not. this%signature(this%base + block, "EADB")) return
            q = this%base + block + 6 + this%offset_size + off_bytes
            page_entries = shiftl(1_int64, page_bits)
            if (count <= page_entries) then
                do i0 = 0, min(count, ea_nelem - first) - 1
                    call entry(q + i0 * entry_size, entry_size, filtered, first + i0, counts, order)
                end do
                return
            end if
            q = q + 4
            npages = count / page_entries
            do pg = 0, npages - 1
                do i0 = pg * page_entries, (pg + 1) * page_entries - 1
                    if (first + i0 >= ea_nelem) return
                    call entry(q + (i0 - pg * page_entries) * entry_size, entry_size, &
                        filtered, first + i0, counts, order)
                end do
                q = q + page_entries * entry_size + 4
            end do
        end subroutine data_block

        subroutine secondary_block(block, nblocks, first, count)
            integer(int64), intent(in) :: block, first, count
            integer, intent(in) :: nblocks
            integer(int64) :: q, bitmap_bytes
            integer :: b

            if (.not. this%signature(this%base + block, "EASB")) return
            q = this%base + block + 6 + this%offset_size + off_bytes
            bitmap_bytes = 0
            if (count > shiftl(1_int64, page_bits)) then
                bitmap_bytes = (nblocks * (count / shiftl(1_int64, page_bits)) + 7) / 8
            end if
            q = q + bitmap_bytes
            do b = 0, nblocks - 1
                db_addr = this%u(q + b * this%offset_size, this%offset_size)
                if (first + b * count >= ea_nelem) return
                if (db_addr /= UNDEFINED) call data_block(db_addr, first + b * count, count)
            end do
        end subroutine secondary_block

        subroutine bt2_chunk(r, size_len)
            integer(int64), intent(in) :: r
            integer, intent(in) :: size_len
            integer(int64) :: offset(rank), address, nbytes, q
            integer :: mask, j

            address = this%u(r, this%offset_size)
            q = r + this%offset_size
            nbytes = chunk_bytes
            mask = 0
            if (size_len > 0) then
                nbytes = this%u(q, size_len)
                mask = int(this%u(q + size_len, 4))
                q = q + size_len + 4
            end if
            do j = 1, rank
                offset(j) = this%u(q, 8) * d%chunk(j)
                q = q + 8
            end do
            call add(address, nbytes, mask, offset)
        end subroutine bt2_chunk

    end subroutine gather_chunks

    !> Element bytes of a dataset in file order, missing storage filled.
    function read_dataset(this, d) result(out)
        class(h5_reader_t), intent(inout) :: this
        type(dataset_t), intent(in) :: d
        integer(int8), allocatable :: out(:)
        integer(int64) :: nelem, nbytes, i, esize
        type(chunk_ref_t), allocatable :: chunks(:)
        integer :: nchunks, k
        integer(int8), allocatable :: raw(:)

        esize = d%type%size
        nelem = product(d%dims)
        if (size(d%dims) > 0) then
            if (d%dims(1) < 0) nelem = 0
        end if
        nbytes = nelem * esize
        allocate (out(nbytes))
        out = 0_int8
        if (size(d%fill) == esize .and. esize > 0) then
            do i = 0, nelem - 1
                out(i * esize + 1:(i + 1) * esize) = d%fill
            end do
        end if
        if (nbytes == 0) return
        select case (d%layout_class)
        case (0)
            out(1:min(nbytes, d%storage_size)) = this%bytes(d%compact_address, &
                min(nbytes, d%storage_size))
        case (1)
            if (d%address /= UNDEFINED) out = this%bytes(this%base + d%address, nbytes)
        case (2)
            call this%gather_chunks(d, chunks, nchunks)
            do k = 1, nchunks
                raw = this%bytes(this%base + chunks(k)%address, chunks(k)%size)
                if (this%bad) return
                call unfilter(raw, chunks(k)%mask)
                if (this%bad) return
                call place(raw, chunks(k)%offset)
            end do
        end select

    contains

        subroutine unfilter(b, mask)
            integer(int8), allocatable, intent(inout) :: b(:)
            integer, intent(in) :: mask
            integer(int8), allocatable :: tmp(:)
            type(fortio_status_t) :: st
            integer :: f

            do f = size(d%filters), 1, -1
                if (btest(mask, f - 1)) cycle
                select case (d%filters(f))
                case (1)
                    call decompress_zlib(b, tmp, st)
                    if (.not. st%ok()) then
                        call this%fail("deflate: " // st%message)
                        return
                    end if
                    call move_alloc(tmp, b)
                case (2)
                    call unshuffle_bytes(b, int(esize), tmp)
                    call move_alloc(tmp, b)
                case (3)
                    b = b(1:size(b) - 4)
                end select
            end do
        end subroutine unfilter

        !> Copy a chunk into `out`, clipped at the dataset edges.
        subroutine place(b, offset)
            integer(int8), intent(in) :: b(:)
            integer(int64), intent(in) :: offset(:)
            integer :: r, j
            integer(int64) :: idx(size(offset)), run, src, dst, stride_c, stride_d

            r = size(offset)
            if (r == 0) return
            run = min(d%chunk(r), d%dims(r) - offset(r))
            if (run <= 0) return
            idx = 0
            do
                if (all(offset(1:r - 1) + idx(1:r - 1) < d%dims(1:r - 1))) then
                    src = 0
                    dst = 0
                    stride_c = 1
                    stride_d = 1
                    do j = r, 1, -1
                        src = src + idx(j) * stride_c
                        dst = dst + (offset(j) + idx(j)) * stride_d
                        stride_c = stride_c * d%chunk(j)
                        stride_d = stride_d * d%dims(j)
                    end do
                    if ((src + run) * esize <= size(b)) then
                        out(dst * esize + 1:(dst + run) * esize) = b(src * esize + 1:(src + run) * esize)
                    end if
                end if
                j = r - 1
                do while (j >= 1)
                    idx(j) = idx(j) + 1
                    if (idx(j) < d%chunk(j)) exit
                    idx(j) = 0
                    j = j - 1
                end do
                if (j < 1) exit
            end do
        end subroutine place

    end function read_dataset

    ! ------------------------------------------------------------------ heaps and B-trees

    !> File address and length of a fractal-heap object, from the heap ID at `id`.
    subroutine fheap_object(this, header, id, address, length)
        class(h5_reader_t), intent(inout) :: this
        integer(int64), intent(in) :: header, id
        integer(int64), intent(out) :: address, length
        integer(int64) :: a, start_size, max_direct, root, offset, max_heap_bits, max_managed
        integer :: id_type, width, root_rows, offset_bytes, length_bytes, filter_len, id0

        address = UNDEFINED
        length = 0
        a = this%base + header
        if (.not. this%signature(a, "FRHP")) return
        filter_len = int(this%u(a + 7, 2))
        max_managed = this%u(a + 10, 4)
        a = a + 14 + 10 * this%length_size + 2 * this%offset_size
        width = int(this%u(a, 2))
        start_size = this%u(a + 2, this%length_size)
        max_direct = this%u(a + 2 + this%length_size, this%length_size)
        max_heap_bits = this%u(a + 2 + 2 * this%length_size, 2)
        root = this%u(a + 6 + 2 * this%length_size, this%offset_size)
        root_rows = int(this%u(a + 6 + 2 * this%length_size + this%offset_size, 2))
        id0 = int(this%u(id, 1))
        id_type = iand(shiftr(id0, 4), 3)
        if (id_type == 2) then
            ! tiny object stored in the ID itself
            length = iand(id0, 15) + 1
            address = id + 1
            return
        end if
        if (id_type /= 0) then
            call this%fail("huge fractal-heap objects are not supported")
            return
        end if
        if (filter_len > 0) then
            call this%fail("filtered fractal heaps are not supported")
            return
        end if
        offset_bytes = int((max_heap_bits + 7) / 8)
        length_bytes = min((log2_floor(max_direct) + 7) / 8, limit_size(max_managed))
        offset = this%u(id + 1, offset_bytes)
        length = this%u(id + 1 + offset_bytes, length_bytes)
        if (root_rows == 0) then
            call direct(root, offset)
        else
            call indirect(root, root_rows, 0_int64)
        end if

    contains

        integer(int64) function row_size(r)
            integer, intent(in) :: r

            if (r <= 1) then
                row_size = start_size
            else
                row_size = start_size * shiftl(1_int64, r - 1)
            end if
        end function row_size

        subroutine direct(block, local)
            integer(int64), intent(in) :: block, local

            if (.not. this%signature(this%base + block, "FHDB")) return
            address = this%base + block + local
        end subroutine direct

        recursive subroutine indirect(block, nrows, block_offset)
            integer(int64), intent(in) :: block, block_offset
            integer, intent(in) :: nrows
            integer(int64) :: q, off, child
            integer :: r, c, max_direct_rows, slot, child_rows

            q = this%base + block
            if (.not. this%signature(q, "FHIB")) return
            q = q + 5 + this%offset_size + offset_bytes
            max_direct_rows = log2_floor(max_direct) - log2_floor(start_size) + 2
            off = block_offset
            slot = 0
            do r = 0, nrows - 1
                do c = 0, width - 1
                    if (offset >= off .and. offset < off + row_size(r)) then
                        child = this%u(q + int(slot, int64) * this%offset_size, this%offset_size)
                        if (r < max_direct_rows) then
                            call direct(child, offset - off)
                        else
                            child_rows = log2_floor(row_size(r)) - &
                                log2_floor(start_size * width) + 1
                            call indirect(child, child_rows, off)
                        end if
                        return
                    end if
                    off = off + row_size(r)
                    slot = slot + 1
                end do
            end do
            call this%fail("fractal-heap offset outside the heap")
        end subroutine indirect

    end subroutine fheap_object

    !> File addresses of all records of a version-2 B-tree, in key order.
    subroutine bt2_records(this, header, records, record_size, nrec)
        class(h5_reader_t), intent(inout) :: this
        integer(int64), intent(in) :: header
        integer(int64), allocatable, intent(out) :: records(:)
        integer, intent(out) :: record_size, nrec
        integer(int64) :: a, node_size, root, total
        integer :: depth, root_nrec, d, max_nrec_size
        integer(int64), allocatable :: max_nrec(:), cum_max(:)
        integer, allocatable :: cum_size(:)

        allocate (records(0))
        nrec = 0
        record_size = 0
        a = this%base + header
        if (.not. this%signature(a, "BTHD")) return
        node_size = this%u(a + 6, 4)
        record_size = int(this%u(a + 10, 2))
        depth = int(this%u(a + 12, 2))
        root = this%u(a + 16, this%offset_size)
        root_nrec = int(this%u(a + 16 + this%offset_size, 2))
        total = this%u(a + 18 + this%offset_size, this%length_size)
        if (root == UNDEFINED .or. total == 0 .or. record_size == 0) return
        allocate (max_nrec(0:depth), cum_max(0:depth), cum_size(0:depth))
        max_nrec(0) = (node_size - 10) / record_size
        cum_max(0) = max_nrec(0)
        cum_size(0) = 0
        max_nrec_size = limit_size(max_nrec(0))
        do d = 1, depth
            max_nrec(d) = (node_size - (10 + this%offset_size + max_nrec_size + cum_size(d - 1))) &
                / (record_size + this%offset_size + max_nrec_size + cum_size(d - 1))
            cum_max(d) = (max_nrec(d) + 1) * cum_max(d - 1) + max_nrec(d)
            cum_size(d) = limit_size(cum_max(d))
        end do
        deallocate (records)
        allocate (records(total))
        call node(root, root_nrec, depth)

    contains

        recursive subroutine node(address, n, level)
            integer(int64), intent(in) :: address
            integer, intent(in) :: n, level
            integer(int64) :: q, ptr
            integer :: i, child_n, psize

            q = this%base + address
            if (level == 0) then
                if (.not. this%signature(q, "BTLF")) return
                do i = 0, n - 1
                    call take(q + 6 + int(i, int64) * record_size)
                end do
                return
            end if
            if (.not. this%signature(q, "BTIN")) return
            q = q + 6
            psize = this%offset_size + max_nrec_size + cum_size(level - 1)
            ptr = q + int(n, int64) * record_size
            do i = 0, n
                child_n = int(this%u(ptr + int(i, int64) * psize + this%offset_size, max_nrec_size))
                call node(this%u(ptr + int(i, int64) * psize, this%offset_size), child_n, level - 1)
                if (this%bad) return
                if (i < n) call take(q + int(i, int64) * record_size)
            end do
        end subroutine node

        subroutine take(q)
            integer(int64), intent(in) :: q

            if (nrec >= size(records)) return
            nrec = nrec + 1
            records(nrec) = q
        end subroutine take

    end subroutine bt2_records

    !> Object `index` of the global heap collection at `collection`.
    function gheap_object(this, collection, index) result(obj)
        class(h5_reader_t), intent(inout) :: this
        integer(int64), intent(in) :: collection, index
        integer(int8), allocatable :: obj(:)
        integer(int64) :: a, p, total, idx, osize
        integer :: i

        if (this%gheap_address /= collection) then
            a = this%base + collection
            if (.not. this%signature(a, "GCOL")) then
                allocate (obj(0))
                return
            end if
            total = this%u(a + 8, this%length_size)
            if (allocated(this%gheap_index)) deallocate (this%gheap_index, this%gheap_offset, &
                this%gheap_size)
            allocate (this%gheap_index(0), this%gheap_offset(0), this%gheap_size(0))
            p = a + 8 + this%length_size
            do while (p + 8 + this%length_size <= a + total)
                idx = this%u(p, 2)
                if (idx == 0) exit
                osize = this%u(p + 8, this%length_size)
                this%gheap_index = [this%gheap_index, idx]
                this%gheap_offset = [this%gheap_offset, p + 8 + this%length_size]
                this%gheap_size = [this%gheap_size, osize]
                p = p + 8 + this%length_size + pad8(int(osize))
            end do
            this%gheap_address = collection
        end if
        do i = 1, size(this%gheap_index)
            if (this%gheap_index(i) == index) then
                obj = this%bytes(this%gheap_offset(i), this%gheap_size(i))
                return
            end if
        end do
        call this%fail("global heap object not found")
        allocate (obj(0))
    end function gheap_object

    ! ------------------------------------------------------------------ attributes

    function parse_attribute(this, a) result(attr)
        class(h5_reader_t), intent(inout) :: this
        integer(int64), intent(in) :: a
        type(attribute_t) :: attr
        integer :: version, flags, name_size, type_size, space_size
        integer(int64) :: p, nelem
        type(message_t) :: msg

        version = int(this%u(a, 1))
        flags = int(this%u(a + 1, 1))
        name_size = int(this%u(a + 2, 2))
        type_size = int(this%u(a + 4, 2))
        space_size = int(this%u(a + 6, 2))
        p = a + 8
        if (version == 3) p = p + 1
        attr%name = text_of(this%bytes(p, int(max(name_size - 1, 0), int64)))
        if (version == 1) then
            p = p + pad8(name_size)
        else
            p = p + name_size
        end if
        if (btest(flags, 0)) then
            msg = this%shared_target(message_t(3, 2, p, int(type_size, int64)), 3)
            attr%type = this%parse_type(msg%address)
        else
            attr%type = this%parse_type(p)
        end if
        if (version == 1) then
            p = p + pad8(type_size)
        else
            p = p + type_size
        end if
        attr%dims = this%parse_space(p)
        if (version == 1) then
            p = p + pad8(space_size)
        else
            p = p + space_size
        end if
        nelem = product(attr%dims)
        if (size(attr%dims) > 0) then
            if (attr%dims(1) < 0) nelem = 0
        end if
        attr%data = this%bytes(p, nelem * attr%type%size)
    end function parse_attribute

    function attributes(this, address) result(attrs)
        class(h5_reader_t), intent(inout) :: this
        integer(int64), intent(in) :: address
        type(attribute_t), allocatable :: attrs(:)
        type(object_t) :: obj
        type(message_t), allocatable :: found(:)
        integer(int64), allocatable :: records(:)
        integer :: i, record_size, nrec, flags
        integer(int64) :: heap, btree, oa, ol

        allocate (attrs(0))
        obj = this%parse_object(address)
        found = this%object_messages(obj, 12)
        do i = 1, size(found)
            attrs = [attrs, this%parse_attribute(found(i)%address)]
        end do
        found = this%object_messages(obj, 21)
        if (size(found) == 0) return
        flags = int(this%u(found(1)%address + 1, 1))
        heap = this%u(found(1)%address + 2 + merge(2, 0, btest(flags, 0)), this%offset_size)
        btree = this%u(found(1)%address + 2 + merge(2, 0, btest(flags, 0)) + this%offset_size, &
            this%offset_size)
        if (heap == UNDEFINED .or. btree == UNDEFINED) return
        call this%bt2_records(btree, records, record_size, nrec)
        do i = 1, nrec
            ! type-8 records start with the heap ID of the attribute message
            call this%fheap_object(heap, records(i), oa, ol)
            if (this%bad) return
            attrs = [attrs, this%parse_attribute(oa)]
        end do
    end function attributes

    ! ------------------------------------------------------------------ public queries

    logical function reader_exists(this, path)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path

        reader_exists = this%resolve(path) /= UNDEFINED
        this%bad = .false.
    end function reader_exists

    logical function reader_is_group(this, path)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int64) :: address
        type(object_t) :: obj
        type(message_t), allocatable :: found(:)

        reader_is_group = .false.
        address = this%resolve(path)
        if (address == UNDEFINED) return
        obj = this%parse_object(address)
        found = this%object_messages(obj, 1)
        reader_is_group = size(found) == 0
        found = this%object_messages(obj, 3)
        if (size(found) > 0) reader_is_group = .false.
        this%bad = .false.
    end function reader_is_group

    subroutine reader_children(this, path, names, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(string_t), allocatable, intent(out) :: names(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int64), allocatable :: targets(:)
        integer(int64) :: address

        allocate (names(0))
        address = this%resolve(path)
        if (address == UNDEFINED) then
            if (this%finish(status)) call status%set(FORTIO_ENOTFOUND, path // " not found")
            return
        end if
        call this%links(address, names, targets)
        if (this%finish(status)) continue
    end subroutine reader_children

    !> Dataset dimensions in file order; empty for a scalar.
    subroutine reader_shape(this, path, dims, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int64), allocatable, intent(out) :: dims(:)
        type(fortio_status_t), intent(inout) :: status
        type(dataset_t) :: d

        allocate (dims(0))
        if (.not. locate(this, path, d, status)) return
        dims = d%dims
    end subroutine reader_shape

    subroutine reader_datatype(this, path, t, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(h5_type_t), intent(out) :: t
        type(fortio_status_t), intent(inout) :: status
        type(dataset_t) :: d

        if (.not. locate(this, path, d, status)) return
        t = d%type
    end subroutine reader_datatype

    logical function locate(this, path, d, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(dataset_t), intent(out) :: d
        type(fortio_status_t), intent(inout) :: status
        integer(int64) :: address

        locate = .false.
        address = this%resolve(path)
        if (address == UNDEFINED) then
            if (this%finish(status)) call status%set(FORTIO_ENOTFOUND, path // " not found")
            return
        end if
        d = this%dataset(address)
        locate = this%finish(status)
    end function locate

    !> Element bytes in file order, with the datatype and dimensions.
    subroutine reader_read_raw(this, path, raw, t, dims, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int8), allocatable, intent(out) :: raw(:)
        type(h5_type_t), intent(out) :: t
        integer(int64), allocatable, intent(out) :: dims(:)
        type(fortio_status_t), intent(inout) :: status
        type(dataset_t) :: d

        allocate (raw(0), dims(0))
        if (.not. locate(this, path, d, status)) return
        raw = read_dataset(this, d)
        t = d%type
        dims = d%dims
        if (this%finish(status)) continue
    end subroutine reader_read_raw

    subroutine reader_read_real64(this, path, values, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8), allocatable :: raw(:)
        type(h5_type_t) :: t
        integer(int64), allocatable :: dims(:)

        call this%read_raw(path, raw, t, dims, status)
        if (.not. status%ok()) return
        call decode_real64(raw, t, values, status)
    end subroutine reader_read_real64

    subroutine reader_read_int64(this, path, values, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int64), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8), allocatable :: raw(:)
        type(h5_type_t) :: t
        integer(int64), allocatable :: dims(:)

        call this%read_raw(path, raw, t, dims, status)
        if (.not. status%ok()) return
        call decode_int64(raw, t, values, status)
    end subroutine reader_read_int64

    subroutine reader_read_int32(this, path, values, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int32), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int64), allocatable :: wide(:)

        call this%read_int64(path, wide, status)
        if (.not. status%ok()) return
        if (any(wide > huge(1_int32) .or. wide < -huge(1_int32) - 1)) then
            call status%set(FORTIO_ETYPE, path // " does not fit 32-bit integers")
            return
        end if
        values = int(wide, int32)
    end subroutine reader_read_int32

    subroutine reader_read_complex(this, path, values, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        complex(real64), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8), allocatable :: raw(:)
        type(h5_type_t) :: t
        integer(int64), allocatable :: dims(:)
        real(real64), allocatable :: re(:), im(:)
        integer(int64) :: n, i, esize
        integer(int8), allocatable :: part(:)

        call this%read_raw(path, raw, t, dims, status)
        if (.not. status%ok()) return
        if (.not. t%is_complex()) then
            call status%set(FORTIO_ETYPE, path // " is not complex")
            return
        end if
        esize = t%size
        n = size(raw) / esize
        allocate (part(n * t%members(1)%size))
        do i = 0, n - 1
            part(i * t%members(1)%size + 1:(i + 1) * t%members(1)%size) = &
                raw(i * esize + t%member_offsets(1) + 1:i * esize + t%member_offsets(1) + t%members(1)%size)
        end do
        call decode_real64(part, t%members(1), re, status)
        do i = 0, n - 1
            part(i * t%members(2)%size + 1:(i + 1) * t%members(2)%size) = &
                raw(i * esize + t%member_offsets(2) + 1:i * esize + t%member_offsets(2) + t%members(2)%size)
        end do
        call decode_real64(part, t%members(2), im, status)
        values = cmplx(re, im, real64)
    end subroutine reader_read_complex

    subroutine reader_read_strings(this, path, values, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(string_t), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8), allocatable :: raw(:)
        type(h5_type_t) :: t
        integer(int64), allocatable :: dims(:)

        call this%read_raw(path, raw, t, dims, status)
        if (.not. status%ok()) return
        call decode_strings(this, raw, t, values, status)
    end subroutine reader_read_strings

    ! attributes ------------------------------------------------------------------

    logical function attribute_of(this, path, name, attr, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path, name
        type(attribute_t), intent(out) :: attr
        type(fortio_status_t), intent(inout) :: status
        type(attribute_t), allocatable :: attrs(:)
        integer(int64) :: address
        integer :: i

        attribute_of = .false.
        address = this%resolve(path)
        if (address == UNDEFINED) then
            if (this%finish(status)) call status%set(FORTIO_ENOTFOUND, path // " not found")
            return
        end if
        attrs = this%attributes(address)
        if (.not. this%finish(status)) return
        do i = 1, size(attrs)
            if (attrs(i)%name == name) then
                attr = attrs(i)
                attribute_of = .true.
                return
            end if
        end do
        call status%set(FORTIO_ENOTFOUND, "attribute " // name // " of " // path // " not found")
    end function attribute_of

    subroutine reader_attribute_names(this, path, names, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(string_t), allocatable, intent(out) :: names(:)
        type(fortio_status_t), intent(inout) :: status
        type(attribute_t), allocatable :: attrs(:)
        integer(int64) :: address
        integer :: i

        allocate (names(0))
        address = this%resolve(path)
        if (address == UNDEFINED) then
            if (this%finish(status)) call status%set(FORTIO_ENOTFOUND, path // " not found")
            return
        end if
        attrs = this%attributes(address)
        if (.not. this%finish(status)) return
        deallocate (names)
        allocate (names(size(attrs)))
        do i = 1, size(attrs)
            names(i) = attrs(i)%name
        end do
    end subroutine reader_attribute_names

    logical function reader_has_attribute(this, path, name)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path, name
        type(attribute_t) :: attr
        type(fortio_status_t) :: status

        reader_has_attribute = attribute_of(this, path, name, attr, status)
    end function reader_has_attribute

    subroutine reader_attribute_shape(this, path, name, dims, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path, name
        integer(int64), allocatable, intent(out) :: dims(:)
        type(fortio_status_t), intent(inout) :: status
        type(attribute_t) :: attr

        allocate (dims(0))
        if (attribute_of(this, path, name, attr, status)) dims = attr%dims
    end subroutine reader_attribute_shape

    subroutine reader_attribute_type(this, path, name, t, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path, name
        type(h5_type_t), intent(out) :: t
        type(fortio_status_t), intent(inout) :: status
        type(attribute_t) :: attr

        if (attribute_of(this, path, name, attr, status)) t = attr%type
    end subroutine reader_attribute_type

    subroutine reader_attribute_raw(this, path, name, raw, t, dims, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path, name
        integer(int8), allocatable, intent(out) :: raw(:)
        type(h5_type_t), intent(out) :: t
        integer(int64), allocatable, intent(out) :: dims(:)
        type(fortio_status_t), intent(inout) :: status
        type(attribute_t) :: attr

        allocate (raw(0), dims(0))
        if (.not. attribute_of(this, path, name, attr, status)) return
        raw = attr%data
        t = attr%type
        dims = attr%dims
    end subroutine reader_attribute_raw

    subroutine reader_attribute_real64(this, path, name, values, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path, name
        real(real64), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(attribute_t) :: attr

        allocate (values(0))
        if (.not. attribute_of(this, path, name, attr, status)) return
        call decode_real64(attr%data, attr%type, values, status)
    end subroutine reader_attribute_real64

    subroutine reader_attribute_int64(this, path, name, values, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path, name
        integer(int64), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(attribute_t) :: attr

        allocate (values(0))
        if (.not. attribute_of(this, path, name, attr, status)) return
        call decode_int64(attr%data, attr%type, values, status)
    end subroutine reader_attribute_int64

    subroutine reader_attribute_strings(this, path, name, values, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path, name
        type(string_t), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(attribute_t) :: attr

        allocate (values(0))
        if (.not. attribute_of(this, path, name, attr, status)) return
        call decode_strings(this, attr%data, attr%type, values, status)
    end subroutine reader_attribute_strings

    !> A text attribute as one string (the first element of a string array).
    subroutine reader_attribute_string(this, path, name, value, status)
        class(h5_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path, name
        type(string_t), intent(out) :: value
        type(fortio_status_t), intent(inout) :: status
        type(string_t), allocatable :: values(:)

        call this%attribute_strings(path, name, values, status)
        if (.not. status%ok()) return
        if (size(values) == 0) then
            value = ""
        else
            value = values(1)
        end if
    end subroutine reader_attribute_string

    ! ------------------------------------------------------------------ decoding

    pure integer(int64) function le(b)
        integer(int8), intent(in) :: b(:)
        integer :: i

        le = 0
        do i = size(b), 1, -1
            le = ior(shiftl(le, 8), int(iand(int(b(i), int32), 255), int64))
        end do
    end function le

    pure function text_of(b) result(text)
        integer(int8), intent(in) :: b(:)
        character(len=:), allocatable :: text
        integer :: i

        allocate (character(len=size(b)) :: text)
        do i = 1, size(b)
            text(i:i) = achar(iand(int(b(i), int32), 255))
        end do
    end function text_of

    !> Element bytes in native order.
    pure function native(b, little_endian) result(out)
        integer(int8), intent(in) :: b(:)
        logical, intent(in) :: little_endian
        integer(int8) :: out(size(b))

        if (little_endian .eqv. (transfer(1_int32, 1_int8) == 1_int8)) then
            out = b
        else
            out = b(size(b):1:-1)
        end if
    end function native

    subroutine decode_real64(raw, t, values, status)
        integer(int8), intent(in) :: raw(:)
        type(h5_type_t), intent(in) :: t
        real(real64), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int64) :: n, i
        integer(int64), allocatable :: ints(:)
        integer :: s

        call status%clear()
        s = t%size
        if (s == 0) then
            allocate (values(0))
            return
        end if
        n = size(raw) / s
        allocate (values(n))
        select case (t%class)
        case (H5_FLOAT)
            do i = 0, n - 1
                select case (s)
                case (8)
                    values(i + 1) = transfer(native(raw(i * 8 + 1:i * 8 + 8), t%little_endian), 1.0_real64)
                case (4)
                    values(i + 1) = real(transfer(native(raw(i * 4 + 1:i * 4 + 4), t%little_endian), &
                        1.0_real32), real64)
                case default
                    call status%set(FORTIO_ENOTSUP, "floating-point size is not supported")
                    return
                end select
            end do
        case (H5_INTEGER, H5_ENUM)
            call decode_int64(raw, t, ints, status)
            if (status%ok()) values = real(ints, real64)
        case default
            call status%set(FORTIO_ETYPE, "data are not numeric")
        end select
    end subroutine decode_real64

    subroutine decode_int64(raw, t, values, status)
        integer(int8), intent(in) :: raw(:)
        type(h5_type_t), intent(in) :: t
        integer(int64), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int64) :: n, i
        integer :: s
        integer(int8) :: b(8)
        logical :: signed, little

        call status%clear()
        if (t%class == H5_ENUM) then
            call decode_int64(raw, t%base(1), values, status)
            return
        end if
        if (t%class /= H5_INTEGER) then
            call status%set(FORTIO_ETYPE, "data are not integers")
            allocate (values(0))
            return
        end if
        s = t%size
        signed = t%signed
        little = t%little_endian
        n = size(raw) / s
        allocate (values(n))
        do i = 0, n - 1
            if (little) then
                b(1:s) = raw(i * s + 1:i * s + s)
            else
                b(1:s) = raw(i * s + s:i * s + 1:-1)
            end if
            values(i + 1) = le(b(1:s))
            if (signed .and. s < 8) then
                if (btest(values(i + 1), 8 * s - 1)) values(i + 1) = values(i + 1) - shiftl(1_int64, 8 * s)
            end if
        end do
    end subroutine decode_int64

    !> Fixed-length and variable-length strings; trailing padding removed.
    subroutine decode_strings(this, raw, t, values, status)
        class(h5_reader_t), intent(inout) :: this
        integer(int8), intent(in) :: raw(:)
        type(h5_type_t), intent(in) :: t
        type(string_t), allocatable, intent(out) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int64) :: n, i, collection, index, nchars
        integer :: s, last
        character(len=:), allocatable :: text
        integer(int8), allocatable :: obj(:)

        call status%clear()
        s = t%size
        if (t%class == H5_STRING) then
            n = size(raw) / s
            allocate (values(n))
            do i = 0, n - 1
                text = text_of(raw(i * s + 1:(i + 1) * s))
                values(i + 1) = unpad(text, t%padding)
            end do
        else if (t%class == H5_VLEN .and. t%vlen_string) then
            n = size(raw) / s
            allocate (values(n))
            do i = 0, n - 1
                nchars = le(raw(i * s + 1:i * s + 4))
                collection = le(raw(i * s + 5:i * s + 4 + this%offset_size))
                index = le(raw(i * s + 5 + this%offset_size:i * s + 8 + this%offset_size))
                if (nchars == 0 .or. collection == 0) then
                    values(i + 1) = ""
                    cycle
                end if
                obj = this%gheap_object(collection, index)
                if (this%bad) exit
                last = int(min(nchars, int(size(obj), int64)))
                values(i + 1) = unpad(text_of(obj(1:last)), t%padding)
            end do
            if (this%finish(status)) continue
        else
            call status%set(FORTIO_ETYPE, "data are not strings")
            allocate (values(0))
        end if
    end subroutine decode_strings

    function unpad(text, padding) result(out)
        character(len=*), intent(in) :: text
        integer, intent(in) :: padding
        type(string_t) :: out
        integer :: nul

        nul = index(text, achar(0))
        if (padding == 2) then
            out = text(1:len_trim(text))
        else if (nul > 0) then
            out = text(1:nul - 1)
        else
            out = text
        end if
    end function unpad

end module fortio_h5read
