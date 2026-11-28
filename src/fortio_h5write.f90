!> General HDF5 writer with NetCDF-4 dimension scales, without the HDF5 library.
!>
!> Raw data go to the file as they are written; groups, object headers, attributes and
!> the global heap of variable-length data are laid out when the file is closed. The
!> file uses superblock version 2 and version-2 object headers with compact link and
!> attribute storage, as HDF5 1.8 and later read them. Datasets are contiguous, or
!> chunked like netCDF-C with deflate (`set_deflate`).
!> Values are given in file element order (last dimension fastest) with their
!> dimensions; a dataset without dimensions is a scalar.
module fortio_h5write
    use, intrinsic :: iso_c_binding, only: c_f_pointer, c_loc
    use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64, real64
    use fortio_status, only: fortio_status_t, FORTIO_EEXIST, FORTIO_ENOTFOUND, FORTIO_ESTATE, &
        FORTIO_ESHAPE, FORTIO_ENOTSUP
    use fortio_string, only: string_t, string, assignment(=), operator(//), operator(==), len
    use fortio_bytes, only: byte_writer_t
    use fortio_checksum, only: lookup3_checksum
    use fortio_compression, only: compress_zlib
    use fortio_deflate, only: shuffle_bytes
    implicit none
    private

    integer(int64), parameter :: UNDEFINED = -1_int64
    integer, parameter :: GROUP = 1, DATASET = 2, NAMED_TYPE = 3, EXTERNAL_LINK = 4
    integer(int64), parameter :: SUPERBLOCK_SIZE = 48
    integer(int64), parameter :: GHEAP_MIN = 4096

    type :: wattr_t
        type(string_t) :: name
        integer(int8), allocatable :: dtype(:)
        integer(int64), allocatable :: dims(:)
        integer(int8), allocatable :: data(:)
        !> Variable-length strings, or (for a dimension list) the referenced object indices.
        type(string_t), allocatable :: vstrings(:)
        integer, allocatable :: refs(:)
        integer(int64), allocatable :: heap_ids(:, :)
    end type wattr_t

    type :: wobj_t
        integer :: kind = 0
        type(string_t) :: path
        type(string_t) :: name
        integer :: parent = 0
        integer, allocatable :: children(:)
        integer(int8), allocatable :: dtype(:)
        integer :: shared_type = 0
        integer(int64), allocatable :: dims(:)
        integer(int64) :: element_size = 0
        integer(int64) :: data_address = UNDEFINED
        !> Deflate level of a chunked dataset, 0 for contiguous storage.
        integer :: deflate = 0
        logical :: shuffle = .false.
        integer(int64) :: stored_size = 0
        !> Chunk dimensions; more than one chunk uses a fixed-array index at index_address.
        integer(int64), allocatable :: chunk(:)
        integer(int64) :: index_address = UNDEFINED
        type(string_t), allocatable :: vstrings(:)
        integer(int64), allocatable :: heap_ids(:, :)
        type(wattr_t), allocatable :: attrs(:)
        integer :: nattrs = 0
        integer(int64) :: address = 0
        integer(int64) :: header_size = 0
        !> File and object path of an external link.
        type(string_t) :: link_file, link_object
    end type wobj_t

    type, public :: h5_writer_t
        private
        type(byte_writer_t) :: out
        logical :: opened = .false.
        integer(int64) :: eof = SUPERBLOCK_SIZE
        type(wobj_t), allocatable :: objs(:)
        integer :: nobjs = 0
        integer :: next_dimid = 0
        !> Size of the user block before the superblock; file addresses are relative
        !> to the superblock.
        integer(int64) :: origin = 0
        integer :: deflate = 0
        logical :: shuffle = .false.
    contains
        procedure :: create => writer_create
        procedure :: close => writer_close
        procedure :: create_group => writer_create_group
        procedure :: external_link => writer_external_link
        procedure, private :: write_real64 => writer_write_real64
        procedure, private :: write_int32 => writer_write_int32
        procedure, private :: write_int64 => writer_write_int64
        procedure, private :: write_complex => writer_write_complex
        procedure, private :: write_strings => writer_write_strings
        generic :: write => write_real64, write_int32, write_int64, write_complex, write_strings
        procedure, private :: attr_text => writer_attr_text
        procedure, private :: attr_strings => writer_attr_strings
        procedure, private :: attr_real64 => writer_attr_real64
        procedure, private :: attr_int32 => writer_attr_int32
        procedure, private :: attr_int64 => writer_attr_int64
        generic :: attribute => attr_text, attr_strings, attr_real64, attr_int32, attr_int64
        procedure :: commit_complex => writer_commit_complex
        procedure :: dimension => writer_dimension
        procedure :: attach_scales => writer_attach_scales
        procedure :: placeholder => writer_placeholder
        procedure :: set_deflate => writer_set_deflate
        procedure, private :: object
        procedure, private :: add_object
        procedure, private :: add_dataset
        procedure, private :: add_attr
    end type h5_writer_t

contains

    ! ------------------------------------------------------------------ encoding helpers

    pure function le_bytes(value, n) result(b)
        integer(int64), intent(in) :: value
        integer, intent(in) :: n
        integer(int8) :: b(n)
        integer :: i

        do i = 1, n
            b(i) = int(ibits(value, 8 * (i - 1), 8) - merge(256, 0, ibits(value, 8 * (i - 1), 8) > 127), &
                int8)
        end do
    end function le_bytes

    pure function text_bytes(text) result(b)
        character(len=*), intent(in) :: text
        integer(int8) :: b(len(text))
        integer :: i, code

        do i = 1, len(text)
            code = iachar(text(i:i))
            if (code > 127) code = code - 256
            b(i) = int(code, int8)
        end do
    end function text_bytes

    logical function host_little()
        host_little = transfer(1_int32, 1_int8) == 1_int8
    end function host_little

    !> Datatype message of a two's-complement integer.
    function dt_int(nbytes, signed) result(b)
        integer, intent(in) :: nbytes
        logical, intent(in) :: signed
        integer(int8), allocatable :: b(:)
        integer :: bits

        bits = 0
        if (.not. host_little()) bits = 1
        if (signed) bits = ior(bits, 8)
        b = [int(16, int8), le_bytes(int(bits, int64), 3), le_bytes(int(nbytes, int64), 4), &
             le_bytes(0_int64, 2), le_bytes(int(8 * nbytes, int64), 2)]
    end function dt_int

    function dt_float64() result(b)
        integer(int8), allocatable :: b(:)
        integer :: bits

        ! mantissa normalization 2 (implied leading 1), sign at bit 63
        bits = ior(32, shiftl(63, 8))
        if (.not. host_little()) bits = ior(bits, 1)
        b = [int(17, int8), le_bytes(int(bits, int64), 3), le_bytes(8_int64, 4), &
             le_bytes(0_int64, 2), le_bytes(64_int64, 2), le_bytes(52_int64, 1), &
             le_bytes(11_int64, 1), le_bytes(0_int64, 1), le_bytes(52_int64, 1), &
             le_bytes(1023_int64, 4)]
    end function dt_float64

    !> Fixed-length null-terminated string of `n` bytes.
    function dt_fixed_string(n, utf8) result(b)
        integer(int64), intent(in) :: n
        logical, intent(in) :: utf8
        integer(int8), allocatable :: b(:)

        b = [int(19, int8), le_bytes(merge(16_int64, 0_int64, utf8), 3), le_bytes(n, 4)]
    end function dt_fixed_string

    function dt_vlen_string(utf8) result(b)
        logical, intent(in) :: utf8
        integer(int8), allocatable :: b(:)
        integer :: bits

        bits = 1
        if (utf8) bits = ior(bits, shiftl(1, 8))
        b = [int(25, int8), le_bytes(int(bits, int64), 3), le_bytes(16_int64, 4), &
             dt_int(1, .false.)]
    end function dt_vlen_string

    !> Compound {r, i} of two IEEE doubles, the NetCDF and h5py complex layout.
    function dt_complex() result(b)
        integer(int8), allocatable :: b(:)

        b = [int(54, int8), le_bytes(2_int64, 3), le_bytes(16_int64, 4), &
             text_bytes("r"), 0_int8, 0_int8, dt_float64(), &
             text_bytes("i"), 0_int8, 8_int8, dt_float64()]
    end function dt_complex

    function dt_reference() result(b)
        integer(int8), allocatable :: b(:)

        b = [int(23, int8), le_bytes(0_int64, 3), le_bytes(8_int64, 4)]
    end function dt_reference

    function dt_vlen_reference() result(b)
        integer(int8), allocatable :: b(:)

        b = [int(25, int8), le_bytes(0_int64, 3), le_bytes(16_int64, 4), dt_reference()]
    end function dt_vlen_reference

    function dataspace(dims) result(b)
        integer(int64), intent(in) :: dims(:)
        integer(int8), allocatable :: b(:)
        integer :: i, stype

        stype = 1
        if (size(dims) == 0) stype = 0
        b = [2_int8, int(size(dims), int8), 0_int8, int(stype, int8)]
        do i = 1, size(dims)
            b = [b, le_bytes(dims(i), 8)]
        end do
    end function dataspace

    function message(kind, flags, body) result(b)
        integer, intent(in) :: kind, flags
        integer(int8), intent(in) :: body(:)
        integer(int8), allocatable :: b(:)

        b = [int(kind, int8), le_bytes(int(size(body), int64), 2), int(flags, int8), body]
    end function message

    !> Number of bits of a positive value: floor(log2(x)) + 1, as HDF5 counts them.
    pure integer function bit_length(x)
        integer(int64), intent(in) :: x
        integer(int64) :: y

        bit_length = 0
        y = x
        do while (y > 0)
            y = shiftr(y, 1)
            bit_length = bit_length + 1
        end do
    end function bit_length

    ! ------------------------------------------------------------------ object table

    integer function object(this, path)
        class(h5_writer_t), intent(in) :: this
        character(len=*), intent(in) :: path
        type(string_t) :: p
        integer :: i

        p = normalized(path)
        do i = 1, this%nobjs
            if (this%objs(i)%path == p) then
                object = i
                return
            end if
        end do
        object = 0
    end function object

    function normalized(path) result(p)
        character(len=*), intent(in) :: path
        type(string_t) :: p
        type(string_t) :: s
        type(string_t), allocatable :: parts(:)
        integer :: i

        s = path
        parts = s%split("/")
        p = ""
        do i = 1, size(parts)
            if (parts(i) == "" .or. parts(i) == ".") cycle
            p = p // "/" // parts(i)
        end do
        if (p == "") p = "/"
    end function normalized

    !> Index of a new object `path` whose parent groups are created as needed.
    recursive integer function add_object(this, path, kind, status) result(idx)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer, intent(in) :: kind
        type(fortio_status_t), intent(inout) :: status
        type(string_t) :: p, parent_path
        type(wobj_t), allocatable :: grown(:)
        integer :: parent, cut

        idx = 0
        if (.not. this%opened) then
            call status%set(FORTIO_ESTATE, "HDF5 writer is not open")
            return
        end if
        p = normalized(path)
        if (this%object(p%str()) > 0) then
            call status%set(FORTIO_EEXIST, p%str() // " exists")
            return
        end if
        cut = p%rfind("/")
        parent_path = p%slice(1, cut - 1)
        if (parent_path == "") parent_path = "/"
        parent = this%object(parent_path%str())
        if (parent == 0) parent = this%add_object(parent_path%str(), GROUP, status)
        if (parent == 0) return
        if (this%objs(parent)%kind /= GROUP) then
            call status%set(FORTIO_EEXIST, parent_path%str() // " is not a group")
            return
        end if
        if (this%nobjs == size(this%objs)) then
            allocate (grown(2 * this%nobjs))
            grown(1:this%nobjs) = this%objs(1:this%nobjs)
            call move_alloc(grown, this%objs)
        end if
        this%nobjs = this%nobjs + 1
        idx = this%nobjs
        this%objs(idx)%kind = kind
        this%objs(idx)%path = p
        this%objs(idx)%name = p%slice(cut + 1)
        this%objs(idx)%parent = parent
        allocate (this%objs(idx)%children(0), this%objs(idx)%attrs(4))
        this%objs(parent)%children = [this%objs(parent)%children, idx]
        call status%clear()
    end function add_object

    ! ------------------------------------------------------------------ public API

    !> Create the file `path`. A `userblock` of 512 bytes or a larger power of two
    !> is left free before the superblock, as IMAS-Core writes 1024 bytes.
    subroutine writer_create(this, path, status, userblock)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status
        integer(int64), intent(in), optional :: userblock
        integer(int64) :: blocks

        this%origin = 0
        if (present(userblock)) then
            blocks = userblock / 512
            if (userblock /= 0 .and. (userblock < 512 .or. mod(userblock, 512_int64) /= 0 .or. &
                iand(blocks, blocks - 1) /= 0)) then
                call status%set(FORTIO_ENOTSUP, "user block must be 0, 512 or a larger power of two")
                return
            end if
            this%origin = userblock
        end if
        call this%out%open(path, status)
        if (.not. status%ok()) return
        this%opened = .true.
        this%eof = SUPERBLOCK_SIZE
        if (allocated(this%objs)) deallocate (this%objs)
        allocate (this%objs(16))
        this%nobjs = 1
        this%objs(1)%kind = GROUP
        this%objs(1)%path = "/"
        this%objs(1)%name = ""
        allocate (this%objs(1)%children(0), this%objs(1)%attrs(4))
        this%next_dimid = 0
    end subroutine writer_create

    !> Link `path` to the object `object` in the HDF5 file `file`.
    subroutine writer_external_link(this, path, file, object, status)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path, file, object
        type(fortio_status_t), intent(inout) :: status
        integer :: idx

        idx = this%add_object(path, EXTERNAL_LINK, status)
        if (idx == 0) return
        this%objs(idx)%link_file = file
        this%objs(idx)%link_object = object
    end subroutine writer_external_link

    subroutine writer_create_group(this, path, status)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status
        integer :: idx

        idx = this%object(path)
        if (idx > 0) then
            if (this%objs(idx)%kind == GROUP) then
                call status%clear()
            else
                call status%set(FORTIO_EEXIST, path // " is not a group")
            end if
            return
        end if
        idx = this%add_object(path, GROUP, status)
    end subroutine writer_create_group

    !> New dataset with its element bytes written at the end of the raw data. With
    !> deflate the data are cut into the chunks netCDF-C would choose, and each chunk
    !> is compressed and written on its own, so only one chunk is held at a time.
    subroutine add_dataset(this, path, dtype, element_size, dims, raw, status, shared_type)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int8), intent(in) :: dtype(:)
        integer(int64), intent(in) :: element_size
        integer(int64), intent(in) :: dims(:)
        integer(int8), intent(in), contiguous :: raw(:)
        type(fortio_status_t), intent(inout) :: status
        integer, intent(in), optional :: shared_type
        integer :: idx

        if (product(dims) * element_size /= size(raw, kind=int64)) then
            call status%set(FORTIO_ESHAPE, path // ": dimensions do not match the values")
            return
        end if
        idx = this%add_object(path, DATASET, status)
        if (idx == 0) return
        this%objs(idx)%dtype = dtype
        this%objs(idx)%dims = dims
        this%objs(idx)%element_size = element_size
        if (present(shared_type)) this%objs(idx)%shared_type = shared_type
        if (size(raw) == 0) return
        this%objs(idx)%data_address = this%eof
        call this%out%seek(this%origin + this%eof + 1)
        if (this%deflate > 0 .and. size(dims) > 0) then
            call write_chunks(this, this%objs(idx), raw, status)
        else
            this%objs(idx)%stored_size = size(raw, kind=int64)
            call this%out%write_bytes(raw, status)
            this%eof = this%eof + this%objs(idx)%stored_size
        end if
    end subroutine add_dataset

    !> Chunk dimensions of netCDF-C (`nc4_find_default_chunksizes2` with its default
    !> DEFAULT_CHUNK_SIZE of 16 MiB): about 16 MiB per
    !> chunk with the dimensions scaled alike, then shrunk so that the edge chunks
    !> overhang as little as possible.
    pure function default_chunks(dims, element_size) result(chunk)
        integer(int64), intent(in) :: dims(:), element_size
        integer(int64) :: chunk(size(dims)), nchunks, overhang
        real(real64) :: scale
        integer :: d

        scale = (16777216.0_real64 / (real(product(dims), real64) * real(element_size, real64))) &
            ** (1.0_real64 / size(dims))
        do d = 1, size(dims)
            chunk(d) = min(dims(d), max(1_int64, int(scale * real(dims(d), real64) - 0.5_real64, int64)))
        end do
        do d = 1, size(dims)
            nchunks = (dims(d) + chunk(d) - 1) / chunk(d)
            overhang = nchunks * chunk(d) - dims(d)
            chunk(d) = chunk(d) - overhang / nchunks
        end do
    end function default_chunks

    subroutine write_chunks(this, o, raw, status)
        class(h5_writer_t), intent(inout) :: this
        type(wobj_t), intent(inout) :: o
        integer(int8), intent(in), contiguous :: raw(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int8), allocatable :: buffer(:), shuffled(:), packed(:), block(:)
        integer(int64), allocatable :: grid(:), at(:), addresses(:), sizes(:)
        integer(int64) :: k, n, width, header
        integer :: r, page_bits

        r = size(o%dims)
        o%deflate = this%deflate
        o%shuffle = this%shuffle .and. o%element_size > 1
        o%chunk = default_chunks(o%dims, o%element_size)
        grid = (o%dims + o%chunk - 1) / o%chunk
        n = product(grid)
        allocate (addresses(n), sizes(n), at(r))
        allocate (buffer(product(o%chunk) * o%element_size))
        do k = 0, n - 1
            call chunk_origin(k, grid, at)
            call gather(at * o%chunk)
            if (o%shuffle) then
                call shuffle_bytes(buffer, int(o%element_size), shuffled)
                call compress_zlib(shuffled, packed, status, level=o%deflate)
            else
                call compress_zlib(buffer, packed, status, level=o%deflate)
            end if
            if (.not. status%ok()) return
            addresses(k + 1) = this%eof
            sizes(k + 1) = size(packed, kind=int64)
            call this%out%write_bytes(packed, status)
            if (.not. status%ok()) return
            this%eof = this%eof + sizes(k + 1)
        end do
        o%stored_size = sizes(1)
        if (n == 1) return
        ! fixed-array index: header, then a data block of (address, size, mask) entries
        width = min(8_int64, 1 + (bit_length(product(o%chunk) * o%element_size) - 1 + 8) / 8)
        page_bits = max(10, bit_length(n - 1))
        header = this%eof
        block = [transfer("FAHD", [0_int8]), 0_int8, 1_int8, int(8 + width + 4, int8), &
            int(page_bits, int8), le_bytes(n, 8), le_bytes(header + 28, 8)]
        block = [block, le_bytes(int(lookup3_checksum(block), int64), 4)]
        call this%out%write_bytes(block, status)
        block = [transfer("FADB", [0_int8]), 0_int8, 1_int8, le_bytes(header, 8)]
        do k = 1, n
            block = [block, le_bytes(addresses(k), 8), le_bytes(sizes(k), int(width)), &
                le_bytes(0_int64, 4)]
        end do
        block = [block, le_bytes(int(lookup3_checksum(block), int64), 4)]
        call this%out%write_bytes(block, status)
        o%index_address = header
        this%eof = header + 28 + size(block, kind=int64)

    contains

        !> Chunk grid position of chunk `k`, last dimension fastest.
        pure subroutine chunk_origin(k, grid, at)
            integer(int64), intent(in) :: k, grid(:)
            integer(int64), intent(out) :: at(:)
            integer(int64) :: rest
            integer :: d

            rest = k
            do d = size(grid), 1, -1
                at(d) = mod(rest, grid(d))
                rest = rest / grid(d)
            end do
        end subroutine chunk_origin

        !> Copy the chunk at element offset `origin` into `buffer`, zero beyond the edges.
        subroutine gather(origin)
            integer(int64), intent(in) :: origin(:)
            integer(int64) :: idx(r), run, src, dst, stride_c, stride_d, es
            integer :: j

            es = o%element_size
            buffer = 0_int8
            run = min(o%chunk(r), o%dims(r) - origin(r)) * es
            idx = 0
            do
                if (all(origin(1:r - 1) + idx(1:r - 1) < o%dims(1:r - 1))) then
                    src = 0
                    dst = 0
                    stride_c = 1
                    stride_d = 1
                    do j = r, 1, -1
                        dst = dst + idx(j) * stride_c
                        src = src + (origin(j) + idx(j)) * stride_d
                        stride_c = stride_c * o%chunk(j)
                        stride_d = stride_d * o%dims(j)
                    end do
                    buffer(dst * es + 1:dst * es + run) = raw(src * es + 1:src * es + run)
                end if
                j = r - 1
                do while (j >= 1)
                    idx(j) = idx(j) + 1
                    if (idx(j) < o%chunk(j)) exit
                    idx(j) = 0
                    j = j - 1
                end do
                if (j < 1) exit
            end do
        end subroutine gather

    end subroutine write_chunks

    function dims_or(dims, n) result(d)
        integer(int64), intent(in), optional :: dims(:)
        integer, intent(in) :: n
        integer(int64), allocatable :: d(:)

        if (present(dims)) then
            d = dims
        else
            d = [int(n, int64)]
        end if
    end function dims_or

    subroutine writer_write_real64(this, path, values, status, dims)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        real(real64), intent(in), target, contiguous :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int64), intent(in), optional :: dims(:)
        integer(int8), pointer :: view(:)
        integer(int8), target :: none(0)

        ! the values are passed as their bytes, without a copy
        view => none
        if (size(values) > 0) call c_f_pointer(c_loc(values), view, [8 * size(values, kind=int64)])
        call this%add_dataset(path, dt_float64(), 8_int64, dims_or(dims, size(values)), view, status)
    end subroutine writer_write_real64

    subroutine writer_write_int32(this, path, values, status, dims)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int32), intent(in), target, contiguous :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int64), intent(in), optional :: dims(:)
        integer(int8), pointer :: view(:)
        integer(int8), target :: none(0)

        view => none
        if (size(values) > 0) call c_f_pointer(c_loc(values), view, [4 * size(values, kind=int64)])
        call this%add_dataset(path, dt_int(4, .true.), 4_int64, dims_or(dims, size(values)), view, status)
    end subroutine writer_write_int32

    subroutine writer_write_int64(this, path, values, status, dims)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int64), intent(in), target, contiguous :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int64), intent(in), optional :: dims(:)
        integer(int8), pointer :: view(:)
        integer(int8), target :: none(0)

        view => none
        if (size(values) > 0) call c_f_pointer(c_loc(values), view, [8 * size(values, kind=int64)])
        call this%add_dataset(path, dt_int(8, .true.), 8_int64, dims_or(dims, size(values)), view, status)
    end subroutine writer_write_int64

    !> Complex values as the compound {r, i}; with `named_type` the dataset uses the
    !> committed complex datatype at that path (see `commit_complex`).
    subroutine writer_write_complex(this, path, values, status, dims, named_type)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        complex(real64), intent(in), target, contiguous :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int64), intent(in), optional :: dims(:)
        character(len=*), intent(in), optional :: named_type
        integer :: shared
        integer(int8), pointer :: view(:)
        integer(int8), target :: none(0)

        shared = 0
        if (present(named_type)) then
            shared = this%object(named_type)
            if (shared == 0) then
                call status%set(FORTIO_ENOTFOUND, named_type // " not found")
                return
            end if
        end if
        view => none
        if (size(values) > 0) call c_f_pointer(c_loc(values), view, [16 * size(values, kind=int64)])
        call this%add_dataset(path, dt_complex(), 16_int64, dims_or(dims, size(values)), view, &
            status, shared)
    end subroutine writer_write_complex

    !> Variable-length UTF-8 strings (the NetCDF-4 string type).
    subroutine writer_write_strings(this, path, values, status, dims)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(string_t), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int64), intent(in), optional :: dims(:)
        integer(int8) :: none(0)
        integer(int64), allocatable :: d(:)
        integer :: idx

        d = dims_or(dims, size(values))
        if (product(d) /= size(values)) then
            call status%set(FORTIO_ESHAPE, path // ": dimensions do not match the values")
            return
        end if
        call this%add_dataset(path, dt_vlen_string(.true.), 16_int64, [0_int64], none, status)
        if (.not. status%ok()) return
        idx = this%nobjs
        this%objs(idx)%dims = d
        this%objs(idx)%vstrings = values
    end subroutine writer_write_strings

    !> Deflate level (1 to 9) for the datasets written from now on, 0 stores them
    !> contiguous. With `shuffle` the bytes of each element are grouped by significance
    !> before deflate, as netCDF4-python does by default.
    subroutine writer_set_deflate(this, level, shuffle)
        class(h5_writer_t), intent(inout) :: this
        integer, intent(in) :: level
        logical, intent(in), optional :: shuffle

        this%deflate = max(0, min(9, level))
        this%shuffle = .false.
        if (present(shuffle)) this%shuffle = shuffle
    end subroutine writer_set_deflate

    !> Scalar one-character dataset without data, the NetCDF-4 `char` variable that
    !> carries only attributes.
    subroutine writer_placeholder(this, path, status)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: none(0)

        call this%add_dataset(path, dt_fixed_string(1_int64, .false.), 1_int64, [0_int64], none, &
            status)
        if (.not. status%ok()) return
        this%objs(this%nobjs)%dims = [integer(int64) ::]
    end subroutine writer_placeholder

    !> Committed compound complex datatype, as written by NetCDF-4 for complex numbers.
    subroutine writer_commit_complex(this, path, status)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status
        integer :: idx

        idx = this%add_object(path, NAMED_TYPE, status)
        if (idx == 0) return
        this%objs(idx)%dtype = dt_complex()
    end subroutine writer_commit_complex

    subroutine add_attr(this, path, attr, status)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(wattr_t), intent(in) :: attr
        type(fortio_status_t), intent(inout) :: status
        type(wattr_t), allocatable :: grown(:)
        integer :: idx, i

        idx = this%object(path)
        if (idx == 0) then
            call status%set(FORTIO_ENOTFOUND, path // " not found")
            return
        end if
        if (allocated(attr%data)) then
            if (size(attr%data) > 60000) then
                ! one object-header message holds at most 65535 bytes
                call status%set(FORTIO_ENOTSUP, "attribute " // attr%name%str() // " is too large")
                return
            end if
        end if
        associate (o => this%objs(idx))
            do i = 1, o%nattrs
                if (o%attrs(i)%name == attr%name) then
                    o%attrs(i) = attr
                    call status%clear()
                    return
                end if
            end do
            if (o%nattrs == size(o%attrs)) then
                allocate (grown(2 * o%nattrs))
                grown(1:o%nattrs) = o%attrs(1:o%nattrs)
                call move_alloc(grown, o%attrs)
            end if
            o%nattrs = o%nattrs + 1
            o%attrs(o%nattrs) = attr
        end associate
        call status%clear()
    end subroutine add_attr

    !> Text attribute as a scalar fixed-length string (the NetCDF text attribute).
    subroutine writer_attr_text(this, path, name, value, status)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path, name
        character(len=*), intent(in) :: value
        type(fortio_status_t), intent(inout) :: status
        type(wattr_t) :: a

        a%name = name
        a%dtype = dt_fixed_string(int(max(len(value), 1), int64), .true.)
        allocate (a%dims(0))
        if (len(value) == 0) then
            a%data = [0_int8]
        else
            a%data = text_bytes(value)
        end if
        call this%add_attr(path, a, status)
    end subroutine writer_attr_text

    !> NUL-terminated ASCII text attribute, as the HDF5 dimension-scale API writes it.
    subroutine scale_text(this, path, name, value, status)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path, name, value
        type(fortio_status_t), intent(inout) :: status
        type(wattr_t) :: a

        a%name = name
        a%dtype = dt_fixed_string(int(len(value) + 1, int64), .false.)
        allocate (a%dims(0))
        a%data = [text_bytes(value), 0_int8]
        call this%add_attr(path, a, status)
    end subroutine scale_text

    subroutine writer_attr_strings(this, path, name, values, status)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path, name
        type(string_t), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        type(wattr_t) :: a

        a%name = name
        a%dtype = dt_vlen_string(.true.)
        a%dims = [size(values, kind=int64)]
        a%vstrings = values
        call this%add_attr(path, a, status)
    end subroutine writer_attr_strings

    !> Numeric attribute; a single value without `dims` is a scalar.
    subroutine writer_attr_real64(this, path, name, values, status, dims)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path, name
        real(real64), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int64), intent(in), optional :: dims(:)
        type(wattr_t) :: a

        a%name = name
        a%dtype = dt_float64()
        a%dims = attr_dims(dims, size(values))
        a%data = transfer(values, [0_int8], 8 * size(values))
        call this%add_attr(path, a, status)
    end subroutine writer_attr_real64

    subroutine writer_attr_int32(this, path, name, values, status, dims)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path, name
        integer(int32), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int64), intent(in), optional :: dims(:)
        type(wattr_t) :: a

        a%name = name
        a%dtype = dt_int(4, .true.)
        a%dims = attr_dims(dims, size(values))
        a%data = transfer(values, [0_int8], 4 * size(values))
        call this%add_attr(path, a, status)
    end subroutine writer_attr_int32

    subroutine writer_attr_int64(this, path, name, values, status, dims)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path, name
        integer(int64), intent(in) :: values(:)
        type(fortio_status_t), intent(inout) :: status
        integer(int64), intent(in), optional :: dims(:)
        type(wattr_t) :: a

        a%name = name
        a%dtype = dt_int(8, .true.)
        a%dims = attr_dims(dims, size(values))
        a%data = transfer(values, [0_int8], 8 * size(values))
        call this%add_attr(path, a, status)
    end subroutine writer_attr_int64

    function attr_dims(dims, n) result(d)
        integer(int64), intent(in), optional :: dims(:)
        integer, intent(in) :: n
        integer(int64), allocatable :: d(:)

        if (present(dims)) then
            d = dims
        else if (n == 1) then
            allocate (d(0))
        else
            d = [int(n, int64)]
        end if
    end function attr_dims

    !> NetCDF-4 dimension `path` of `length` without a coordinate variable.
    subroutine writer_dimension(this, path, length, status)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        integer(int64), intent(in) :: length
        type(fortio_status_t), intent(inout) :: status
        integer(int8) :: none(0)
        integer :: idx
        character(len=10) :: digits

        call this%add_dataset(path, dt_float64(), 8_int64, [0_int64], none, status)
        if (.not. status%ok()) return
        idx = this%nobjs
        this%objs(idx)%dims = [length]
        write (digits, "(i10)") length
        call scale_text(this, path, "CLASS", "DIMENSION_SCALE", status)
        call scale_text(this, path, "NAME", &
            "This is a netCDF dimension but not a netCDF variable." // digits, status)
        call this%attribute(path, "_Netcdf4Dimid", [int(this%next_dimid, int32)], status)
        this%next_dimid = this%next_dimid + 1
    end subroutine writer_dimension

    !> Attach dimension scales to the dimensions of `path`, slowest first. A scale that is
    !> not yet marked becomes a coordinate variable named after itself.
    subroutine writer_attach_scales(this, path, scales, status)
        class(h5_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(string_t), intent(in) :: scales(:)
        type(fortio_status_t), intent(inout) :: status
        type(wattr_t) :: a
        integer :: idx, i, s, k

        idx = this%object(path)
        if (idx == 0) then
            call status%set(FORTIO_ENOTFOUND, path // " not found")
            return
        end if
        if (size(scales) /= size(this%objs(idx)%dims)) then
            call status%set(FORTIO_ESHAPE, path // ": one scale per dimension is needed")
            return
        end if
        if (size(scales) == 1) then
            if (this%object(scales(1)%str()) == idx) then
                ! a coordinate variable is the scale of its own dimension
                call mark_scale(idx)
                return
            end if
        end if
        a%name = "DIMENSION_LIST"
        a%dtype = dt_vlen_reference()
        a%dims = [size(scales, kind=int64)]
        allocate (a%refs(size(scales)))
        do i = 1, size(scales)
            s = this%object(scales(i)%str())
            if (s == 0) then
                call status%set(FORTIO_ENOTFOUND, scales(i)%str() // " not found")
                return
            end if
            if (s == idx) then
                call status%set(FORTIO_ESHAPE, path // " cannot be a scale of its own dimension")
                return
            end if
            a%refs(i) = s
            call mark_scale(s)
        end do
        call this%add_attr(path, a, status)

    contains

        !> Mark a dataset as a coordinate variable unless it is a scale already.
        subroutine mark_scale(s)
            integer, intent(in) :: s
            logical :: marked

            marked = .false.
            do k = 1, this%objs(s)%nattrs
                if (this%objs(s)%attrs(k)%name == "CLASS") marked = .true.
            end do
            if (marked) return
            call scale_text(this, this%objs(s)%path%str(), "CLASS", "DIMENSION_SCALE", status)
            call scale_text(this, this%objs(s)%path%str(), "NAME", this%objs(s)%name%str(), status)
            call this%attribute(this%objs(s)%path%str(), "_Netcdf4Dimid", &
                [int(this%next_dimid, int32)], status)
            this%next_dimid = this%next_dimid + 1
        end subroutine mark_scale
    end subroutine writer_attach_scales

    ! ------------------------------------------------------------------ close

    subroutine writer_close(this, status)
        class(h5_writer_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status
        integer(int8), allocatable :: block(:), gheap(:), vdata(:), sb(:)
        integer(int64) :: pos, heap_start, vdata_start, heap_size
        integer :: i

        if (.not. this%opened) then
            call status%clear()
            return
        end if
        ! Variable-length payloads go to global-heap collections; their positions depend
        ! only on payload sizes, so the layout is computed before any address is final.
        heap_start = this%eof
        call plan_heap(this, heap_start, heap_size)
        vdata_start = heap_start + heap_size
        pos = vdata_start
        do i = 1, this%nobjs
            if (allocated(this%objs(i)%vstrings) .and. this%objs(i)%kind == DATASET) then
                this%objs(i)%data_address = pos
                pos = pos + 16 * size(this%objs(i)%vstrings, kind=int64)
            end if
        end do
        ! header sizes do not depend on the addresses they contain
        do i = 1, this%nobjs
            if (this%objs(i)%kind == EXTERNAL_LINK) cycle
            block = encode_object(this, i)
            this%objs(i)%header_size = size(block, kind=int64)
            this%objs(i)%address = pos
            pos = pos + this%objs(i)%header_size
        end do
        ! payloads
        gheap = build_heap(this)
        call this%out%seek(this%origin + heap_start + 1)
        if (size(gheap) > 0) call this%out%write_bytes(gheap, status)
        do i = 1, this%nobjs
            if (allocated(this%objs(i)%vstrings) .and. this%objs(i)%kind == DATASET) then
                vdata = vlen_elements(this%objs(i)%vstrings, this%objs(i)%heap_ids)
                call this%out%seek(this%origin + this%objs(i)%data_address + 1)
                if (size(vdata) > 0) call this%out%write_bytes(vdata, status)
            end if
        end do
        do i = 1, this%nobjs
            if (this%objs(i)%kind == EXTERNAL_LINK) cycle
            block = encode_object(this, i)
            call this%out%seek(this%origin + this%objs(i)%address + 1)
            call this%out%write_bytes(block, status)
            if (.not. status%ok()) return
        end do
        ! libhdf5 stores the end-of-file address including the user block
        sb = [-119_int8, text_bytes("HDF" // achar(13) // achar(10) // achar(26) // achar(10)), &
              2_int8, 8_int8, 8_int8, 0_int8, le_bytes(this%origin, 8), le_bytes(UNDEFINED, 8), &
              le_bytes(this%origin + pos, 8), le_bytes(this%objs(1)%address, 8)]
        sb = [sb, le_bytes(int(lookup3_checksum(sb), int64), 4)]
        call this%out%seek(this%origin + 1)
        call this%out%write_bytes(sb, status)
        if (.not. status%ok()) return
        call this%out%close(status)
        this%opened = .false.
    end subroutine writer_close

    !> Assign every variable-length payload a collection and index.
    subroutine plan_heap(this, start, total)
        type(h5_writer_t), intent(inout) :: this
        integer(int64), intent(in) :: start
        integer(int64), intent(out) :: total
        integer(int64) :: coll, used, nidx, need
        integer :: i, k, j

        total = 0
        coll = start
        used = 16
        nidx = 0
        do i = 1, this%nobjs
            associate (o => this%objs(i))
                if (allocated(o%vstrings)) then
                    allocate (o%heap_ids(2, size(o%vstrings)))
                    do j = 1, size(o%vstrings)
                        call place(int(len(o%vstrings(j)), int64), o%heap_ids(:, j))
                    end do
                end if
                do k = 1, o%nattrs
                    associate (a => o%attrs(k))
                        if (allocated(a%vstrings)) then
                            allocate (a%heap_ids(2, size(a%vstrings)))
                            do j = 1, size(a%vstrings)
                                call place(int(len(a%vstrings(j)), int64), a%heap_ids(:, j))
                            end do
                        else if (allocated(a%refs)) then
                            allocate (a%heap_ids(2, size(a%refs)))
                            do j = 1, size(a%refs)
                                call place(8_int64, a%heap_ids(:, j))
                            end do
                        end if
                    end associate
                end do
            end associate
        end do
        if (nidx > 0) total = coll + max(used, GHEAP_MIN) - start

    contains

        subroutine place(nbytes, id)
            integer(int64), intent(in) :: nbytes
            integer(int64), intent(out) :: id(2)

            ! the HDF5 library stores "" as an empty heap object, a null id reads as NIL
            need = 16 + 8 * ((nbytes + 7) / 8)
            if (nidx == 65535 .or. (nidx > 0 .and. used + need > 1048576)) then
                coll = coll + max(used, GHEAP_MIN)
                used = 16
                nidx = 0
            end if
            nidx = nidx + 1
            used = used + need
            id = [coll, nidx]
        end subroutine place

    end subroutine plan_heap

    !> Collections in the order of `plan_heap`, concatenated.
    function build_heap(this) result(heap)
        type(h5_writer_t), intent(in) :: this
        integer(int8), allocatable :: heap(:)
        integer(int8), allocatable :: coll(:)
        integer(int64) :: current
        integer :: i, k, j

        allocate (heap(0), coll(0))
        current = -1
        do i = 1, this%nobjs
            associate (o => this%objs(i))
                if (allocated(o%vstrings)) then
                    do j = 1, size(o%vstrings)
                        call put(o%heap_ids(:, j), text_bytes(o%vstrings(j)%str()))
                    end do
                end if
                do k = 1, o%nattrs
                    associate (a => o%attrs(k))
                        if (allocated(a%vstrings)) then
                            do j = 1, size(a%vstrings)
                                call put(a%heap_ids(:, j), text_bytes(a%vstrings(j)%str()))
                            end do
                        else if (allocated(a%refs)) then
                            do j = 1, size(a%refs)
                                call put(a%heap_ids(:, j), le_bytes(this%objs(a%refs(j))%address, 8))
                            end do
                        end if
                    end associate
                end do
            end associate
        end do
        call seal()

    contains

        subroutine put(id, payload)
            integer(int64), intent(in) :: id(2)
            integer(int8), intent(in) :: payload(:)
            integer(int64) :: padded

            if (id(1) == 0) return
            if (id(1) /= current) then
                call seal()
                current = id(1)
                coll = [text_bytes("GCOL"), 1_int8, 0_int8, 0_int8, 0_int8, le_bytes(0_int64, 8)]
            end if
            padded = 8 * ((size(payload) + 7) / 8)
            coll = [coll, le_bytes(id(2), 2), le_bytes(1_int64, 2), le_bytes(0_int64, 4), &
                    le_bytes(size(payload, kind=int64), 8), payload, &
                    [(0_int8, j=1, int(padded) - size(payload))]]
        end subroutine put

        subroutine seal()
            integer(int64) :: total, free

            if (current < 0) return
            total = max(size(coll, kind=int64), GHEAP_MIN)
            free = total - size(coll, kind=int64)
            if (free >= 16) then
                coll = [coll, le_bytes(0_int64, 2), le_bytes(0_int64, 2), le_bytes(0_int64, 4), &
                        le_bytes(free, 8), [(0_int8, j=1, int(free) - 16)]]
            else if (free > 0) then
                coll = [coll, [(0_int8, j=1, int(free))]]
            end if
            coll(9:16) = le_bytes(total, 8)
            heap = [heap, coll]
            current = -1
        end subroutine seal

    end function build_heap

    !> Variable-length elements: length, collection address and object index.
    function vlen_elements(values, ids) result(b)
        type(string_t), intent(in) :: values(:)
        integer(int64), intent(in) :: ids(:, :)
        integer(int8), allocatable :: b(:)
        integer :: j

        allocate (b(16 * size(values)))
        do j = 1, size(values)
            b(16 * (j - 1) + 1:16 * j) = [le_bytes(int(len(values(j)), int64), 4), &
                le_bytes(ids(1, j), 8), le_bytes(ids(2, j), 4)]
        end do
    end function vlen_elements

    function encode_object(this, i) result(b)
        type(h5_writer_t), intent(in) :: this
        integer, intent(in) :: i
        integer(int8), allocatable :: b(:), body(:), msgs(:)
        integer :: k, c, width
        integer(int64) :: nbytes

        allocate (msgs(0))
        associate (o => this%objs(i))
            select case (o%kind)
            case (GROUP)
                msgs = [msgs, message(2, 0, [0_int8, 0_int8, le_bytes(UNDEFINED, 8), &
                    le_bytes(UNDEFINED, 8)])]
                msgs = [msgs, message(10, 0, [0_int8, 0_int8])]
                do k = 1, size(o%children)
                    c = o%children(k)
                    if (this%objs(c)%kind == EXTERNAL_LINK) then
                        ! flags 25: two-byte name length, link type (64, external) and
                        ! character set present; the link value is "file\0object\0"
                        body = [0_int8, text_bytes(this%objs(c)%link_file%str()), 0_int8, &
                            text_bytes(this%objs(c)%link_object%str()), 0_int8]
                        body = [1_int8, 25_int8, 64_int8, 1_int8, &
                            le_bytes(int(len(this%objs(c)%name), int64), 2), &
                            text_bytes(this%objs(c)%name%str()), &
                            le_bytes(size(body, kind=int64), 2), body]
                    else
                        ! flags 17: two-byte name length, character set present (UTF-8)
                        body = [1_int8, 17_int8, 1_int8, &
                            le_bytes(int(len(this%objs(c)%name), int64), 2), &
                            text_bytes(this%objs(c)%name%str()), le_bytes(this%objs(c)%address, 8)]
                    end if
                    msgs = [msgs, message(6, 0, body)]
                end do
            case (DATASET)
                msgs = [msgs, message(1, 0, dataspace(o%dims))]
                if (o%shared_type > 0) then
                    msgs = [msgs, message(3, 3, [3_int8, 2_int8, &
                        le_bytes(this%objs(o%shared_type)%address, 8)])]
                else
                    msgs = [msgs, message(3, 1, o%dtype)]
                end if
                msgs = [msgs, message(5, 1, [3_int8, 10_int8])]
                nbytes = product(o%dims) * o%element_size
                if (o%deflate > 0) then
                    ! filter pipeline version 2: shuffle (id 2, element size), deflate (id 1)
                    if (o%shuffle) then
                        msgs = [msgs, message(11, 0, [2_int8, 2_int8, le_bytes(2_int64, 2), &
                            le_bytes(0_int64, 2), le_bytes(1_int64, 2), le_bytes(o%element_size, 4), &
                            le_bytes(1_int64, 2), le_bytes(0_int64, 2), le_bytes(1_int64, 2), &
                            le_bytes(int(o%deflate, int64), 4)])]
                    else
                        msgs = [msgs, message(11, 0, [2_int8, 1_int8, le_bytes(1_int64, 2), &
                            le_bytes(0_int64, 2), le_bytes(1_int64, 2), &
                            le_bytes(int(o%deflate, int64), 4)])]
                    end if
                    ! HDF5 requires the shortest encoding of the largest chunk dimension
                    width = (bit_length(max(maxval(o%chunk), o%element_size)) + 7) / 8
                    if (width == 0) width = 1
                    if (o%index_address == UNDEFINED) then
                        ! single chunk index (1), flag bit 1: the chunk is filtered
                        body = [4_int8, 2_int8, 2_int8, int(size(o%dims) + 1, int8), int(width, int8)]
                    else
                        body = [4_int8, 2_int8, 0_int8, int(size(o%dims) + 1, int8), int(width, int8)]
                    end if
                    do k = 1, size(o%dims)
                        body = [body, le_bytes(o%chunk(k), width)]
                    end do
                    body = [body, le_bytes(o%element_size, width)]
                    if (o%index_address == UNDEFINED) then
                        body = [body, 1_int8, le_bytes(o%stored_size, 8), le_bytes(0_int64, 4), &
                            le_bytes(o%data_address, 8)]
                    else
                        ! fixed array index (3) with its page bits, as written by write_chunks
                        body = [body, 3_int8, int(max(10, bit_length(product( &
                            (o%dims + o%chunk - 1) / o%chunk) - 1)), int8), le_bytes(o%index_address, 8)]
                    end if
                    msgs = [msgs, message(8, 0, body)]
                else
                    msgs = [msgs, message(8, 0, [3_int8, 1_int8, le_bytes(o%data_address, 8), &
                        le_bytes(nbytes, 8)])]
                end if
            case (NAMED_TYPE)
                msgs = [msgs, message(3, 1, o%dtype)]
                ! the link and every dataset that uses the type hold a reference
                msgs = [msgs, message(22, 0, [0_int8, le_bytes(int(1 + count( &
                    this%objs(1:this%nobjs)%shared_type == i), int64), 4)])]
            end select
            do k = 1, o%nattrs
                msgs = [msgs, message(12, 0, encode_attr(o%attrs(k)))]
            end do
        end associate
        b = [text_bytes("OHDR"), 2_int8, 2_int8, le_bytes(size(msgs, kind=int64), 4), msgs]
        b = [b, le_bytes(int(lookup3_checksum(b), int64), 4)]
    end function encode_object

    function encode_attr(a) result(b)
        type(wattr_t), intent(in) :: a
        integer(int8), allocatable :: b(:), space(:), data(:)
        integer :: j

        space = dataspace(a%dims)
        if (allocated(a%vstrings)) then
            data = vlen_elements(a%vstrings, a%heap_ids)
        else if (allocated(a%refs)) then
            allocate (data(16 * size(a%refs)))
            do j = 1, size(a%refs)
                data(16 * (j - 1) + 1:16 * j) = [le_bytes(1_int64, 4), le_bytes(a%heap_ids(1, j), 8), &
                    le_bytes(a%heap_ids(2, j), 4)]
            end do
        else
            data = a%data
        end if
        b = [3_int8, 0_int8, le_bytes(int(len(a%name) + 1, int64), 2), &
             le_bytes(size(a%dtype, kind=int64), 2), le_bytes(size(space, kind=int64), 2), 1_int8, &
             text_bytes(a%name%str()), 0_int8, a%dtype, space, data]
    end function encode_attr

end module fortio_h5write
