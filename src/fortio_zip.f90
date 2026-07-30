module fortio_zip
    !! Streaming interface for interoperable ZIP32 archives.
    use, intrinsic :: iso_fortran_env, only: int8, int32, int64
    use fortio_compression, only: calculate_crc32, compress_raw
    use fortio_status, only: fortio_status_t, FORTIO_EIO, FORTIO_ESTATE
    implicit none
    private

    public :: zip_writer_t

    integer(int32), parameter :: ZIP_LOCAL_SIGNATURE = int(z'04034B50', int32)
    integer(int32), parameter :: ZIP_CENTRAL_SIGNATURE = int(z'02014B50', int32)
    integer(int32), parameter :: ZIP_END_SIGNATURE = int(z'06054B50', int32)
    integer, parameter :: ZIP_UTF8_FLAG = 2048
    integer, parameter :: ZIP_DEFLATE_METHOD = 8
    integer(int64), parameter :: ZIP32_MAX = int(z'FFFFFFFF', int64)

    type :: zip_entry_t
        character(len=:), allocatable :: name
        integer(int8), allocatable :: compressed(:)
        integer(int32) :: checksum = 0
        integer(int64) :: original_size = 0
        integer(int64) :: local_offset = 0
    end type zip_entry_t

    type :: zip_writer_t
        !! Independent ZIP32 writer with byte, text, and file entry overloads.
        private
        character(len=:), allocatable :: path
        type(zip_entry_t), allocatable :: entries(:)
        logical :: opened = .false.
    contains
        procedure :: open => zip_open
        procedure, private :: add_bytes => zip_add_bytes
        procedure, private :: add_text => zip_add_text
        generic :: add => add_bytes, add_text
        procedure :: add_file => zip_add_file
        procedure :: close => zip_close
    end type zip_writer_t

contains

    subroutine zip_open(this, path, status)
        !! Start a new archive, replacing `path` when `close()` succeeds.
        class(zip_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status

        call status%clear()
        if (len_trim(path) == 0) then
            call status%set(FORTIO_EIO, "ZIP path is empty")
            return
        end if
        this%path = trim(path)
        if (allocated(this%entries)) deallocate(this%entries)
        allocate(this%entries(0))
        this%opened = .true.
    end subroutine zip_open

    subroutine zip_add_bytes(this, name, bytes, status, level)
        !! Add one named byte payload.
        class(zip_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer(int8), intent(in) :: bytes(:)
        type(fortio_status_t), intent(inout) :: status
        integer, intent(in), optional :: level
        type(zip_entry_t), allocatable :: grown(:)
        integer :: compressed_size, selected_level

        call status%clear()
        if (.not. this%opened) then
            call status%set(FORTIO_ESTATE, "ZIP writer is not open")
            return
        end if
        if (len_trim(name) == 0) then
            call status%set(FORTIO_EIO, "ZIP entry name is empty")
            return
        end if
        if (index(name, achar(0)) /= 0) then
            call status%set(FORTIO_EIO, "ZIP entry name contains NUL")
            return
        end if
        if (len_trim(name) > 65535) then
            call status%set(FORTIO_EIO, "ZIP32 entry name is too long")
            return
        end if
        if (zip_name_exists(this, trim(name))) then
            call status%set(FORTIO_EIO, "ZIP entry name already exists")
            return
        end if
        selected_level = 6
        if (present(level)) selected_level = level
        if (selected_level < 0 .or. selected_level > 9) then
            call status%set(FORTIO_EIO, "compression level must be between zero and nine")
            return
        end if
        allocate(grown(size(this%entries) + 1))
        if (size(this%entries) > 0) grown(:size(this%entries)) = this%entries
        grown(size(grown))%name = trim(name)
        grown(size(grown))%checksum = calculate_crc32(bytes, size(bytes))
        grown(size(grown))%original_size = size(bytes, kind=int64)
        call compress_raw(bytes, size(bytes), grown(size(grown))%compressed, &
            compressed_size)
        if (compressed_size /= size(grown(size(grown))%compressed)) then
            call status%set(FORTIO_EIO, "Deflate encoder failed")
            return
        end if
        call move_alloc(grown, this%entries)
    end subroutine zip_add_bytes

    subroutine zip_add_text(this, name, text, status, level)
        !! Add one named text payload using the string's encoded bytes.
        class(zip_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: name, text
        type(fortio_status_t), intent(inout) :: status
        integer, intent(in), optional :: level
        integer(int8), allocatable :: bytes(:)
        integer :: i

        allocate(bytes(len(text)))
        do i = 1, len(text)
            bytes(i) = int(iachar(text(i:i)), int8)
        end do
        if (present(level)) then
            call this%add_bytes(name, bytes, status, level)
        else
            call this%add_bytes(name, bytes, status)
        end if
    end subroutine zip_add_text

    subroutine zip_add_file(this, source_path, status, archive_name, level)
        !! Read a file and add it under `archive_name` or `source_path`.
        class(zip_writer_t), intent(inout) :: this
        character(len=*), intent(in) :: source_path
        type(fortio_status_t), intent(inout) :: status
        character(len=*), intent(in), optional :: archive_name
        integer, intent(in), optional :: level
        integer(int8), allocatable :: bytes(:)
        character(len=:), allocatable :: name
        integer(int64) :: file_size
        integer :: io_status, unit

        call status%clear()
        inquire(file=source_path, size=file_size, iostat=io_status)
        if (io_status /= 0 .or. file_size < 0) then
            call status%set(FORTIO_EIO, "cannot inspect ZIP source file")
            return
        end if
        allocate(bytes(file_size))
        open(newunit=unit, file=source_path, access="stream", form="unformatted", &
            status="old", action="read", iostat=io_status)
        if (io_status /= 0) then
            call status%set(FORTIO_EIO, "cannot open ZIP source file")
            return
        end if
        if (file_size > 0) read(unit, iostat=io_status) bytes
        close(unit)
        if (io_status /= 0) then
            call status%set(FORTIO_EIO, "cannot read ZIP source file")
            return
        end if
        name = source_path
        if (present(archive_name)) name = archive_name
        if (present(level)) then
            call this%add_bytes(name, bytes, status, level)
        else
            call this%add_bytes(name, bytes, status)
        end if
    end subroutine zip_add_file

    subroutine zip_close(this, status)
        !! Write local entries, the central directory, and the end record.
        class(zip_writer_t), intent(inout) :: this
        type(fortio_status_t), intent(inout) :: status
        integer(int64) :: central_offset, central_size, position
        integer :: i, io_status, unit

        call status%clear()
        if (.not. this%opened) then
            call status%set(FORTIO_ESTATE, "ZIP writer is not open")
            return
        end if
        if (size(this%entries) > 65535) then
            call status%set(FORTIO_EIO, "ZIP32 entry limit exceeded")
            return
        end if
        if (.not. zip32_fits(this)) then
            call status%set(FORTIO_EIO, "ZIP32 size or offset limit exceeded")
            return
        end if
        open(newunit=unit, file=this%path, access="stream", form="unformatted", &
            status="replace", action="write", iostat=io_status)
        if (io_status /= 0) then
            call status%set(FORTIO_EIO, "cannot create ZIP archive")
            return
        end if
        position = 0
        do i = 1, size(this%entries)
            this%entries(i)%local_offset = position
            call write_local_entry(unit, this%entries(i), position, io_status)
            if (io_status /= 0) exit
        end do
        central_offset = position
        if (io_status == 0) then
            do i = 1, size(this%entries)
                call write_central_entry(unit, this%entries(i), position, io_status)
                if (io_status /= 0) exit
            end do
        end if
        central_size = position - central_offset
        if (io_status == 0) then
            call write_end_record(unit, size(this%entries), central_size, &
                central_offset, io_status)
        end if
        close(unit)
        if (io_status /= 0) then
            call status%set(FORTIO_EIO, "cannot write ZIP archive")
            return
        end if
        this%opened = .false.
    end subroutine zip_close

    logical function zip32_fits(this)
        class(zip_writer_t), intent(in) :: this
        integer(int64) :: central_size, local_size
        integer :: i

        zip32_fits = .false.
        local_size = 0_int64
        central_size = 0_int64
        do i = 1, size(this%entries)
            if (this%entries(i)%original_size > ZIP32_MAX) return
            if (size(this%entries(i)%compressed, kind=int64) > ZIP32_MAX) return
            if (local_size > ZIP32_MAX) return
            local_size = local_size + 30 + len(this%entries(i)%name) + &
                size(this%entries(i)%compressed, kind=int64)
            central_size = central_size + 46 + len(this%entries(i)%name)
        end do
        if (local_size > ZIP32_MAX) return
        if (central_size > ZIP32_MAX) return
        if (local_size + central_size + 22 > ZIP32_MAX) return
        zip32_fits = .true.
    end function zip32_fits

    logical function zip_name_exists(this, name)
        class(zip_writer_t), intent(in) :: this
        character(len=*), intent(in) :: name
        integer :: i

        zip_name_exists = .false.
        do i = 1, size(this%entries)
            if (this%entries(i)%name == name) then
                zip_name_exists = .true.
                return
            end if
        end do
    end function zip_name_exists

    subroutine write_local_entry(unit, entry, position, io_status)
        integer, intent(in) :: unit
        type(zip_entry_t), intent(in) :: entry
        integer(int64), intent(inout) :: position
        integer, intent(out) :: io_status
        integer(int8), allocatable :: header(:), name_bytes(:)

        call encode_name(entry%name, name_bytes)
        allocate(header(30))
        header = 0
        call put_u32(header, 1, ZIP_LOCAL_SIGNATURE)
        call put_u16(header, 5, 20)
        call put_u16(header, 7, ZIP_UTF8_FLAG)
        call put_u16(header, 9, ZIP_DEFLATE_METHOD)
        call put_u32(header, 15, entry%checksum)
        call put_size32(header, 19, size(entry%compressed, kind=int64))
        call put_size32(header, 23, entry%original_size)
        call put_u16(header, 27, size(name_bytes))
        write(unit, iostat=io_status) header, name_bytes, entry%compressed
        if (io_status == 0) position = position + size(header) + size(name_bytes) + &
            size(entry%compressed)
    end subroutine write_local_entry

    subroutine write_central_entry(unit, entry, position, io_status)
        integer, intent(in) :: unit
        type(zip_entry_t), intent(in) :: entry
        integer(int64), intent(inout) :: position
        integer, intent(out) :: io_status
        integer(int8), allocatable :: header(:), name_bytes(:)

        call encode_name(entry%name, name_bytes)
        allocate(header(46))
        header = 0
        call put_u32(header, 1, ZIP_CENTRAL_SIGNATURE)
        call put_u16(header, 5, 20)
        call put_u16(header, 7, 20)
        call put_u16(header, 9, ZIP_UTF8_FLAG)
        call put_u16(header, 11, ZIP_DEFLATE_METHOD)
        call put_u32(header, 17, entry%checksum)
        call put_size32(header, 21, size(entry%compressed, kind=int64))
        call put_size32(header, 25, entry%original_size)
        call put_u16(header, 29, size(name_bytes))
        call put_size32(header, 43, entry%local_offset)
        write(unit, iostat=io_status) header, name_bytes
        if (io_status == 0) position = position + size(header) + size(name_bytes)
    end subroutine write_central_entry

    subroutine write_end_record(unit, count, central_size, central_offset, io_status)
        integer, intent(in) :: unit, count
        integer(int64), intent(in) :: central_size, central_offset
        integer, intent(out) :: io_status
        integer(int8) :: record(22)

        record = 0
        call put_u32(record, 1, ZIP_END_SIGNATURE)
        call put_u16(record, 9, count)
        call put_u16(record, 11, count)
        call put_size32(record, 13, central_size)
        call put_size32(record, 17, central_offset)
        write(unit, iostat=io_status) record
    end subroutine write_end_record

    subroutine encode_name(name, bytes)
        character(len=*), intent(in) :: name
        integer(int8), allocatable, intent(out) :: bytes(:)
        integer :: i

        allocate(bytes(len(name)))
        do i = 1, len(name)
            bytes(i) = int(iachar(name(i:i)), int8)
        end do
    end subroutine encode_name

    subroutine put_u16(buffer, offset, value)
        integer(int8), intent(inout) :: buffer(:)
        integer, intent(in) :: offset, value

        buffer(offset) = int(iand(value, 255), int8)
        buffer(offset + 1) = int(iand(ishft(value, -8), 255), int8)
    end subroutine put_u16

    subroutine put_u32(buffer, offset, value)
        integer(int8), intent(inout) :: buffer(:)
        integer, intent(in) :: offset
        integer(int32), intent(in) :: value
        integer :: i

        do i = 0, 3
            buffer(offset + i) = int(iand(ishft(value, -8*i), 255), int8)
        end do
    end subroutine put_u32

    subroutine put_size32(buffer, offset, value)
        integer(int8), intent(inout) :: buffer(:)
        integer, intent(in) :: offset
        integer(int64), intent(in) :: value
        integer :: i

        do i = 0, 3
            buffer(offset + i) = int(iand(ishft(value, -8*i), 255_int64), int8)
        end do
    end subroutine put_size32

end module fortio_zip
