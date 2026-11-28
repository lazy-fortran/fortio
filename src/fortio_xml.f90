!> Streaming XML 1.0 reader: one event per element start, element end and text run.
!>
!> The document is read into memory once; names and attribute values are spans into
!> it and are decoded only when asked for. Comments, processing instructions and the
!> document type declaration are skipped. Attribute values are normalized as the XML
!> specification requires (tab, line feed and carriage return become spaces). An
!> unknown entity reference is kept literally.
module fortio_xml
    use fortio_status, only: fortio_status_t, FORTIO_EFORMAT, FORTIO_EIO, FORTIO_ESTATE
    use fortio_string, only: string_t, string, assignment(=)
    implicit none
    private

    integer, parameter, public :: XML_EOF = 0
    integer, parameter, public :: XML_START = 1
    integer, parameter, public :: XML_END = 2
    integer, parameter, public :: XML_TEXT = 3
    integer, parameter, public :: XML_ERROR = -1

    type :: span_t
        integer :: first = 1
        integer :: last = 0
    end type span_t

    type :: attribute_t
        type(span_t) :: name
        type(span_t) :: value
    end type attribute_t

    type, public :: xml_reader_t
        private
        character(len=:), allocatable :: doc
        integer :: pos = 1
        integer :: event = XML_EOF
        type(span_t) :: name_span
        type(span_t) :: text_span
        logical :: text_raw = .false.
        logical :: pending_end = .false.
        logical :: skip_blank = .true.
        type(attribute_t), allocatable :: attrs(:)
        integer :: nattr = 0
        type(span_t), allocatable :: stack(:)
        integer :: depth_ = 0
        type(fortio_status_t) :: error
    contains
        procedure :: open_file => reader_open_file
        procedure :: open_text => reader_open_text
        procedure :: next => reader_next
        procedure :: name => reader_name
        procedure :: is => reader_is
        procedure :: text => reader_text
        procedure :: attribute_count => reader_attribute_count
        procedure :: attribute_name => reader_attribute_name
        procedure :: attribute_value => reader_attribute_value
        procedure :: attribute => reader_attribute
        procedure :: has_attribute => reader_has_attribute
        procedure :: depth => reader_depth
        procedure :: line => reader_line
        procedure :: status => reader_status
    end type xml_reader_t

contains

    !> Read the document at `path`; `keep_blank` also reports whitespace-only text.
    subroutine reader_open_file(this, path, status, keep_blank)
        class(xml_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: path
        type(fortio_status_t), intent(inout) :: status
        logical, intent(in), optional :: keep_blank
        integer :: unit, ios
        integer(8) :: nbytes
        character(len=256) :: msg
        character(len=:), allocatable :: doc

        open (newunit=unit, file=path, access="stream", form="unformatted", &
              status="old", action="read", iostat=ios, iomsg=msg)
        if (ios /= 0) then
            call status%set(FORTIO_EIO, "cannot open " // path // ": " // msg(1:len_trim(msg)))
            return
        end if
        inquire (unit=unit, size=nbytes)
        allocate (character(len=nbytes) :: doc)
        if (nbytes > 0) read (unit, iostat=ios, iomsg=msg) doc
        close (unit)
        if (ios /= 0) then
            call status%set(FORTIO_EIO, "cannot read " // path // ": " // msg(1:len_trim(msg)))
            return
        end if
        call this%open_text(doc, keep_blank)
        call status%clear()
    end subroutine reader_open_file

    subroutine reader_open_text(this, text, keep_blank)
        class(xml_reader_t), intent(inout) :: this
        character(len=*), intent(in) :: text
        logical, intent(in), optional :: keep_blank

        this%doc = text
        this%pos = 1
        if (this%doc(1:min(3, len(this%doc))) == achar(239) // achar(187) // achar(191)) then
            this%pos = 4
        end if
        this%event = XML_EOF
        this%pending_end = .false.
        this%skip_blank = .true.
        if (present(keep_blank)) this%skip_blank = .not. keep_blank
        if (.not. allocated(this%attrs)) allocate (this%attrs(16))
        if (.not. allocated(this%stack)) allocate (this%stack(32))
        this%nattr = 0
        this%depth_ = 0
        call this%error%clear()
    end subroutine reader_open_text

    !> Advance to the next event and return its kind.
    integer function reader_next(this) result(event)
        class(xml_reader_t), intent(inout) :: this
        integer :: n, p, close_pos

        event = XML_ERROR
        if (this%event == XML_ERROR) return
        this%nattr = 0
        if (this%pending_end) then
            this%pending_end = .false.
            this%depth_ = this%depth_ - 1
            event = finish(XML_END)
            return
        end if
        n = len(this%doc)
        do
            p = this%pos
            if (p > n) then
                if (this%depth_ > 0) then
                    event = fail("unexpected end of document inside an element")
                else
                    event = finish(XML_EOF)
                end if
                return
            end if
            if (this%doc(p:p) /= "<") then
                close_pos = index(this%doc(p:), "<")
                if (close_pos == 0) then
                    close_pos = n + 1
                else
                    close_pos = close_pos + p - 1
                end if
                this%pos = close_pos
                if (this%skip_blank) then
                    if (verify(this%doc(p:close_pos - 1), " " // achar(9) // achar(10) // achar(13)) == 0) cycle
                end if
                if (this%depth_ == 0) then
                    if (verify(this%doc(p:close_pos - 1), " " // achar(9) // achar(10) // achar(13)) /= 0) then
                        event = fail("text outside the root element")
                        return
                    end if
                    cycle
                end if
                this%text_span = span_t(p, close_pos - 1)
                this%text_raw = .false.
                event = finish(XML_TEXT)
                return
            end if
            if (starts(p, "<?")) then
                if (.not. skip_to(p + 2, "?>")) return
            else if (starts(p, "<!--")) then
                if (.not. skip_to(p + 4, "-->")) return
            else if (starts(p, "<![CDATA[")) then
                close_pos = index(this%doc(p + 9:), "]]>")
                if (close_pos == 0) then
                    event = fail("unterminated CDATA section")
                    return
                end if
                close_pos = close_pos + p + 8
                this%text_span = span_t(p + 9, close_pos - 1)
                this%text_raw = .true.
                this%pos = close_pos + 3
                event = finish(XML_TEXT)
                return
            else if (starts(p, "<!")) then
                if (.not. skip_declaration(p + 2)) return
            else if (starts(p, "</")) then
                event = end_tag(p + 2)
                return
            else
                event = start_tag(p + 1)
                return
            end if
        end do

    contains

        integer function finish(kind)
            integer, intent(in) :: kind

            this%event = kind
            finish = kind
        end function finish

        integer function fail(message)
            character(len=*), intent(in) :: message

            call this%error%set(FORTIO_EFORMAT, "XML line " // line_text() // ": " // message)
            this%event = XML_ERROR
            fail = XML_ERROR
        end function fail

        function line_text() result(text)
            character(len=:), allocatable :: text
            type(string_t) :: s

            s = string(this%line())
            text = s%str()
        end function line_text

        logical function starts(at, prefix)
            integer, intent(in) :: at
            character(len=*), intent(in) :: prefix

            starts = .false.
            if (at + len(prefix) - 1 > n) return
            starts = this%doc(at:at + len(prefix) - 1) == prefix
        end function starts

        logical function skip_to(from, terminator)
            integer, intent(in) :: from
            character(len=*), intent(in) :: terminator
            integer :: hit

            skip_to = .false.
            hit = 0
            if (from <= n) hit = index(this%doc(from:), terminator)
            if (hit == 0) then
                event = fail("unterminated markup " // terminator)
                return
            end if
            this%pos = from + hit - 1 + len(terminator)
            skip_to = .true.
        end function skip_to

        logical function skip_declaration(from)
            integer, intent(in) :: from
            integer :: q, brackets
            character(len=1) :: quote

            skip_declaration = .false.
            brackets = 0
            quote = " "
            q = from
            do while (q <= n)
                if (quote /= " ") then
                    if (this%doc(q:q) == quote) quote = " "
                else if (this%doc(q:q) == '"' .or. this%doc(q:q) == "'") then
                    quote = this%doc(q:q)
                else if (this%doc(q:q) == "[") then
                    brackets = brackets + 1
                else if (this%doc(q:q) == "]") then
                    brackets = brackets - 1
                else if (this%doc(q:q) == ">" .and. brackets == 0) then
                    this%pos = q + 1
                    skip_declaration = .true.
                    return
                end if
                q = q + 1
            end do
            event = fail("unterminated declaration")
        end function skip_declaration

        integer function name_end(from)
            integer, intent(in) :: from

            name_end = scan(this%doc(from:), " " // achar(9) // achar(10) // achar(13) // "/>=")
            if (name_end == 0) then
                name_end = n
            else
                name_end = name_end + from - 2
            end if
        end function name_end

        integer function skip_space(from)
            integer, intent(in) :: from

            skip_space = verify(this%doc(from:), " " // achar(9) // achar(10) // achar(13))
            if (skip_space == 0) then
                skip_space = n + 1
            else
                skip_space = skip_space + from - 1
            end if
        end function skip_space

        integer function start_tag(from) result(kind)
            integer, intent(in) :: from
            integer :: q, last, vstart, vend
            character(len=1) :: quote
            type(attribute_t), allocatable :: grown(:)
            type(span_t), allocatable :: deeper(:)

            if (from > n) then
                kind = fail("unterminated start tag")
                return
            end if
            last = name_end(from)
            if (last < from) then
                kind = fail("element without a name")
                return
            end if
            this%name_span = span_t(from, last)
            q = last + 1
            do
                q = skip_space(q)
                if (q > n) then
                    kind = fail("unterminated start tag")
                    return
                end if
                if (this%doc(q:q) == ">") then
                    this%pos = q + 1
                    exit
                end if
                if (starts(q, "/>")) then
                    this%pos = q + 2
                    this%pending_end = .true.
                    exit
                end if
                last = name_end(q)
                if (last < q) then
                    kind = fail("malformed attribute")
                    return
                end if
                vstart = skip_space(last + 1)
                if (vstart > n) then
                    kind = fail("unterminated start tag")
                    return
                end if
                if (this%doc(vstart:vstart) /= "=") then
                    kind = fail("attribute without a value")
                    return
                end if
                vstart = skip_space(vstart + 1)
                if (vstart > n) then
                    kind = fail("unterminated start tag")
                    return
                end if
                quote = this%doc(vstart:vstart)
                if (quote /= '"' .and. quote /= "'") then
                    kind = fail("unquoted attribute value")
                    return
                end if
                vend = index(this%doc(vstart + 1:), quote)
                if (vend == 0) then
                    kind = fail("unterminated attribute value")
                    return
                end if
                vend = vend + vstart
                if (this%nattr == size(this%attrs)) then
                    allocate (grown(2 * size(this%attrs)))
                    grown(1:this%nattr) = this%attrs(1:this%nattr)
                    call move_alloc(grown, this%attrs)
                end if
                this%nattr = this%nattr + 1
                this%attrs(this%nattr) = attribute_t(span_t(q, last), span_t(vstart + 1, vend - 1))
                q = vend + 1
            end do
            if (this%depth_ == size(this%stack)) then
                allocate (deeper(2 * size(this%stack)))
                deeper(1:this%depth_) = this%stack(1:this%depth_)
                call move_alloc(deeper, this%stack)
            end if
            this%depth_ = this%depth_ + 1
            this%stack(this%depth_) = this%name_span
            kind = finish(XML_START)
        end function start_tag

        integer function end_tag(from) result(kind)
            integer, intent(in) :: from
            integer :: last, q
            type(span_t) :: open_name

            last = name_end(from)
            q = skip_space(last + 1)
            if (last < from .or. q > n) then
                kind = fail("malformed end tag")
                return
            end if
            if (this%doc(q:q) /= ">") then
                kind = fail("malformed end tag")
                return
            end if
            if (this%depth_ == 0) then
                kind = fail("end tag without start tag")
                return
            end if
            open_name = this%stack(this%depth_)
            if (this%doc(from:last) /= this%doc(open_name%first:open_name%last)) then
                kind = fail("end tag " // this%doc(from:last) // " does not match " &
                            // this%doc(open_name%first:open_name%last))
                return
            end if
            this%name_span = span_t(from, last)
            this%depth_ = this%depth_ - 1
            this%pos = q + 1
            kind = finish(XML_END)
        end function end_tag

    end function reader_next

    !> Element name of the current start or end event.
    function reader_name(this) result(name)
        class(xml_reader_t), intent(in) :: this
        type(string_t) :: name

        name = this%doc(this%name_span%first:this%name_span%last)
    end function reader_name

    !> True if the current event is a start or end of element `name`.
    logical function reader_is(this, name)
        class(xml_reader_t), intent(in) :: this
        character(len=*), intent(in) :: name

        reader_is = this%doc(this%name_span%first:this%name_span%last) == name &
            .and. this%name_span%last - this%name_span%first + 1 == len(name)
    end function reader_is

    !> Decoded content of the current text event.
    function reader_text(this) result(text)
        class(xml_reader_t), intent(in) :: this
        type(string_t) :: text

        if (this%text_raw) then
            text = normalize_lines(this%doc(this%text_span%first:this%text_span%last))
        else
            text = decode(this%doc(this%text_span%first:this%text_span%last), .false.)
        end if
    end function reader_text

    integer function reader_attribute_count(this)
        class(xml_reader_t), intent(in) :: this

        reader_attribute_count = this%nattr
    end function reader_attribute_count

    function reader_attribute_name(this, i) result(name)
        class(xml_reader_t), intent(in) :: this
        integer, intent(in) :: i
        type(string_t) :: name

        name = this%doc(this%attrs(i)%name%first:this%attrs(i)%name%last)
    end function reader_attribute_name

    function reader_attribute_value(this, i) result(value)
        class(xml_reader_t), intent(in) :: this
        integer, intent(in) :: i
        type(string_t) :: value

        value = decode(this%doc(this%attrs(i)%value%first:this%attrs(i)%value%last), .true.)
    end function reader_attribute_value

    !> Value of attribute `name` of the current start event, `default` (or empty) if absent.
    function reader_attribute(this, name, default) result(value)
        class(xml_reader_t), intent(in) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: default
        type(string_t) :: value
        integer :: i

        i = find_attribute(this, name)
        if (i > 0) then
            value = this%attribute_value(i)
        else if (present(default)) then
            value = default
        else
            value = ""
        end if
    end function reader_attribute

    logical function reader_has_attribute(this, name)
        class(xml_reader_t), intent(in) :: this
        character(len=*), intent(in) :: name

        reader_has_attribute = find_attribute(this, name) > 0
    end function reader_has_attribute

    integer function find_attribute(this, name)
        class(xml_reader_t), intent(in) :: this
        character(len=*), intent(in) :: name
        integer :: i

        do i = 1, this%nattr
            associate (s => this%attrs(i)%name)
                if (s%last - s%first + 1 /= len(name)) cycle
                if (this%doc(s%first:s%last) == name) then
                    find_attribute = i
                    return
                end if
            end associate
        end do
        find_attribute = 0
    end function find_attribute

    !> Number of open elements, the current start element included.
    integer function reader_depth(this)
        class(xml_reader_t), intent(in) :: this

        reader_depth = this%depth_
    end function reader_depth

    !> Line of the current reading position, counted from 1.
    integer function reader_line(this)
        class(xml_reader_t), intent(in) :: this
        integer :: i

        reader_line = 1
        do i = 1, min(this%pos, len(this%doc)) - 1
            if (this%doc(i:i) == achar(10)) reader_line = reader_line + 1
        end do
    end function reader_line

    function reader_status(this) result(status)
        class(xml_reader_t), intent(in) :: this
        type(fortio_status_t) :: status

        status = this%error
        if (this%event == XML_ERROR .and. status%ok()) then
            call status%set(FORTIO_ESTATE, "XML reader in error state")
        end if
    end function reader_status

    pure function normalize_lines(raw) result(out)
        character(len=*), intent(in) :: raw
        character(len=:), allocatable :: out
        integer :: i, k

        if (index(raw, achar(13)) == 0) then
            out = raw
            return
        end if
        allocate (character(len=len(raw)) :: out)
        k = 0
        i = 1
        do while (i <= len(raw))
            k = k + 1
            if (raw(i:i) == achar(13)) then
                out(k:k) = achar(10)
                if (i < len(raw)) then
                    if (raw(i + 1:i + 1) == achar(10)) i = i + 1
                end if
            else
                out(k:k) = raw(i:i)
            end if
            i = i + 1
        end do
        out = out(1:k)
    end function normalize_lines

    !> Replace entity and character references; in attributes, whitespace becomes a space.
    function decode(raw, attribute) result(out)
        character(len=*), intent(in) :: raw
        logical, intent(in) :: attribute
        character(len=:), allocatable :: out
        character(len=:), allocatable :: text
        character(len=4) :: utf8
        integer :: i, k, semi, code, nb, ios

        text = normalize_lines(raw)
        if (attribute) then
            do i = 1, len(text)
                if (text(i:i) == achar(9) .or. text(i:i) == achar(10)) text(i:i) = " "
            end do
        end if
        if (index(text, "&") == 0) then
            out = text
            return
        end if
        allocate (character(len=len(text)) :: out)
        k = 0
        i = 1
        do while (i <= len(text))
            if (text(i:i) /= "&") then
                k = k + 1
                out(k:k) = text(i:i)
                i = i + 1
                cycle
            end if
            semi = index(text(i:), ";")
            if (semi == 0) semi = len(text) + 1 - i
            semi = semi + i - 1
            select case (text(i + 1:semi - 1))
            case ("lt")
                utf8(1:1) = "<"
                nb = 1
            case ("gt")
                utf8(1:1) = ">"
                nb = 1
            case ("amp")
                utf8(1:1) = "&"
                nb = 1
            case ("quot")
                utf8(1:1) = '"'
                nb = 1
            case ("apos")
                utf8(1:1) = "'"
                nb = 1
            case default
                if (text(i + 1:i + 2) == "#x") then
                    read (text(i + 3:semi - 1), "(z8)", iostat=ios) code
                else if (text(i + 1:i + 1) == "#") then
                    read (text(i + 2:semi - 1), *, iostat=ios) code
                else
                    ios = 1
                end if
                if (ios /= 0 .or. code < 0 .or. code > 1114111) then
                    ! keep an unknown or malformed reference literally
                    out(k + 1:k + semi - i + 1) = text(i:semi)
                    k = k + semi - i + 1
                    i = semi + 1
                    cycle
                end if
                call encode_utf8(code, utf8, nb)
            end select
            out(k + 1:k + nb) = utf8(1:nb)
            k = k + nb
            i = semi + 1
        end do
        out = out(1:k)
    end function decode

    pure subroutine encode_utf8(code, bytes, nb)
        integer, intent(in) :: code
        character(len=4), intent(out) :: bytes
        integer, intent(out) :: nb

        if (code < 128) then
            nb = 1
            bytes(1:1) = achar(code)
        else if (code < 2048) then
            nb = 2
            bytes(1:1) = achar(192 + code / 64)
            bytes(2:2) = achar(128 + mod(code, 64))
        else if (code < 65536) then
            nb = 3
            bytes(1:1) = achar(224 + code / 4096)
            bytes(2:2) = achar(128 + mod(code / 64, 64))
            bytes(3:3) = achar(128 + mod(code, 64))
        else
            nb = 4
            bytes(1:1) = achar(240 + code / 262144)
            bytes(2:2) = achar(128 + mod(code / 4096, 64))
            bytes(3:3) = achar(128 + mod(code / 64, 64))
            bytes(4:4) = achar(128 + mod(code, 64))
        end if
    end subroutine encode_utf8

end module fortio_xml
