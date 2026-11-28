program test_xml
    use fortio_status, only: fortio_status_t
    use fortio_string, only: string_t, string, len, assignment(=), operator(//), operator(==)
    use fortio_xml, only: xml_reader_t, XML_EOF, XML_START, XML_END, XML_TEXT, XML_ERROR
    implicit none

    type(string_t) :: doc_path, dump_path, script

    doc_path = argument(1)
    dump_path = argument(2)
    script = argument(3)
    call check_small_document()
    call check_errors()
    call check_against_expat()

contains

    function argument(i) result(value)
        integer, intent(in) :: i
        type(string_t) :: value
        character(len=4096) :: buffer
        integer :: n

        call get_command_argument(i, buffer, n)
        if (n == 0) error stop "usage: test_xml DOC DUMP ORACLE"
        value = buffer(1:n)
    end function argument

    subroutine expect(condition, what)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: what

        if (.not. condition) error stop what
    end subroutine expect

    subroutine check_small_document()
        type(xml_reader_t) :: xml
        type(string_t) :: text

        call xml%open_text('<?xml version="1.0"?><IDSs><version>4.1.1</version>' // &
            '<field name="n_tor" data_type="INT_0D" documentation="a &lt; b"/></IDSs>')
        call step(xml, XML_START, "IDSs")
        call step(xml, XML_START, "version")
        call step(xml, XML_TEXT, "")
        text = xml%text()
        call expect(text == "4.1.1", "version value")
        call step(xml, XML_END, "version")
        call step(xml, XML_START, "field")
        call expect(xml%depth() == 2 .and. xml%attribute_count() == 3, "field attributes")
        call expect(xml%attribute("name") == "n_tor", "attribute by name")
        call expect(xml%attribute("documentation") == "a < b", "decoded attribute")
        call expect(xml%attribute("units", "-") == "-", "default of an absent attribute")
        call expect(.not. xml%has_attribute("units"), "absent attribute")
        call step(xml, XML_END, "field")
        call step(xml, XML_END, "IDSs")
        call step(xml, XML_EOF, "")
        call expect(xml%depth() == 0, "depth at the end")
    end subroutine check_small_document

    subroutine step(xml, kind, name)
        type(xml_reader_t), intent(inout) :: xml
        integer, intent(in) :: kind
        character(len=*), intent(in) :: name
        integer :: event

        event = xml%next()
        if (event /= kind) error stop "unexpected event before " // name
        if (kind == XML_START .or. kind == XML_END) then
            if (.not. xml%is(name)) error stop "unexpected element instead of " // name
        end if
    end subroutine step

    subroutine check_errors()
        call expect(last_event("<a><b></a>") == XML_ERROR, "mismatched end tag")
        call expect(last_event("<a>") == XML_ERROR, "unclosed element")
        call expect(last_event("<a x=1/>") == XML_ERROR, "unquoted attribute")
        call expect(last_event("<a x='1/>") == XML_ERROR, "unterminated attribute")
        call expect(last_event("<a><!-- x</a>") == XML_ERROR, "unterminated comment")
        call expect(last_event("</a>") == XML_ERROR, "stray end tag")
        call expect(last_event("x<a/>") == XML_ERROR, "text before the root")
        call expect(last_event("<a>&bogus;</a>") == XML_EOF, "unknown entity is not fatal")
    end subroutine check_errors

    integer function last_event(text)
        character(len=*), intent(in) :: text
        type(xml_reader_t) :: xml
        type(fortio_status_t) :: status

        call xml%open_text(text)
        do
            last_event = xml%next()
            if (last_event == XML_EOF .or. last_event == XML_ERROR) exit
        end do
        status = xml%status()
        if (last_event == XML_ERROR) call expect(len(status%message) > 0, "error message")
    end function last_event

    !> Dump all events in the oracle's format and let expat compare them.
    subroutine check_against_expat()
        type(xml_reader_t) :: xml
        type(fortio_status_t) :: status
        type(string_t) :: pending
        logical :: have_text
        integer :: unit, event, i, exit_code

        call execute_command_line("python3 " // script%str() // " generate " // &
            doc_path%str() // " " // dump_path%str(), exitstat=exit_code)
        call expect(exit_code == 0, "oracle could not generate the document")
        call xml%open_file(doc_path%str(), status, keep_blank=.true.)
        call expect(status%ok(), "open generated document")
        open (newunit=unit, file=dump_path%str(), status="replace", action="write", &
              access="stream", form="unformatted")
        have_text = .false.
        do
            event = xml%next()
            if (event /= XML_TEXT .and. have_text) then
                call put(unit, "T " // escape(pending))
                have_text = .false.
            end if
            select case (event)
            case (XML_START)
                call put(unit, "S " // xml%name())
                do i = 1, xml%attribute_count()
                    call put(unit, "A " // xml%attribute_name(i) // "=" // escape(xml%attribute_value(i)))
                end do
            case (XML_END)
                call put(unit, "E " // xml%name())
            case (XML_TEXT)
                if (have_text) then
                    pending = pending // xml%text()
                else
                    pending = xml%text()
                    have_text = .true.
                end if
            case (XML_EOF)
                exit
            case default
                status = xml%status()
                error stop status%message
            end select
        end do
        close (unit)
        call execute_command_line("python3 " // script%str() // " verify " // &
            doc_path%str() // " " // dump_path%str(), exitstat=exit_code)
        call expect(exit_code == 0, "event stream differs from expat")
    end subroutine check_against_expat

    subroutine put(unit, line)
        integer, intent(in) :: unit
        type(string_t), intent(in) :: line

        write (unit) line%str() // achar(10)
    end subroutine put

    function escape(text) result(out)
        type(string_t), intent(in) :: text
        type(string_t) :: out

        out = text%replace("\", "\\")
        out = out%replace(achar(10), "\n")
        out = out%replace(achar(9), "\t")
    end function escape

end program test_xml
