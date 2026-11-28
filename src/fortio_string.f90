!> Deferred-length string value with operators, so call sites need no `trim`.
!>
!> A `string_t` holds exactly its characters: trailing blanks are significant and
!> comparisons are exact, unlike blank-padded intrinsic character comparison.
module fortio_string
    use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite
    implicit none
    private

    public :: string_t, string, join, len, assignment(=)
    public :: operator(//), operator(==), operator(/=)
    public :: operator(<), operator(<=), operator(>), operator(>=)

    character(len=*), parameter :: whitespace = " " // achar(9) // achar(10) &
        // achar(11) // achar(12) // achar(13)

    type :: string_t
        private
        character(len=:), allocatable :: text
    contains
        procedure :: str => string_str
        procedure :: length => string_length
        procedure :: is_empty => string_is_empty
        procedure :: char_at => string_char_at
        procedure :: slice => string_slice
        procedure :: find => string_find
        procedure :: rfind => string_rfind
        procedure :: count => string_count
        procedure :: starts_with => string_starts_with
        procedure :: ends_with => string_ends_with
        procedure :: strip => string_strip
        procedure :: lstrip => string_lstrip
        procedure :: rstrip => string_rstrip
        procedure :: upper => string_upper
        procedure :: lower => string_lower
        procedure :: replace => string_replace
        procedure :: split => string_split
        procedure :: to_int => string_to_int
        procedure :: to_real => string_to_real
        procedure, private :: write_formatted => string_write_formatted
        generic :: write(formatted) => write_formatted
    end type string_t

    !> Construct a string from text, an integer, a real or a logical.
    interface string
        module procedure string_from_char, string_from_int32, string_from_int64
        module procedure string_from_real32, string_from_real64, string_from_logical
    end interface string

    interface len
        module procedure string_len
    end interface len

    interface assignment(=)
        module procedure assign_from_char, assign_to_char
    end interface assignment(=)

    interface operator(//)
        module procedure cat_ss, cat_sc, cat_cs
    end interface operator(//)

    interface operator(==)
        module procedure eq_ss, eq_sc, eq_cs
    end interface operator(==)

    interface operator(/=)
        module procedure ne_ss, ne_sc, ne_cs
    end interface operator(/=)

    interface operator(<)
        module procedure lt_ss, lt_sc, lt_cs
    end interface operator(<)

    interface operator(<=)
        module procedure le_ss, le_sc, le_cs
    end interface operator(<=)

    interface operator(>)
        module procedure gt_ss, gt_sc, gt_cs
    end interface operator(>)

    interface operator(>=)
        module procedure ge_ss, ge_sc, ge_cs
    end interface operator(>=)

contains

    pure function chars(s) result(text)
        type(string_t), intent(in) :: s
        character(len=:), allocatable :: text

        if (allocated(s%text)) then
            text = s%text
        else
            text = ""
        end if
    end function chars

    elemental function string_from_char(text) result(s)
        character(len=*), intent(in) :: text
        type(string_t) :: s

        s%text = text
    end function string_from_char

    elemental function string_from_int32(value) result(s)
        integer(int32), intent(in) :: value
        type(string_t) :: s

        s = string_from_int64(int(value, int64))
    end function string_from_int32

    elemental function string_from_int64(value) result(s)
        integer(int64), intent(in) :: value
        type(string_t) :: s
        character(len=20) :: digits
        integer(int64) :: rest
        integer :: pos

        rest = abs(value)
        pos = len(digits) + 1
        do
            pos = pos - 1
            digits(pos:pos) = achar(iachar("0") + int(mod(rest, 10_int64)))
            rest = rest / 10_int64
            if (rest == 0_int64) exit
        end do
        if (value == -huge(value) - 1_int64) then
            s%text = "-9223372036854775808"
        else if (value < 0_int64) then
            s%text = "-" // digits(pos:)
        else
            s%text = digits(pos:)
        end if
    end function string_from_int64

    !> Shortest decimal text that reads back to the same real32 value.
    impure elemental function string_from_real32(value) result(s)
        real(real32), intent(in) :: value
        type(string_t) :: s

        s = shortest_real(real(value, real64), 9, .true.)
    end function string_from_real32

    !> Shortest decimal text that reads back to the same real64 value.
    impure elemental function string_from_real64(value) result(s)
        real(real64), intent(in) :: value
        type(string_t) :: s

        s = shortest_real(value, 17, .false.)
    end function string_from_real64

    impure function shortest_real(value, max_digits, single) result(s)
        real(real64), intent(in) :: value
        integer, intent(in) :: max_digits
        logical, intent(in) :: single
        type(string_t) :: s
        character(len=40) :: buffer
        character(len=16) :: fmt
        real(real64) :: back
        real(real32) :: back32
        integer :: digits, ios

        if (ieee_is_nan(value)) then
            s%text = "nan"
            return
        end if
        if (.not. ieee_is_finite(value)) then
            if (value > 0) then
                s%text = "inf"
            else
                s%text = "-inf"
            end if
            return
        end if
        do digits = 1, max_digits
            write (fmt, "(a,i0,a,i0,a)") "(es", digits + 8, ".", digits - 1, "e3)"
            write (buffer, fmt) value
            if (single) then
                read (buffer, *, iostat=ios) back32
                if (ios == 0 .and. back32 == real(value, real32)) exit
            else
                read (buffer, *, iostat=ios) back
                if (ios == 0 .and. back == value) exit
            end if
        end do
        s%text = compact_exponent(adjustl(buffer))
    end function shortest_real

    !> Positional text for decimal exponents -4 to 15 and `d.ddde<exp>` otherwise,
    !> from the `es` text `text`.
    pure function compact_exponent(text) result(out)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        character(len=:), allocatable :: sign, digits
        integer :: epos, last, e, n

        epos = index(text, "E")
        read (text(epos + 1:len_trim(text)), *) e
        sign = ""
        if (text(1:1) == "-") sign = "-"
        digits = text(len(sign) + 1:len(sign) + 1)
        if (epos > len(sign) + 3) digits = digits // text(len(sign) + 3:epos - 1)
        last = verify(digits, "0", back=.true.)
        if (last == 0) then
            out = sign // "0"
            return
        end if
        digits = digits(1:last)
        n = len(digits)
        if (e < -4 .or. e > 15) then
            out = sign // digits(1:1)
            if (n > 1) out = out // "." // digits(2:)
            out = out // "e" // chars(string_from_int64(int(e, int64)))
        else if (e < 0) then
            out = sign // "0." // repeat("0", -e - 1) // digits
        else if (n <= e + 1) then
            out = sign // digits // repeat("0", e + 1 - n)
        else
            out = sign // digits(1:e + 1) // "." // digits(e + 2:)
        end if
    end function compact_exponent

    elemental function string_from_logical(value) result(s)
        logical, intent(in) :: value
        type(string_t) :: s

        if (value) then
            s%text = "true"
        else
            s%text = "false"
        end if
    end function string_from_logical

    pure function string_str(self) result(text)
        class(string_t), intent(in) :: self
        character(len=:), allocatable :: text

        text = chars(self)
    end function string_str

    elemental integer function string_len(s)
        type(string_t), intent(in) :: s

        string_len = 0
        if (allocated(s%text)) string_len = len(s%text)
    end function string_len

    elemental integer function string_length(self)
        class(string_t), intent(in) :: self

        string_length = 0
        if (allocated(self%text)) string_length = len(self%text)
    end function string_length

    elemental logical function string_is_empty(self)
        class(string_t), intent(in) :: self

        string_is_empty = self%length() == 0
    end function string_is_empty

    !> Character at position `i`, counted from 1; negative `i` counts from the end.
    elemental function string_char_at(self, i) result(c)
        class(string_t), intent(in) :: self
        integer, intent(in) :: i
        character(len=1) :: c
        integer :: pos

        pos = i
        if (pos < 0) pos = self%length() + 1 + pos
        if (pos < 1 .or. pos > self%length()) error stop "string_t%char_at: index out of range"
        c = self%text(pos:pos)
    end function string_char_at

    !> Characters `first` to `last`, clipped to the string; negative indices count from the end.
    elemental function string_slice(self, first, last) result(s)
        class(string_t), intent(in) :: self
        integer, intent(in) :: first
        integer, intent(in), optional :: last
        type(string_t) :: s
        integer :: n, i, j

        n = self%length()
        i = first
        if (i < 0) i = n + 1 + i
        j = n
        if (present(last)) j = last
        if (j < 0) j = n + 1 + j
        i = max(i, 1)
        j = min(j, n)
        if (j < i) then
            s%text = ""
        else
            s%text = self%text(i:j)
        end if
    end function string_slice

    elemental integer function string_find(self, sub, start)
        class(string_t), intent(in) :: self
        character(len=*), intent(in) :: sub
        integer, intent(in), optional :: start
        integer :: from

        from = 1
        if (present(start)) from = max(start, 1)
        string_find = 0
        if (from > self%length() + 1) return
        string_find = index(self%text(from:), sub)
        if (string_find > 0) string_find = string_find + from - 1
    end function string_find

    elemental integer function string_rfind(self, sub)
        class(string_t), intent(in) :: self
        character(len=*), intent(in) :: sub

        string_rfind = index(chars(self), sub, back=.true.)
    end function string_rfind

    !> Number of non-overlapping occurrences of `sub`.
    elemental integer function string_count(self, sub)
        class(string_t), intent(in) :: self
        character(len=*), intent(in) :: sub
        integer :: pos, hit

        string_count = 0
        if (len(sub) == 0) return
        pos = 1
        do
            hit = self%find(sub, pos)
            if (hit == 0) exit
            string_count = string_count + 1
            pos = hit + len(sub)
        end do
    end function string_count

    elemental logical function string_starts_with(self, prefix)
        class(string_t), intent(in) :: self
        character(len=*), intent(in) :: prefix

        string_starts_with = .false.
        if (len(prefix) > self%length()) return
        string_starts_with = self%text(1:len(prefix)) == prefix
    end function string_starts_with

    elemental logical function string_ends_with(self, suffix)
        class(string_t), intent(in) :: self
        character(len=*), intent(in) :: suffix
        integer :: n

        n = self%length()
        string_ends_with = .false.
        if (len(suffix) > n) return
        string_ends_with = self%text(n - len(suffix) + 1:n) == suffix
    end function string_ends_with

    elemental function string_strip(self) result(s)
        class(string_t), intent(in) :: self
        type(string_t) :: s

        s = self%lstrip()
        s = s%rstrip()
    end function string_strip

    elemental function string_lstrip(self) result(s)
        class(string_t), intent(in) :: self
        type(string_t) :: s
        integer :: first

        first = verify(chars(self), whitespace)
        if (first == 0) then
            s%text = ""
        else
            s%text = self%text(first:)
        end if
    end function string_lstrip

    elemental function string_rstrip(self) result(s)
        class(string_t), intent(in) :: self
        type(string_t) :: s
        integer :: last

        last = verify(chars(self), whitespace, back=.true.)
        if (last == 0) then
            s%text = ""
        else
            s%text = self%text(1:last)
        end if
    end function string_rstrip

    elemental function string_upper(self) result(s)
        class(string_t), intent(in) :: self
        type(string_t) :: s
        integer :: i, code

        s%text = chars(self)
        do i = 1, len(s%text)
            code = iachar(s%text(i:i))
            if (code >= iachar("a") .and. code <= iachar("z")) s%text(i:i) = achar(code - 32)
        end do
    end function string_upper

    elemental function string_lower(self) result(s)
        class(string_t), intent(in) :: self
        type(string_t) :: s
        integer :: i, code

        s%text = chars(self)
        do i = 1, len(s%text)
            code = iachar(s%text(i:i))
            if (code >= iachar("A") .and. code <= iachar("Z")) s%text(i:i) = achar(code + 32)
        end do
    end function string_lower

    !> Replace every non-overlapping occurrence of `old`, scanning left to right.
    elemental function string_replace(self, old, new) result(s)
        class(string_t), intent(in) :: self
        character(len=*), intent(in) :: old, new
        type(string_t) :: s
        character(len=:), allocatable :: out
        integer :: pos, hit

        if (len(old) == 0) then
            s%text = chars(self)
            return
        end if
        out = ""
        pos = 1
        do
            hit = self%find(old, pos)
            if (hit == 0) exit
            out = out // self%text(pos:hit - 1) // new
            pos = hit + len(old)
        end do
        s%text = out // self%text(pos:)
    end function string_replace

    !> Fields between separators `sep`; without `sep`, runs of whitespace separate
    !> and leading or trailing whitespace gives no empty field.
    pure function string_split(self, sep) result(parts)
        class(string_t), intent(in) :: self
        character(len=*), intent(in), optional :: sep
        type(string_t), allocatable :: parts(:)
        character(len=:), allocatable :: text
        integer :: pos, hit, first, last, n

        text = chars(self)
        if (present(sep)) then
            if (len(sep) == 0) error stop "string_t%split: empty separator"
            n = self%count(sep) + 1
            allocate (parts(n))
            pos = 1
            do n = 1, size(parts) - 1
                hit = index(text(pos:), sep) + pos - 1
                parts(n)%text = text(pos:hit - 1)
                pos = hit + len(sep)
            end do
            parts(size(parts))%text = text(pos:)
            return
        end if
        allocate (parts(0))
        pos = 1
        do
            first = verify(text(pos:), whitespace)
            if (first == 0) exit
            first = first + pos - 1
            last = scan(text(first:), whitespace)
            if (last == 0) then
                last = len(text)
            else
                last = last + first - 2
            end if
            parts = [parts, string_from_char(text(first:last))]
            pos = last + 1
        end do
    end function string_split

    !> Integer value of the stripped text; `ok` is false for anything else.
    function string_to_int(self, ok) result(value)
        class(string_t), intent(in) :: self
        logical, intent(out), optional :: ok
        integer(int64) :: value
        type(string_t) :: t
        integer :: ios

        t = self%strip()
        value = 0_int64
        ios = 1
        if (t%length() > 0 .and. verify(t%text, "+-0123456789") == 0) then
            read (t%text, *, iostat=ios) value
        end if
        if (present(ok)) then
            ok = ios == 0
        else if (ios /= 0) then
            error stop "string_t%to_int: not an integer"
        end if
    end function string_to_int

    !> Real value of the stripped text; `ok` is false for anything else.
    function string_to_real(self, ok) result(value)
        class(string_t), intent(in) :: self
        logical, intent(out), optional :: ok
        real(real64) :: value
        type(string_t) :: t
        integer :: ios

        t = self%strip()
        value = 0
        ios = 1
        if (t%length() > 0 .and. verify(t%text, "+-.0123456789eEdDinfatyINFATY") == 0) then
            read (t%text, *, iostat=ios) value
        end if
        if (present(ok)) then
            ok = ios == 0
        else if (ios /= 0) then
            error stop "string_t%to_real: not a real number"
        end if
    end function string_to_real

    subroutine string_write_formatted(self, unit, iotype, v_list, iostat, iomsg)
        class(string_t), intent(in) :: self
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        if (iotype == "" .and. size(v_list) < 0) return
        write (unit, "(a)", iostat=iostat, iomsg=iomsg) chars(self)
    end subroutine string_write_formatted

    !> Join `parts` with `sep` between consecutive elements.
    pure function join(parts, sep) result(s)
        type(string_t), intent(in) :: parts(:)
        character(len=*), intent(in), optional :: sep
        type(string_t) :: s
        integer :: i, total, pos, n, nsep

        nsep = 0
        if (present(sep)) nsep = len(sep)
        total = sum(len(parts)) + nsep * max(size(parts) - 1, 0)
        allocate (character(len=total) :: s%text)
        pos = 1
        do i = 1, size(parts)
            if (i > 1 .and. nsep > 0) then
                s%text(pos:pos + nsep - 1) = sep
                pos = pos + nsep
            end if
            n = len(parts(i))
            if (n > 0) s%text(pos:pos + n - 1) = parts(i)%text
            pos = pos + n
        end do
    end function join

    elemental subroutine assign_from_char(lhs, rhs)
        type(string_t), intent(out) :: lhs
        character(len=*), intent(in) :: rhs

        lhs%text = rhs
    end subroutine assign_from_char

    subroutine assign_to_char(lhs, rhs)
        character(len=:), allocatable, intent(out) :: lhs
        type(string_t), intent(in) :: rhs

        lhs = chars(rhs)
    end subroutine assign_to_char

    elemental function cat_ss(a, b) result(s)
        type(string_t), intent(in) :: a, b
        type(string_t) :: s

        s%text = chars(a) // chars(b)
    end function cat_ss

    elemental function cat_sc(a, b) result(s)
        type(string_t), intent(in) :: a
        character(len=*), intent(in) :: b
        type(string_t) :: s

        s%text = chars(a) // b
    end function cat_sc

    elemental function cat_cs(a, b) result(s)
        character(len=*), intent(in) :: a
        type(string_t), intent(in) :: b
        type(string_t) :: s

        s%text = a // chars(b)
    end function cat_cs

    !> -1, 0 or 1 by character code order; a proper prefix sorts first.
    pure integer function compare(a, b)
        character(len=*), intent(in) :: a, b
        integer :: i, n

        n = min(len(a), len(b))
        do i = 1, n
            if (a(i:i) /= b(i:i)) then
                if (iachar(a(i:i)) < iachar(b(i:i))) then
                    compare = -1
                else
                    compare = 1
                end if
                return
            end if
        end do
        compare = 0
        if (len(a) < len(b)) compare = -1
        if (len(a) > len(b)) compare = 1
    end function compare

    elemental logical function eq_ss(a, b)
        type(string_t), intent(in) :: a, b
        eq_ss = compare(chars(a), chars(b)) == 0
    end function eq_ss

    elemental logical function eq_sc(a, b)
        type(string_t), intent(in) :: a
        character(len=*), intent(in) :: b
        eq_sc = compare(chars(a), b) == 0
    end function eq_sc

    elemental logical function eq_cs(a, b)
        character(len=*), intent(in) :: a
        type(string_t), intent(in) :: b
        eq_cs = compare(a, chars(b)) == 0
    end function eq_cs

    elemental logical function ne_ss(a, b)
        type(string_t), intent(in) :: a, b
        ne_ss = compare(chars(a), chars(b)) /= 0
    end function ne_ss

    elemental logical function ne_sc(a, b)
        type(string_t), intent(in) :: a
        character(len=*), intent(in) :: b
        ne_sc = compare(chars(a), b) /= 0
    end function ne_sc

    elemental logical function ne_cs(a, b)
        character(len=*), intent(in) :: a
        type(string_t), intent(in) :: b
        ne_cs = compare(a, chars(b)) /= 0
    end function ne_cs

    elemental logical function lt_ss(a, b)
        type(string_t), intent(in) :: a, b
        lt_ss = compare(chars(a), chars(b)) < 0
    end function lt_ss

    elemental logical function lt_sc(a, b)
        type(string_t), intent(in) :: a
        character(len=*), intent(in) :: b
        lt_sc = compare(chars(a), b) < 0
    end function lt_sc

    elemental logical function lt_cs(a, b)
        character(len=*), intent(in) :: a
        type(string_t), intent(in) :: b
        lt_cs = compare(a, chars(b)) < 0
    end function lt_cs

    elemental logical function le_ss(a, b)
        type(string_t), intent(in) :: a, b
        le_ss = compare(chars(a), chars(b)) <= 0
    end function le_ss

    elemental logical function le_sc(a, b)
        type(string_t), intent(in) :: a
        character(len=*), intent(in) :: b
        le_sc = compare(chars(a), b) <= 0
    end function le_sc

    elemental logical function le_cs(a, b)
        character(len=*), intent(in) :: a
        type(string_t), intent(in) :: b
        le_cs = compare(a, chars(b)) <= 0
    end function le_cs

    elemental logical function gt_ss(a, b)
        type(string_t), intent(in) :: a, b
        gt_ss = compare(chars(a), chars(b)) > 0
    end function gt_ss

    elemental logical function gt_sc(a, b)
        type(string_t), intent(in) :: a
        character(len=*), intent(in) :: b
        gt_sc = compare(chars(a), b) > 0
    end function gt_sc

    elemental logical function gt_cs(a, b)
        character(len=*), intent(in) :: a
        type(string_t), intent(in) :: b
        gt_cs = compare(a, chars(b)) > 0
    end function gt_cs

    elemental logical function ge_ss(a, b)
        type(string_t), intent(in) :: a, b
        ge_ss = compare(chars(a), chars(b)) >= 0
    end function ge_ss

    elemental logical function ge_sc(a, b)
        type(string_t), intent(in) :: a
        character(len=*), intent(in) :: b
        ge_sc = compare(chars(a), b) >= 0
    end function ge_sc

    elemental logical function ge_cs(a, b)
        character(len=*), intent(in) :: a
        type(string_t), intent(in) :: b
        ge_cs = compare(a, chars(b)) >= 0
    end function ge_cs

end module fortio_string
