program test_string
    use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
    use fortio_string, only: string_t, string, join, len, assignment(=), operator(//), &
        operator(==), operator(/=), operator(<), operator(<=), operator(>), operator(>=)
    implicit none

    call check_construction()
    call check_comparison()
    call check_search()
    call check_transform()
    call check_split_join()
    call check_numbers()
    call check_io()

contains

    subroutine expect(condition, what)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: what

        if (.not. condition) error stop what
    end subroutine expect

    subroutine check_construction()
        type(string_t) :: s, empty
        character(len=:), allocatable :: c

        s = "path/to "
        call expect(len(s) == 8, "trailing blank kept")
        call expect(s%str() == "path/to ", "str")
        call expect(len(empty) == 0 .and. empty%is_empty(), "default is empty")
        call expect(empty == "", "default equals empty text")
        c = s
        call expect(len(c) == 8, "assignment to deferred character")
        s = s // "x" // string("y")
        call expect(s == "path/to xy", "concatenation")
        call expect(("<" // s) == "<path/to xy", "character on the left")
        call expect(s%char_at(1) == "p" .and. s%char_at(-1) == "y", "char_at")
        call expect(s%slice(6) == "to xy" .and. s%slice(-2) == "xy", "slice to end")
        call expect(s%slice(1, 4) == "path" .and. s%slice(5, 2) == "", "slice range")
    end subroutine check_construction

    subroutine check_comparison()
        type(string_t) :: a, b, words(3)

        a = "abc"
        b = "abc "
        call expect(a /= b, "trailing blank is significant")
        call expect(("abc" == "abc ") .and. .not. (a == "abc "), "unlike intrinsic padding")
        call expect(a < b .and. a <= b .and. b > a .and. b >= a, "prefix sorts first")
        call expect(string("B") < "a", "character code order")
        call expect("abd" > a .and. "abb" < a, "mixed operands")
        words = [string("x"), string("y"), string("x")]
        call expect(all((words == "x") .eqv. [.true., .false., .true.]), "elemental comparison")
    end subroutine check_comparison

    subroutine check_search()
        type(string_t) :: s

        s = "time_slice[]&toroidal_mode[]&n_tor"
        call expect(s%find("[]") == 11, "find")
        call expect(s%find("[]", 12) == 27, "find from start")
        call expect(s%find("zz") == 0, "find absent")
        call expect(s%rfind("&") == 29, "rfind")
        call expect(s%count("[]") == 2 .and. s%count("&") == 2, "count")
        call expect(s%starts_with("time") .and. .not. s%starts_with("toroidal"), "starts_with")
        call expect(s%ends_with("n_tor") .and. .not. s%ends_with("n_tor "), "ends_with")
    end subroutine check_search

    subroutine check_transform()
        type(string_t) :: s

        s = achar(9) // "  Mixed Case " // achar(10)
        call expect(s%strip() == "Mixed Case", "strip")
        call expect(s%lstrip() == "Mixed Case " // achar(10), "lstrip")
        call expect(s%rstrip() == achar(9) // "  Mixed Case", "rstrip")
        s = "Mixed Case"
        call expect(s%upper() == "MIXED CASE", "upper")
        call expect(s%lower() == "mixed case", "lower")
        s = "a.b.c"
        call expect(s%replace(".", "/") == "a/b/c", "replace")
        call expect(s%replace(".", "") == "abc", "replace by nothing")
        s = "aaa"
        call expect(s%replace("aa", "b") == "ba", "non-overlapping replace")
    end subroutine check_transform

    subroutine check_split_join()
        type(string_t) :: s
        type(string_t), allocatable :: parts(:)

        s = "profiles_1d(2)/grid/rho_tor_norm"
        parts = s%split("/")
        call expect(size(parts) == 3, "split count")
        call expect(parts(1) == "profiles_1d(2)" .and. parts(3) == "rho_tor_norm", "split fields")
        call expect(join(parts, "/") == s, "join inverts split")
        s = "/a//b/"
        parts = s%split("/")
        call expect(size(parts) == 5, "empty fields kept")
        call expect(parts(1) == "" .and. parts(3) == "" .and. parts(5) == "", "empty fields")
        s = "  1.5   2  " // achar(9) // "x "
        parts = s%split()
        call expect(size(parts) == 3, "whitespace split count")
        call expect(parts(1) == "1.5" .and. parts(2) == "2" .and. parts(3) == "x", "whitespace split")
        s = "   "
        parts = s%split()
        call expect(size(parts) == 0, "blank split is empty")
        call expect(join(parts, ",") == "", "join of nothing")
        call expect(join([string("a"), string("b")]) == "ab", "join without separator")
    end subroutine check_split_join

    subroutine check_numbers()
        type(string_t) :: s
        logical :: ok
        real(real64) :: values(7), back
        integer :: i

        call expect(string(0_int32) == "0" .and. string(-42_int32) == "-42", "int32 text")
        call expect(string(huge(1_int64)) == "9223372036854775807", "int64 max")
        call expect(string(-huge(1_int64) - 1_int64) == "-9223372036854775808", "int64 min")
        call expect(string(.true.) == "true" .and. string(.false.) == "false", "logical text")
        call expect(string(0.1_real64) == "0.1", "shortest 0.1")
        call expect(string(1.0_real64) == "1" .and. string(-2.5_real64) == "-2.5", "short reals")
        call expect(string(1.0e300_real64) == "1e300", "large exponent")
        call expect(string(0.1_real32) == "0.1", "shortest real32")
        call expect(string(1.0e15_real64) == "1000000000000000", "positional up to 1e15")
        call expect(string(1.0e16_real64) == "1e16", "exponent from 1e16")
        call expect(string(1.25e-4_real64) == "0.000125", "positional down to 1e-4")
        call expect(string(-1.0e-5_real64) == "-1e-5", "exponent below 1e-4")
        call expect(string(12.5_real64) == "12.5" .and. string(0.0_real64) == "0", "plain")
        values = [0.1_real64, 1.0_real64/3, 2.0_real64**(-1074), huge(1.0_real64), &
                  -6.02214076e23_real64, 1.0_real64 + epsilon(1.0_real64), 123456789.0_real64]
        do i = 1, size(values)
            s = string(values(i))
            back = s%to_real(ok)
            call expect(ok .and. back == values(i), "real round trip")
        end do
        s = " -17 "
        call expect(s%to_int(ok) == -17_int64 .and. ok, "to_int")
        s = "1.5"
        i = int(s%to_int(ok))
        call expect(.not. ok, "to_int rejects a real")
        s = "abc"
        back = s%to_real(ok)
        call expect(.not. ok, "to_real rejects text")
    end subroutine check_numbers

    subroutine check_io()
        type(string_t) :: s
        character(len=32) :: buffer

        s = "x y"
        write (buffer, "(dt,a)") s, "|"
        call expect(buffer(1:4) == "x y|", "formatted write keeps blanks")
    end subroutine check_io

end program test_string
