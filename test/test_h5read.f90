program test_h5read
    use, intrinsic :: iso_fortran_env, only: int8, int64, real64
    use fortio_status, only: fortio_status_t
    use fortio_string, only: string_t, string, join, len, assignment(=), operator(//), &
        operator(==)
    use fortio_h5read, only: h5_reader_t, h5_type_t, H5_STRING, H5_VLEN, H5_COMPOUND
    implicit none

    type(string_t) :: dir, script
    character(len=4096) :: buffer
    integer :: n, exit_code

    call get_command_argument(1, buffer, n)
    dir = "build/fortio-h5read"
    if (n > 0) dir = buffer(1:n)
    call get_command_argument(2, buffer, n)
    script = "test/fixtures/h5read_oracle.py"
    if (n > 0) script = buffer(1:n)
    call execute_command_line("python3 " // script%str() // " generate " // dir%str(), &
        exitstat=exit_code)
    if (exit_code /= 0) error stop "oracle could not generate the files"
    call dump(dir // "/earliest.h5")
    call dump(dir // "/latest.h5")
    call execute_command_line("python3 " // script%str() // " verify " // dir%str(), &
        exitstat=exit_code)
    if (exit_code /= 0) error stop "Fortio differs from h5py"

contains

    subroutine dump(path)
        type(string_t), intent(in) :: path
        type(h5_reader_t) :: h5
        type(fortio_status_t) :: status
        integer :: unit

        call h5%open(path%str(), status)
        if (.not. status%ok()) error stop status%message
        open (newunit=unit, file=path%str() // ".dump", status="replace", action="write", &
              access="stream", form="unformatted")
        call walk(h5, unit, string(""))
        close (unit)
        call h5%close(status)
    end subroutine dump

    recursive subroutine walk(h5, unit, group)
        type(h5_reader_t), intent(inout) :: h5
        integer, intent(in) :: unit
        type(string_t), intent(in) :: group
        type(fortio_status_t) :: status
        type(string_t), allocatable :: names(:), attrs(:)
        type(string_t) :: path
        integer(int64), allocatable :: dims(:)
        type(h5_type_t) :: t
        integer :: i, k

        call h5%children(group%str() // "/", names, status)
        if (.not. status%ok()) error stop status%message
        do i = 1, size(names)
            path = group // "/" // names(i)
            if (h5%is_group(path%str())) then
                call put(unit, "G " // path)
                call walk(h5, unit, path)
            else
                call h5%shape(path%str(), dims, status)
                if (.not. status%ok()) cycle
                if (size(dims) > 0) then
                    if (dims(1) < 0) then
                        call put(unit, "D " // path // " null")
                        call attributes(h5, unit, path)
                        cycle
                    end if
                end if
                call put(unit, "D " // path // " " // join(string(dims), " "))
                call h5%datatype(path%str(), t, status)
                call dataset_values(h5, unit, path, t)
            end if
            call attributes(h5, unit, path)
        end do
    end subroutine walk

    subroutine attributes(h5, unit, path)
        type(h5_reader_t), intent(inout) :: h5
        integer, intent(in) :: unit
        type(string_t), intent(in) :: path
        type(fortio_status_t) :: status
        type(string_t), allocatable :: names(:), text(:)
        type(h5_type_t) :: t
        real(real64), allocatable :: x(:)
        integer :: i, k

        call h5%attribute_names(path%str(), names, status)
        if (.not. status%ok()) error stop status%message
        do i = 1, size(names)
            call put(unit, "A " // path // " " // names(i))
            call h5%attribute_type(path%str(), names(i)%str(), t, status)
            if (t%class == H5_STRING .or. t%class == H5_VLEN) then
                call h5%read_attribute(path%str(), names(i)%str(), text, status)
                if (.not. status%ok()) error stop status%message
                do k = 1, size(text)
                    call put(unit, "S " // escape(text(k)))
                end do
            else
                call h5%read_attribute(path%str(), names(i)%str(), x, status)
                if (.not. status%ok()) error stop status%message
                do k = 1, size(x)
                    call put(unit, "N " // string(x(k)))
                end do
            end if
        end do
    end subroutine attributes

    subroutine dataset_values(h5, unit, path, t)
        type(h5_reader_t), intent(inout) :: h5
        integer, intent(in) :: unit
        type(string_t), intent(in) :: path
        type(h5_type_t), intent(in) :: t
        type(fortio_status_t) :: status
        real(real64), allocatable :: x(:)
        complex(real64), allocatable :: z(:)
        type(string_t), allocatable :: text(:)
        integer(int64) :: k

        if (t%class == H5_STRING .or. t%class == H5_VLEN) then
            call h5%read(path%str(), text, status)
            if (.not. status%ok()) error stop path%str() // ": " // status%message
            do k = 1, size(text)
                call put(unit, "S " // escape(text(k)))
            end do
        else if (t%is_complex()) then
            call h5%read(path%str(), z, status)
            if (.not. status%ok()) error stop path%str() // ": " // status%message
            do k = 1, size(z)
                call put(unit, "C " // string(z(k)%re) // " " // string(z(k)%im))
            end do
        else
            call h5%read(path%str(), x, status)
            if (.not. status%ok()) error stop path%str() // ": " // status%message
            do k = 1, size(x)
                call put(unit, "N " // string(x(k)))
            end do
        end if
    end subroutine dataset_values

    function escape(s) result(out)
        type(string_t), intent(in) :: s
        type(string_t) :: out

        out = s%replace("\", "\\")
        out = out%replace(achar(10), "\n")
    end function escape

    subroutine put(unit, line)
        integer, intent(in) :: unit
        type(string_t), intent(in) :: line

        write (unit) line%str() // achar(10)
    end subroutine put

end program test_h5read
