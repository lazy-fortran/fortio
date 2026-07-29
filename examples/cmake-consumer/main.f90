program main
    use fortio, only: fortio_file_t
    implicit none

    type(fortio_file_t) :: file

    if (.not. same_type_as(file, file)) error stop "fortio type unavailable"
end program main
