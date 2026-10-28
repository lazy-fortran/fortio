program test_hdf5_filtered_read
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use fortio, only: fortio_file_t, fortio_status_t, FORTIO_ENOTSUP
    implicit none
    type(fortio_file_t) :: file
    type(fortio_status_t) :: status
    character(len=1024) :: prefix, generator, path
    character(len=8), parameter :: versions(2) = ['earliest', 'latest  ']
    character(len=1) :: suffix
    integer(int32), allocatable :: integers(:)
    real(real64), allocatable :: matrix(:, :), vector(:)
    real(real64), parameter :: expected(8) = &
        [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, &
        5.0_real64, 6.0_real64, 7.0_real64, 8.0_real64]
    integer :: version, shuffle, i, command_status

    call get_command_argument(1, prefix)
    if (len_trim(prefix) == 0) prefix = 'build/filtered-oracle'
    call get_command_argument(2, generator)
    if (len_trim(generator) == 0) &
        generator = 'test/fixtures/make_hdf5_filtered_oracle.py'
    call execute_command_line('python3 '//trim(generator)//' '//trim(prefix), &
        exitstat=command_status)
    if (command_status /= 0) error stop 'h5py fixture generation failed'
    do version = 1, 2
        do shuffle = 0, 1
            write(suffix, '(I1)') shuffle
            path = trim(prefix)//'-'//trim(versions(version))//'-'//suffix//'.h5'
            call file%open(trim(path), status)
            if (.not. status%ok()) error stop status%message
            if (version == 1) then
                call file%read('unfiltered_integer', integers, status)
                if (.not. status%ok()) error stop status%message
                if (size(integers) /= 3) error stop 'unfiltered integer shape'
                if (any(integers /= [3, -17, 42])) error stop 'unfiltered integer values'
                call file%read('unfiltered_vector', vector, status)
                if (.not. status%ok()) error stop status%message
                if (size(vector) /= 8) error stop 'unfiltered vector shape'
                if (any(vector /= expected)) error stop 'unfiltered vector values'
                call file%read('unfiltered_matrix', matrix, status)
                if (.not. status%ok()) error stop status%message
                if (any(shape(matrix) /= [32, 16])) error stop 'unfiltered matrix shape'
                if (any(matrix /= reshape([(real(i, real64), i=1, 512)], [32, 16]))) &
                    error stop 'unfiltered matrix values'
            end if
            call file%read('integer', integers, status)
            if (.not. status%ok()) error stop status%message
            print *, trim(versions(version)), shuffle, integers
            if (size(integers) /= 3) error stop 'filtered integer shape'
            if (any(integers /= [3, -17, 42])) error stop 'filtered integer values'
            call file%read('matrix', matrix, status)
            if (.not. status%ok()) error stop status%message
            if (any(shape(matrix) /= [32, 16])) error stop 'filtered matrix shape'
            if (any(matrix /= reshape([(real(i, real64), i=1, 512)], [32, 16]))) &
                error stop 'filtered matrix values'
            call file%read('multichunk', integers, status)
            if (status%code /= FORTIO_ENOTSUP) &
                error stop 'unsupported chunk index must report ENOTSUP'
            call file%read('fletcher', vector, status)
            if (status%code /= FORTIO_ENOTSUP) then
                print *, 'fletcher status', status%code, 'values', vector
                error stop 'applied fletcher32 chunk must report ENOTSUP'
            end if
            call file%read('fletcher_integer', integers, status)
            if (status%code /= FORTIO_ENOTSUP) then
                print *, 'fletcher_integer status', status%code, 'values', integers
                error stop 'applied fletcher32 integer chunk must report ENOTSUP'
            end if
            call file%read('fletcher_corrupt', vector, status)
            if (status%code /= FORTIO_ENOTSUP) then
                print *, 'fletcher_corrupt status', status%code, 'values', vector
                error stop 'corrupt fletcher32 chunk must report ENOTSUP'
            end if
            call file%read('fletcher_skipped', vector, status)
            if (.not. status%ok()) error stop status%message
            if (any(vector /= expected)) error stop 'skipped filter values'
            call file%read('pipeline_skipped', vector, status)
            if (.not. status%ok()) error stop status%message
            if (any(vector /= expected)) error stop 'skipped pipeline values'
            call file%close(status)
            if (.not. status%ok()) error stop status%message
        end do
    end do
end program test_hdf5_filtered_read
