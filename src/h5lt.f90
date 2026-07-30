module h5lt
    use hdf5_tools, only: HID_T, HSIZE_T, SIZE_T, h5_get_dataset_info
    implicit none
    private

    public :: h5ltget_dataset_info_f

contains

    subroutine h5ltget_dataset_info_f(loc_id, dset_name, dims, type_class, &
            type_size, hdferr)
        integer(HID_T), intent(in) :: loc_id
        character(len=*), intent(in) :: dset_name
        integer(HSIZE_T), intent(out) :: dims(:)
        integer, intent(out) :: type_class
        integer(SIZE_T), intent(out) :: type_size
        integer, intent(out) :: hdferr

        call h5_get_dataset_info(loc_id, dset_name, dims, type_class, type_size, &
            hdferr)
    end subroutine h5ltget_dataset_info_f

end module h5lt
