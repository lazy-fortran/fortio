module hdf5_tools_f2003
    use, intrinsic :: iso_fortran_env, only: int64
    use, intrinsic :: iso_c_binding, only: c_size_t
    use hdf5_tools
    implicit none

    integer(int64), parameter :: H5T_NATIVE_INTEGER = 1_int64
    integer(int64), parameter :: H5T_NATIVE_DOUBLE = 2_int64
    integer, parameter :: SIZE_T = c_size_t
end module hdf5_tools_f2003
