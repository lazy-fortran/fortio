cmake_minimum_required(VERSION 3.20)
project(fortio VERSION 0.1.0 LANGUAGES Fortran)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

add_library(fortio
    src/fortio_status.f90
    src/fortio_bytes.f90
    src/fortio_netcdf_classic.f90
    src/fortio_netcdf_writer.f90
    src/fortio_hdf5_reader.f90
    src/fortio.f90
    src/netcdf.f90
)
add_library(fortio::fortio ALIAS fortio)
set_target_properties(fortio PROPERTIES
    Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules"
)
target_include_directories(fortio PUBLIC
    "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules>"
    "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>"
)

include(CTest)
if(BUILD_TESTING)
    add_executable(test_bytes test/test_bytes.f90)
    target_link_libraries(test_bytes PRIVATE fortio)
    add_test(NAME bytes COMMAND test_bytes)

    add_executable(test_classic_read test/test_classic_read.f90)
    target_link_libraries(test_classic_read PRIVATE fortio)
    add_test(NAME classic_read COMMAND test_classic_read
        "${CMAKE_CURRENT_BINARY_DIR}/oracle.nc"
        "${CMAKE_CURRENT_SOURCE_DIR}/test/fixtures/oracle.cdl")

    add_executable(test_classic_write_oracle test/test_classic_write_oracle.f90)
    target_link_libraries(test_classic_write_oracle PRIVATE fortio)
    add_test(NAME classic_write COMMAND test_classic_write_oracle
        "${CMAKE_CURRENT_BINARY_DIR}/fortio-written.nc")
    add_test(NAME classic_write_oracle
        COMMAND ncdump -v scalar,x_values,matrix
        "${CMAKE_CURRENT_BINARY_DIR}/fortio-written.nc")
    set_tests_properties(classic_write_oracle PROPERTIES DEPENDS classic_write)

    add_executable(test_hdf5_read test/test_hdf5_read.f90)
    target_link_libraries(test_hdf5_read PRIVATE fortio)
    add_test(NAME hdf5_read COMMAND test_hdf5_read
        "${CMAKE_CURRENT_BINARY_DIR}/oracle.h5"
        "${CMAKE_CURRENT_SOURCE_DIR}/test/fixtures/make_hdf5_oracle.py")
endif()

install(TARGETS fortio EXPORT fortio-targets
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
)
install(DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules/"
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}" FILES_MATCHING PATTERN "*.mod")
install(EXPORT fortio-targets NAMESPACE fortio::
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/fortio")

configure_package_config_file(
    cmake/fortio-config.cmake.in
    "${CMAKE_CURRENT_BINARY_DIR}/fortio-config.cmake"
    INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/fortio"
)
write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/fortio-config-version.cmake"
    VERSION "${PROJECT_VERSION}"
    COMPATIBILITY SameMajorVersion
)
install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/fortio-config.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/fortio-config-version.cmake"
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/fortio"
)
