cmake_minimum_required(VERSION 3.20)
project(fortio VERSION 0.1.0 LANGUAGES Fortran)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

add_library(fortio
    src/fortio_status.f90
    src/fortio_checksum.f90
    src/fortio_bytes.f90
    src/fortio_netcdf_classic.f90
    src/fortio_netcdf_writer.f90
    src/fortio_hdf5_reader.f90
    src/fortio_hdf5_writer.f90
    src/fortio.f90
    src/hdf5_tools.f90
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
    add_executable(test_checksum test/test_checksum.f90)
    target_link_libraries(test_checksum PRIVATE fortio)
    add_test(NAME checksum COMMAND test_checksum)

    add_executable(test_bytes test/test_bytes.f90)
    target_link_libraries(test_bytes PRIVATE fortio)
    add_test(NAME bytes COMMAND test_bytes)

    add_executable(test_classic_read test/test_classic_read.f90)
    target_link_libraries(test_classic_read PRIVATE fortio)
    add_test(NAME classic_read COMMAND test_classic_read
        "${CMAKE_CURRENT_BINARY_DIR}/oracle.nc"
        "${CMAKE_CURRENT_SOURCE_DIR}/test/fixtures/oracle.cdl")

    add_executable(test_netcdf_metadata test/test_netcdf_metadata.f90)
    target_link_libraries(test_netcdf_metadata PRIVATE fortio)
    add_test(NAME netcdf_metadata COMMAND test_netcdf_metadata
        "${CMAKE_CURRENT_BINARY_DIR}/metadata-oracle.nc"
        "${CMAKE_CURRENT_SOURCE_DIR}/test/fixtures/oracle.cdl")

    add_executable(test_netcdf_attributes test/test_netcdf_attributes.f90)
    target_link_libraries(test_netcdf_attributes PRIVATE fortio)
    add_test(NAME netcdf_attributes COMMAND test_netcdf_attributes
        "${CMAKE_CURRENT_BINARY_DIR}/attribute-oracle.nc"
        "${CMAKE_CURRENT_SOURCE_DIR}/test/fixtures/oracle.cdl")

    add_executable(test_netcdf_hyperslab test/test_netcdf_hyperslab.f90)
    target_link_libraries(test_netcdf_hyperslab PRIVATE fortio)
    add_test(NAME netcdf_hyperslab COMMAND test_netcdf_hyperslab
        "${CMAKE_CURRENT_BINARY_DIR}/hyperslab-oracle.nc"
        "${CMAKE_CURRENT_SOURCE_DIR}/test/fixtures/oracle.cdl")

    add_executable(test_netcdf_characters test/test_netcdf_characters.f90)
    target_link_libraries(test_netcdf_characters PRIVATE fortio)
    add_test(NAME netcdf_characters COMMAND test_netcdf_characters
        "${CMAKE_CURRENT_BINARY_DIR}/character-oracle.nc"
        "${CMAKE_CURRENT_SOURCE_DIR}/test/fixtures/oracle.cdl")

    add_executable(test_netcdf_control test/test_netcdf_control.f90)
    target_link_libraries(test_netcdf_control PRIVATE fortio)
    add_test(NAME netcdf_control COMMAND test_netcdf_control)
    add_test(NAME netcdf_control_oracle COMMAND ncdump -v after_redef control.nc)
    set_tests_properties(netcdf_control_oracle PROPERTIES DEPENDS netcdf_control)

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

    add_executable(test_hdf5_tools_read test/test_hdf5_tools_read.f90)
    target_link_libraries(test_hdf5_tools_read PRIVATE fortio)
    add_test(NAME hdf5_tools_read COMMAND test_hdf5_tools_read
        "${CMAKE_CURRENT_BINARY_DIR}/hdf5-tools-oracle.h5"
        "${CMAKE_CURRENT_SOURCE_DIR}/test/fixtures/make_hdf5_oracle.py")

    add_executable(test_hdf5_write_oracle test/test_hdf5_write_oracle.f90)
    target_link_libraries(test_hdf5_write_oracle PRIVATE fortio)
    add_test(NAME hdf5_write_oracle COMMAND test_hdf5_write_oracle
        "${CMAKE_CURRENT_BINARY_DIR}/fortio-written.h5")

    add_executable(test_hdf5_tools_write test/test_hdf5_tools_write.f90)
    target_link_libraries(test_hdf5_tools_write PRIVATE fortio)
    add_test(NAME hdf5_tools_write COMMAND test_hdf5_tools_write
        "${CMAKE_CURRENT_BINARY_DIR}/hdf5-tools-written.h5")
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
