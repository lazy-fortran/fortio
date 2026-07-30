cmake_minimum_required(VERSION 3.20)
project(fortio VERSION 0.1.0 LANGUAGES C Fortran)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)
option(FORTIO_BUILD_BENCHMARKS "Build comparisons against system NetCDF/HDF5" OFF)
option(FORTIO_INSTALL "Install fortio targets and package files" ${PROJECT_IS_TOP_LEVEL})

add_library(fortio
    src/fortio_posix_io.c
    src/fortio_posix.f90
    src/fortio_status.f90
    src/fortio_checksum.f90
    src/fortio_bytes.f90
    src/fortio_netcdf_classic.f90
    src/fortio_netcdf_writer.f90
    src/fortio_hdf5_reader.f90
    src/fortio_hdf5_writer.f90
    src/fortio.f90
    src/hdf5_tools.f90
    src/hdf5_tools_f2003.f90
    src/netcdf.f90
)
add_library(fortio::fortio ALIAS fortio)
target_link_libraries(fortio PUBLIC pthread)
set_target_properties(fortio PROPERTIES
    Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules"
)
target_include_directories(fortio PUBLIC
    "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules>"
    "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>"
)
if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # fortio uses standard allocatable assignment internally. Some consumers,
    # including libneo, disable that language behavior project-wide.
    target_compile_options(fortio PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:-frealloc-lhs>)
endif()

if(FORTIO_BUILD_BENCHMARKS)
    find_program(NF_CONFIG nf-config REQUIRED)
    execute_process(COMMAND "${NF_CONFIG}" --includedir
        OUTPUT_VARIABLE NETCDFF_INCLUDE_DIR OUTPUT_STRIP_TRAILING_WHITESPACE
        COMMAND_ERROR_IS_FATAL ANY)
    execute_process(COMMAND "${NF_CONFIG}" --flibs
        OUTPUT_VARIABLE NETCDFF_LINK_FLAGS OUTPUT_STRIP_TRAILING_WHITESPACE
        COMMAND_ERROR_IS_FATAL ANY)
    separate_arguments(NETCDFF_LINK_FLAGS NATIVE_COMMAND "${NETCDFF_LINK_FLAGS}")

    add_executable(benchmark_fortio_netcdf benchmark/netcdf_roundtrip.f90)
    target_link_libraries(benchmark_fortio_netcdf PRIVATE fortio)

    add_executable(benchmark_native_netcdf benchmark/netcdf_roundtrip.f90)
    target_include_directories(benchmark_native_netcdf PRIVATE "${NETCDFF_INCLUDE_DIR}")
    target_link_libraries(benchmark_native_netcdf PRIVATE ${NETCDFF_LINK_FLAGS})

    find_package(HDF5 COMPONENTS Fortran REQUIRED)
    add_executable(benchmark_fortio_hdf5 benchmark/hdf5_roundtrip_fortio.f90)
    target_link_libraries(benchmark_fortio_hdf5 PRIVATE fortio)

    add_executable(benchmark_native_hdf5 benchmark/hdf5_roundtrip_native.f90)
    target_include_directories(benchmark_native_hdf5 PRIVATE ${HDF5_Fortran_INCLUDE_DIRS})
    target_link_libraries(benchmark_native_hdf5 PRIVATE ${HDF5_Fortran_LIBRARIES})
endif()

include(CTest)
if(BUILD_TESTING)
    find_package(OpenMP REQUIRED COMPONENTS Fortran)
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

    add_executable(test_netcdf4_read test/test_netcdf4_read.f90)
    target_link_libraries(test_netcdf4_read PRIVATE fortio)
    add_test(NAME netcdf4_read COMMAND test_netcdf4_read
        "${CMAKE_CURRENT_BINARY_DIR}/netcdf4-oracle.nc"
        "${CMAKE_CURRENT_SOURCE_DIR}/test/fixtures/netcdf4-oracle.cdl")

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
    add_test(NAME netcdf_control_reuse_oracle
        COMMAND ncdump -v second_write,later_write control-second.nc)
    set_tests_properties(netcdf_control_reuse_oracle PROPERTIES DEPENDS netcdf_control)

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

    add_executable(test_thread_safety test/test_thread_safety.f90)
    target_link_libraries(test_thread_safety PRIVATE fortio OpenMP::OpenMP_Fortran)
    add_test(NAME thread_safety COMMAND test_thread_safety
        "${CMAKE_CURRENT_BINARY_DIR}/thread-oracle.h5"
        "${CMAKE_CURRENT_SOURCE_DIR}/test/fixtures/make_hdf5_oracle.py"
        "${CMAKE_CURRENT_BINARY_DIR}/thread-oracle.nc"
        "${CMAKE_CURRENT_SOURCE_DIR}/test/fixtures/oracle.cdl")

    add_executable(test_concurrent_hdf5_writes test/test_concurrent_hdf5_writes.f90)
    target_link_libraries(test_concurrent_hdf5_writes PRIVATE fortio OpenMP::OpenMP_Fortran)
    add_test(NAME concurrent_hdf5_writes COMMAND test_concurrent_hdf5_writes
        "${CMAKE_CURRENT_BINARY_DIR}/concurrent-writes.h5"
        "${CMAKE_CURRENT_SOURCE_DIR}/test/fixtures/verify_thread_writes.py")
endif()

if(FORTIO_INSTALL)
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
endif()
