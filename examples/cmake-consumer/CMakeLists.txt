cmake_minimum_required(VERSION 3.20)
project(fortio_consumer LANGUAGES Fortran)

find_package(fortio CONFIG REQUIRED)
add_executable(fortio_consumer main.f90)
target_link_libraries(fortio_consumer PRIVATE fortio::fortio)
