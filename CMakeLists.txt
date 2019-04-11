cmake_minimum_required(VERSION 3.14)
project(H5hut)

option(USE_FORTRAN "Build Fortran library" ON)

include_directories("${PROJECT_SOURCE_DIR}/src/include")
add_subdirectory(src/h5core)

if (USE_FORTRAN)
  enable_language(Fortran)
  add_subdirectory(src/Fortran)
endif (USE_FORTRAN)
