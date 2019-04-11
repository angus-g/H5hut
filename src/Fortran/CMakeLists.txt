set(h5hutf_SOURCES
  H5_const.f90 H5_file.f90 H5_model.f90 H5_file_attribs.f90 H5_step_attribs.f90
  H5_log.f90 H5_err.f90 H5Part_model.f90 H5Part_io.f90
  H5Block_model.f90 H5Block_attribs.f90 H5Block_io.f90)
set(h5hutf_SOURCES_PATH ${h5hutf_SOURCES})

set(CMAKE_Fortran_FORMAT FREE)

list(TRANSFORM h5hutf_SOURCES_PATH PREPEND ${CMAKE_CURRENT_SOURCE_DIR}/)

set(INTERFACE_FILE ${CMAKE_CURRENT_BINARY_DIR}/h5hut.f)

# if (${CMAKE_Fortran_COMPILER_ID} EQUAL GNU)
#   set(CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS} -ffree-line-length-none)
# endif (${CMAKE_Fortran_COMPILER_ID} EQUAL GNU)

add_custom_command(
  OUTPUT ${INTERFACE_FILE}
  COMMAND awk [[/INTEGER\*8 :: / { print "       " $0 } /PARAMETER / { print "       " $0 } /INTEGER\*8 FUNCTION/ { print "       " $1 " " $3 }]] ${h5hutf_SOURCES_PATH} > ${INTERFACE_FILE}
  VERBATIM
  DEPENDS ${h5hutf_SOURCES})

include(FortranCInterface)
FortranCInterface_HEADER(FC.h MACRO_NAMESPACE "FC_")

add_library(H5hutf
  H5.c H5_attribs.c H5_model.c H5Part_model.c H5Part_io.c
  H5Block_model.c H5Block_attribs.c H5Block_io.c
  H5hut.f90)

target_include_directories(H5hutf PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")

