HDF5ROOT = @HDF5ROOT@

# COMPILERS
CC = @CC@
CXX = @CXX@

HDF5INC = -I@HDF5ROOT@/include
MPIINC = -I@MPIROOT@/include
INCL = $(HDF5INC) ${MPIINC} -I../../

CFLAGS = @CFLAGS@ $(INCL)

OBJEXT = o
lib_LIBRARIES = libH5XX.a
include_HEADERS = H5Fed.hh
libH5XX_a_SOURCES = H5Fed.cc

libH5XX.a: $(libH5XX_a_OBJECTS)
	${AR} rucs $@ $^

%.o : %.cc
	$(CXX) $(CFLAGS) $(INC) -c $<
