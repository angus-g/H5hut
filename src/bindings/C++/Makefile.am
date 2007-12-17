HDF5ROOT = @HDF5ROOT@

# COMPILERS
CC = @CC@
CXX = @CXX@

CFLAGS = @CFLAGS@ @MPIINC@ @HDF5ROOT@/include

lib_LIBRARIES = libH5XX.a
include_HEADERS = H5Fed.hh
libH5XX_a_SOURCES = H5Fed.cc

libH5XX.a: $(libH5XX_a_OBJECTS)
	${AR} rucs $@ $^

%.o : %.cc
	$(CXX) $(CFLAGS) $(INC) -c $<
