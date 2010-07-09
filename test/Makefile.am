# test level Makefile.am

#SUBDIRS = H5Block H5Fed H5Part

OBJEXT=o

HDFLIB = -L@HDF5ROOT@/lib -lhdf5 -lz @SZLIB@
H5LIB = -L../src/lib -lH5hutC -lH5core
LIBS = $(H5LIB) $(HDFLIB) @MPILIB@

INCLUDES = -I../src/include -I@HDF5ROOT@/include @MPIINC@

noinst_PROGRAMS = h5u_test

TESTS_ENVIRONMENT = env LD_LIBRARY_PATH=@HDF5ROOT@/lib:$(LD_LIBRARY_PATH)
TESTS = h5u_test

EXTRA_PROGRAMS = h5u_test

h5u_test_SOURCES =	\
		h5u_test.c \
		h5u_write.c \
		h5u_read.c \
		testframe.c \
		testframe.h \
		params.h

clean: clean-am
	rm -f *.h5

