# test level Makefile.am

OBJEXT=o

HDFLIB = -L@HDF5ROOT@/lib -lhdf5 -lz @SZLIB@
LIBS = -L@H5P_LIB_LOC@ @H5P_LIB_NAME@ $(HDFLIB) -lm

INCLUDES = -I@H5P_LIB_LOC@ -I@HDF5ROOT@/include @MPIINC@

noinst_PROGRAMS = @TTARGET@

TESTS_ENVIRONMENT = env LD_LIBRARY_PATH=@HDF5ROOT@/lib:$(LD_LIBRARY_PATH)
TESTS = @TTARGET@

EXTRA_PROGRAMS = test testf

test_SOURCES =	test.c \
		testframe.c \
		write.c \
		read.c \
		testframe.h \
		params.h

testf_SOURCES = testf.F90

clean: clean-am
	rm -f *.h5

