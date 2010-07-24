# test level Makefile.am

#SUBDIRS = H5Block H5Fed H5Part

OBJEXT=o

AM_LDFLAGS = -L../src/h5core -L../src/C @LDFLAGS@
LIBS = -lH5hutC -lH5hut @LIBS@

INCLUDES = -I../src/include @INCLUDES@

noinst_PROGRAMS = @BUILD_TESTS@

TESTS_ENVIRONMENT = env LD_LIBRARY_PATH=@HDF5ROOT@/lib:$(LD_LIBRARY_PATH)
TESTS = @BUILD_TESTS@

EXTRA_PROGRAMS = h5u_test h5b_test

h5u_test_SOURCES =	\
		h5u_test.c \
		h5u_write.c \
		h5u_read.c \
		testframe.c \
		testframe.h \
		params.h

h5u_test_DEPENDENCIES = \
		../src/h5core/libH5hut.a \
		../src/C/libH5hutC.a

h5b_test_SOURCES =	\
		h5b_test.c \
		h5b_write.c \
		h5b_read.c \
		testframe.c \
		testframe.h \
		params.h

h5b_test_DEPENDENCIES = \
		../src/h5core/libH5hut.a \
		../src/C/libH5hutC.a

clean: clean-am
	rm -f *.h5

