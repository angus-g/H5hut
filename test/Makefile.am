# test level Makefile.am

#SUBDIRS = H5Block H5Fed H5Part
SUBDIRS = H5Fed

OBJEXT=o

AM_LDFLAGS = -I../src/include @AM_CPPFLAGS@

noinst_PROGRAMS = @BUILD_TESTS@

TESTS_ENVIRONMENT = env LD_LIBRARY_PATH=@HDF5ROOT@/lib:$(LD_LIBRARY_PATH)
TESTS = @BUILD_TESTS@

EXTRA_PROGRAMS = h5u_test h5b_test h5_attach_test

h5u_test_SOURCES =	\
		h5u_test.c \
		h5u_write.c \
		h5u_read.c \
		testframe.c \
		testframe.h \
		params.h

h5u_test_LDADD = \
		../src/C/libH5hutC.la \
		../src/h5core/libH5hut.la

h5b_test_SOURCES =	\
		h5b_test.c \
		h5b_write.c \
		h5b_read.c \
		testframe.c \
		testframe.h \
		params.h

h5b_test_LDADD = \
		../src/C/libH5hutC.la \
		../src/h5core/libH5hut.la

h5_attach_test_SOURCES = \
		h5_attach_test.c

h5_attach_test_LDADD = \
		../src/C/libH5hutC.la \
		../src/h5core/libH5hut.la

clean-local:
	${RM} -f *.h5

