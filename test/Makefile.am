# test level Makefile.am

OBJEXT=o

AM_CPPFLAGS = -I../src/include @AM_CPPFLAGS@

LDADD = \
	../src/lib/libH5hut.a

noinst_PROGRAMS = @BUILD_TESTS@

TESTS_ENVIRONMENT = env LD_LIBRARY_PATH=@HDF5_PREFIX@/lib:$(LD_LIBRARY_PATH)
TESTS = @BUILD_TESTS@

EXTRA_PROGRAMS = h5u_test h5b_test h5_attach_test

h5u_test_SOURCES =	\
		h5u_test.c \
		h5u_write.c \
		h5u_read.c \
		testframe.c \
		testframe.h \
		params.h

h5b_test_SOURCES =	\
		h5b_test.c \
		h5b_write.c \
		h5b_read.c \
		testframe.c \
		testframe.h \
		params.h

h5_attach_test_SOURCES = \
		h5_attach_test.c

clean-local:
	${RM} -f *.h5

