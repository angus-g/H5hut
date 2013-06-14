
if ENABLE_FORTRAN
AM_CPPFLAGS += -I$(top_srcdir)/src/include

F90_FILES =			\
	H5F.f90			\
	H5_constF.f90		\
	H5_attribsF.f90		\
	H5PartF.f90		\
	H5Part_ioF.f90		\
	H5BlockF.f90		\
	H5Block_attribsF.f90	\
	H5Block_ioF.f90

EXTRA_HEADERS =

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST =			\
	TestUnderscoreC.c 	\
	TestUnderscore.f 	\
	h5_private.h		\
	$(F90_FILES)

# Files that I don't want to include in the dist tar ball
nodist_include_HEADERS = 	\
	$(top_srcdir)/src//include/H5hutF.h

# What to build... Will be determined by configure script.
lib_LTLIBRARIES = libH5hutF.la

include_HEADERS =		\
	$(top_srcdir)/src/include/H5hutF.h

libH5hutF_la_SOURCES =		\
	H5.c			\
	H5_attribs.c		\
	H5Part.c		\
	H5Part_io.c		\
	H5Block.c		\
	H5Block_attribs.c	\
	H5Block_io.c

libH5hutF_la_DEPENDENCIES =	\
	$(top_srcdir)/src/include/H5hutF.h

libH5hutF_la_LDFLAGS = -version-info 2:0:0 -rpath '$(libdir)'

$(top_srcdir)/src/include/H5hutF.h: $(F90_FILES)
	awk '/INTEGER\*8 :: /{print "\t" $$0}' $^ >$@
	awk '/PARAMETER /{print "\t" $$0}' $^ >> $@
	awk '/INTEGER\*8 FUNCTION/{print "\t" $$1 " " $$3}' $^ >>$@

all-local: $(top_srcdir)/src/include/H5hutF.h
	$(INSTALL) -m644 .libs/libH5hutF.a $(top_srcdir)/src/lib
endif

clean: clean-am

clean-local:
	$(RM) -f *~
