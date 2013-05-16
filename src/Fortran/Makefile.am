
if ENABLE_FORTRAN
AM_CPPFLAGS += -I$(top_srcdir)/src/include

F90_FILES =     	H5.f90 \
			H5_attribs.f90 \
			H5Part.f90 \
			H5Block.f90 \
			H5Block_readwrite.f90

EXTRA_HEADERS =

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST =		TestUnderscoreC.c \
			TestUnderscore.f \
			$(F90_FILES)

# Files that I don't want to include in the dist tar ball
#nodist_include_HEADERS = ../include/H5hutF.h @UNDERSCORE_H@
nodist_include_HEADERS = $(top_srcdir)/src/include/H5hutF.h

# What to build... Will be determined by configure script.
lib_LTLIBRARIES = libH5hutF.la

include_HEADERS =		\
	$(top_srcdir)/src/include/H5hutF.h

libH5hutF_la_SOURCES = 	\
	H5_F.c			\
	H5_attribs_F.c		\
	H5Part_F.c		\
	H5Block_F.c		\
	H5Block_readwrite_F.c

libH5hutF_la_DEPENDENCIES = $(top_srcdir)/src/include/H5hutF.h

libH5hutF_la_LDFLAGS = -version-info 2:0:0 -rpath '$(libdir)'

$(top_srcdir)/src/include/H5hutF.h: $(F90_FILES)
	awk '/INTEGER\*8 FUNCTION/{print "\t" $$1 " " $$3}' $^ >$@

all-local:
	$(INSTALL) -m755 -d ../lib
	$(INSTALL) -m644 .libs/libH5hutF.a ../lib
endif

clean: clean-am

clean-local:
	$(RM) -f *~
