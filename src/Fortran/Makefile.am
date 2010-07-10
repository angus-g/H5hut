# src/Fortran level Makefile.am

OBJEXT=o

HDFLIB = -L@HDF5ROOT@/lib -lhdf5 -lz @SZLIB@
LIBS = $(HDFLIB) @MPILIB@

INCLUDES = -I../include -I@HDF5ROOT@/include @MPIINC@

F90_FILES =     	H5.f90 \
			H5_attribs.f90 \
			H5Part.f90 \
			H5Block.f90 \
			H5Block_readwrite.f90

EXTRA_HEADERS =		../include/H5hut.h

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST =		TestUnderscoreC.c \
			TestUnderscore.f \
			$(F90_FILES)

# Files that I don't want to include in the dist tar ball
#nodist_include_HEADERS = ../include/H5hutF.h @UNDERSCORE_H@
nodist_include_HEADERS = ../include/H5hutF.h

# What to build... Will be determined by configure script.
lib_LIBRARIES = @FORTRAN_LIB@

# Listing of all possible targets that I may build.
EXTRA_LIBRARIES = libH5hutF.a

libH5hutF_a_SOURCES =	H5_F.c \
			H5_attribs_F.c \
			H5Part_F.c \
			H5Block_F.c \
			H5Block_readwrite_F.c

all: ../include/H5hutF.h ../lib/libH5hutF.a

../include/H5hutF.h: $(F90_FILES)
	awk '/INTEGER\*8 FUNCTION/{print "\t" $$1 " " $$3}' $^ >$@

../lib/libH5hutF.a: libH5hutF.a
	-cp $^ $@

