# src/Fortran level Makefile.am

OBJEXT=o

HDFLIB = -L@HDF5ROOT@/lib -lhdf5 -lz @SZLIB@
LIBS = $(HDFLIB) @MPILIB@

INCLUDES = -I../include -I@HDF5ROOT@/include @MPIINC@

INCLUDES = -I@HDF5ROOT@/include @MPIINC@

EXTRA_HEADERS = 	H5PartPrivate.h \
			H5BlockPrivate.h \
			H5MultiBlockPrivate.h \
			H5PartAttrib.h

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST =		TestUnderscoreC.c \
			TestUnderscore.f \
			H5Part.f90 \
			H5PartAttrib.f90 \
			H5Block.f90 \
			H5BlockReadWrite.f90 \
			$(EXTRA_HEADERS)

# Files that I don't want to include in the dist tar ball
nodist_include_HEADERS = ../include/H5PartF.h @UNDERSCORE_H@

# What to build... Will be determined by configure script.
lib_LIBRARIES = @FORTRAN_LIB@

# Listing of all possible targets that I may build.
EXTRA_LIBRARIES = libH5hutF.a

# Header files that I wish to install in $(prefix)/include
include_HEADERS =				\
	../include/H5hut.h			\
	../include/H5.h				\
	../include/H5_inquiry.h			\
	../include/H5_attribs.h			\
	../include/H5Block.h			\
	../include/H5Fed.h			\
	../include/H5Fed_store.h		\
	../include/H5Fed_tags.h			\
	../include/H5Part.h

libH5hutF_a_SOURCES =	$(libH5Part_a_SOURCES) \
			H5PartF.c \
			H5PartAttribF.c \
			H5BlockF.c \
			H5BlockReadWriteF.c

all: ../include/H5PartF.h ./lib/libH5hutF.a

../include/H5PartF.h: H5Part.f90 H5PartAttrib.f90 H5Block.f90 H5BlockReadWrite.f90
	awk '/INTEGER\*8 FUNCTION/{print "\t" $$1 " " $$3}' $^ >$@

../lib/libH5hutF.a: libH5hutF.a
	-cp $^ $@

