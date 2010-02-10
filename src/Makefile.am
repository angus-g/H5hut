# src level Makefile.am

OBJEXT=o

INCLUDES = -I@HDF5ROOT@/include @MPIINC@

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST =		TestUnderscoreC.c \
			TestUnderscore.f \
			generate-attr.py \
			generate-h5bl-readwrite.py \
			generate-h5multi-readwrite.py \
			H5Part.f90 \
			H5PartAttrib.f90 \
			H5Block.f90 \
			H5BlockReadWrite.f90

# Files that I don't want to include in the dist tar ball
nodist_include_HEADERS = H5PartF.h @UNDERSCORE_H@

# What to build... Will be determined by configure script.
lib_LIBRARIES = @MTARGET@

# Listing of all possible targets that I may build.
EXTRA_LIBRARIES = libH5Part.a libH5PartF.a

# Header files that I wish to install in $(prefix)/include
include_HEADERS = 	H5Part.h \
			H5PartTypes.h \
			H5PartErrors.h \
			H5PartAttrib.h \
			H5Block.h \
			H5BlockTypes.h \
			H5BlockErrors.h \
			H5BlockReadWrite.h \
			H5MultiBlock.h \
			H5MultiBlockTypes.h \
			H5MultiBlockErrors.h \
			H5MultiBlockReadWrite.h \
			H5PartF.h \
			@UNDERSCORE_H@

# Listing of all possible headers that I may include
EXTRA_HEADERS =		H5PartPrivate.h \
			H5BlockPrivate.h \
			H5MultiBlockPrivate.h \
			H5PartAttrib.h

# Listing of sources
libH5Part_a_SOURCES =	H5Part.c \
			H5PartAttrib.c \
			H5Block.c \
			H5BlockReadWrite.c \
			H5MultiBlock.c \
			H5MultiBlockReadWrite.c

libH5PartF_a_SOURCES =	H5PartF.c \
			H5PartAttribF.c \
			H5BlockF.c \
			H5BlockReadWriteF.c

H5PartF.h: H5Part.f90 H5PartAttrib.f90 H5Block.f90 H5BlockReadWrite.f90
	awk '/INTEGER\*8 FUNCTION/{print "\t" $$1 " " $$3}' $^ >$@

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

libpH5Part.a: libH5Part.a
	$(RM) $@
	ln -s $^ $@

libpH5PartF.a: libH5PartF.a
	$(RM) $@
	ln -s $^ $@

