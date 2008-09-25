# src level Makefile.am

# PATH SETTING (IMPORTED FROM CONFIGURE)
HDF5ROOT = @HDF5ROOT@

OBJEXT = o

# COMPILERS
CC = @CC@

#INCLUDES
HDFINC = -I$(HDF5ROOT)/include
MPIINC = @MPIINC@

INC = $(HDFINC) ${MPIINC} -I. -I../

# H5Part header file location
H5PINC = -I@prefix@/include

# COMPILER FLAGS
CFLAGS = @CFLAGS@ ${INC}

# H5Part compiled library location
H5PLIB = -L@prefix@/lib

# HDF5 LIBRARY
HDFLIB = -L$(HDF5ROOT)/lib -lhdf5 -lz $(SZLIB) @LDFLAGS@

# SZ LIBRARY
SZLIB = @SZLIB@

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST = $(EXTRA_HEADERS)

# Files that I don't want to include in the dist tar ball
nodist_include_HEADERS =

# What to build... Will be determined by configure script.
lib_LIBRARIES = libH5_core.a

# Listing of all possible targets that I may build.
EXTRA_LIBRARIES = libH5_core.a

# Header files that I wish to install in $(prefix)/include
include_HEADERS = \
	h5_attribs.h \
	h5_core.h \
	h5_errorhandling.h \
	h5_errorhandling_private.h \
	h5_hdf5.h \
	h5_maps.h \
	h5_openclose.h \
	h5_qsort_private.h \
	h5_readwrite.h \
	h5_readwrite_private.h \
	h5_types.h \
	h5b_errorhandling_private.h \
	h5t_boundaries.h \
	h5t_boundaries_private.h \
	h5t_errorhandling_private.h \
	h5t_map.h \
	h5t_openclose.h \
	h5t_readwrite.h \
	h5t_readwrite_private.h \
	h5t_storemesh.h
	h5t_storemesh_private.h
	h5u_errorhandling_private.h \
	h5u_readwrite.h

# Listing of all possible headers that I may include
EXTRA_HEADERS =

# Listing of sources
libH5_core_a_SOURCES = \
	h5_attribs.c \
	h5_errorhandling.c \
	h5_hdf5.c \
	h5_maps.c \
	h5_openclose.c \
	h5_qsort.c \
	h5_qsort_r.c \
	h5_readwrite.c \
	h5t_boundaries.c \
	h5t_errorhandling.c \
	h5t_map.c \
	h5t_openclose.c \
	h5t_readwrite.c \
	h5t_storemesh.c \
	h5u_readwrite.c

all: libH5_core.a

libH5_core.a: $(libH5_core_a_OBJECTS)
	echo $(libH5_a_OBJECTS)
	${AR} rucs $@ $^

%.o : %.c
	$(CC) $(CFLAGS) $(INC) -c $<

$(libH5_core_a_OBJECTS): h5_core.h

#errorhandling.o: errorhandling.c

clean:
	$(RM) -f *~ *.o *.a *.so

distclean: clean
	$(RM) -f *.a
	$(RM) -rf .deps
	$(RM) -rf .libs
	$(RM) -f Makefile
