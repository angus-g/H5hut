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
lib_LIBRARIES = libH5.a

# Listing of all possible targets that I may build.
EXTRA_LIBRARIES = libH5.a

# Header files that I wish to install in $(prefix)/include
include_HEADERS = \
	h5_core.h \
	h5_types.h \
	attribs.h \
	errorhandling.h \
	readwrite.h \
	t_openclose.h \
	t_readwrite.h \
	u_readwrite.h 

# Listing of all possible headers that I may include
EXTRA_HEADERS =

# Listing of sources
libH5_a_SOURCES = \
	general.c \
	attribs.c \
	errorhandling.c \
	t_openclose.c \
	readwrite.c \
	u_readwrite.c \
	t_readwrite.c

all: libH5.a

libH5.a: $(libH5_a_OBJECTS)
	echo $(libH5_a_OBJECTS)
	${AR} rucs $@ $^
 
%.o : %.c
	$(CC) $(CFLAGS) $(INC) -c $<

$(libH5_a_OBJECTS): h5_core.h

errorhandling.o: errorhandling.c

clean:
	$(RM) -f *~ *.o *.a *.so

distclean: clean
	$(RM) -f *.a
	$(RM) -rf .deps
	$(RM) -rf .libs
	$(RM) -f Makefile
