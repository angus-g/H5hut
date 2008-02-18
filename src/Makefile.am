# src level Makefile.am

# PATH SETTING (IMPORTED FROM CONFIGURE)
HDF5ROOT = @HDF5ROOT@

# COMPILERS
CC = @CC@

#INCLUDES
HDFINC = -I$(HDF5ROOT)/include
MPIINC = @MPIINC@

INC = $(HDFINC) ${MPIINC} -I./h5

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
EXTRA_DIST = TestUnderscoreC.c TestUnderscore.f $(EXTRA_HEADERS)

# Files that I don't want to include in the dist tar ball
nodist_include_HEADERS = @UNDERSCORE_H@


# What to build... Will be determined by configure script.
OBJEXT = o
lib_LIBRARIES = @MTARGET@

# Listing of all possible targets that I may build.
EXTRA_LIBRARIES = libH5Part.a

# Header files that I wish to install in $(prefix)/include
include_HEADERS = \
	H5Part.h \
	H5Block.h \
	H5Fed.h

# Listing of all possible headers that I may include
EXTRA_HEADERS =

# Listing of sources
libH5Part_a_SOURCES = \
	H5Part.c \
	H5Block.c \
	H5Fed.c \
	H5Fed_adjacency.c \
	H5Fed_dof.c \
	H5Fed_statistics.c \
	H5Fed_storeretrieve.c

SUBDIRS = h5 bindings

# Specific building instruction (What compilers to use...)
# ------------ Serial Lib build commands ------------
libH5Part.a: $(libH5Part_a_OBJECTS)
	${AR} rucs $@ $^
 
%.o : %.c
	$(CC) $(CFLAGS) $(INC) -c $<

H5Part.o: H5Part.c H5Part.h
H5Block.o: H5Block.c H5Part.h H5Block.h

clean: clean-recursive
	$(RM) -f *~ *.o *.a *.so

distclean:
	$(RM) -f *.a
	$(RM) -f Underscore.h

#	$(RM) -rf .deps
#	$(RM) -rf .libs
#	$(RM) -f Makefile
