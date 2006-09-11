# src level Makefile.am

# PATH SETTING (IMPORTED FROM CONFIGURE)
HDF5ROOT = @HDF5ROOT@
PHDF5ROOT = @PHDF5ROOT@

# COMPILERS
CC = @CC@
MPICC = @MPICC@

# COMPILER FLAGS
CFLAGS = @CFLAGS@
PCFLAGS = $(CFLAGS) -DPARALLEL_IO -DH5_HAVE_PARALLEL -DMPICH_IGNORE_CXX_SEEK

#INCLUDES
HDFINC = -I$(HDF5ROOT)/include
PHDFINC = -I$(PHDF5ROOT)/include
PINC = $(PHDFINC) $(MPIINC)
INC = $(HDFINC)
MPIINC = @MPIINC@

# H5Part header file location
H5PINC = -I@prefix@/include

# H5Part compiled library location
H5PLIB = -L@prefix@/lib

# HDF5 LIBRARY
HDFLIB = -L$(HDF5ROOT)/lib -lhdf5 -lz $(SZLIB) @LDFLAGS@

# SZ LIBRARY
SZLIB = @SZLIB@

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST = H5Part.cc H5Part++.cc H5Part++.hh TestUnderscoreC.c TestUnderscore.f COPYRIGHT

# Files that I don't want to include in the dist tar ball
nodist_include_HEADERS = @UNDERSCORE_H@

# What to build... Will be determined by configure script.
lib_LIBRARIES = @MTARGET@

# Listing of all possible targets that I may build.
EXTRA_LIBRARIES = libH5Part.a libH5PartF.a libpH5Part.a libpH5PartF.a

# Header files that I wish to install in $(prefix)/include
include_HEADERS = H5Part.inc H5Part.h H5Part.hh @UNDERSCORE_H@

# Listing of all possible headers that I may include
EXTRA_HEADERS = H5Part.inc H5Part.h H5Part.hh Underscore.h

# Listing of sources
libH5Part_a_SOURCES = H5Part.c H5Block.c

libH5PartF_a_SOURCES = H5Part.c H5PartF.c H5Block.c

libpH5Part_a_SOURCES = H5Part.c H5Block.c

libpH5PartF_a_SOURCES = H5Part.c H5PartF.c H5Block.c


H5Part.inc:	H5PartF90.inc
	grep "INTEGER.* FUNCTION" $< | while read type func name rest; do echo "      $$type $$name"; done > $@

# Specific building instruction (What compilers to use...)
# ------------ Serial Lib build commands ------------
libH5Part.a: H5Part.o H5Block.o
	${AR} rucs $@ $^
 
libH5PartF.a: H5Part.o H5PartF.o H5Block.o
	${AR} rucs $@ $^

H5Part.o: H5Part.c H5Part.h H5PartPrivate.h H5PartTypes.h
	$(CC) $(CFLAGS) $(INC) -c $<

H5Block.o: H5Block.c H5Part.h H5PartPrivate.h H5PartTypes.h H5Block.h H5BlockTypes.h
	$(CC) $(CFLAGS) $(INC) -c $<
 
H5PartF.o: H5PartF.c Underscore.h H5Part.h
	$(CC) $(CFLAGS) $(INC) -w -c $<


# ----------- Build Parallel H5Part Stuff ------------

libpH5Part.a: pH5Part.o pH5Block.o
	${AR} rucs $@ $^

libpH5PartF.a: pH5Part.o pH5PartF.o pH5Block.o
	${AR} rucs $@ $^

pH5Part.o: H5Part.c H5Part.h H5PartPrivate.h H5PartTypes.h
	$(MPICC) $(CFLAGS) $(INC) -c $< -o $@

pH5Block.o: H5Block.c H5Part.h H5PartPrivate.h H5PartTypes.h H5Block.h H5BlockTypes.h
	$(MPICC) $(CFLAGS) $(INC) -c $< -o $@


pH5PartF.o: H5PartF.c Underscore.h H5Part.h
	$(MPICC) $(PCFLAGS) $(PINC) -w -c $< -o $@


clean:
	rm -f *~ *.o *.a *.so

distclean: clean
	rm -f *.a
	rm -rf .deps
	rm -rf .libs
	rm -f Underscore.h
	rm -f H5Part_py_wrap.c
	rm -f H5Part.py
	rm -f Makefile
