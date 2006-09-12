# test level Makefile.am

# PATH SETTING
HDF5ROOT = @HDF5ROOT@
PHDF5ROOT = @PHDF5ROOT@

# COMPILER SETTING
CXX = @CXX@
FC = @FC@
MPIFC = @MPIFC@
MPICXX = @MPICXX@
MPICC = @MPICC@

# COMPILER FLAG SETTING
CFLAGS = @CFLAGS@
PCFLAGS = $(CFLAGS) -DPARALLEL_IO -DH5_HAVE_PARALLEL -DMPICH_IGNORE_CXX_SEEK
ADDFLAGS = @ADDFLAGS@
FFLAGS = @FFLAGS@ $(ADDFLAGS) 
PFFLAGS = @FFLAGS@ $(CFLAGS)

# LIBRARIES
SZLIB = @SZLIB@
HDFLIB = -L$(HDF5ROOT)/lib -lhdf5 -lz $(SZLIB) @LDFLAGS@
PHDFLIB = -L$(PHDF5ROOT)/lib -lhdf5 -lz $(SZLIB) @LDFLAGS@
MPILIB = @MPILIB@
LIBS = $(HDFLIB) -lm @STDCXX@
PLIBS = $(PHDFLIB) $(MPILIB) -lm @STDCXX@

# H5Part compiled library location
# H5PLIB = -L@prefix@/lib
H5PLIB = -L@H5P_LIB_LOC@

# INCLUDES
HDFINC = -I$(HDF5ROOT)/include
PHDFINC = -I$(PHDF5ROOT)/include
MPIINC = @MPIINC@
PINC = $(PHDFINC) $(MPIINC) $(H5PINC)
INC = $(HDFINC) $(H5PINC)

# H5Part header file location
# H5PINC = -I@prefix@/include
H5PINC = -I@H5P_LIB_LOC@

# What to build... make install will place these files in the $(prefix)/bin directory.
bin_PROGRAMS = @TTARGET@

# Some useful scripts that I wish to place in the $(prefix)/bin directory.
bin_SCRIPTS =
#bin_SCRIPTS = JAC_H5PartTestP_script.scr JAC_H5testFpar_script.scr JAC_RUN_ALL_script.scr

# Listing of all programs that maybe built. (Has to know statically...)
EXTRA_PROGRAMS = Bench H5PartTest H5testF H5PartTestP H5PartAndreasTest H5testFpar H5PartTestParallel H5test H5BlockTest1

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST = Bench.c H5ecloudExample.cc H5ParallelTest.cc H5test.cc $(bin_SCRIPTS)## TO BE TAILORED LATER...

# Listing of sources
Bench_SOURCES = Bench.c

H5PartAndreasTest_SOURCES = H5PartAndreasTest.cc

H5PartTest_SOURCES = H5PartTest.cc

H5PartTestP_SOURCES = H5PartTest.cc

H5testF_SOURCES = H5testF.f

H5testFpar_SOURCES = H5testFpar.f

H5PartTestParallel_SOURCES = H5PartTestParallel.cc

H5test_SOURCES = H5test.cc

H5BlockTest1_SOURCES = H5BlockTest1.c

# Specific building instruction (What compilers to use...)
# ------------ Build Tests ------------

Bench: Bench.c
	$(MPICC) $(PCFLAGS) $(PINC) -o Bench Bench.c $(H5PLIB) -lpH5Part $(PLIBS)

#####################################################################################################################

H5PartTest: H5PartTest.o
	$(CXX) -o H5PartTest H5PartTest.o $(H5PLIB) -lH5Part $(LIBS)

H5PartTest.o: H5PartTest.cc
	$(CXX) $(CFLAGS) $(INC) -DREGRESSIONTEST -g -c H5PartTest.cc

#####################################################################################################################

H5PartTestP: H5PartTestP.o
	$(MPICXX) $(PINC) -o H5PartTestP H5PartTestP.o $(H5PLIB) -lpH5Part $(PLIBS)

H5PartTestP.o: H5PartTest.cc
	$(MPICXX) $(PCFLAGS) $(PINC) -DREGRESSIONTEST  -c H5PartTest.cc -o H5PartTestP.o

#####################################################################################################################

H5PartTestParallel: H5PartTestParallel.o
	$(MPICXX) -o H5PartTestParallel H5PartTestParallel.o $(H5PLIB) -lpH5Part $(PLIBS)

H5PartTestParallel.o: H5PartTestParallel.cc
	$(MPICXX) $(PCFLAGS) $(PINC) -c H5PartTestParallel.cc

#####################################################################################################################

H5PartAndreasTest: H5PartAndreasTest.o
	$(MPICXX)  -o H5PartAndreasTest H5PartAndreasTest.o $(H5PLIB) -lpH5Part $(PLIBS)

H5PartAndreasTest.o: H5PartAndreasTest.cc
	$(MPICXX) $(PCFLAGS) $(PINC) -c H5PartAndreasTest.cc

#####################################################################################################################

H5testF: H5testF.o
	$(FC)  -o H5testF H5testF.o $(H5PLIB) -lH5PartF -lH5Part $(LIBS)

H5testF.o: H5testF.f
	$(FC) $(FFLAGS) -c $(H5PINC) H5testF.f

#####################################################################################################################

H5testFpar: H5testFpar.o
	$(MPIFC)  -o $@ $< $(H5PLIB) -lpH5PartF -lpH5Part $(PLIBS)

H5testFpar.o: H5testFpar.f90
	$(MPIFC) $(PFFLAGS) -c $(H5PINC) $<

#####################################################################################################################

H5test: H5test.o
	$(CXX) -o H5test H5test.o $(H5PLIB) -lH5Part $(LIBS)

H5test.o: H5test.cc
	$(CXX) $(CFLAGS) $(INC) -DREGRESSIONTEST -g -c H5test.cc

#####################################################################################################################

H5BlockTest1:	H5BlockTest1.o
	$(CC) -o $@ $< $(H5PLIB) -lH5Part $(LIBS)

H5BlockTest1.o: H5BlockTest1.c
	$(CC) $(CFLAGS) $(INC) -g -c $<

H5BlockWrite1:	H5BlockWrite1.o
	$(CC) -o $@ $< $(H5PLIB) -lH5Part $(LIBS)

H5BlockWrite1.o: H5BlockWrite1.c
	$(CC) $(CFLAGS) $(INC) -g -c $<

H5BlockReadParallel1:	H5BlockReadParallel1.o
	$(CC) -o $@ $< $(H5PLIB) -lpH5Part $(PLIBS)

H5BlockReadParallel1.o: H5BlockReadParallel1.c
	$(CC) $(CFLAGS) -DPARALLEL_IO $(INC) -g -c $<

H5BlockWriteScalarField1Proc:	H5BlockWriteScalarField1Proc.o
	$(CC) -o $@ $< $(H5PLIB) -lpH5Part $(PLIBS)

H5BlockWriteScalarField1Proc.o: H5BlockWriteScalarField1Proc.c
	$(CC) $(CFLAGS) -DPARALLEL_IO $(INC) -g -c $<


#####################################################################################################################

H5BlockParallelTest1: H5BlockParallelTest1.o
	$(MPICC)  -o $@ $< $(H5PLIB) -lpH5Part $(PLIBS)

H5BlockParallelTest1.o: H5BlockParallelTest1.c
	$(MPICC) $(PCFLAGS) $(PINC) -c $<

#####################################################################################################################
clean:
	rm -f *~ *.o *.o H5testF H5PartTest H5PartTestP H5PartAndreasTest H5PartTestParallel H5testFpar Bench

distclean: clean
	rm -fr H5testF H5PartTest H5PartTestP H5PartAndreasTest H5PartTestParallel H5test
	rm -rf .deps
	rm -rf .libs
	rm -f parttest.h5
	rm -rf config.status config.log config.h Makefile


#####################################################################################################################
