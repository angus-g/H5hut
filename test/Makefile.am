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
PLIBS = -lpH5Part $(PHDFLIB) $(MPILIB) -lm @STDCXX@

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
EXTRA_PROGRAMS =

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST = Bench.c H5ecloudExample.cc H5ParallelTest.cc H5test.cc $(bin_SCRIPTS)

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
	$(MPICC) $(PCFLAGS) $(PINC) -o $@ $< $(H5PLIB) -lpH5Part $(PLIBS)

###############################################################################

H5PartTest: H5PartTest.o
	$(CXX) -o $@ $< $(H5PLIB) -lH5Part $(LIBS)

H5PartTest.o: H5PartTest.cc
	$(CXX) $(CFLAGS) $(INC) -DREGRESSIONTEST -g -c $<

###############################################################################

H5PartTestP: H5PartTestP.o
	$(MPICXX) $(PINC) -o $@ $< $(H5PLIB) -lpH5Part $(PLIBS)

H5PartTestP.o: H5PartTest.cc
	$(MPICXX) $(PCFLAGS) $(PINC) -DREGRESSIONTEST  -c $< -o $@

###############################################################################

H5PartTestParallel: H5PartTestParallel.o
	$(MPICXX) -o $@ $< $(H5PLIB) -lpH5Part $(PLIBS)

H5PartTestParallel.o: H5PartTestParallel.cc
	$(MPICXX) $(PCFLAGS) $(PINC) -c $<

###############################################################################

H5PartAndreasTest: H5PartAndreasTest.o
	$(MPICXX)  -o $@ $< $(H5PLIB) -lpH5Part $(PLIBS)

H5PartAndreasTest.o: H5PartAndreasTest.cc
	$(MPICXX) $(PCFLAGS) $(PINC) -c $<

###############################################################################

H5testF: H5testF.o
	$(FC)  -o $@ $< $(H5PLIB) -lH5PartF -lH5Part $(LIBS)

H5testF.o: H5testF.f
	$(FC) $(FFLAGS) -c $(H5PINC) $<

###############################################################################

H5testFpar: H5testFpar.o
	$(MPIFC)  -o $@ $< $(H5PLIB) -lpH5PartF -lpH5Part $(PLIBS)

H5testFpar.o: H5testFpar.f90
	$(MPIFC) $(PFFLAGS) -c $(H5PINC) $<

###############################################################################

H5test: H5test.o
	$(CXX) -o $@ -c $< $(H5PLIB) -lH5Part $(LIBS)

H5test.o: H5test.cc
	$(CXX) $(CFLAGS) $(INC) -DREGRESSIONTEST -g -c $<

###############################################################################
% : %.o
	$(MPICC) -o $@ $< $(H5PLIB) $(PLIBS)

H5BlockTestScalarField.o: H5BlockTestScalarField.c
	$(CC) $(CFLAGS) $(INC) -g -c $<

H5BlockTestScalarField:	H5BlockTestScalarField.o
	$(CC) -o $@ $< $(H5PLIB) $(LIBS)

H5BlockParTestScalarField.o: H5BlockParTestScalarField.c
	$(MPICC) $(PCFLAGS) $(INC) -g -c $<

H5BlockParTestScalarField: H5BlockParTestScalarField.o

H5BlockDissolveGhosts.o: H5BlockDissolveGhosts.c
	$(CC) $(CFLAGS) $(INC) -g -c $<

H5BlockDissolveGhosts: H5BlockDissolveGhosts.o
	$(CC) -o $@ $< $(H5PLIB) $(PLIBS)

###############################################################################
clean:
	${RM} -f *~ *.o  ${bin_PROGRAMS}

distclean: clean
	${RM} -rf .deps
	${RM} -rf .libs
	${RM} -f parttest.h5
	${RM} -rf config.status config.log config.h Makefile



