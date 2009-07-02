# test level Makefile.am

# PATH SETTING
HDF5ROOT = @HDF5ROOT@

# COMPILER SETTING
CXX = @CXX@
FC = @FC@
MPIFC = @MPIFC@
MPICXX = @MPICXX@
MPICC = @MPICC@

# COMPILER FLAG SETTING
CFLAGS = @CFLAGS@
FFLAGS = @FFLAGS@ @MPIINC@

# LIBRARIES
SZLIB = @SZLIB@
HDFLIB = -L$(HDF5ROOT)/lib -lhdf5 -lz $(SZLIB) @LDFLAGS@
MPILIB = @MPILIB@
H5LIB = -L@H5P_LIB_LOC@ -lH5Part

LIBS = ${H5LIB} $(HDFLIB) $(MPILIB) -lm @STDCXX@

# H5Part compiled library location
# H5PLIB = -L@prefix@/lib

# INCLUDES
HDFINC = -I$(HDF5ROOT)/include
MPIINC = @MPIINC@
H5INC = -I@H5P_LIB_LOC@

INC = $(HDFINC) $(MPIINC) $(H5INC)


# What to build... make install will place these files in the $(prefix)/bin directory.
bin_PROGRAMS = @TTARGET@

# Some useful scripts that I wish to place in the $(prefix)/bin directory.
bin_SCRIPTS =
#bin_SCRIPTS = JAC_H5PartTestP_script.scr JAC_H5testFpar_script.scr JAC_RUN_ALL_script.scr


# Listing of all programs that maybe built. (Has to know statically...)
EXTRA_PROGRAMS =

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST = 	Bench.c \
		H5ParallelTest.cc \
		H5PartTest.cc H5PartTestParallel.cc \
		H5PartAndreasTest.cc \
		H5test.cc H5testF.f \
		H5testFpar.F90 \
		H5BlockParTestScalarField.c H5BlockParTestScalarFieldF.F90 \
		H5BlockTestAttributes.c H5BlockTestAttributesF.F90 \
		$(bin_SCRIPTS)

# Specific building instruction (What compilers to use...)
# ------------ Build Tests ------------

Bench: Bench.c
	$(CC) $(CFLAGS) $(INC) -o $@ $< $(H5PLIB) -lH5Part $(LIBS)

###############################################################################

H5PartTest: H5PartTest.o
	$(CXX) -o $@ $< $(H5PLIB) -lH5Part $(LIBS)

H5PartTest.o: H5PartTest.cc
	$(CXX) $(CFLAGS) $(INC) -DREGRESSIONTEST -g -c $<

###############################################################################

H5PartTestP: H5PartTestP.o
	$(CXX) $(INC) -o $@ $< $(H5LIB) -lH5Part $(LIBS)

H5PartTestP.o: H5PartTest.cc
	$(CXX) $(CFLAGS) $(INC) -DREGRESSIONTEST  -c $< -o $@

###############################################################################

H5PartTestParallel: H5PartTestParallel.o
	$(CXX) -o $@ $< $(H5LIB) -lH5Part $(LIBS)

H5PartTestParallel.o: H5PartTestParallel.cc
	$(CXX) $(CFLAGS) $(INC) -c $<

###############################################################################

H5PartAndreasTest: H5PartAndreasTest.o
	$(CXX)  -o $@ $< $(H5LIB) -lpH5Part $(LIBS)

H5PartAndreasTest.o: H5PartAndreasTest.cc
	$(CXX) $(CFLAGS) $(INC) -c $<

###############################################################################

H5testF: H5testF.o
	$(FC)  -o $@ $< $(H5LIB) -lH5PartF -lH5Part $(LIBS)

H5testF.o: H5testF.f
	$(FC) $(FFLAGS) -c $(H5INC) $<

###############################################################################

H5testFpar: H5testFpar.o
	$(MPIFC)  -o $@ $< $(H5LIB) -lpH5PartF -lpH5Part $(LIBS)

H5testFpar.o: H5testFpar.F90
	$(MPIFC) $(FFLAGS) -c $(H5INC) $<

###############################################################################

H5test: H5test.o
	$(CXX) -o $@ $< $(H5LIB) $(LIBS)

H5test.o: H5test.cc
	$(CXX) $(CFLAGS) $(INC) -DREGRESSIONTEST -g -c $<

###############################################################################
% : %.o
	$(CC) -o $@ $< $(H5LIB) $(LIBS)

%.o : %.c
	$(CC) $(CFLAGS) $(INC) -g -c $<

%.o : %.F90
	${FC} $(FFLAGS) -c $(H5INC) $<

H5BlockTestAttributes.o: H5BlockTestAttributes.c

H5BlockTestAttributes:	H5BlockTestAttributes.o

H5BlockTestAttributesF.o: H5BlockTestAttributesF.F90

H5BlockTestAttributesF:	H5BlockTestAttributesF.o
	$(FC)  -o $@ $< $(H5LIB) -lH5PartF -lH5Part $(LIBS)

H5BlockParTestScalarField.o: H5BlockParTestScalarField.c

H5BlockParTestScalarField: H5BlockParTestScalarField.o

H5BlockParTestScalarFieldF.o: H5BlockParTestScalarFieldF.F90

H5BlockParTestScalarFieldF: H5BlockParTestScalarFieldF.o
	$(FC)  -o $@ $< $(H5LIB) -lH5PartF -lH5Part $(LIBS)

H5BlockDissolveGhosts.o: H5BlockDissolveGhosts.c

H5BlockDissolveGhosts: H5BlockDissolveGhosts.o


###############################################################################
clean:
	${RM} -f *~ *.o  ${bin_PROGRAMS}

distclean: clean
	${RM} -rf .deps
	${RM} -rf .libs
	${RM} -f parttest.h5
	${RM} -rf config.status config.log config.h Makefile



