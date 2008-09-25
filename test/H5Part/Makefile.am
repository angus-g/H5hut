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
H5LIB = -L@H5P_LIB_LOC@ -L@H5P_LIB_LOC@/h5_core -lH5_core -lH5

LIBS = ${H5LIB} $(HDFLIB) $(MPILIB) -lm @STDCXX@

# H5Part compiled library location
# H5PLIB = -L@prefix@/lib

# INCLUDES
HDFINC = -I$(HDF5ROOT)/include
MPIINC = @MPIINC@
H5INC = -I@H5P_LIB_LOC@ -I@H5P_LIB_LOC@/h5

INC = $(HDFINC) $(MPIINC) $(H5INC)


# What to build... make install will place these files in the $(prefix)/bin directory.
bin_PROGRAMS = @TUTARGET@

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
		H5testFpar.f90 \
		$(bin_SCRIPTS)

# Specific building instruction (What compilers to use...)
# ------------ Build Tests ------------

Bench: Bench.c
	$(CC) $(CFLAGS) $(INC) -o $@ $< $(H5PLIB) $(LIBS)

###############################################################################

H5PartTest: H5PartTest.o
	$(CXX) -o $@ $< $(H5PLIB) $(LIBS)

H5PartTest.o: H5PartTest.cc
	$(CXX) $(CFLAGS) $(INC) -DREGRESSIONTEST -g -c $<

###############################################################################

H5PartTestP: H5PartTestP.o
	$(CXX) $(INC) -o $@ $< $(H5LIB)  $(LIBS)

H5PartTestP.o: H5PartTest.cc
	$(CXX) $(CFLAGS) $(INC) -DREGRESSIONTEST  -c $< -o $@

###############################################################################

H5PartTestParallel: H5PartTestParallel.o
	$(CXX) -o $@ $< $(H5LIB) $(LIBS)

H5PartTestParallel.o: H5PartTestParallel.cc
	$(CXX) $(CFLAGS) $(INC) -c $<

###############################################################################

H5PartAndreasTest: H5PartAndreasTest.o
	$(CXX)  -o $@ $< $(H5LIB)  $(LIBS)

H5PartAndreasTest.o: H5PartAndreasTest.cc
	$(CXX) $(CFLAGS) $(INC) -c $<

###############################################################################

H5testF: H5testF.o
	$(FC)  -o $@ $< $(H5LIB) -lH5PartF  $(LIBS)

H5testF.o: H5testF.f
	$(FC) $(FFLAGS) -c $(H5INC) $<

###############################################################################

H5testFpar: H5testFpar.o
	$(MPIFC)  -o $@ $< $(H5LIB) -lpH5PartF $(LIBS)

H5testFpar.o: H5testFpar.f90
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

%.o : %.f90
	${FC} $(FFLAGS) -c $(H5INC) $<

###############################################################################
clean:
	${RM} -f *~ *.o  ${bin_PROGRAMS}

distclean: clean
	${RM} -rf .deps
	${RM} -rf .libs
	${RM} -f parttest.h5
	${RM} -rf config.status config.log config.h Makefile



