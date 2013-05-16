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
H5LIB = -L${abs_top_builddir}/src/lib -lH5Core -lH5

LIBS = ${H5LIB} $(HDFLIB) $(MPILIB) -lm @STDCXX@

# H5Part compiled library location
# H5PLIB = -L@prefix@/lib

# INCLUDES
HDFINC = -I$(HDF5ROOT)/include
MPIINC = @MPIINC@
H5INC = -I${abs_top_builddir}/src/include

INC = $(HDFINC) $(MPIINC) $(H5INC)

# What to build... make install will place these files in the $(prefix)/bin directory.
bin_PROGRAMS = @TBTARGET@

# Some useful scripts that I wish to place in the $(prefix)/bin directory.
bin_SCRIPTS =
#bin_SCRIPTS = JAC_H5PartTestP_script.scr JAC_H5testFpar_script.scr JAC_RUN_ALL_script.scr

# Listing of all programs that maybe built. (Has to know statically...)
EXTRA_PROGRAMS =

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST = 	\
		H5BlockParTestScalarField.c H5BlockParTestScalarFieldF.f90 \
		H5BlockTestAttributes.c H5BlockTestAttributesF.f90 \
		$(bin_SCRIPTS)

# Specific building instruction (What compilers to use...)
# ------------ Build Tests ------------

###############################################################################

% : %.o
	$(CC) -o $@ $< $(H5LIB) $(LIBS)

%.o : %.c
	$(CC) $(CFLAGS) $(INC) -g -c $<

%.o : %.f90
	${FC} $(FFLAGS) -c $(H5INC) $<

H5BlockTestAttributes.o: H5BlockTestAttributes.c

H5BlockTestAttributes:	H5BlockTestAttributes.o

H5BlockTestAttributesF.o: H5BlockTestAttributesF.f90

H5BlockTestAttributesF:	H5BlockTestAttributesF.o
	$(FC)  -o $@ $< $(H5LIB) -lH5PartF -lH5Part $(LIBS)

H5BlockParTestScalarField.o: H5BlockParTestScalarField.c

H5BlockParTestScalarField: H5BlockParTestScalarField.o

H5BlockParTestScalarFieldF.o: H5BlockParTestScalarFieldF.f90

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

