# test level Makefile.am

OBJEXT = o

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
H5LIB = -L@H5P_LIB_LOC@ -L@H5P_LIB_LOC@/h5 -lH5Part -lH5

LIBS = ${H5LIB} $(HDFLIB) $(MPILIB) -lm @STDCXX@

# H5Part compiled library location
# H5PLIB = -L@prefix@/lib

# INCLUDES
HDFINC = -I$(HDF5ROOT)/include
MPIINC = @MPIINC@
H5INC = -I@H5P_LIB_LOC@ -I@H5P_LIB_LOC@/h5

INC = $(HDFINC) $(MPIINC) $(H5INC)


# What to build... make install will place these files in the $(prefix)/bin directory.
bin_PROGRAMS = simple_tet read_tet simple_triangle read_triangle

# Some useful scripts that I wish to place in the $(prefix)/bin directory.
bin_SCRIPTS =



# Listing of all programs that maybe built. (Has to know statically...)
EXTRA_PROGRAMS =

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST = 	\
		simple_tet.c \
		read_tet.c \
		simple_triangle.c \
		read_triangle.c \
		$(bin_SCRIPTS)

all:	$(bin_PROGRAMS)

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



