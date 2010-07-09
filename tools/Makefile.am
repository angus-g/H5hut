# tools level Makefile.am

OBJEXT=o

HDFLIB = -L@HDF5ROOT@/lib -lhdf5 -lz @SZLIB@
H5LIB = -L../src/lib -lH5hutC -lH5core
LIBS = $(H5LIB) $(HDFLIB) @MPILIB@

INCLUDES = -I../src/include -I@HDF5ROOT@/include @MPIINC@

# What to build... make install will place these files in the $(prefix)/bin directory.
bin_PROGRAMS = @BUILD_TOOLS@

# Listing of all programs that maybe built. (Has to know statically...)
EXTRA_PROGRAMS = h5pAttrib h5pToGNUplot # homdynToH5p

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST = h5pAttrib.cc h5pToGNUplot.cc## TO BE TAILORED LATER...

# Listing of sources
h5pAttrib_SOURCES = h5pAttrib.cc

h5pToGNUplot_SOURCES = h5pToGNUplot.cc

#homdynToH5p_SOURCES = homdynToH5p.cc

