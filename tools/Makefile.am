# tools level Makefile.am

OBJEXT=o

HDFLIB = -L@HDF5ROOT@/lib -lhdf5 -lz @SZLIB@
LIBS = -L@H5P_LIB_LOC@ @H5P_LIB_NAME@ $(HDFLIB) -lm @STDCXX@

INCLUDES = -I@H5P_LIB_LOC@ -I@HDF5ROOT@/include @MPIINC@

# What to build... make install will place these files in the $(prefix)/bin directory.
bin_PROGRAMS = @BUILD_TOOLS@

# Listing of all programs that maybe built. (Has to know statically...)
EXTRA_PROGRAMS =	h5pAttrib \
			h5pToGNUplot \
			homdynToH5p \
			H5PartBench \
			H5BlockBench

# Listing of sources
h5pAttrib_SOURCES =	h5pAttrib.cc
h5pToGNUplot_SOURCES =	h5pToGNUplot.cc
homdynToH5p_SOURCES =	homdynToH5p.cc
H5PartBench_SOURCES =	H5PartBench.c
H5BlockBench_SOURCES =	H5BlockBench.c

