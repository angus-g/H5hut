# tools level Makefile.am

# PATH SETTING
HDF5ROOT = @HDF5ROOT@

# COMPILER SETTING
CXX = @CXX@
TOOLS_CXX = @TOOLS_CXX@

# COMPILER FLAG SETTING
CFLAGS = @CFLAGS@

# # -L$(HDF5ROOT)/lib -lhdf5

# LIBRARIES
SZLIB = @SZLIB@
HDFLIB = @TOOLS_HDFLIB@ -lz $(SZLIB)
LIBS = $(HDFLIB) $(MPILIB) -lm @STDCXX@ @LDFLAGS@

# H5Part compiled library location
# H5PLIB = -L@prefix@/lib
H5PLIB = -L@H5P_LIB_LOC@ @TOOLS_H5PART_LIB@

# INCLUDES
HDFINC = -I$(HDF5ROOT)/include
INC = $(HDFINC) $(H5PINC) $(MPIINC)

# H5Part header file location
# H5PINC = -I@prefix@/include
H5PINC = -I@H5P_LIB_LOC@

# What to build... make install will place these files in the $(prefix)/bin directory.
bin_PROGRAMS = @BUILD_TOOLS@

# Listing of all programs that maybe built. (Has to know statically...)
EXTRA_PROGRAMS = h5pAttrib h5pToGNUplot homdynToH5p

# Extra files that I wish to include in the dist tar ball.
EXTRA_DIST = h5pAttrib.cc h5pToGNUplot.cc## TO BE TAILORED LATER...

# Listing of sources
h5pAttrib_SOURCES = h5pAttrib.cc

h5pToGNUplot_SOURCES = h5pToGNUplot.cc

homdynToH5p_SOURCES = homdynToH5p.cc

# Specific building instruction (What compilers to use...)
# ------------ Build Tools ------------

h5pAttrib: h5pAttrib.o
	$(TOOLS_CXX) -o h5pAttrib h5pAttrib.o $(H5PLIB) $(LIBS)

h5pAttrib.o: h5pAttrib.cc
	$(TOOLS_CXX) $(CFLAGS) $(INC) -g -c h5pAttrib.cc


h5pToGNUplot: h5pToGNUplot.o
	$(TOOLS_CXX) -o h5pToGNUplot h5pToGNUplot.o $(H5PLIB) $(LIBS)

h5pToGNUplot.o: h5pToGNUplot.cc
	$(TOOLS_CXX) $(CFLAGS) $(INC) -g -c $<

homdynToH5p : homdynToH5p.o
	$(TOOLS_CXX) -o $@ $< $(H5PLIB) $(LIBS)

homdynToH5p.o: homdynToH5p.cc
	$(TOOLS_CXX) $(CFLAGS) $(INC) -g -c $<


clean:
	rm -f *~ *.o h5pAttrib h5pToGNUplot

distclean: clean
	rm -rf .deps
	rm -rf Makefile


# # bash-3.00$ /usr/bin/mpcc_r -g -O2 -I/scratch/scratchdirs/cristina/hdf5/hdf5_par/include -I/u2/antino/trunk/src -c h5pAttrib.cc
# # bash-3.00$ /usr/bin/mpcc_r -o h5pAttrib h5pAttrib.o -L/u2/antino/trunk/src -lpH5Part -L/scratch/scratchdirs/cristina/hdf5/hdf5_par/lib -lhdf5 -lz  -lm


#####################################################################################################################
