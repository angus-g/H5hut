# tools level Makefile.am

OBJEXT=o

AM_LDFLAGS = -L../src/lib @LDFLAGS@
LIBS = -lH5hutC -lH5hut @LIBS@

INCLUDES = -I../src/include @INCLUDES@

# What to build... make install will place these files in the $(prefix)/bin directory.
bin_PROGRAMS = @BUILD_TOOLS@
EXTRA_PROGRAMS = h5pAttrib h5PartDcToVtk

# Listing of all programs that maybe built. (Has to know statically...)
EXTRA_SCRIPTS = h5hutcc

# Extra files that I wish to include in the dist tar ball.
#EXTRA_DIST = h5pAttrib.cc h5pToGNUplot.cc## TO BE TAILORED LATER...

# Listing of sources
h5pAttrib_SOURCES = h5pAttrib.cc

#h5pToGNUplot_SOURCES = h5pToGNUplot.cc

#homdynToH5p_SOURCES = homdynToH5p.cc

h5PartDcToVtk_SOURCES = h5PartDcToVtk.cc

clean: clean-am

