# tools level Makefile.am

SUBDIRS =

if ENABLE_VTKCONVERTER
SUBDIRS +=	vtk2h5grid
endif

AM_LDFLAGS = -L../src/lib @LDFLAGS@
LIBS = -lH5hut @LIBS@

AM_CPPFLAGS = -I../src/include @AM_CPPFLAGS@

LDADD = \
	../src/C/libH5hutC.la \
	../src/h5core/libH5hut.la

# What to build... make install will place these files in the $(prefix)/bin directory.
bin_PROGRAMS = @BUILD_TOOLS@
EXTRA_PROGRAMS = h5pAttrib

# Listing of all programs that maybe built. (Has to know statically...)
EXTRA_SCRIPTS = h5hutcc

# Extra files that I wish to include in the dist tar ball.
#EXTRA_DIST = h5pAttrib.cc h5pToGNUplot.cc## TO BE TAILORED LATER...

# Listing of sources
h5pAttrib_SOURCES = h5pAttrib.cc

#h5pToGNUplot_SOURCES = h5pToGNUplot.cc

clean: clean-am

