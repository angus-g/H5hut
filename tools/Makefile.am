# tools level Makefile.am

OBJEXT=o

LDFLAGS = -L../src/lib @LFLAGS@
LIBS = -lH5hutC -lH5hut @LIBS@

INCLUDES = -I../src/include @INCLUDES@

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

install-exec-local:
	@$(INSTALL) h5hutcc $(DESTDIR)$(bindir)/h5hutcc

