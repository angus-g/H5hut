AM_CPPFLAGS += -I${abs_top_builddir}/src/include -Wno-deprecated
FFLAGS += -cpp $(AM_CPPFLAGS)
LDFLAGS += -L${abs_top_builddir}/src/lib

LDADD = 

LDADD += -lH5hut -lvtkCommon -lvtkIO


bin_PROGRAMS = 

bin_PROGRAMS += vtk2h5grid
vtk2h5grid_SOURCES = vtk2h5grid.cc

distclean-local:
	$(RM) *~
