AM_CPPFLAGS += -I${abs_top_builddir}/src/include -Wno-deprecated
FFLAGS += -cpp $(AM_CPPFLAGS)
AM_LDFLAGS += -L${abs_top_builddir}/src/lib

LDADD = -lH5hut -lvtkIO -lvtkFiltering -lvtkCommon -lvtksys

bin_PROGRAMS = vtk2h5grid
vtk2h5grid_SOURCES = vtk2h5grid.cc

distclean-local:
	$(RM) *~
