AM_CPPFLAGS = -I${abs_top_srcdir}/src/include -Wno-deprecated
FFLAGS += -cpp $(AM_CPPFLAGS)
AM_LDFLAGS += -L${abs_top_builddir}/src/h5core

LDADD = 

LDADD += -lH5hut -lvtkIO -lvtkFiltering -lvtkCommon -lvtksys -lpthread -ldl


bin_PROGRAMS = 

bin_PROGRAMS += vtk2h5grid
vtk2h5grid_SOURCES = vtk2h5grid.cc

distclean-local:
	$(RM) *~
