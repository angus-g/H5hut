AM_CPPFLAGS += -I${abs_top_builddir}/src/include
FFLAGS += -cpp $(AM_CPPFLAGS)
LDFLAGS += -L${abs_top_builddir}/src/lib

LDADD = 

if ENABLE_C
LDADD +=
endif

if ENABLE_FORTRAN
LDADD += -lH5hutF
endif

LDADD += -lH5hut

noinst_PROGRAMS = 

if ENABLE_C
noinst_PROGRAMS += read_write
endif

if ENABLE_FORTRAN
noinst_PROGRAMS += H5BlockExampleF
endif

read_write_SOURCES = read_write.c
H5BlockExampleF_SOURCES = H5BlockExampleF.f90

%.o : %.f90
	$(FC)  $(FFLAGS) -c $<


distclean-local:
	$(RM) *~
