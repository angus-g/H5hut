AM_CPPFLAGS += -I${abs_top_srcdir}/src/include
FFLAGS += -cpp $(AM_CPPFLAGS)
AM_LDFLAGS += -L${abs_top_builddir}/src/h5core/.libs

LDADD = 

if ENABLE_FORTRAN
LDADD += -lH5hutF
AM_LDFLAGS += -L${abs_top_builddir}/src/Fortran/.libs
endif

LDADD += -lH5hut

noinst_PROGRAMS = 

if ENABLE_C
noinst_PROGRAMS += 	\
	fields		\
	read_write
endif

if ENABLE_FORTRAN
noinst_PROGRAMS += H5BlockExampleF
endif

fields_SOURCES = fields.c
read_write_SOURCES = read_write.c
H5BlockExampleF_SOURCES = H5BlockExampleF.f90

%.o : %.f90
	$(FC)  $(FFLAGS) -c $<

distclean-local:
	$(RM) *~
