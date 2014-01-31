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
	read_write_scalar_field
endif

if ENABLE_FORTRAN
noinst_PROGRAMS += read_write_scalar_fieldf
endif

fields_SOURCES = fields.c
read_write_scalar_field_SOURCES = read_write_scalar_field.c
read_write_scalar_fieldf_SOURCES = read_write_scalar_fieldf.f90

%.o : %.f90
	$(FC)  $(FFLAGS) -c $<

clean-local:
	$(RM) *~
