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
noinst_PROGRAMS +=		\
	attach_file		\
	openclose		\
	query			\
	read_file_attribs	\
	read_step_attribs	\
	write_file_attribs	\
	write_step_attribs
endif

if ENABLE_FORTRAN
noinst_PROGRAMS +=		\
	openclosef		\
	queryf			\
	read_file_attribsf	\
	read_step_attribsf	\
	write_file_attribsf 	\
	write_step_attribsf

openclosef_SOURCES = openclosef.f90
queryf_SOURCES = queryf.f90
read_file_attribsf_SOURCES = read_file_attribsf.f90
read_step_attribsf_SOURCES = read_step_attribsf.f90
write_file_attribsf_SOURCES = write_file_attribsf.f90
write_step_attribsf_SOURCES = write_step_attribsf.f90
endif

%.o : %.f90
	$(FC)  $(FFLAGS) -c $<
