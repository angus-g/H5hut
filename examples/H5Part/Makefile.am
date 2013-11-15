AM_CPPFLAGS += -I${abs_top_srcdir}/src/include
FFLAGS += -cpp $(AM_CPPFLAGS)
AM_LDFLAGS += -L${abs_top_builddir}/src/h5core/.libs

LDADD = 

if ENABLE_C
LDADD +=
endif

if ENABLE_FORTRAN
LDADD += -lH5hutF
AM_LDFLAGS += -L${abs_top_builddir}/src/Fortran/.libs
endif

LDADD += -lH5hut

noinst_PROGRAMS = 

if ENABLE_C
noinst_PROGRAMS +=		\
	read_canonicalview	\
	read_setnparticles	\
	read_setview		\
	read_strided		\
	write_core_vfd		\
	write_setnparticles	\
	write_setview		\
	write_strided
endif

if ENABLE_FORTRAN
noinst_PROGRAMS +=		\
	read_canonicalviewf	\
	read_setnparticlesf	\
	read_setviewf		\
	read_stridedf		\
	write_core_vfdf		\
	write_setnparticlesf	\
	write_setviewf		\
	write_stridedf
endif

EXTRA_PROGRAMS = 

read_canonicalviewf_SOURCES = read_canonicalviewf.f90
read_setnparticlesf_SOURCES = read_setnparticlesf.f90
read_setviewf_SOURCES = read_setviewf.f90
read_stridedf_SOURCES = read_stridedf.f90
write_core_vfdf_SOURCES = write_core_vfdf.f90
write_setnparticles_SOURCES = write_setnparticles.f90
write_setviewf_SOURCES = write_setviewf.f90
write_stridedf_SOURCES = write_stridedf.f90

%.o : %.f90
	$(FC)  $(FFLAGS) -c $<

clean-local:
	$(RM) *~


