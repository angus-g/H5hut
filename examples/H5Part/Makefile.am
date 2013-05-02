AM_CPPFLAGS += -I${abs_top_builddir}/src/include
FFLAGS += -cpp $(AM_CPPFLAGS)
AM_LDFLAGS += -L${abs_top_builddir}/src/lib

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
noinst_PROGRAMS +=		\
	Bench			\
	H5test			\
	core_vfd		\
	particles		\
	read_canonicalview	\
	write_setnumparticles	\
	write_setview
endif

if ENABLE_FORTRAN
noinst_PROGRAMS += H5testF
endif

EXTRA_PROGRAMS = Bench H5test H5testF

Bench_SOURCES = Bench.c
H5test_SOURCES = H5test.c
core_vfd_SOURCES = core_vfd.c
particles_SOURCES = particles.c
read_canonicalview_SOURCES = read_canonicalview.c
write_setnumparticles_SOURCES = write_setnumparticles.c
write_setview_SOURCES = write_setview.c
H5testF_SOURCES = H5testF.f90

%.o : %.f90
	$(FC)  $(FFLAGS) -c $<

clean-local:
	$(RM) *~
