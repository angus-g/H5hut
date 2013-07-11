AM_CPPFLAGS += -I${abs_top_srcdir}/src/include
FFLAGS += -cpp $(AM_CPPFLAGS)
AM_LDFLAGS += -L${abs_top_builddir}/src/h5core/.libs

LDADD = 

if ENABLE_FORTRAN
LDADD += -lH5hutF
endif

LDADD += -lH5hut

noinst_PROGRAMS = 

if ENABLE_C
noinst_PROGRAMS += 	\
	openclose	\
	file_attribs	\
	attach_file
endif

if ENABLE_FORTRAN
noinst_PROGRAMS += 
endif

#EXTRA_PROGRAMS = openclose

#openclose_SOURCES = openclose.c

#%.o : %.f90
#	$(FC)  $(FFLAGS) -c $<
