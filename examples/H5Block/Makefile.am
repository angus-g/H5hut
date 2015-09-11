#
# Copyright (c) 2006-2015, The Regents of the University of California,
# through Lawrence Berkeley National Laboratory (subject to receipt of any
# required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
# Institut (Switzerland).  All rights reserved.!
#
# License: see file COPYING in top level of source distribution.
#

AM_CPPFLAGS += -I${abs_top_srcdir}/src/include
FFLAGS += -cpp $(AM_CPPFLAGS)
AM_LDFLAGS += -L${abs_top_builddir}/src/h5core/.libs

LDADD = 

if ENABLE_EXAMPLES
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

endif

%.o : %.f90
	$(FC)  $(FFLAGS) -c $<

clean-local:
	$(RM) *~
