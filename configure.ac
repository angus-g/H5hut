# Every configure script must call AC_INIT before doing anything else.
# AC_INIT (package, version, [bug-report], [tarname])
AC_INIT([H5hut], [1.99.0], [h5part@lists.psi.ch], H5Part)


# Ensure that a recent enough version of Autoconf is being used. 
# If the version of Autoconf being used to create configure is earlier than version, 
# print an error message to the standard error output and do not create configure.
AC_PREREQ(2.59)


# should be called right after AC_INIT.
# configure scripts can create a C header file containing `#define' directives. 
# The AC_CONFIG_HEADERS macro selects this kind of output.
AC_CONFIG_HEADER(config.h)

###############################################################################
################# --enable-xxx and --with-xxx Argument ########################
###############################################################################

AC_ARG_ENABLE(
	[64],
	[AC_HELP_STRING([--enable-64],
	[Compile using 64-bit flags [default=no]])],
	[USE_64=$enableval])

AC_ARG_ENABLE(
	[fortran],
	[AC_HELP_STRING([--enable-fortran],
	[Compile the Fortran interface [default=no]])],
	[USE_FORTRAN=$enableval])

AC_ARG_ENABLE(
	[parallel],
	[AC_HELP_STRING([--enable-parallel],
	[Compile the MPI/IO interface [default=no]])],
	[USE_PARALLEL=$enableval])

AC_ARG_ENABLE(
	[tools],
	[AC_HELP_STRING([--enable-tools],
	[Compile h5part tools [default=no]])],
	[USE_TOOLS=$enableval])

AC_ARG_WITH(
	[mpi],
	[AC_HELP_STRING([--with-mpi],
	[path to MPI installation [default=""]])],
	[MPIPATH=$withval], [MPIPATH=""])

AC_ARG_WITH(
	[hdf5],
	[AC_HELP_STRING([--with-hdf5],
	[path to HDF5 installation [default=""]])],
	[HDF5PATH=$withval], [HDF5PATH=""])

###############################################################################
################# A SIMPLE WORK AROUND TO USE ENV. VARS #######################
###############################################################################

SAVE_CC=$CC
SAVE_CXX=$CXX
SAVE_FC=$FC
SAVE_MPICC=$MPICC
SAVE_MPICXX=$MPICXX
SAVE_MPIFC=$MPIFC
SAVE_MPILIB=$MPILIB
SAVE_MPIINC=$MPIINC
SAVE_CFLAGS=$CFLAGS
SAVE_FFLAGS=$FFLAGS
SAVE_MPIROOT=$MPIROOT
SAVE_HDF5ROOT=$HDF5ROOT
SAVE_LDFLAGS=$LDFLAGS

###############################################################################
############### PATH SERACH FUNCTION - to be used later... ####################
###############################################################################
# /*@@
#   @routine    CCTK_Search
#   @date       Wed Jul 21 11:16:35 1999
#   @author     Tom Goodale
#   @desc
#   Used to search for something in various directories
#   @enddesc
#@@*/

PATH_Search() {
	eval  $1=""
	if test $# -lt 4 ; then
		h5part_basedir=""
	else
		h5part_basedir="$4/"
	fi
	for h5part_place in $2; do
		AC_MSG_CHECKING([looking in $h5part_place ... ])
		if test -r "$h5part_basedir$h5part_place/$3" ; then
			AC_MSG_RESULT([found])
			eval $1="$h5part_place"
			break
		fi
		AC_MSG_RESULT([no])
	done
	return
}


###############################################################################
############# MISC SETTINGS INCLUDING C & C++ COMPILER SETTING ################
###############################################################################
# Compute the canonical host-system type variable, host, and its three
# individual parts host_cpu, host_vendor, and host_os.
AC_CANONICAL_HOST


uname=`uname -s`
if test $uname = "AIX"; then
	AC_MSG_CHECKING([if system is AIX])
	AC_MSG_RESULT([OK])

	# If on AIX, define _ALL_SOURCE. Allows the use of some BSD functions. 
	# Should be called before any macros that run the C compiler.
	AC_AIX
fi


# Determine a C compiler to use. 
# If CC is not already set in the environment, check for gcc and cc, then
# for other C compilers. 
# Set output variable CC to the name of the compiler found.
AC_PROG_CC(pgcc pathcc icc cc_r gcc cc)


# Determine a C++ compiler to use. 
# Check if the environment variable CXX or CCC (in that order) is set; 
# if so, then set output variable CXX to its value.
# Otherwise, if the macro is invoked without an argument, 
# then search for a C++ compiler under the likely names (first g++ and c++
# then other names). 
# If none of those checks succeed, then as a last resort set CXX to g++.
AC_PROG_CXX(pgCC pathCC icc cc_r g++ gcc cc)


# Set output variable INSTALL to the path of a BSD-compatible install program,
# if one is found in the current PATH. 
# Otherwise, set INSTALL to `dir/install-sh -c`
AC_PROG_INSTALL


# AM_INIT_AUTOMAKE is required to use autoconf with automake
AM_INIT_AUTOMAKE()

AC_PROG_RANLIB

# Default prefix for bindir, etc... (eg >> ./build/bin)
AC_PREFIX_DEFAULT(`pwd`/build)


# AC_DEFINE_UNQUOTED (variable, value, [description])
# Define the C preprocessor variable variable to value
# Use this macro instead of AC_DEFINE when variable or value is a shell variable. 
AC_DEFINE_UNQUOTED(MY_BUILD_VENDOR, "$host_vendor", "")
AC_DEFINE_UNQUOTED(MY_BUILD_CPU, "$host_cpu", "")
AC_DEFINE_UNQUOTED(MY_BUILD_OS, "$host_os", "")
AC_DEFINE_UNQUOTED(MY_GNUNAME, "${host_cpu}-${host_vendor}-${host_os}", "")
AC_DEFINE_UNQUOTED(MY_UNAME, "$uname", "")


###############################################################################
######################## CONFIGURE LINE OPTIONS ###############################
###############################################################################

############################ fortran enabled ##################################
AC_MSG_CHECKING([if fortran interface enabled])
if test "X$USE_FORTRAN" = "Xyes"; then
	AC_MSG_RESULT([yes])

	AC_PROG_FC(pgf90 ifort xlf_r pathf90 g95 g90 ftn gfortran)
	if test -z "$FC" ; then
		AC_MSG_ERROR([Cannot find a fortran compiler!!!])
		exit 1
	fi

 	if test ! $uname = "AIX"; then
		FFLAGS="${FFLAGS} -fPIC"
	fi
 
	if test $FC = "g90"; then
		FFLAGS="${FFLAGS} -fno-second-underscore"
	fi

	if test $FC = "g95"; then
		FFLAGS="${FFLAGS} -fno-second-underscore"
	fi

	AC_MSG_CHECKING([symbol convention in object files])
	`cd src && rm -f TestUnderscore.o TestUnderscoreC.o TestUnderscore`
	`cd src && ${FC} ${FFLAGS} -c TestUnderscore.f`
	`cd src && ${CC} ${CFLAGS} -c TestUnderscoreC.c`
	`cd src && ${FC} ${FFLAGS} -o TestUnderscore TestUnderscore.o TestUnderscoreC.o`

	if test -f src/TestUnderscore ; then
		UNDERSCORE_H=Underscore.h
		`cd src && ./TestUnderscore > Underscore.h`
		AC_MSG_RESULT([ok])
	else
		AC_MSG_RESULT([nok])
		AC_MSG_ERROR([Cannot build fortran executables!!!])
		exit 1
	fi
else
	AC_MSG_RESULT([no])
fi


######################## parallel interface enabled ###########################
AC_MSG_CHECKING([if parallel interface enabled])
if test "X$USE_PARALLEL" = "Xyes"; then
	AC_MSG_RESULT([yes])

	CFLAGS="${CFLAGS} -DPARALLEL_IO -DH5_HAVE_PARALLEL -DMPICH_IGNORE_CXX_SEEK"

	AC_MSG_CHECKING([if we can compile MPI code without setting flags])
	AC_TRY_LINK([#include "mpi.h"], [
		MPI_Comm comm;
		int n;
		MPI_Comm_size( comm, &n ); ], 
		[AC_MSG_RESULT([yes]); r='yes'], [AC_MSG_RESULT([no]); r='no'] )

	if test "X$r" = "Xno"; then
		AC_PATH_PROGS([MPICC], [mpicc mpcc_r], [], [$PATH])
		AC_PATH_PROGS([MPICXX], [mpicxx mpcc_r], [], [$PATH])
		if test -z "$MPICC" -o -z "$MPICXX"; then 
			AC_MSG_CHECKING([for MPI root ])
			AC_MSG_RESULT([])
			if test -n "$MPIROOT"; then
				P=${MPIROOT}
			elif test -n "$MPIHOME"; then
				P=${MPIHOME}
			elif test -n "$MPIPATH"; then
				P=${MPIPATH}
			else
				P=''
				P="$P /usr"
				P="$P /usr/local"
				P="$P /usr/local/mpi"
				P="$P /usr/local/packages/mpi"
				P="$P /usr/local/mpich2"
				P="$P /usr/local/mpich"
				P="$P /opt/xt-mpt/default/mpich2-64/P2"
			fi
			PATH_Search MPIROOT "$P" include/mpi.h
			if test ! -n "$MPIROOT"; then
				AC_MSG_ERROR([Cannot determine MPI root!!!])
				exit 1
			fi

			if test -e "${MPIROOT}/bin/mpicc"; then
				MPICC=${MPIROOT}/bin/mpicc
			fi

			if test -e "${MPIROOT}/bin/mpicxx"; then
				MPICXX=${MPIROOT}/bin/mpicxx
			fi
		fi

		if test -z "$MPICC" -o -z "$MPICXX"; then 
			MPIINC="${MPIINC} -I${MPIROOT}/include"
			FFLAGS="${FFLAGS} -I${MPIROOT}/include"
			AC_MSG_CHECKING([for name of MPI lib ])
			if test -e ${MPIROOT}/lib/libmpi.a; then
				MPILIB="-L${MPIROOT}/lib -lmpi"
			elif test -e ${MPIROOT}/lib/libmpi.so; then
				MPILIB="-L${MPIROOT}/lib -lmpi"
			elif test -e ${MPIROOT}/lib/libmpich.a; then
				MPILIB="-L${MPIROOT}/lib -lmpich"
			elif test -e ${MPIROOT}/lib/libmpich.so; then
				MPILIB="-L${MPIROOT}/lib -lmpich"
			else
				AC_MSG_RESULT([not found])
				exit 1
			fi
			AC_MSG_RESULT([${MPILIB}])
		fi

		if test -n "$MPICC"; then
			CC=${MPICC}
		fi
  
		if test -n "$MPICXX"; then
			CXX=${MPICXX}
		fi
	fi

	MTARGET="libpH5Part.a"
	TPTARGET="${TPTARGET} H5PartTestP H5PartAndreasTest Bench"
	TBTARGET="${TBTARGET} H5BlockTestAttributes"
	TBTARGET="${TBTARGET} H5BlockParTestScalarField"

	# parallel + fortran
	if test "X$USE_FORTRAN" = "Xyes"; then
		AC_PATH_PROGS([MPIFC], [mpxlf_r mpif90], [], [$PATH])

		if test -z "${MPIFC}" ; then
			if test -e "${MPIROOT}/bin/mpif90"; then
				MPIFC=${MPIROOT}/bin/mpif90
			fi
		fi
 
		# if MPIFC empty
		if test ! -n "$MPIFC"; then
			AC_MSG_CHECKING([ No MPIFC detected. Setting MPIFC to FC.])
			MPIFC=$FC
			AC_MSG_RESULT([MPIFC =  $MPIFC])
		fi 

		if test -n "$MPIFC"; then
			FC=${MPIFC}
		fi

		MTARGET="${MTARGET} libpH5PartF.a"
		TPTARGET="${TPTARGET} H5testFpar"
		TBTARGET="${TBTARGET} H5BlockParTestScalarFieldF"
	fi

else  # --enable-parallel=no
	AC_MSG_RESULT([no])

	MTARGET="libH5Part.a"
	TPTARGET="${TPTARGET} H5test"
	TBTARGET="${TBTARGET} H5BlockTestAttributes"

	if test "X$USE_FORTRAN" = "Xyes"; then
		MTARGET="${MTARGET} libH5PartF.a"
		TPTARGET="${TPTARGET} H5testF"
		TBTARGET="${TBTARGET} H5BlockTestAttributesF"
	fi
fi

AC_MSG_CHECKING([wheter tools are enabled])
if test "X$USE_TOOLS" = "Xyes"; then
	AC_MSG_RESULT([yes])
	BUILD_TOOLS="h5pAttrib h5pToGNUplot # homdynToH5p"
else
	AC_MSG_RESULT([no])
fi

###############################################################################
######################### PATH CHECKING & SETTING #############################
###############################################################################

AC_MSG_CHECKING([for HDF5 root ])
AC_MSG_RESULT([])
if test -n "${HDF5ROOT}"; then
	P=${HDF5ROOT}
elif test -n "${HDF5HOME}" ; then
	P=${HDF5HOME}
elif test -n "${HDF5PATH}" ; then
	P=${HDF5PATH}
else
	P=''
	P="$P /usr"
	P="$P /usr/local"
	P="$P /usr/local/hdf5"
	P="$P /usr/local/packages/hdf5"
	P="$P /apps/hdf5"

	if test "X$USE_PARALLEL" = "Xyes"; then
		P="$P /usr/local/phdf5"
		P="$P /usr/local/hdf5/hdf5_par"
		if test "X$USE_64" = "Xyes"; then
			P="$P /usr/common/usg/hdf5/64/default/parallel"
		else
			P="$P /usr/common/usg/hdf5/32/default/parallel"
		fi
	else
		P="$P /usr/local/hdf5/hdf5_serial"
		if test "X$USE_64" = "Xyes"; then
			P="$P /usr/common/usg/hdf5/64/default/serial"
		else
			P="$P /usr/common/usg/hdf5/32/default/serial"
		fi
	fi
fi
PATH_Search HDF5ROOT "$P" include/hdf5.h
if test -z "$HDF5ROOT"; then
	AC_MSG_ERROR([Cannot determine HDF5 root!!!])
	exit 1
fi

AC_MSG_CHECKING([if we need to link to libsz ])
if test -n "$HDF5ROOT"; then
	if test -f $HDF5ROOT/lib/libsz.a; then
		AC_MSG_RESULT([yes])
		SZLIB="-L$HDF5ROOT/lib/ -lsz"
	else
		AC_MSG_RESULT([no])
		SZLIB=""
	fi
fi

AC_MSG_CHECKING([for static zlib root ])
AC_MSG_RESULT([ ])
PATH_Search ZLIBROOT '/apps/zlib' lib/libz.a
if test -n "$ZLIBROOT"; then
	LDFLAGS="$LDFLAGS -L$ZLIBROOT/lib"
fi

###############################################################################
################# A SIMPLE WORK AROUND TO USE ENV. VARS #######################
###############################################################################

#if there was an external input for the variable...
if test -n "$SAVE_CC"; then
	CC=$SAVE_CC
fi

if test -n "$SAVE_CXX"; then
	CXX=$SAVE_CXX
fi

if test -n "$SAVE_FC"; then
	FC=$SAVE_FC
fi

if test -n "$SAVE_MPICC"; then
	MPICC=$SAVE_MPICC
fi

if test -n "$SAVE_MPICXX"; then
	MPICXX=$SAVE_MPICXX
fi

if test -n "$SAVE_MPIFC"; then
	MPIFC=$SAVE_MPIFC
fi

if test -n "$SAVE_MPILIB"; then
	MPILIB=$SAVE_MPILIB
fi

if test -n "$SAVE_MPIINC"; then
	MPIINC=$SAVE_MPIINC
fi

#CFLAGS="$SAVE_CFLAGS ${CFLAGS}"
if test -n "$SAVE_CFLAGS"; then
	CFLAGS=$SAVE_CFLAGS $CFLAGS
fi

if test -n "$SAVE_FFLAGS"; then
	FFLAGS=$SAVE_FFLAGS
fi

if test -n "$SAVE_MPIROOT"; then
	MPIROOT=$SAVE_MPIROOT
fi

if test -n "$SAVE_HDF5ROOT"; then
	HDF5ROOT=$SAVE_HDF5ROOT
fi

if test -n "$SAVE_LDFLAGS"; then
	LDFLAGS=$SAVE_LDFLAGS
fi

if test -n "$MPICXX"; then
	TOOLS_CXX=$MPICXX
	TOOLS_H5PART_LIB="-lpH5Part"
	TOOLS_HDFLIB="-L$HDF5ROOT/lib -lhdf5"
else
	TOOLS_CXX=$CXX
	TOOLS_H5PART_LIB="-lH5Part"
	TOOLS_HDFLIB="-L$HDF5ROOT/lib -lhdf5"
fi

###############################################################################
############## EXPORTING VARIABLES & CREATING OUTPUT FILES ####################
###############################################################################
# AC_SUBST (variable, [value])
# Create an output variable from a shell variable. 
# Make AC_OUTPUT substitute the variable variable into output files (typically one or more `Makefile's). 
# This means that AC_OUTPUT will replace instances of `@variable@' in input files with the value that 
# the shell variable variable has when AC_OUTPUT is called. 
# This value of variable should not contain literal newlines.
# If value is given, in addition assign it to variable. 
AC_SUBST(MPIROOT)
AC_SUBST(HDF5ROOT)
AC_SUBST(MPIINC)
AC_SUBST(MPILIB)
AC_SUBST(MPICC)
AC_SUBST(MPICXX)
AC_SUBST(MPIFC)
AC_SUBST(MTARGET)
AC_SUBST(TPTARGET)
AC_SUBST(TBTARGET)
AC_SUBST(TFTARGET)
AC_SUBST(SZLIB)
AC_SUBST(CFLAGS)
AC_SUBST(FFLAGS)
AC_SUBST(STDCXX)
AC_SUBST(UNDERSCORE_H)
AC_SUBST(BUILD_TOOLS)
AC_SUBST(LDFLAGS)
AC_SUBST(TOOLS_CXX)
AC_SUBST(TOOLS_H5PART_LIB)
AC_SUBST(TOOLS_HDFLIB)

# Make AC_OUTPUT create each `file' by copying an input file (by default `file.in'),
# substituting the output variable values.
AC_CONFIG_FILES([
	Makefile
	doc/Makefile
	src/Makefile
	src/C/Makefile
	src/h5core/Makefile
	test/Makefile
	test/H5Part/Makefile
	test/H5Block/Makefile
	test/H5Fed/Makefile
	tools/Makefile
])

AC_OUTPUT

###############################################################################
########################## PRINTING SUMMARY ###################################
###############################################################################
AC_MSG_RESULT([ ])
AC_MSG_RESULT([Summary:])
AC_MSG_RESULT([ ])
AC_MSG_RESULT([Host OS:             $host_os])
AC_MSG_RESULT([Host CPU:            $host_cpu])
AC_MSG_RESULT([Host vendor:         $host_vendor])
AC_MSG_RESULT([Build libraries:     $MTARGET])
AC_MSG_RESULT([Build test programs: $TTARGET])
AC_MSG_RESULT([Build tools:         $BUILD_TOOLS])
AC_MSG_RESULT([CC =                 $CC])
AC_MSG_RESULT([CXX =                $CXX])
AC_MSG_RESULT([FC =                 $FC])
AC_MSG_RESULT([MPICC =              $MPICC])
AC_MSG_RESULT([MPICXX =             $MPICXX])
AC_MSG_RESULT([MPIFC =              $MPIFC])
AC_MSG_RESULT([CFLAGS =             $CFLAGS])
AC_MSG_RESULT([FFLAGS =             $FFLAGS])
AC_MSG_RESULT([MPILIB =             $MPILIB])
AC_MSG_RESULT([MPIINC =             $MPIINC])
AC_MSG_RESULT([MPIROOT =            $MPIROOT])
AC_MSG_RESULT([HDF5ROOT =           $HDF5ROOT])
AC_MSG_RESULT([LDFLAGS =            $LDFLAGS])
AC_MSG_RESULT([ ])
