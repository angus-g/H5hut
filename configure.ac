# Every configure script must call AC_INIT before doing anything else.
# AC_INIT (package, version, [bug-report], [tarname])
AC_INIT([H5hut], [1.99.6], [h5part@lists.psi.ch], H5hut)


# Ensure that a recent enough version of Autoconf is being used. 
# If the version of Autoconf being used to create configure is earlier than version, 
# print an error message to the standard error output and do not create configure.
AC_PREREQ(2.60)

# should be called right after AC_INIT.
# configure scripts can create a C header file containing `#define' directives. 
# The AC_CONFIG_HEADERS macro selects this kind of output.
AC_CONFIG_HEADERS(config.h)

AC_CONFIG_MACRO_DIR([m4])

# AM_INIT_AUTOMAKE is required to use autoconf with automake
AM_INIT_AUTOMAKE

BUILD_LIBS='libH5hut'
USE_C='yes'

###############################################################################
################# --enable-xxx and --with-xxx Argument ########################
###############################################################################

AC_ARG_ENABLE(
	[debug],
	[AC_HELP_STRING([--enable-debug],
	[Compile with debug flags [default=no]])],
	[USE_DEBUG=$enableval])

AC_ARG_ENABLE(
	[c],
	[AC_HELP_STRING([--enable-c],
	[Compile the C interface [default=yes]])],
	[USE_C=$enableval])

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

AC_ARG_WITH(
	[hdf5],
	[AC_HELP_STRING([--with-hdf5],
	[path to HDF5 installation [default=""]])],
	[HDF5PATH=$withval], [HDF5PATH=""])

AC_ARG_WITH(
	[mpi],
	[AC_HELP_STRING([--with-mpi],
	[path to MPI installation [default=""]])],
	[MPIPATH=$withval;PATH=$MPIPATH/bin:$PATH], [MPIPATH=""])

AC_ARG_WITH(
	[lustre],
	[AC_HELP_STRING([--with-lustre],
	[path to lustre user API [default=""]])],
	[LUSTREPATH=$withval], [LUSTREPATH=""])

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

AC_PROG_MAKE_SET

# Determine a C/C++ compiler to use. 
# If CC is not already set in the environment, check for gcc and cc, then
# for other C compilers. 
# Set output variable CC to the name of the compiler found.
if test "x$USE_PARALLEL" = "xyes"; then
   	CCOMPILERS="mpicc cc"
	CXXCOMPILERS="mpic++ c++"
else
	CCOMPILERS="pgcc pathcc icc gcc cc_r cc"
	CXXCOMPILERS="pgcc pathcc icc g++ cc_r c++"
fi

AC_PROG_CC($CCOMPILERS)
CC=`which $CC`
AC_PROG_CXX($CXXCOMPILERS)
CXX=`which $CXX`

# Use macro to set C99 mode instead of checking for gcc, which breaks with
# parallel builds.
AC_PROG_CC_C99
if test "x$ac_cv_prog_cc_c99" = "xno"; then
	AC_MSG_ERROR([Cannot set C compiler to use C99 standard!])
	exit 1
fi

# Set output variable INSTALL to the path of a BSD-compatible install program,
# if one is found in the current PATH. 
# Otherwise, set INSTALL to `dir/install-sh -c`
AC_PROG_INSTALL
AC_PROG_AWK

# Disable shared libraries by default: can be enabled with --enable-shared
LT_INIT([disable-shared])
AC_PROG_LIBTOOL

# Default prefix for bindir, etc... (eg >> ./build/bin)
AC_PREFIX_DEFAULT(`pwd`/build)

# Checks for header files.
AC_CHECK_HEADERS([fcntl.h limits.h stdint.h stdlib.h string.h sys/ioctl.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_CHECK_TYPES([ptrdiff_t])

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([memmove memset pow strchr strdup strerror strstr])

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

AC_MSG_CHECKING([if debug is enabled])

if test "X$USE_DEBUG" = "Xyes"; then
    AC_MSG_RESULT([yes])
    CFLAGS="$CFLAGS -g"
    FFLAGS="$FFLAGS -g"
else
    AC_MSG_RESULT([no])
fi

############################ fortran enabled ##################################
AC_MSG_CHECKING([if C interface enabled])
if test "X$USE_C" = "Xyes"; then
	AC_MSG_RESULT([yes])
	BINDINGS="$BINDINGS C"
	LIB_C="libH5hutC.la"
	BUILD_LIBS="$BUILD_LIBS libH5hutC"
	BUILD_TESTS="$BUILD_TESTS h5u_test h5b_test"
	BUILD_TOOLS="$BUILD_TOOLS h5hutcc h5PartDcToVtk h5PartSurfaceToVtk"
fi

############################ fortran enabled ##################################
AC_MSG_CHECKING([if fortran interface enabled])
if test "X$USE_FORTRAN" = "Xyes"; then
	AC_MSG_RESULT([yes])
	BINDINGS="$BINDINGS Fortran"
	LIB_FORTRAN="libH5hutF.la"
        BUILD_LIBS="$BUILD_LIBS libH5hutF"

	if test "X$USE_PARALLEL" = "Xyes"; then
		AC_PROG_FC(mpif90 mpif77)
	else
		AC_PROG_FC(pgf90 ifort xlf_r pathf90 g95 g90 ftn gfortran)
	fi

	if test -z "$FC" ; then
		AC_MSG_ERROR([Cannot find a Fortran compiler!])
		exit 1
	fi

	if test $FC = "g90"; then
		FFLAGS="${FFLAGS} -fno-second-underscore"
	fi

	if test $FC = "g95"; then
		FFLAGS="${FFLAGS} -fno-second-underscore"
	fi

	AC_MSG_CHECKING([symbol convention in object files])
	`cd src/Fortran && rm -f TestUnderscore.o TestUnderscoreC.o TestUnderscore`
	`cd src/Fortran && ${FC} ${FFLAGS} -c TestUnderscore.f`
	`cd src/Fortran && ${CC} ${CFLAGS} -c TestUnderscoreC.c`
	`cd src/Fortran && ${FC} ${FFLAGS} -o TestUnderscore TestUnderscore.o TestUnderscoreC.o`

	if test -f src/Fortran/TestUnderscore ; then
		UNDERSCORE_H=Underscore.h
		`cd src/Fortran && ./TestUnderscore > Underscore.h`
		AC_MSG_RESULT([ok])
	else
		AC_MSG_RESULT([nok])
		AC_MSG_ERROR([Cannot determine the symbon convention for Fortran object files!])
		exit 1
	fi

else
	AC_MSG_RESULT([no])
fi


######################## parallel interface enabled ###########################
AC_MSG_CHECKING([if parallel interface enabled])
if test "X$USE_PARALLEL" = "Xyes"; then
	AC_MSG_RESULT([yes])

	CFLAGS="${CFLAGS} -DPARALLEL_IO -DMPICH_IGNORE_CXX_SEEK"

	AC_MSG_CHECKING([if we can compile MPI code without setting flags])
	AC_TRY_LINK([#include "mpi.h"], [
		MPI_Comm comm;
		int n;
		MPI_Comm_size( comm, &n ); ], 
		[AC_MSG_RESULT([yes]); r='yes'], [AC_MSG_RESULT([no]); r='no'] )

	if test "X$r" = "Xno"; then
		AC_MSG_ERROR([MPI wrapper can't compile or link MPI program! Please set the INCLUDE and LIBS variables manually.])
		exit 1
	fi

	TPTARGET="${TPTARGET} H5PartTestP H5PartAndreasTest Bench"
	TBTARGET="${TBTARGET} H5BlockTestAttributes"
	TBTARGET="${TBTARGET} H5BlockParTestScalarField"

	# parallel + fortran
	if test "X$USE_FORTRAN" = "Xyes"; then
		TPTARGET="${TPTARGET} H5testFpar"
		TBTARGET="${TBTARGET} H5BlockParTestScalarFieldF"
	fi

else  # --enable-parallel=no
	AC_MSG_RESULT([no])

	TPTARGET="${TPTARGET} H5test"
	TBTARGET="${TBTARGET} H5BlockTestAttributes"

	if test "X$USE_FORTRAN" = "Xyes"; then
		TPTARGET="${TPTARGET} H5testF"
		TBTARGET="${TBTARGET} H5BlockTestAttributesF"
	fi
fi

###############################################################################
######################### PATH CHECKING & SETTING #############################
###############################################################################

AC_MSG_CHECKING([for HDF5 root ])
AC_MSG_RESULT([])
if test -n "${HDF5PATH}" ; then
	P=${HDF5PATH}
elif test -n "${HDF5ROOT}"; then
	P=${HDF5ROOT}
elif test -n "${HDF5HOME}" ; then
	P=${HDF5HOME}
elif test -n "${HDF5_DIR}" ; then
	P=${HDF5_DIR}
else
	P=''
	P="$P /usr"
	P="$P /usr/local"
	P="$P /usr/local/hdf5"
	P="$P /usr/local/packages/hdf5"
	P="$P /apps/hdf5"
	P="$P /opt/hdf5"
fi
PATH_Search HDF5ROOT "$P" include/hdf5.h
if test -z "$HDF5ROOT"; then
	AC_MSG_ERROR([Cannot find an HDF5 library!])
	exit 1
fi

INCLUDES="$INCLUDES -I$HDF5ROOT/include"
LDFLAGS="$LDFLAGS -L$HDF5ROOT/lib"
LIBS="$LIBS -lhdf5"

AC_MSG_CHECKING([if we need to link to libsz ])
if test -n "$HDF5ROOT"; then
	if test -f $HDF5ROOT/lib/libsz.a; then
		AC_MSG_RESULT([yes])
		LDFLAGS="$LDFLAGS -L$HDF5ROOT/lib"
                LIBS="$LIBS -lsz"
	else
		AC_MSG_RESULT([no])
	fi
fi

AC_MSG_CHECKING([for lustre API ])
AC_MSG_RESULT([])
if test -n "${LUSTREPATH}"; then
	P=${LUSTREPATH}
elif test -n "${LUSTREROOT}" ; then
	P=${LUSTREROOT}
elif test -n "${LUSTREHOME}" ; then
	P=${LUSTREHOME}
elif test -n "${LUSTRE_DIR}" ; then
	P=${LUSTRE_DIR}
else
	P=''
	P="$P /usr"
	P="$P /usr/local"
	P="$P /usr/local/lustre"
	P="$P /opt/lustre"
fi
PATH_Search LUSTREROOT "$P" usr/include/lustre/liblustreapi.h
if test -z "$LUSTREROOT"; then
	AC_MSG_WARN([Couldn't locate the lustre API... building without support for lustre striping!])
else
        CFLAGS="$CFLAGS -DH5_USE_LUSTRE"
        INCLUDES="$INCLUDES -I$LUSTREROOT/usr/include"
        LDFLAGS="$LDFLAGS -L$LUSTREROOT/usr/lib"
        LIBS="$LIBS -llustreapi"
fi

LIBS="$LIBS -lz -lm"

###############################################################################
############## EXPORTING VARIABLES & CREATING OUTPUT FILES ####################
###############################################################################
# AC_SUBST (variable, [value])
# Create an output variable from a shell variable.  Make AC_OUTPUT substitute
# the variable variable into output files (typically one or more `Makefile's).
# This means that AC_OUTPUT will replace instances of `@variable@' in input
# files with the value that the shell variable variable has when AC_OUTPUT is
# called.  This value of variable should not contain literal newlines.  If
# value is given, in addition assign it to variable. 
AC_SUBST(HDF5ROOT)
AC_SUBST(LUSTREROOT)
AC_SUBST(CFLAGS)
AC_SUBST(FFLAGS)
AC_SUBST(INCLUDES)
AC_SUBST(LDFLAGS)
AC_SUBST(LIBS)
AC_SUBST(UNDERSCORE_H)
AC_SUBST(LIB_C)
AC_SUBST(LIB_FORTRAN)
AC_SUBST(BINDINGS)
AC_SUBST(BUILD_TESTS)
AC_SUBST(BUILD_TOOLS)

# Make AC_OUTPUT create each `file' by copying an input file (by default `file.in'),
# substituting the output variable values.
AC_CONFIG_FILES([
	Makefile
	src/Makefile
	src/C/Makefile
	src/h5core/Makefile
        src/Fortran/Makefile
	test/Makefile
        test/H5Fed/Makefile
	tools/Makefile
	tools/h5hutcc
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
AC_MSG_RESULT([Build libraries:     $BUILD_LIBS])
AC_MSG_RESULT([Build test programs: $BUILD_TESTS])
AC_MSG_RESULT([Build tools:         $BUILD_TOOLS])
AC_MSG_RESULT([CC =                 $CC])
AC_MSG_RESULT([CXX =                $CXX])
AC_MSG_RESULT([FC =                 $FC])
AC_MSG_RESULT([CFLAGS =             $CFLAGS])
AC_MSG_RESULT([FFLAGS =             $FFLAGS])
AC_MSG_RESULT([INCLUDES =           $INCLUDES])
AC_MSG_RESULT([LDFLAGS =            $LDFLAGS])
AC_MSG_RESULT([LIBS =               $LIBS])
AC_MSG_RESULT([HDF5ROOT =           $HDF5ROOT])
AC_MSG_RESULT([LUSTREROOT =         $LUSTREROOT])
AC_MSG_RESULT([ ])
