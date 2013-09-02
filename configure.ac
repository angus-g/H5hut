AC_INIT([H5hut], [1.99.12], [h5part@lists.psi.ch], H5hut)
AC_PREREQ(2.60)
AC_CONFIG_HEADERS(config.h)
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE

ENABLE_DEBUG='no'
ENABLE_C='yes'
ENABLE_FORTRAN='no'
ENABLE_PARALLEL='no'

###############################################################################
# --enable-xxx and --with-xxx Arguments
AC_ARG_ENABLE(
	[debug],
	[AS_HELP_STRING([--enable-debug], [Compile with debug flags [default=no]])],
	[ENABLE_DEBUG=$enableval])
AM_CONDITIONAL([ENABLE_DEBUG], [test "$ENABLE_DEBUG" = "yes"])

AC_ARG_ENABLE(
	[c],
	[AS_HELP_STRING([--enable-c],
	[Compile the C interface [default=yes]])],
	[ENABLE_C=$enableval])
AM_CONDITIONAL([ENABLE_C], [test "$ENABLE_C" = "yes"])

AC_ARG_ENABLE(
	[fortran],
	[AS_HELP_STRING([--enable-fortran],
	[Compile the Fortran interface [default=no]])],
	[ENABLE_FORTRAN=$enableval])
AM_CONDITIONAL([ENABLE_FORTRAN], [test "$ENABLE_FORTRAN" = "yes"])

AC_ARG_ENABLE(
	[parallel],
	[AS_HELP_STRING([--enable-parallel],
	[Compile the MPI/IO interface [default=no]])],
	[ENABLE_PARALLEL=$enableval])
AM_CONDITIONAL([ENABLE_PARALLEL], [test "$ENABLE_PARALLEL" = "yes"])

AC_ARG_ENABLE(
	[large-indices],
	[AC_HELP_STRING([--enable-large-indices],
	[Compile with 64bit local IDs and indices [default=no]])],
	[USE_LARGE_INDICES=$enableval])

AC_ARG_ENABLE(
        [experimental],
        [AS_HELP_STRING([--enable-experimental],
        [Compile experimental code [default=no]])],
        [ENABLE_EXPERIMENTAL=$enableval])
AM_CONDITIONAL([ENABLE_EXPERIMENTAL], [test "$ENABLE_EXPERIMENTAL" = "yes"])

AC_ARG_ENABLE(
	[vtkconverter],
	[AS_HELP_STRING([--enable-vtkconverter],
	[Compile the vtk to H5hut converter for grids [default=no]])],
	[ENABLE_VTKCONVERTER=$enableval])
AM_CONDITIONAL([ENABLE_VTKCONVERTER], [test "$ENABLE_VTKCONVERTER" = "yes"])

AC_ARG_WITH(
	[hdf5],
	[AS_HELP_STRING([--with-hdf5],
	[path to HDF5 installation [default=""]])],
	[HDF5_PREFIX=$withval], [])

AC_ARG_WITH(
	[mpi],
	[AS_HELP_STRING([--with-mpi],
	[path to MPI installation [default=""]])],
	[PATH=$MPI_PREFIX/bin:$PATH])

AC_ARG_WITH(
	[lustre],
	[AS_HELP_STRING([--with-lustre],
	[path to lustre user API [default=""]])],
	[LUSTREPATH=$withval], [LUSTREPATH=""])

AC_ARG_WITH(
	[parmetis],
	[AC_HELP_STRING([--with-parmetis],
	[path to ParMETIS API [default=""]])],
	[PARMETISPATH=$withval], [PARMETISPATH=""])


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
# MISC SETTINGS INCLUDING C & C++ COMPILER SETTING

AC_CANONICAL_HOST
AC_PROG_MAKE_SET

# debug enabled?
AC_MSG_CHECKING([if debug is enabled])
if test "X$ENABLE_DEBUG" = "Xyes"; then
    AC_MSG_RESULT([yes])
    CFLAGS="$CFLAGS -g -Wall"
    CXXFLAGS="$CFLAGS -g -Wall"
    FFLAGS="$FFLAGS -g"
else
    AC_MSG_RESULT([no])
fi

# Determine a C/C++ compiler to use. 
if test "x$ENABLE_PARALLEL" = "xyes"; then
   	CCOMPILERS="mpicc cc"
	CXXCOMPILERS="mpic++ CC"
else
	CCOMPILERS="pgcc pathcc icc gcc cc_r cc"
	CXXCOMPILERS="pgcc pathcc icc g++ cc_r c++"
fi

AC_PROG_CC($CCOMPILERS)
CC=`which $CC`
AC_PROG_CXX($CXXCOMPILERS)
CXX=`which $CXX`

AC_PROG_CC_C99
if test "x$ac_cv_prog_cc_c99" = "xno"; then
	AC_MSG_ERROR([Cannot set C compiler to use C99 standard!])
	exit 1
fi

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
# PROCESS ARGUMENTS

# C enabled?
AC_MSG_CHECKING([if C interface enabled])
if test "X$ENABLE_C" = "Xyes"; then
	AC_MSG_RESULT([yes])
else
        AC_MSG_RESULT([no])
fi

# fortran enabled?
AC_MSG_CHECKING([if fortran interface enabled])
if test "X$ENABLE_FORTRAN" = "Xyes"; then
	AC_MSG_RESULT([yes])

	if test "X$ENABLE_PARALLEL" = "Xyes"; then
		AC_PROG_FC(mpif90 mpif77)
	else
		AC_PROG_FC(pgf90 ifort xlf_r pathf90 g95 g90 ftn gfortran)
	fi
        FC=`which $FC`
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
		`cd src/Fortran && ./TestUnderscore > Underscore.h`
		AC_MSG_RESULT([ok])
	else
		AC_MSG_RESULT([nok])
		AC_MSG_ERROR([Cannot determine the symbol convention for Fortran object files!])
		exit 1
	fi

else
	AC_MSG_RESULT([no])
fi

######################## large indices enabled ###########################
AC_MSG_CHECKING([if large indices are enabled])
if test "X$USE_LARGE_INDICES" = "Xyes"; then
	AC_MSG_RESULT([yes])
	CFLAGS="${CFLAGS} -DUSE_LARGE_INDICES"
else
	AC_MSG_RESULT([no])
fi

######################## parallel interface enabled ###########################
AC_MSG_CHECKING([if parallel interface enabled])
if test "X$ENABLE_PARALLEL" = "Xyes"; then
	AC_MSG_RESULT([yes])
	CPPFLAGS="${CPPFLAGS} -DPARALLEL_IO -DMPICH_IGNORE_CXX_SEEK"

        if test "X$ENABLE_EXPERIMENTAL" = "Xyes"; then
        	AC_MSG_CHECKING([for ParMETIS])
                if test "X$PARMETISPATH" != "X"; then
                        CPPFLAGS="$CPPFLAGS -I$PARMETISPATH/include"
	                LDFLAGS="$LDFLAGS -L$PARMETISPATH/lib"
	        fi

                LIBS="$LIBS -lparmetis -lmetis"
	        AC_TRY_LINK([#include <parmetis.h>], [
	                ParMETIS_V3_PartGeom (NULL, NULL, NULL, NULL, NULL);],
		        [AC_MSG_RESULT([yes]); r='yes'], [AC_MSG_RESULT([no]); r='no'] )

	        if test "X$r" = "Xno"; then
		        AC_MSG_ERROR([ParMETIS])
		        exit 1
	        fi
        fi
else  # --enable-parallel=no
	AC_MSG_RESULT([no])
fi

###############################################################################
# LIBRARY PATHS
if test -z "${HDF5_PREFIX}"; then
	# alternatives to HDF5_PREFIX
	if test -n "${HDF5_DIR}"; then
		HDF5_PREFIX=${HDF5_DIR}
	fi
fi
if test -n "${HDF5_PREFIX}"; then
        CPPFLAGS="$CPPFLAGS -I$HDF5_PREFIX/include"
        LDFLAGS="$LDFLAGS -L$HDF5_PREFIX/lib"
fi
AC_CHECK_LIB([z], [compress2])
AC_CHECK_LIB([hdf5], [H5open])
AS_IF([test "x$ac_cv_lib_hdf5_H5open" != xyes], 
        [AC_MSG_ERROR([Cannot compile and link a HDF5 program]); 
        exit 1])


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
        CPPFLAGS="$CPPFLAGS -I$LUSTREROOT/usr/include"
        LDFLAGS="$LDFLAGS -L$LUSTREROOT/usr/lib"
        LIBS="$LIBS -llustreapi"
fi

LIBS="$LIBS -lm"

###############################################################################
# TOOLS
AC_MSG_CHECKING([if we have to compile the VTK to H5hut grid converter])
if test "X$ENABLE_VTKCONVERTER" = "Xyes"; then
	AC_MSG_RESULT([yes])
        # :TODO: add test whether we can compile/link a prog with vtk
else
	AC_MSG_RESULT([no])
fi

###############################################################################
# EXPORTING VARIABLES & CREATING OUTPUT FILES
AM_CPPFLAGS=$CPPFLAGS
AM_LDFLAGS=$LDFLAGS

AC_SUBST(HDF5_PREFIX)
AC_SUBST(CFLAGS)
AC_SUBST(FFLAGS)
AC_SUBST(AM_CPPFLAGS)
AC_SUBST(AM_LDFLAGS)

AC_CONFIG_FILES([
	Makefile
	src/Makefile
	src/h5core/Makefile
	src/include/Makefile
	src/include/h5core/Makefile
        src/C/Makefile
        src/Fortran/Makefile
	test/Makefile
        doc/Makefile
	tools/Makefile
	tools/h5hutcc
        tools/vtk2h5grid/Makefile
        examples/H5/Makefile
        examples/H5Part/Makefile
        examples/H5Block/Makefile
        examples/H5Fed/Makefile
])

AC_OUTPUT

###############################################################################
# PRINTING SUMMARY
AC_MSG_RESULT([ ])
AC_MSG_RESULT([Summary:])
AC_MSG_RESULT([ ])
AC_MSG_RESULT([Host OS:             $host_os])
AC_MSG_RESULT([Host CPU:            $host_cpu])
AC_MSG_RESULT([Host vendor:         $host_vendor])
AC_MSG_RESULT([CC =                 $CC])
AC_MSG_RESULT([CXX =                $CXX])
AC_MSG_RESULT([FC =                 $FC])
AC_MSG_RESULT([CFLAGS =             $CFLAGS])
AC_MSG_RESULT([FFLAGS =             $FFLAGS])
AC_MSG_RESULT([AM_CPPFLAGS =        $AM_CPPFLAGS])
AC_MSG_RESULT([AM_LDFLAGS =         $AM_LDFLAGS])
AC_MSG_RESULT([LIBS =               $LIBS])
AC_MSG_RESULT([HDF5_PREFIX =        $HDF5_PREFIX])
AC_MSG_RESULT([LUSTREROOT =         $LUSTREROOT])
AC_MSG_RESULT([ ])
