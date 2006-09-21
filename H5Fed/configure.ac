## Process this file with autoconf to produce a configure script
##
## author    - benedikt oswald and patrick leidenberger
## modified  - 2006 aug 21, pl, creation.
## modified  - 2006 aug 24, pl, add h5fed path.
##
## to do     : The rlog lib needs a -DRLOG_COMPONENT="some name", I don't know
##             how tho do this so that the quotation marks are in the output
##             stream as well, pl, 2006 jul 04.
##
## objective - develop input file for GNU autotools/configure
## project   - H5Fed
##

AC_PREREQ(2.59)
AC_COPYRIGHT([This configure script is copyright by Benedikt Oswald and Patrick Leidenberger, 2006])

AC_INIT([gmshtohdf5fed], [1.0], [in case of problems mail to: benedikt.oswald@psi.ch or patrick.leidenberger@psi.ch])

# library creation
AC_PROG_LIBTOOL

# standard macros
AC_CONFIG_AUX_DIR(./)
AM_INIT_AUTOMAKE
AC_CONFIG_HEADER([./config.h:./config.in])

# set cppflags to zero
CPPFLAGS=""
CFFLAGS=""
CXXFLAGS=""

# provide include directories
AC_SUBST([AM_CPPFLAGS], '-I$(top_srcdir)/libsrc/stdincl -I$(top_srcdir)/libsrc/gmsh -I$(top_srcdir)/libsrc/h5fed')
AC_SUBST([GMSH2H5FED_LIBS],'$(top_srcdir)/libsrc/gmsh/libgmsh.la $(top_srcdir)/libsrc/h5fed/libh5fed.la')

# Make available external libraries
AC_ARG_WITH(boost,
            AC_HELP_STRING([--with-boost=PREFIX],
            [prefix where the Boost libraries and include files are to be found]),
						[BOOST_PREFIX=$withval
						 LDFLAGS="-lz -lboost_program_options "])

AC_ARG_WITH(rlog,
            AC_HELP_STRING([--with-rlog=PREFIX],
            [prefix where the rlog libraries and include files are to be found]),
						[RLOG_PREFIX=$withval
						 CPPFLAGS="-I"$RLOG_PREFIX"/include "${CPPFLAGS}
						 LDFLAGS=$RLOG_PREFIX"/lib/librlog.la "${LDFLAGS}])

AC_ARG_ENABLE(rlog-debug,
              AC_HELP_STRING([--disable-rlog-debug],
              [disable the debug output channel of rlog]),
              [],
              [CFFLAGS="-DUSE_RLOG_DEBUG_CHANNEL "${CFFLAGS}
               CXXFLAGS="-DUSE_RLOG_DEBUG_CHANNEL "${CXXFLAGS}])

AC_ARG_ENABLE(rlog-error,
              AC_HELP_STRING([--disable-rlog-error],
              [disable the error output channel of rlog]),
              [],
              [CFFLAGS="-DUSE_RLOG_ERROR_CHANNEL "${CFFLAGS}
               CXXFLAGS="-DUSE_RLOG_ERROR_CHANNEL "${CXXFLAGS}])

AC_ARG_ENABLE(rlog-info,
              AC_HELP_STRING([--disable-rlog-info],
              [disable the info output channel of rlog]),
              [],
              [CFFLAGS="-DUSE_RLOG_INFO_CHANNEL "${CFFLAGS}
               CXXFLAGS="-DUSE_RLOG_INFO_CHANNEL "${CXXFLAGS}])

AC_ARG_ENABLE(rlog-warning,
              AC_HELP_STRING([--disable-rlog-warning],
              [disable the warning output channel of rlog]),
              [],
              [CFFLAGS="-DUSE_RLOG_WARNING_CHANNEL "${CFFLAGS}
               CXXFLAGS="-DUSE_RLOG_WARNING_CHANNEL "${CXXFLAGS}])

AC_ARG_WITH(hdf5,
            AC_HELP_STRING([--with-hdf5=PREFIX],
            [prefix to hdf5 libraries and include files]),
               [HDF5_PREFIX=$withval
				 CPPFLAGS="-DHAVE_HDF5 -I"$HDF5_PREFIX"/include "${CPPFLAGS}
			     LDFLAGS="-L"$HDF5_PREFIX"/lib -lhdf5 "${LDFLAGS}])
#			     LDFLAGS=$HDF5_PREFIX"/lib/libhdf5.la "${LDFLAGS}])


# Set C/C++ compiler flags.
CPPFLAGS=${CPPFLAGS}
CFFLAGS="-g3 "${CFFLAGS}
CXXFLAGS="-g3 "${CXXFLAGS}

AM_INIT_AUTOMAKE([1.9 foreign])

AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_INSTALL([install])

AC_CONFIG_FILES([makefile
                   applications/makefile
                   libsrc/makefile
                     libsrc/gmsh/makefile
                   libsrc/h5fed/makefile])


AC_OUTPUT
