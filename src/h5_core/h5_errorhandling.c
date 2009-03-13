#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core.h"
#include "h5_core_private.h"

static h5_errorhandler_t	_h5_errhandler = h5_report_errorhandler;
static h5_int32_t _h5_debuglevel = 0;

/*!
  \ingroup h5_core
  \defgroup h5_core_errorhandling
*/
//static char *__funcname = "NONE";

const char * const H5_O_MODES[] = {
	"H5_O_RDWR",
	"H5_O_RDONLY",
	"H5_O_WRONLY",
	"H5_O_APPEND"
};

/*!
  \ingroup h5_core_errorhandling

  Set debug/verbosity level. On level 0 all output will be supressed (even
  error messages). On level 1 error messages, on level 2 warning messages
  and on level 3 informational messages will be printed. On level 4 debug
  messages will be printed. 

  Values less than 0 are equivalent to 0. Values greater than 4 are equivalent
  to 4.

  \return \c H5_SUCCESS on success.
  \return \c H5_ERR_INVAL if debug level is invalid.
*/
h5_err_t
h5_set_debuglevel (
	h5_id_t debuglevel	/*!< debug level */
	) {
	if ( debuglevel < 0 || debuglevel > 5 ) return H5_ERR_INVAL;
	_h5_debuglevel = debuglevel;
	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_errorhandling

  Get current debug/verbosity level.

  \return current debug level
*/
h5_id_t
h5_get_debuglevel (
	void
	) {
	return _h5_debuglevel;
}

/*!
  \ingroup h5_core_errorhandling

  Set own error handler.

  \return \c H5_SUCCESS
*/
h5_err_t
h5_set_errorhandler (
	h5_errorhandler_t handler
	) {
	_h5_errhandler = handler;
	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_errorhandling

  Return pointer to current error handler.

  \return \c H5_SUCCESS
*/
h5_errorhandler_t
h5_get_errorhandler (
	void
	) {
	return _h5_errhandler;
}

/*!
  \ingroup h5_core_errorhandling

  Get current error number.

  \return \c H5_SUCCESS
*/
h5_err_t
h5_get_errno (
	h5_file_t * const f
	) {
	return f->__errno;
}

/*!
  \ingroup h5_core_errorhandling

  Set error number.

  \return \c H5_SUCCESS
*/
void
h5_set_errno (
	h5_file_t * const f,
	h5_err_t h5_errno
	) {
	f->__errno = h5_errno;
}


/*!
  \ingroup h5_core_errorhandling

  This is the H5 default error handler.  If an error occures, the
  error message will be printed, if debug level is greater than 0.

  \return \c f->__errno
*/
h5_err_t
h5_report_errorhandler (
	h5_file_t * const f,
	const char *fmt,
	va_list ap
	) {

	if ( _h5_debuglevel > 0 ) {
		h5_verror ( f, fmt, ap );
	}
	return f->__errno;
}

/*!
  \ingroup h5_core_errorhandling

  If an error occures, the error message will be printed and the
  program exists with the error code given in \c f->__errno.
*/
h5_err_t
h5_abort_errorhandler (
	h5_file_t * const f,
	const char *fmt,
	va_list ap
	) {

	if ( _h5_debuglevel > 0 ) {
		fprintf ( stderr, "%s: ", f->__funcname );
		vfprintf ( stderr, fmt, ap );
	}
	exit (-(int)f->__errno);
}

static void
_vprintf (
	FILE* f,
	const char *prefix,
	const char *__funcname,
	const char *fmt,
	va_list ap
	) {
	char *fmt2 = (char*)malloc(
		strlen ( prefix ) +
		strlen ( fmt ) + 
		strlen ( __funcname ) + 16 );
	if ( fmt2 == NULL ) return;
	sprintf ( fmt2, "%s: %s: %s\n", prefix, __funcname, fmt ); 
	vfprintf ( f, fmt2, ap );
	free ( fmt2 );
}

/*!
  \ingroup h5_core_errorhandling

  Print error message via error handler.

  \return \c f->__errno
*/
h5_err_t
h5_error (
	h5_file_t * const f,
	h5_err_t __errno,
	const char *fmt,
	...
	) {
	f->__errno = __errno;
	va_list ap;
	va_start ( ap, fmt );

	(*_h5_errhandler)( f, fmt, ap );

	va_end ( ap );
	return f->__errno;
}

/*!
  \ingroup h5_core_errorhandling

  Print error message to \c stderr. For use in error handlers only.
*/
void
h5_verror (
	h5_file_t * const f,
	const char *fmt,
	va_list ap
	) {

	if ( _h5_debuglevel < 1 ) return;
	_vprintf ( stderr, "E", f->__funcname, fmt, ap );
}


/*!
  \ingroup h5_core_errorhandling

  Print a warning message to \c stderr.
*/
void
h5_vwarn (
	h5_file_t * const f,
	const char *fmt,
	va_list ap
	) {

	if ( _h5_debuglevel < 2 ) return;
	_vprintf ( stderr, "W", f->__funcname, fmt, ap );
}

/*!
  \ingroup h5_core_errorhandling

  Print a warning message to \c stderr.
*/
void
h5_warn (
	h5_file_t * const f,
	const char *fmt,
	...
	) {

	va_list ap;
	va_start ( ap, fmt );
	h5_vwarn ( f, fmt, ap );
	va_end ( ap );
}

/*!
  \ingroup h5_core_errorhandling

  Print an informational message to \c stdout.
*/
void
h5_vinfo (
	h5_file_t * const f,
	const char *fmt,
	va_list ap
	) {

	if ( _h5_debuglevel < 3 ) return;
	_vprintf ( stdout, "I", f->__funcname, fmt, ap );
}

/*!
  \ingroup h5_core_errorhandling

  Print an informational message to \c stdout.
*/
void
h5_info (
	h5_file_t * const f,
	const char *fmt,
	...
	) {

	va_list ap;
	va_start ( ap, fmt );
	h5_vinfo ( f, fmt, ap );
	va_end ( ap );
}

/*!
  \ingroup h5_core_errorhandling

  Print a debug message to \c stdout.
*/
void
h5_vdebug (
	h5_file_t * const f,
	const char *fmt,
	va_list ap
	) {

	if ( _h5_debuglevel < 4 ) return;
	_vprintf ( stdout, "D", f->__funcname, fmt, ap );
}

/*!
  \ingroup h5_core_errorhandling

  Print a debug message to \c stdout.
*/
void
h5_debug (
	h5_file_t * const f,
	const char *fmt,
	...
	) {

	va_list ap;
	va_start ( ap, fmt );
	h5_vdebug ( f, fmt, ap );
	va_end ( ap );
}

/*!
  \ingroup h5_core_errorhandling

  Set function name. This name will used as prefix to all message.
*/
void
h5_set_funcname (
	h5_file_t * const f,
	const char * const fname
	) {
	f->__funcname = (char * const) fname;
}

/*!
  \ingroup h5_core_errorhandling

  Get function name.
*/
const char *
h5_get_funcname (
	h5_file_t * const f
	) {
	return f->__funcname;
}

