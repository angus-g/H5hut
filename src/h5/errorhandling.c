#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "H5PartTypes.h"
#include "H5Part.h"
#include "H5PartPrivate.h"
#include "H5PartErrors.h"
#include "H5.h"

h5part_error_handler	_err_handler = H5PartReportErrorHandler;
h5part_int64_t		_h5part_errno = H5PART_SUCCESS;
h5part_int64_t		_debug = 0;

static char *__funcname = "NONE";

h5part_int64_t
H5_set_debuglevel (
	h5part_int64_t level
	) {
	_debug = level;
	return H5PART_SUCCESS;
}

h5part_int64_t
H5_get_debuglevel (
	void
	) {
	return _debug;
}

h5part_int64_t
H5_set_errorhandler (
	h5part_error_handler handler
	) {
	_err_handler = handler;
	return H5PART_SUCCESS;
}

h5part_error_handler
H5_get_errorhandler (
	void
	) {
	return _err_handler;
}

h5part_int64_t
H5_get_errno (
	void
	) {
	return _h5part_errno;
}


/*!
  \ingroup h5part_errhandle

  This is the H5Part default error handler.  If an error occures, an
  error message will be printed and an error number will be returned.

  \return value given in \c eno
*/
h5part_int64_t
H5_report_errorhandler (
	const char *funcname,
	const h5part_int64_t eno,
	const char *fmt,
	...
	) {

	_h5part_errno = eno;
	if ( _debug > 0 ) {
		va_list ap;
		va_start ( ap, fmt );
		H5_vprint_error ( fmt, ap );
		va_end ( ap );
	}
	return _h5part_errno;
}

/*!
  \ingroup h5part_errhandle

  If an error occures, an error message will be printed and the
  program exists with the error code given in \c eno.
*/
h5part_int64_t
H5_abort_errorhandler (
	const char *funcname,
	const h5part_int64_t eno,
	const char *fmt,
	...
	) {

	_h5part_errno = eno;
	if ( _debug > 0 ) {
		va_list ap;
		va_start ( ap, fmt );
		fprintf ( stderr, "%s: ", funcname );
		vfprintf ( stderr, fmt, ap );
		fprintf ( stderr, "\n" );
	}
	exit (-(int)_h5part_errno);
}

static void
_vprintf (
	FILE* f,
	const char *prefix,
	const char *fmt,
	va_list ap
	) {
	char *fmt2 = (char*)malloc( strlen ( prefix ) +strlen ( fmt ) + strlen ( __funcname ) + 16 );
	if ( fmt2 == NULL ) return;
	sprintf ( fmt2, "%s: %s: %s\n", prefix, __funcname, fmt ); 
	vfprintf ( stderr, fmt2, ap );
	free ( fmt2 );
}

void
H5_vprint_error (
	const char *fmt,
	va_list ap
	) {

	if ( _debug < 1 ) return;
	_vprintf ( stderr, "E", fmt, ap );
}

void
H5_print_error (
	const char *fmt,
	...
	) {

	va_list ap;
	va_start ( ap, fmt );
	H5_vprint_error ( fmt, ap );
	va_end ( ap );
}

void
H5_vprint_warn (
	const char *fmt,
	va_list ap
	) {

	if ( _debug < 2 ) return;
	_vprintf ( stderr, "W", fmt, ap );
}

void
H5_print_warn (
	const char *fmt,
	...
	) {

	va_list ap;
	va_start ( ap, fmt );
	H5_vprint_warn ( fmt, ap );
	va_end ( ap );
}

void
H5_vprint_info (
	const char *fmt,
	va_list ap
	) {

	if ( _debug < 3 ) return;
	_vprintf ( stdout, "I", fmt, ap );
}

void
H5_print_info (
	const char *fmt,
	...
	) {

	va_list ap;
	va_start ( ap, fmt );
	H5_vprint_info ( fmt, ap );
	va_end ( ap );
}

void
H5_vprint_debug (
	const char *fmt,
	va_list ap
	) {

	if ( _debug < 4 ) return;
	_vprintf ( stdout, "D", fmt, ap );
}

void
H5_print_debug (
	const char *fmt,
	...
	) {

	va_list ap;
	va_start ( ap, fmt );
	H5_vprint_debug ( fmt, ap );
	va_end ( ap );
}

void
H5_set_funcname (
	char  * const fname
	) {
	__funcname = fname;
}

const char *
H5_get_funcname (
	void
	) {
	return __funcname;
}
