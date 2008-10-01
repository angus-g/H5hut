#ifndef __ERRORHANDLING_H
#define __ERRORHANDLING_H

#define h5_debug	h5_print_debug
#define h5_info		h5_print_info
#define h5_warn		h5_print_warn
#if 0
#define h5_error	h5_print_error
#endif

#define SET_FNAME( fname )	h5_set_funcname( fname );
#define CHECK_FILEHANDLE( f ) \
	if ( f == NULL ) \
		return HANDLE_H5_BADFD_ERR;

#define CHECK_WRITABLE_MODE( f )  \
	if ( f->mode==H5_O_RDONLY ) \
		return (*h5_get_errorhandler()) (	\
			h5_get_funcname(), \
			H5_ERR_INVAL, \
			"Attempting to write to read-only file" );

#define CHECK_READONLY_MODE( f )  \
	if ( ! f->mode==H5_O_RDONLY ) \
		return (*h5_get_errorhandler()) (	\
			h5_get_funcname(), \
			H5_ERR_INVAL, \
			"Operation is not allowed on writable files." );

#define CHECK_TIMEGROUP( f ) \
	if ( f->step_gid <= 0 ) \
		return (*h5_get_errorhandler()) (	\
			h5_get_funcname(), \
			H5_ERR_INVAL, \
			"Internal error: step_gid <= 0.");

h5_err_t
h5_set_debuglevel (
	h5_id_t level
	);

h5_err_t
h5_get_debuglevel (
	void
	);

h5_err_t
h5_set_errorhandler (
	h5_error_handler handler
	);

h5_error_handler
h5_get_errorhandler (
	void
	);

h5_err_t
h5_get_errno (
	void
	);

void
h5_set_errno (
	h5_err_t h5_errno
	);

h5_err_t
h5_report_errorhandler (
	const char *funcname,
	const h5_err_t eno,
	const char *fmt,
	...
	);

h5_err_t
h5_report_verrorhandler (
	const char *funcname,
	const h5_err_t eno,
	const char *fmt,
	va_list ap
	);

h5_err_t
h5_abort_errorhandler (
	const char *funcname,
	const h5_err_t eno,
	const char *fmt,
	...
	) ;

h5_err_t
h5_error (
	h5_err_t error_no,
	const char *fmt,
	...
	);

void
h5_vprint_error (
	const char *fmt,
	va_list ap
	);

void
h5_print_error (
	const char *fmt,
	... )
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
;

void
h5_vprint_warn (
	const char *fmt,
	va_list ap
	);

void
h5_print_warn (
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
;

void
h5_vprint_info (
	const char *fmt,
	va_list ap
	);

void
h5_print_info (
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
;

void
h5_vprint_debug (
	const char *fmt,
	va_list ap
	);

void
h5_print_debug (
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
;

void
h5_set_funcname (
	const char  * const fname
	);

const char *
h5_get_funcname (
	void
	);

const char *
h5_get_objname (
	hid_t id
	);

#endif
