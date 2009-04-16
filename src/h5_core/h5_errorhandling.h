#ifndef __H5_ERRORHANDLING_H
#define __H5_ERRORHANDLING_H

#define SET_FNAME( f, fname )	h5_set_funcname( f, fname );
#define CHECK_FILEHANDLE( f )					\
	if ( h5_check_filehandle ( f ) != H5_SUCCESS )		\
		return h5_get_errno( f );


#define CHECK_WRITABLE_MODE( f )					\
	if ( f->mode==H5_O_RDONLY )					\
		return h5_error (					\
			f,						\
			H5_ERR_INVAL,					\
			"Attempting to write to read-only file" );

#define CHECK_READONLY_MODE( f )					\
	if ( ! f->mode==H5_O_RDONLY )					\
		return h5_error (					\
			f,						\
			H5_ERR_INVAL,					\
			"Operation is not allowed on writable files." );

#define CHECK_TIMEGROUP( f )						\
	if ( f->step_gid <= 0 )						\
		return h5_error (					\
			f,						\
			H5_ERR_INVAL,					\
			"Internal error: step_gid <= 0.");

h5_err_t
h5_set_debuglevel (
	h5_id_t debuglevel
	);

h5_err_t
h5_get_debuglevel (
	void
	);

h5_err_t
h5_set_errorhandler (
	h5_errorhandler_t errorhandler
	);

h5_errorhandler_t
h5_get_errorhandler (
	void
	);

h5_err_t
h5_get_errno (
	h5_file_t * const f
	);

void
h5_set_errno (
	h5_file_t * const f,
	h5_err_t h5_errno
	);

h5_err_t
h5_report_errorhandler (
	h5_file_t * const f,
	const char *fmt,
	va_list ap
	);

h5_err_t
h5_abort_errorhandler (
	h5_file_t * const f,
	const char *fmt,
	va_list ap
	) ;

h5_err_t
h5_error (
	h5_file_t * const f,
	h5_err_t error_no,
	const char *fmt,
	...
	)
#ifdef __GNUC__
	__attribute__ ((format (printf, 3, 4)))
#endif
;

void
h5_verror (
	h5_file_t * const f,
	const char *fmt,
	va_list ap
	);

void
h5_vwarn (
	h5_file_t * const f,
	const char *fmt,
	va_list ap
	);

void
h5_warn (
	h5_file_t * const f,
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 2, 3)))
#endif
;

void
h5_vinfo (
	h5_file_t * const f,
	const char *fmt,
	va_list ap
	);

void
h5_info (
	h5_file_t * const f,
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 2, 3)))
#endif
;

void
h5_vdebug (
	h5_file_t * const f,
	const char *fmt,
	va_list ap
	);

void
h5_debug (
	h5_file_t * const f,
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 2, 3)))
#endif
;

void
h5_set_funcname (
	h5_file_t * const f,
	const char  * const fname
	);

const char *
h5_get_funcname (
	h5_file_t * const f
	);

#endif
