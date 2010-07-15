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
			"Time step is invalid! Have you set the time step?");

#define h5_error_not_implemented( f, file, func, lino )		     \
	h5_error(						     \
		f,						     \
		H5_ERR_NOT_IMPLEMENTED,				     \
		"%s: Function \"%s\", line %d not yet implemented!", \
		file, func, lino );

#define h5_error_internal( f, file, func, lino )   \
	h5_error(				   \
		f,				   \
		H5_ERR_INTERNAL,		   \
		"%s: Internal error: %s line %d!", \
		file, func, lino )


h5_err_t
h5_set_debuglevel (
	const h5_id_t level
	);

h5_err_t
h5_get_debuglevel (
	void
	);

h5_err_t
h5_set_errorhandler (
	const h5_errorhandler_t errorhandler
	);

h5_errorhandler_t
h5_get_errorhandler (
	void
	);

h5_err_t
h5_get_errno (
	const h5_file_t * const f
	);

void
h5_set_errno (
	h5_file_t * const f,
	const h5_err_t h5_errno
	);

h5_err_t
h5_report_errorhandler (
	const h5_file_t * const f,
	const char *fmt,
	va_list ap
	);

h5_err_t
h5_abort_errorhandler (
	const h5_file_t * const f,
	const char *fmt,
	va_list ap
	);

h5_err_t
h5_error (
	h5_file_t * const f,
	const h5_err_t error_no,
	const char *fmt,
	...
	)
#ifdef __GNUC__
	__attribute__ ((format (printf, 3, 4)))
#endif
;

void
h5_verror (
	const h5_file_t * const f,
	const char *fmt,
	va_list ap
	);

void
h5_vwarn (
	const h5_file_t * const f,
	const char *fmt,
	va_list ap
	);

void
h5_warn (
	const h5_file_t * const f,
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 2, 3)))
#endif
;

void
h5_vinfo (
	const h5_file_t * const f,
	const char *fmt,
	va_list ap
	);

void
h5_info (
	const h5_file_t * const f,
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 2, 3)))
#endif
;

void
h5_vdebug (
	const h5_file_t * const f,
	const char *fmt,
	va_list ap
	);

void
h5_debug (
	const h5_file_t * const f,
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
	const h5_file_t * const f
	);

#endif
