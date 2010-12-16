#ifndef __H5_ERRORHANDLING_H
#define __H5_ERRORHANDLING_H

extern h5_int32_t h5priv_debug_level;

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

void
h5priv_vprintf (
	FILE* f,
	const char* prefix,
	const char* __funcname,
	const char* fmt,
	va_list ap
	);

const char *
h5_get_funcname (
	const h5_file_t * const f
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
	const h5_file_t* const f,
	const char* fmt,
	va_list ap
	);

/*!
  \ingroup h5_core_errorhandling

  Print a warning message to \c stderr.
*/

static inline void
h5_warn (
	const h5_file_t * const f,
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 2, 3)))
#endif
;
static inline void
h5_warn (
	const h5_file_t* const f,
	const char* fmt,
	...
	) {
	if (h5priv_debug_level >= 2) {
		va_list ap;
		va_start (ap, fmt);
		h5priv_vprintf (stderr, "W", h5_get_funcname(f), fmt, ap);
		va_end (ap);
	}
}

/*!
  \ingroup h5_core_errorhandling

  Print an informational message to \c stdout.
*/
static inline void
h5_info (
	const h5_file_t * const f,
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 2, 3)))
#endif
;
static inline void
h5_info (
	const h5_file_t* const f,
	const char* fmt,
	...
	) {
	if (h5priv_debug_level >= 3) {
		va_list ap;
		va_start (ap, fmt);
		h5priv_vprintf (stdout, "I", h5_get_funcname(f), fmt, ap);
		va_end (ap);
	}
}

/*!
  \ingroup h5_core_errorhandling

  Print a debug message to \c stdout.
*/
#if defined(HAVE__VA_ARGS__)
#define h5_debug(f, ...)						\
	if (h5priv_debug_level >= 4) {					\
		h5priv_vprintf (stdout, "D", h5_get_funcname(f), __VA_ARGS__); \
	}
#else
static inline void
h5_debug (
	const h5_file_t * const f,
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 2, 3)))
#endif
	;

static inline void
h5_debug (
	const h5_file_t * const f,
	const char *fmt,
	...
	) {
	if (h5priv_debug_level >= 4) {
		va_list ap;
		va_start (ap, fmt);
		h5priv_vprintf (stdout, "D", h5_get_funcname(f), fmt, ap);
		va_end (ap);
	}
}
#endif

void
h5_set_funcname (
	h5_file_t * const f,
	const char  * const fname
	);

#define H5_API_ENTER {						\
		h5_set_funcname( f, __func__ );			\
		h5_debug (f, "%s", " ");			\
	}							\

#define H5_API_RETURN(retval)			\
			     			\
	goto exit;				\
	exit:					\
	return retval;				\

#endif
