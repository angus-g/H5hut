#ifndef __H5_ERRORHANDLING_H
#define __H5_ERRORHANDLING_H

#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif

enum h5_rtypes {
	e_int = 0,
	e_ssize_t,
	e_char_p,
	e_void_p,
	e_h5_err_t,
	e_h5_int64_t,
	e_h5_id_t,
	e_h5_ssize_t,
	e_h5_errorhandler_t,
	e_h5_file_p,
	e_h5t_lvl_idx_t,
	e_h5t_iterator_p,
	e_h5_loc_id_t,
	e_h5_loc_idx_t,
	e_hid_t,
	e_H5O_type_t,
	e_herr_t
};

struct call_stack_entry {
	char* name;
	enum h5_rtypes type;
};

struct call_stack {
	int level;
	struct call_stack_entry entry[1024];
};

extern h5_int32_t h5_debug_level;
extern int h5_initialized;
extern struct call_stack h5_call_stack;
extern h5_err_t	h5_errno;

void
h5_initialize (
	void
	);

#define CHECK_FILEHANDLE( f )					\
	if ( h5_check_filehandle ( f ) != H5_SUCCESS )		\
		return h5_get_errno();


#define CHECK_WRITABLE_MODE( f )					\
	if ( f->mode==H5_O_RDONLY )					\
		return h5_error (					\
			H5_ERR_INVAL,					\
			"Attempting to write to read-only file" );

#define CHECK_READONLY_MODE( f )					\
	if ( ! f->mode==H5_O_RDONLY )					\
		return h5_error (					\
			H5_ERR_INVAL,					\
			"Operation is not allowed on writable files." );

#define CHECK_TIMEGROUP( f )						\
	if ( f->step_gid <= 0 )						\
		return h5_error (					\
			H5_ERR_INVAL,					\
			"Time step is invalid! Have you set the time step?");

#define h5_error_not_implemented()				     \
	h5_error(						     \
		H5_ERR_NOT_IMPLEMENTED,				     \
		"%s: Function \"%s\", line %d not yet implemented!", \
		__FILE__, __func__, __LINE__);

#define h5_error_internal()   \
	h5_error(				   \
		H5_ERR_INTERNAL,		   \
		"%s: Internal error: %s line %d!", \
		__FILE__, __func__, __LINE__)


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
	void
	);

void
h5_set_errno (
	const h5_err_t h5_errno
	);

static inline void
h5_call_stack_init (
	const char* fname,
	enum h5_rtypes type
	) {
	h5_call_stack.level = 0;
	h5_call_stack.entry[0].name = (char *)fname;
	h5_call_stack.entry[0].type = type;
}

static inline void
h5_call_stack_push (
	const char* fname,
	enum h5_rtypes type
	) {
	h5_call_stack.entry[h5_call_stack.level].name = (char *)fname;
	h5_call_stack.entry[h5_call_stack.level].type = type;
	h5_call_stack.level++;
}

static inline const char*
h5_call_stack_pop (
	void
	) {
	return h5_call_stack.entry[--h5_call_stack.level].name;
}

static inline const char*
h5_call_stack_get_name (
	void
	) {
	return h5_call_stack.entry[h5_call_stack.level-1].name;
}

static inline const char*
h5_get_funcname (
	void
	) {
	return h5_call_stack.entry[0].name;
}

static inline enum h5_rtypes
h5_call_stack_get_type (
	void
	) {
	return h5_call_stack.entry[h5_call_stack.level-1].type;
}

static inline int
h5_call_stack_get_level (
	void
	) {
	return h5_call_stack.level;
}

static inline const char*
h5_call_stack_reset (
	void
	) {
	h5_call_stack.level = 0;
	return h5_call_stack.entry[0].name;
}

h5_err_t
h5_report_errorhandler (
	const char *fmt,
	va_list ap
	);

h5_err_t
h5_abort_errorhandler (
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

h5_err_t
h5_error (
	const h5_err_t error_no,
	const char *fmt,
	...
	)
#ifdef __GNUC__
	__attribute__ ((format (printf, 2, 3)))
#endif
;

void
h5_verror (
	const char* fmt,
	va_list ap
	);

/*!
  \ingroup h5_core_errorhandling

  Print a warning message to \c stderr.
*/

static inline h5_err_t
h5_warn (
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
;
static inline h5_err_t
h5_warn (
	const char* fmt,
	...
	) {
	if (h5_debug_level >= 2) {
		va_list ap;
		va_start (ap, fmt);
		h5priv_vprintf (stderr, "W", h5_get_funcname(), fmt, ap);
		va_end (ap);
	}
	return H5_NOK;
}

/*!
  \ingroup h5_core_errorhandling

  Print an informational message to \c stdout.
*/
static inline void
h5_info (
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
;
static inline void
h5_info (
	const char* fmt,
	...
	) {
	if (h5_debug_level >= 3) {
		va_list ap;
		va_start (ap, fmt);
		h5priv_vprintf (stdout, "I", h5_get_funcname(), fmt, ap);
		va_end (ap);
	}
}

/*!
  \ingroup h5_core_errorhandling

  Print a debug message to \c stdout.
*/
static inline void
h5_debug (
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
	;

static inline void
h5_debug (
	const char *fmt,
	...
	) {
	if (h5_debug_level >= 4) {
		char prefix[1024];
		snprintf (prefix, sizeof(prefix), "%*s %s",
			  h5_call_stack_get_level(), "",
			  h5_call_stack_get_name());
		va_list ap;
		va_start (ap, fmt);
		h5priv_vprintf (stdout, "D", prefix, fmt, ap);
		va_end (ap);
	}
}

#ifdef __cplusplus
}
#endif
	
#endif
