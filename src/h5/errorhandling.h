#ifndef __ERRORHANDLING_H
#define __ERRORHANDLING_H

#define h5_debug	h5_print_debug
#define h5_info		h5_print_info
#define h5_warn		h5_print_warn
#define h5_error	h5_print_error

h5part_int64_t
h5_set_debuglevel (
	h5part_int64_t level
	);

h5part_int64_t
h5_get_debuglevel (
	void
	);

h5part_int64_t
h5_set_errorhandler (
	h5part_error_handler handler
	);

h5part_error_handler
h5_get_errorhandler (
	void
	);

h5part_int64_t
h5_get_errno (
	void
	);

h5part_int64_t
h5_report_errorhandler (
	const char *funcname,
	const h5part_int64_t eno,
	const char *fmt,
	...
	);

h5part_int64_t
h5_abort_errorhandler (
	const char *funcname,
	const h5part_int64_t eno,
	const char *fmt,
	...
	) ;

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
