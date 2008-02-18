#ifndef __ERRORHANDLING_H
#define __ERRORHANDLING_H

#define H5_debug	H5_print_debug
#define H5_info		H5_print_info
#define H5_warn		H5_print_warn
#define H5_error	H5_print_error

h5part_int64_t
H5_set_debuglevel (
	h5part_int64_t level
	);

h5part_int64_t
H5_get_debuglevel (
	void
	);

h5part_int64_t
H5_set_errorhandler (
	h5part_error_handler handler
	);

h5part_error_handler
H5_get_errorhandler (
	void
	);

h5part_int64_t
H5_get_errno (
	void
	);

h5part_int64_t
H5_report_errorhandler (
	const char *funcname,
	const h5part_int64_t eno,
	const char *fmt,
	...
	);

h5part_int64_t
H5_abort_errorhandler (
	const char *funcname,
	const h5part_int64_t eno,
	const char *fmt,
	...
	) ;

void
H5_vprint_error (
	const char *fmt,
	va_list ap
	);

void
H5_print_error (
	const char *fmt,
	... )
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
;

void
H5_vprint_warn (
	const char *fmt,
	va_list ap
	);

void
H5_print_warn (
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
;

void
H5_vprint_info (
	const char *fmt,
	va_list ap
	);

void
H5_print_info (
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
;

void
H5_vprint_debug (
	const char *fmt,
	va_list ap
	);

void
H5_print_debug (
	const char *fmt,
	...
	)
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
;

void
H5_set_funcname (
	const char  * const fname
	);

const char *
H5_get_funcname (
	void
	);

const char *
H5_get_objname (
	hid_t id
	);

#endif
