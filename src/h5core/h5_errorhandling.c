#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

static h5_errorhandler_t	h5_errhandler = h5_report_errorhandler;
h5_err_t			h5_errno;
h5_int32_t			h5_debug_level = 1;
int				h5_initialized = 0;
struct call_stack		h5_call_stack;
int				h5_myproc = 0;

char *h5_rfmts[] = {
	[e_int]			"%d",
	[e_ssize_t]		"%ld",
	[e_char_p]		"%s",
	[e_void_p]		"%p",
	[e_h5_err_t]		"%lld",
	[e_h5_int64_t]		"%lld",
	[e_h5_id_t]		"%lld",
	[e_h5_ssize_t]		"%lld",
	[e_h5_errorhandler_t]	"%p",
	[e_h5_file_p]		"%p",
	[e_h5t_lvl_idx_t]	"%d",
	[e_h5t_iterator_p]	"%p",
	[e_h5_loc_id_t]		"%ld",
	[e_h5_loc_idx_t]	"%ld",
	[e_hid_t]		"%ld",
	[e_H5O_type_t]		"%ld",
	[e_herr_t]		"%ld"
};

/*!
  \ingroup h5_core
  \defgroup h5_core_errorhandling
*/
const char* const H5_O_MODES[] = {
	"H5_O_RDWR",
	"H5_O_RDONLY",
	"H5_O_WRONLY",
	"H5_O_APPEND"
};

void
h5_initialize (
	void
	) {
	memset (&h5_call_stack, 0, sizeof (h5_call_stack));
	h5_initialized = 1;
#ifdef PARALLEL_IO
	if (h5priv_mpi_comm_rank (MPI_COMM_WORLD, &h5_myproc) < 0) {
		exit (42);
	}
#endif
}


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
	const h5_id_t level	/*!< debug level */
	) {
	if (level < 0) 
		h5_debug_level = (1 << 20) - 1;
	else
		h5_debug_level = level;
	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_errorhandling

  Get current debug/verbosity level.

  \return current debug level
*/
h5_err_t
h5_get_debuglevel (
	void
	) {
	return h5_debug_level;
}

/*!
  \ingroup h5_core_errorhandling

  Set own error handler.

  \return \c H5_SUCCESS
*/
h5_err_t
h5_set_errorhandler (
	const h5_errorhandler_t handler
	) {
	h5_errhandler = handler;
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
	return h5_errhandler;
}

/*!
  \ingroup h5_core_errorhandling

  Get current error number.

  \return \c H5_SUCCESS
*/
h5_err_t
h5_get_errno (
	void
	) {
	return h5_errno;
}

/*!
  \ingroup h5_core_errorhandling

  Set error number.

  \return \c H5_SUCCESS
*/
void
h5_set_errno (
	const h5_err_t errno
	) {
	h5_errno = errno;
}


/*!
  \ingroup h5_core_errorhandling

  This is the H5 default error handler.  If an error occures, the
  error message will be printed, if debug level is greater than 0.

  \return \c f->__errno
*/
h5_err_t
h5_report_errorhandler (
	const char* fmt,
	va_list ap
	) {
	if (h5_debug_level > 0) {
		h5_verror (fmt, ap);
	}
	return h5_errno;
}

/*!
  \ingroup h5_core_errorhandling

  If an error occures, the error message will be printed and the
  program exists with the error code given in \c f->__errno.
*/
h5_err_t
h5_abort_errorhandler (
	const char* fmt,
	va_list ap
	) {
	if (h5_debug_level > 0) {
		h5_verror (fmt, ap);
	}
#ifdef PARALLEL_IO
        MPI_Abort(MPI_COMM_WORLD, -(int)h5_errno);
#else
	exit (-(int)h5_errno);
#endif
	return -(int)h5_errno; // never executed, just to supress a warning
}

void
h5priv_vprintf (
	FILE* f,
	const char* prefix,
	const char* __funcname,
	const char* fmt,
	va_list ap
	) {
	char fmt2[2048];
	snprintf (fmt2, sizeof(fmt2), "[proc %d] %s: %s: %s\n", h5_myproc, prefix,
		  __funcname, fmt); 
	vfprintf (f, fmt2, ap);
}

/*!
  \ingroup h5_core_errorhandling

  Print error message via error handler.

  \return \c f->__errno
*/
h5_err_t
h5_error (
	const h5_err_t errno_,
	const char* fmt,
	...
	) {
	h5_errno = errno_;
	va_list ap;
	va_start (ap, fmt);

	(*h5_errhandler)(fmt, ap);

	va_end (ap);
	return h5_errno;
}

/*!
  \ingroup h5_core_errorhandling

  Print error message to \c stderr. For use in error handlers only.
*/
void
h5_verror (
	const char* fmt,
	va_list ap
	) {

	if (h5_debug_level < 1) return;
	h5priv_vprintf (stderr, "E", h5_call_stack.entry[0].name, fmt, ap);
}
