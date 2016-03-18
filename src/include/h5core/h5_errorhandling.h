/*
  Copyright (c) 2006-2016, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5CORE_H5_ERRORHANDLING_H
#define __H5CORE_H5_ERRORHANDLING_H

#include <stdarg.h>
#include <stdio.h>
#include "h5core/h5_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
   \addtogroup h5_error
   @{
*/

#define H5_SUCCESS		0               ///< Function performs successfully
#define H5_OK			H5_SUCCESS      ///< Alias for \c H5_SUCCESS
#define H5_NOK			-1              ///< 
#define H5_FAILURE 		-2              ///< Function runs into an error
#define H5_ERR			H5_FAILURE      ///< Alias for H5_FAILURE

#define H5_ERR_BADF		-9              ///< Something is wrong with the file handle.
#define H5_ERR_NOMEM		-12             ///< Out of memory.
#define H5_ERR_INVAL		-22             ///< Invalid argument.

#define H5_ERR_VIEW		-100            ///< Something is wrong with the view.
#define H5_ERR_NOENTRY		-101            ///< A lookup failed.

#define H5_ERR_MPI		-201            ///< A MPI error occured.
#define H5_ERR_HDF5		-202            ///< A HDF5 error occured.
#define H5_ERR_H5		-203            ///< Unspecified error in H5 module.
#define H5_ERR_H5PART		-204            ///< Unspecified error in H5Part module.
#define H5_ERR_H5BLOCK		-205            ///< Unspecified error in H5Block module.
#define H5_ERR_H5FED		-206            ///< Unspecified error in H5Fed module.

#define H5_ERR_INTERNAL		-253            ///< Internal error.
#define H5_ERR_NOT_IMPLEMENTED	-254            ///< Function not yet implemented.

/** @}*/

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
	e_h5_file_t,
	e_h5_lvl_idx_t,
	e_h5t_iterator_p,
	e_h5_loc_id_t,
	e_h5_loc_idx_t,
	e_hid_t,
	e_H5O_type_t,
	e_h5_glb_elem_p,
    	e_h5_prop_p,
        e_h5_prop_t,
    	e_h5_prop_file_p,
        e_h5_prop_file_t,
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
extern struct call_stack h5_call_stack;
extern h5_err_t h5_errno;

#define h5_error_not_implemented()				     \
	h5_error(						     \
		H5_ERR_NOT_IMPLEMENTED,				     \
		"%s: Function '%s', line %d not yet implemented!",   \
		__FILE__, __func__, __LINE__);

#define h5_error_internal()   \
        h5_error(                                  \
                H5_ERR_INTERNAL,                   \
                "%s: Internal error: %s line %d!", \
                __FILE__, __func__, __LINE__)


h5_err_t
h5_set_debuglevel (
	const h5_id_t);

h5_err_t
h5_get_debuglevel (
	void);

h5_err_t
h5_set_errorhandler (
	const h5_errorhandler_t);

h5_errorhandler_t
h5_get_errorhandler (
	void);

h5_err_t
h5_get_errno (
	void);

void
h5_set_errno (
	const h5_err_t);

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
