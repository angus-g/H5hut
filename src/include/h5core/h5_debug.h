/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5CORE_H5_DEBUG_H
#define __H5CORE_H5_DEBUG_H

#include "h5core/h5_types.h"
#include "h5core/h5_errorhandling.h"

#ifdef __cplusplus
extern "C" {
#endif

#define H5_DEBUG_USER		(1<<2)
#define H5_DEBUG_API		(1<<3)
#define H5_DEBUG_CORE_API	(1<<4)
#define H5_DEBUG_PRIV_API	(1<<5)
#define H5_DEBUG_PRIV_FUNC	(1<<6)
#define H5_DEBUG_HDF5		(1<<7)
#define H5_DEBUG_MPI		(1<<8)
#define H5_DEBUG_MALLOC		(1<<9)
#define H5_DEBUG_CLIB		(1<<10)

#define H5_DEBUG_ALL		(-1)

extern char* h5_rfmts[];

//////////////////////////////////////////////////////////////////////////////
// function enter macro
#if defined(NDEBUG)

#define __API_ENTER(type, mask, fmt, ...)				\
	h5_call_stack_init (__func__,e_##type);				\
	type ret_value = (type)H5_ERR;

#define __FUNC_ENTER(type, mask, fmt, ...)				\
	type ret_value = (type)H5_ERR;

#else   // NDEBUG not defined

#define __API_ENTER(type, mask, fmt, ...)				\
	h5_call_stack_push (__func__,e_##type);				\
	type ret_value = (type)H5_ERR;					\
	if (h5_debug_level & mask ) {					\
		h5_debug ("(" fmt ")", __VA_ARGS__);			\
	}

#define __FUNC_ENTER(type, mask, fmt, ...)				\
	type ret_value = (type)H5_ERR;					\
	if (h5_debug_level & mask ) {					\
		h5_call_stack_push (__func__,e_##type);			\
		h5_debug ("(" fmt ")", __VA_ARGS__);			\
	}								\

#endif
//
//////////////////////////////////////////////////////////////////////////////

#define __API_LEAVE(expr) {						\
	ret_value = expr;						\
	goto done;							\
	}

#define __FUNC_LEAVE(expr) {						\
	ret_value = expr;						\
	goto done;							\
}

//////////////////////////////////////////////////////////////////////////////
// function return macro
#if defined(NDEBUG)

#define __API_RETURN(expr, mask)					\
	ret_value = expr;						\
	goto done;							\
done:									\
	h5_call_stack_reset ();						\
	return ret_value;

#define __FUNC_RETURN(expr, mask)					\
	ret_value = expr;						\
	goto done;							\
done:									\
	return ret_value;

#else  // NDEBUG not defined

#define __API_RETURN(expr, mask)					\
	ret_value = expr;						\
	goto done;							\
done:									\
	if (h5_debug_level & mask ) {					\
		char fmt[256];						\
		snprintf (fmt, sizeof(fmt), "return: %s",		\
			  h5_rfmts[h5_call_stack_get_type()]);		\
		h5_debug (fmt, ret_value);				\
	}								\
	h5_call_stack_reset ();						\
	return ret_value;

#define __FUNC_RETURN(expr, mask)					\
	ret_value = expr;						\
	goto done;							\
done:									\
	if (h5_debug_level & mask ) {					\
		char fmt[256];						\
		snprintf (fmt, sizeof(fmt), "return: %s",		\
			  h5_rfmts[h5_call_stack_get_type()]);		\
		h5_debug (fmt, ret_value);				\
		h5_call_stack_pop();					\
	}								\
	return ret_value;

#endif
//
//////////////////////////////////////////////////////////////////////////////

#define H5_API_ENTER(type, fmt, ...)					\
	if (!h5_initialized) {						\
		h5_initialize();					\
	}								\
	__API_ENTER(type, H5_DEBUG_API, fmt, __VA_ARGS__)
#define H5_API_LEAVE(expr)		__API_LEAVE(expr)
#define H5_API_RETURN(expr)		__API_RETURN(expr, H5_DEBUG_API);


#define TRY( func )							\
	if ((int64_t)(ptrdiff_t)(func) <= (int64_t)H5_ERR) {		\
		goto done;						\
	}

#ifdef __cplusplus
}
#endif

#endif
