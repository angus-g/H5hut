#ifndef __H5_CORE_H
#define __H5_CORE_H

#define UNUSED_ARGUMENT(x) (void)x

#define H5_DEBUG_USER		(1<<2)
#define H5_DEBUG_API		(1<<3)
#define H5_DEBUG_CORE_API	(1<<4)
#define H5_DEBUG_PRIV_API	(1<<5)
#define H5_DEBUG_PRIV_FUNC	(1<<6)
#define H5_DEBUG_HDF5		(1<<7)
#define H5_DEBUG_MALLOC		(1<<8)
#define H5_DEBUG_CLIB		(1<<9)
#define H5_DEBUG_ALL		(-1)

extern char* h5_rfmts[];
#define __FUNC_ENTER(type)		\
	h5_call_stack_push (__func__,e_##type); \
	type ret_value = (type)H5_ERR;

#define __FUNC_ARGS0(mask)		\
	if (h5_debug_level & mask ) {		\
		h5_debug ("(void)");	\
	}

#define __FUNC_ARGS1(fmt, a1, mask)		\
	if (h5_debug_level & mask ) {		\
		h5_debug ("(" fmt ")", a1);	\
	}

#define __FUNC_ARGS2(fmt, a1, a2, mask)		\
	if (h5_debug_level & mask ) {		\
		h5_debug ("(" fmt ")", a1, a2);	\
	}

#define __FUNC_ARGS3(fmt, a1, a2, a3, mask)		\
	if (h5_debug_level & mask ) {			\
		h5_debug ("(" fmt ")", a1, a2, a3);	\
	}

#define __FUNC_ARGS4(fmt, a1, a2, a3, a4, mask)		\
	if (h5_debug_level & mask ) {			\
		h5_debug ("(" fmt ")", a1, a2, a3, a4);	\
	}

#define __FUNC_ARGS5(fmt, a1, a2, a3, a4, a5, mask)	\
	if (h5_debug_level & mask ) {			\
		h5_debug ("(" fmt ")", a1, a2, a3, a4, a5);	\
	}

#define __FUNC_ENTER0(type, mask)		\
	__FUNC_ENTER(type);			\
	__FUNC_ARGS0(mask);

#define __FUNC_ENTER1(type, fmt, a1, mask)	\
	__FUNC_ENTER(type);			\
	__FUNC_ARGS1(fmt, a1, mask);

#define __FUNC_ENTER2(type, fmt, a1, a2, mask)	\
	__FUNC_ENTER(type);			\
	__FUNC_ARGS2(fmt, a1, a2, mask);

#define __FUNC_ENTER3(type, fmt, a1, a2, a3, mask)	\
	__FUNC_ENTER(type);				\
	__FUNC_ARGS3(fmt, a1, a2, a3, mask);

#define __FUNC_ENTER4(type, fmt, a1, a2, a3, a4, mask)	\
	__FUNC_ENTER(type);				\
	__FUNC_ARGS4(fmt, a1, a2, a3, a4, mask);

#define __FUNC_ENTER5(type, fmt, a1, a2, a3, a4, a5, mask)	\
	__FUNC_ENTER(type);				\
	__FUNC_ARGS5(fmt, a1, a2, a3, a4, a5, mask);

#define __FUNC_LEAVE(expr) {			\
	ret_value = expr;			\
	goto done;				\
}

#define __FUNC_RETURN(expr, mask)		\
	ret_value = expr;			\
	goto done;				\
done:						\
	if (h5_debug_level & mask ) {			\
		char fmt[256];					\
		snprintf (fmt, sizeof(fmt), "return: %s",	\
			  h5_rfmts[h5_call_stack_get_type()]);	\
		h5_debug (fmt, ret_value);			\
	}							\
	h5_call_stack_pop();					\
	return ret_value;


#define H5_API_ENTER(type)					\
	if (!h5_initialized) {					\
		h5_initialize();				\
	}							\
	__FUNC_ENTER(type);

#define H5_API_ENTER1(type, fmt, a1)		\
	H5_API_ENTER(type);			\
	__FUNC_ARGS1(fmt, a1, H5_DEBUG_API)

#define H5_API_ENTER2(type, fmt, a1, a2)	\
	H5_API_ENTER(type);			\
	__FUNC_ARGS2(fmt, a1,a2, H5_DEBUG_API)

#define H5_API_ENTER3(type, fmt, a1, a2, a3)	\
	H5_API_ENTER(type);			\
	__FUNC_ARGS3(fmt, a1,a2,a3, H5_DEBUG_API)

#define H5_API_LEAVE(expr)		__FUNC_LEAVE(expr)
#define H5_API_RETURN(expr)		__FUNC_RETURN(expr, H5_DEBUG_API);


#define TRY( func )							\
	if ((int64_t)(ptrdiff_t)(func) <= (int64_t)H5_ERR) {		\
		ret_value = H5_ERR;					\
		goto done;						\
	}

#define TRY2( func )							\
	if ((int64_t)(ptrdiff_t)(func) <= (int64_t)H5_ERR) {		\
		ret_value = (void*) H5_ERR;				\
		goto done;						\
	}

#define TRY3( expr, ret_val )						\
	if ((int64_t)(ptrdiff_t)(expr) <= (int64_t)H5_ERR) {		\
		ret_value = ret_val;					\
		goto done;						\
	}
#define ON_ERROR

#include "h5_types.h"
#include "h5_errno.h"

#include "h5_attribs.h"
#include "h5_hdf5.h"
#include "h5_maps.h"
#include "h5_openclose.h"
#include "h5_readwrite.h"
#include "h5_syscall.h"

#include "h5u_readwrite.h"
#include "h5u_model.h"

#include "h5b_readwrite.h"
#include "h5b_model.h"
#include "h5b_attribs.h"

#include "h5t_core.h"

#include "h5_errorhandling.h"

#endif
