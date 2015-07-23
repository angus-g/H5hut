/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5_DEBUG_PRIVATE_H
#define __H5_DEBUG_PRIVATE_H

#include "h5core/h5_debug.h"

#define H5_CORE_API_ENTER(type, fmt, ...)				\
	__FUNC_ENTER(type, H5_DEBUG_CORE_API, fmt, __VA_ARGS__)
#define H5_CORE_API_LEAVE(value)	__FUNC_LEAVE(value)
#define H5_CORE_API_RETURN(value)	__FUNC_RETURN(value, H5_DEBUG_CORE_API)


#define H5_PRIV_API_ENTER(type, fmt, ...)				\
	__FUNC_ENTER(type, H5_DEBUG_PRIV_API, fmt, __VA_ARGS__)
#define H5_PRIV_API_LEAVE(value)	__FUNC_LEAVE(value)
#define H5_PRIV_API_RETURN(value)	__FUNC_RETURN(value, H5_DEBUG_PRIV_API)


#define H5_PRIV_FUNC_ENTER(type, fmt, ...)				\
	__FUNC_ENTER(type, H5_DEBUG_PRIV_FUNC, fmt, __VA_ARGS__ )
#define H5_PRIV_FUNC_LEAVE(value)	__FUNC_LEAVE(value)
#define H5_PRIV_FUNC_RETURN(value)	__FUNC_RETURN(value, H5_DEBUG_PRIV_FUNC)


#define HDF5_WRAPPER_ENTER(type, fmt, ...)				\
	__FUNC_ENTER(type, H5_DEBUG_HDF5, fmt, __VA_ARGS__ )
#define HDF5_WRAPPER_LEAVE(value)	__FUNC_LEAVE(value)
#define HDF5_WRAPPER_RETURN(value)	__FUNC_RETURN(value, H5_DEBUG_HDF5)


#define MPI_WRAPPER_ENTER(type, fmt, ...)				\
	__FUNC_ENTER(type, H5_DEBUG_MPI, fmt, __VA_ARGS__ )
#define MPI_WRAPPER_LEAVE(value)	__FUNC_LEAVE(value)
#define MPI_WRAPPER_RETURN(value)	__FUNC_RETURN(value, H5_DEBUG_MPI)

#define H5_INLINE_FUNC_ENTER(type)	type ret_value = (type)H5_ERR;
#define H5_INLINE_FUNC_LEAVE(expr)	__FUNC_LEAVE(expr)
#define H5_INLINE_FUNC_RETURN(expr)	__FUNC_RETURN(expr, 0)

#endif
