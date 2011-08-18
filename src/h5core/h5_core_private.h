#ifndef __H5_CORE_PRIVATE_H
#define __H5_CORE_PRIVATE_H

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

/* WARNING! Changing these values will alter the data model and introduce
 * file incompatibilities with previous versions. */
#define H5_DATANAME_LEN		64
#define H5_STEPNAME_LEN		64
#define H5_STEPNAME		"Step"
#define H5_STEPWIDTH		1
#define H5_BLOCKNAME		"Block"
#define H5_BLOCKNAME_X		"0"
#define H5_BLOCKNAME_Y		"1"
#define H5_BLOCKNAME_Z		"2"
#define H5_ATTACHMENT		"Attachment"

#include "h5_va_macros.h"
#include "h5_types_private.h"

#include "h5_attribs_private.h"
#include "h5_errorhandling_private.h"
#include "h5_fcmp_private.h"
#include "h5_hdf5_private.h"
#include "h5_hsearch_private.h"
#include "h5_maps_private.h"
#include "h5_mpi_private.h"
#include "h5_qsort_private.h"
#include "h5_readwrite_private.h"
#include "h5_syscall_private.h"
#ifdef H5_USE_LUSTRE
#include "h5_lustre_private.h"
#endif

#include "h5b_types_private.h"
#include "h5u_types_private.h"

#include "h5b_errorhandling_private.h"
#include "h5b_model_private.h"

#include "h5t_core_private.h"

#include "h5u_errorhandling_private.h"
#include "h5u_types_private.h"


#ifdef IPL_XT3
# define SEEK_END 2 
#endif

#endif
