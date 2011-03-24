#ifndef __H5_CORE_PRIVATE_H
#define __H5_CORE_PRIVATE_H

#define H5_CORE_API_ENTER(type)		__FUNC_ENTER(type)
#define H5_CORE_API_ENTER0(type)	__FUNC_ENTER(type)
#define H5_CORE_API_ENTER1(type, fmt, a1)	\
	__FUNC_ENTER1(type, fmt, a1, H5_DEBUG_CORE_API)
#define H5_CORE_API_ENTER2(type, fmt, a1, a2)		\
	__FUNC_ENTER2(type, fmt, a1, a2, H5_DEBUG_CORE_API)
#define H5_CORE_API_ENTER3(type, fmt, a1, a2, a3)		\
	__FUNC_ENTER3(type, fmt, a1, a2, a3, H5_DEBUG_CORE_API)
#define H5_CORE_API_ENTER4(type, fmt, a1, a2, a3, a4)		\
	__FUNC_ENTER4(type, fmt, a1, a2, a3, a4, H5_DEBUG_CORE_API)
#define H5_CORE_API_LEAVE(value)	__FUNC_LEAVE(value)
#define H5_CORE_API_RETURN(value)	__FUNC_RETURN(value, H5_DEBUG_CORE_API)

#define H5_PRIV_API_ENTER(type)		__FUNC_ENTER(type)
#define H5_PRIV_API_ENTER1(type, fmt, a1)	\
	__FUNC_ENTER1(type, fmt, a1, H5_DEBUG_PRIV_API)
#define H5_PRIV_API_ENTER2(type, fmt, a1, a2)		\
	__FUNC_ENTER2(type, fmt, a1, a2, H5_DEBUG_PRIV_API)
#define H5_PRIV_API_ENTER3(type, fmt, a1, a2, a3)		\
	__FUNC_ENTER3(type, fmt, a1, a2, a3, H5_DEBUG_PRIV_API)
#define H5_PRIV_API_LEAVE(value)	__FUNC_LEAVE(value)
#define H5_PRIV_API_RETURN(value)	__FUNC_RETURN(value, H5_DEBUG_PRIV_API)

#define H5_PRIV_FUNC_ENTER(type)	__FUNC_ENTER(type)
#define H5_PRIV_FUNC_ENTER1(type, fmt, a1)	\
	__FUNC_ENTER1(type, fmt, a1, H5_DEBUG_PRIV_FUNC)
#define H5_PRIV_FUNC_ENTER2(type, fmt, a1, a2)	\
	__FUNC_ENTER2(type, fmt, a1, a2, H5_DEBUG_PRIV_FUNC)
#define H5_PRIV_FUNC_ENTER3(type, fmt,  a1, a2, a3)		\
	__FUNC_ENTER3(type, fmt,a1, a2, a3, H5_DEBUG_PRIV_FUNC)
#define H5_PRIV_FUNC_ENTER4(type, fmt,  a1, a2, a3, a4)		\
	__FUNC_ENTER4(type, fmt,a1, a2, a3, a4, H5_DEBUG_PRIV_FUNC)
#define H5_PRIV_FUNC_LEAVE(value)	__FUNC_LEAVE(value)
#define H5_PRIV_FUNC_RETURN(value)	__FUNC_RETURN(value, H5_DEBUG_PRIV_FUNC)

#define HDF5_WRAPPER_ENTER(type)	__FUNC_ENTER(type)
#define HDF5_WRAPPER_ENTER0(type)		\
	__FUNC_ENTER0(type, H5_DEBUG_HDF5)
#define HDF5_WRAPPER_ENTER1(type, fmt, a1)		\
	__FUNC_ENTER1(type, fmt, a1, H5_DEBUG_HDF5)
#define HDF5_WRAPPER_ENTER2(type, fmt, a1, a2)		\
	__FUNC_ENTER2(type, fmt, a1, a2, H5_DEBUG_HDF5)
#define HDF5_WRAPPER_ENTER3(type, fmt, a1, a2, a3)		\
	__FUNC_ENTER3(type, fmt, a1, a2, a3, H5_DEBUG_HDF5)
#define HDF5_WRAPPER_ENTER4(type, fmt, a1, a2, a3, a4)		\
	__FUNC_ENTER4(type, fmt, a1, a2, a3, a4, H5_DEBUG_HDF5)
#define HDF5_WRAPPER_ENTER5(type, fmt, a1, a2, a3, a4, a5)	\
	__FUNC_ENTER5(type, fmt, a1, a2, a3, a4, a5, H5_DEBUG_HDF5)
#define HDF5_WRAPPER_LEAVE(value)	__FUNC_LEAVE(value)
#define HDF5_WRAPPER_RETURN(value)	__FUNC_RETURN(value, H5_DEBUG_HDF5)

#define MALLOC_WRAPPER_ENTER1(type, fmt, a1)		\
	__FUNC_ENTER1(type, fmt, a1, H5_DEBUG_MALLOC)
#define MALLOC_WRAPPER_ENTER2(type, fmt, a1, a2)		\
	__FUNC_ENTER2(type, fmt, a1, a2, H5_DEBUG_MALLOC)
#define MALLOC_WRAPPER_ENTER3(type, fmt, a1, a2, a3)		\
	__FUNC_ENTER3(type, fmt, a1, a2, a3, H5_DEBUG_MALLOC)
#define MALLOC_WRAPPER_LEAVE(value)	__FUNC_LEAVE(value)
#define MALLOC_WRAPPER_RETURN(value)	__FUNC_RETURN(value, H5_DEBUG_MALLOC)

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
