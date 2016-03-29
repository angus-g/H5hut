/*
  Copyright (c) 2006-2016, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5_DEBUG_H
#define __H5_DEBUG_H

#include "h5core/h5_types.h"

/**
   \addtogroup h5_debug
   @{
*/

#ifdef __cplusplus
extern "C" {
#endif

/**
  Set debug mask. The debug mask is an or'ed value of

  - \c H5_DEBUG_API:	    C-API calls
  - \c H5_DEBUG_CORE_API:   core API calls.
  - \c H5_DEBUG_PRIV_API:   private API calls
  - \c H5_DEBUG_PRIV_FUNC:  static functions
  - \c H5_DEBUG_HDF5:	    HDF5 wrapper calls
  - \c H5_DEBUG_MPI:	    MPI wrapper calls
  - \c H5_DEBUG_MALLOC:	    memory allocation
  - \c H5_DEBUG_ALL:	    enable all

  \return \c H5_SUCCESS

  \see H5GetDebugMask()

  \note 
  | Release    | Change                               |
  | :------    | :-----			  	      |
  | \c 1.99.15 | Function introduced in this release. |
*/
static inline h5_err_t
H5SetDebugMask (
	const h5_id_t mask     ///< [in] debug mask
	) {
	return h5_set_debuglevel (mask & ~0x03);
}

/**
  Get debug mask.

  \return   debug mask

  \see H5SetDebugMask()

  \note 
  | Release    | Change                               |
  | :------    | :-----			  	      |
  | \c 1.99.15 | Function introduced in this release. |
*/
static inline h5_id_t
H5GetDebugMask (
	void
	) {
	return (h5_get_debuglevel () & ~0x03);
}

#ifdef __cplusplus
}
#endif

///< @}
#endif
