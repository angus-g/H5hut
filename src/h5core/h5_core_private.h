#ifndef __H5_CORE_PRIVATE_H
#define __H5_CORE_PRIVATE_H

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

#include "h5_types_private.h"

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

#define TRY( func )						\
	if ((int64_t)(ptrdiff_t)(func) <= (int64_t)H5_ERR)	\
		return H5_ERR;

#define UNUSED_ARGUMENT(x) (void)x

#ifdef IPL_XT3
# define SEEK_END 2 
#endif

#endif
