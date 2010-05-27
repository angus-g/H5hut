#ifndef __H5_CORE_PRIVATE_H
#define __H5_CORE_PRIVATE_H

#include "h5_types_private.h"

#include "h5_errorhandling_private.h"
#include "h5_fcmp_private.h"
#include "h5_hdf5_private.h"
#include "h5_hsearch_private.h"
#include "h5_mpi_private.h"
#include "h5_qsort_private.h"
#include "h5_readwrite_private.h"
#include "h5_syscall_private.h"

#include "h5b_types_private.h"
#include "h5u_types_private.h"

#include "h5b_errorhandling_private.h"

#include "h5t_core_private.h"

#include "h5u_errorhandling_private.h"
#include "h5u_types_private.h"

#define H5PART_GROUPNAME_STEP	"Step"

#define H5B_CONTAINER_GRPNAME	"Block"
#define H5BLOCK_GROUPNAME_BLOCK	H5B_CONTAINER_GRPNAME


#define TRY( func )						\
	if ((int64_t)(ptrdiff_t)(func) <= (int64_t)H5_ERR)	\
		return H5_ERR;

/*!
  The functions declared here are not part of the API, but may be used
  in extensions like H5Block. We name these functions "private".

  \note
  Private function may change there interface even in stable versions.
  Don't use them in applications!
*/

h5_int64_t
h5_get_num_particles (
	h5_file_t* const f
	);

herr_t
h5_iteration_operator (
	hid_t group_id,
	const char* member_name,
	void* operator_data
	);


h5_int64_t
_H5Part_get_object_name (
	h5_file_t* const f,
	hid_t group_id,
	const char* group_name,
	const hid_t type,
	const h5_int64_t idx,
	char* obj_name,
	const h5_int64_t len_obj_name
	);


char*
_H5Part_strdupfor2c (
	const char* s,
	const ssize_t len
	);

char*
_H5Part_strc2for (
	char* const str,
	const ssize_t l_str
	);


#ifdef IPL_XT3
# define SEEK_END 2 
#endif

#endif
