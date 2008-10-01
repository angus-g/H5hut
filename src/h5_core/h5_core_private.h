#ifndef __H5_PRIVATE_H
#define __H5_PRIVATE_H

#include "h5t_types_private.h"

#include "h5_errorhandling_private.h"
#include "h5_qsort_private.h"
#include "h5_syscall_private.h"
#include "h5b_errorhandling_private.h"
#include "h5t_boundaries_private.h"
#include "h5t_map_private.h"
#include "h5t_errorhandling_private.h"
#include "h5t_readwrite_private.h"
#include "h5t_storemesh_private.h"
#include "h5u_errorhandling_private.h"

#define H5PART_GROUPNAME_STEP	"Step"

#define H5B_CONTAINER_GRPNAME	"Block"

#define H5T_CONTAINER_GRPNAME	"Topo"

#define H5BLOCK_GROUPNAME_BLOCK	H5B_CONTAINER_GRPNAME

#define H5_TET_MASK		( (h5_id_t) (0xffffffff >> 3) )
#define _h5t_build_triangle_id( idx, entity_id ) \
	( (idx << (sizeof(entity_id)*8 - 3)) | (entity_id & H5_TET_MASK))

#if __SIZEOF_POINTER__ == 4
#define TRY(func) if ( (int32_t)(func) == (int32_t)(-1) ) return H5_ERR;
#define TRY2(func,exception) if ( (int32_t)(func) == (int32_t)(-1) ) goto exception;
#elif __SIZEOF_POINTER__ == 8
#define TRY(func)						\
	if ( (int64_t)(func) == (int64_t)(-1) )			\
		return H5_ERR;
#define TRY2(func,exception)			\
	if ( (int64_t)(func) == (int64_t)(-1) )	\
		goto exception;
#else
  #error "Unknown pointer size!"
#endif


/*!
  The functions declared here are not part of the API, but may be used
  in extensions like H5Block. We name these functions "private".

  \note
  Private function may change there interface even in stable versions.
  Don't use them in applications!
*/

struct _iter_op_data {
	int stop_idx;
	int count;
	int type;
	char *name;
	size_t len;
	char *pattern;
};

h5_int64_t
h5_set_step (
	h5_file_t *f,
	const h5_int64_t step
	);

h5_int64_t
h5_get_num_particles (
	h5_file_t *f
	);

herr_t
h5_iteration_operator (
	hid_t group_id,
	const char *member_name,
	void *operator_data
	);



#define SET_FNAME( fname )	h5_set_funcname( fname );

hid_t
h5_normalize_h5_type (
	hid_t type
	);

h5_int64_t
h5_read_attrib (
	hid_t id,
	const char *attrib_name,
	void *attrib_value
	);

h5_int64_t
h5_write_attrib (
	hid_t id,
	const char *attrib_name,
	const hid_t attrib_type,
	const void *attrib_value,
	const hsize_t attrib_nelem
	);

h5_int64_t
h5_get_attrib_info (
	hid_t id,
	const h5_int64_t attrib_idx,
	char *attrib_name,
	const h5_int64_t len_attrib_name,
	h5_int64_t *attrib_type,
	h5_int64_t *attrib_nelem
	);

h5_int64_t
h5_get_num_objects (
	hid_t group_id,
	const char *group_name,
	const hid_t type
	);

h5_int64_t
h5_get_num_objects_matching_pattern (
	hid_t group_id,
	const char *group_name,
	const hid_t type,
	char * const pattern
	);

h5_int64_t
_H5Part_get_object_name (
	hid_t group_id,
	const char *group_name,
	const hid_t type,
	const h5_int64_t idx,
	char *obj_name,
	const h5_int64_t len_obj_name
	);


char *
_H5Part_strdupfor2c (
	const char *s,
	const ssize_t len
	);

char *
_H5Part_strc2for (
	char * const str,
	const ssize_t l_str
	);


#ifdef IPL_XT3
# define SEEK_END 2 
#endif

#endif
