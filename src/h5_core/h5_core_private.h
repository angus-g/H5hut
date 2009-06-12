#ifndef __H5_CORE_PRIVATE_H
#define __H5_CORE_PRIVATE_H

#include "h5_types_private.h"
#include "h5b_types_private.h"
#include "h5t_types_private.h"
#include "h5u_types_private.h"

#include "h5_errorhandling_private.h"
#include "h5_fcmp_private.h"
#include "h5_hdf5_private.h"
#include "h5_mpi_private.h"
#include "h5_qsort_private.h"
#include "h5_readwrite_private.h"
#include "h5_syscall_private.h"

#include "h5b_errorhandling_private.h"

#include "h5t_adjacencies_private.h"
#include "h5t_boundaries_private.h"
#include "h5t_consts_private.h"
#include "h5t_errorhandling_private.h"
#include "h5t_map_private.h"
#include "h5t_readwrite_private.h"
#include "h5t_retrieve_private.h"
#include "h5t_storemesh_private.h"

#include "h5u_errorhandling_private.h"
#include "h5u_types_private.h"

#define H5PART_GROUPNAME_STEP	"Step"

#define H5B_CONTAINER_GRPNAME	"Block"

#define H5T_CONTAINER_GRPNAME	"Topo"

#define H5BLOCK_GROUPNAME_BLOCK	H5B_CONTAINER_GRPNAME

/*
 ID's: 64bit

 Vertices:
   000100vv tttttttt tttttttt tttttttt tttttttt tttttttt tttttttt tttttttt
   3V TT TT TT TT TT TT

 Edges:
   00100eee tttttttt tttttttt tttttttt tttttttt tttttttt tttttttt tttttttt
   2E TT TT TT TT TT TT

 Trinagles:
   001100dd tttttttt tttttttt tttttttt tttttttt tttttttt tttttttt tttttttt
   1D TT TT TT TT TT TT

 Tets:
   01000000 tttttttt tttttttt tttttttt tttttttt tttttttt tttttttt tttttttt
   00 TT TT TT TT TT TT

*/
#define H5T_ELEM_MASK		( (h5_id_t) (ULLONG_MAX >> 8) )
#define H5T_ELEM_TYPE_MASK	( 7ull << (sizeof(h5_id_t)*8-4) )
#define H5T_FACE_MASK		(15ull << (sizeof(h5_id_t)*7) )

#define H5T_ELEM_TYPE_VERTEX	((h5_id_t)1 << (sizeof(h5_id_t)*8-4))
#define H5T_ELEM_TYPE_EDGE	((h5_id_t)2 << (sizeof(h5_id_t)*8-4))
#define H5T_ELEM_TYPE_TRIANGLE	((h5_id_t)3 << (sizeof(h5_id_t)*8-4))
#define H5T_ELEM_TYPE_TET	((h5_id_t)4 << (sizeof(h5_id_t)*8-4))


#define _h5t_build_vertex_id( face_id, elem_id )	 \
	( H5T_ELEM_TYPE_VERTEX |			 \
	  ((h5_id_t)(face_id) << (sizeof(h5_id_t)*7)) |	 \
	  ((elem_id) & H5T_ELEM_MASK))

#define _h5t_build_edge_id( face_id, elem_id )		 \
	( H5T_ELEM_TYPE_EDGE |				 \
	  ((h5_id_t)(face_id) << (sizeof(h5_id_t)*7)) |	 \
	  ((elem_id) & H5T_ELEM_MASK))

#define _h5t_build_triangle_id( face_id, elem_id )	 \
	( H5T_ELEM_TYPE_TRIANGLE |			 \
	  ((h5_id_t)(face_id) << (sizeof(h5_id_t)*7)) |	 \
	  ((elem_id) & H5T_ELEM_MASK))

#define _h5t_get_entity_type( entity_id )			\
	( entity_id & H5T_ELEM_TYPE_MASK ) 

#define _h5t_get_face_id( entity_id )				\
	( (entity_id & H5T_FACE_MASK) >> (sizeof(h5_id_t)*7) )

#define _h5t_get_elem_id( entity_id )		\
	(  entity_id & H5T_ELEM_MASK )

#define TRY( func )					\
	if ( (int64_t)(ptrdiff_t)(func) < (int64_t)0 )	\
		return H5_ERR;

/*!
  The functions declared here are not part of the API, but may be used
  in extensions like H5Block. We name these functions "private".

  \note
  Private function may change there interface even in stable versions.
  Don't use them in applications!
*/


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


h5_int64_t
_H5Part_get_object_name (
	h5_file_t * const f,
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
