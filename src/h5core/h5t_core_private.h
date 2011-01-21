#ifndef __H5T_CORE_PRIVATE_H
#define __H5T_CORE_PRIVATE_H

#define H5T_CONTAINER_GRPNAME	"Topo"

#include "h5t_tags_private.h"

#include "h5t_types_private.h"

#include "h5t_access_private.h"
#include "h5t_adjacencies_private.h"
#include "h5t_consts_private.h"
#include "h5t_hsearch_private.h"
#include "h5t_map_private.h"
#include "h5t_openclose_private.h"
#include "h5t_ref_elements_private.h"
#include "h5t_readwrite_private.h"
#include "h5t_retrieve_private.h"
#include "h5t_store_private.h"

#include "h5t_errorhandling_private.h"


#if !(defined(ULLONG_MAX))
#define ULLONG_MAX  (LLONG_MAX * 2ULL + 1ULL)
#endif

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
#define H5T_TYPE_MASK		( 7ull << (sizeof(h5_id_t)*7+4) )
#define H5T_FACE_MASK		(15ull << (sizeof(h5_id_t)*7) )

#define H5T_TYPE_VERTEX		(1)
#define H5T_TYPE_EDGE		(2)
#define H5T_TYPE_TRIANGLE	(3)
#define H5T_TYPE_TET		(4)

#define h5tpriv_set_entity_type( type, elem_idx )		\
	(							\
		((h5_id_t)(type) << (sizeof(h5_id_t)*8-4)) |	\
		((h5_id_t)(elem_idx))				\
		)
#define h5tpriv_get_entity_type( entity_id )			\
	((entity_id & H5T_TYPE_MASK) >> (sizeof(h5_id_t)*8-4))


#define h5tpriv_build_entity_id( type, face_idx, elem_idx )		\
	(							\
		((h5_id_t)(type) << (sizeof(h5_id_t)*8-4)) |	\
		((h5_id_t)(face_idx) << (sizeof(h5_id_t)*7)) |	\
		((h5_id_t)(elem_idx) & H5T_ELEM_MASK)		\
		)

#define h5tpriv_build_entity_id2( face_id, elem_idx )			\
	(							\
		((h5_id_t)(face_id) << (sizeof(h5_id_t)*7)) |	\
		((h5_id_t)(elem_idx) & H5T_ELEM_MASK)		\
		)

#define h5tpriv_build_face_id( type_id, face_idx )	\
	(type_id << 4 | face_idx)

#define h5tpriv_build_vertex_id( face_idx, elem_idx )			\
	( h5tpriv_build_entity_id ( H5T_TYPE_VERTEX, face_idx, elem_idx ) )

#define h5tpriv_build_edge_id( face_idx, elem_idx )			\
	( h5tpriv_build_entity_id ( H5T_TYPE_EDGE, face_idx, elem_idx ) )

#define h5tpriv_build_triangle_id( face_idx, elem_idx )	 \
	( h5tpriv_build_entity_id ( H5T_TYPE_TRIANGLE, face_idx, elem_idx ) )

#define h5tpriv_build_tet_id( face_idx, elem_idx )	 \
	( h5tpriv_build_entity_id ( H5T_TYPE_TET, face_idx, elem_idx ) )


#define h5tpriv_get_face_idx( entity_id )				\
	( (entity_id & H5T_FACE_MASK) >> (sizeof(h5_id_t)*7) )

#define h5tpriv_get_face_id( entity_id )				\
	( (entity_id & (H5T_TYPE_MASK|H5T_FACE_MASK)) >> (sizeof(h5_id_t)*7) )

#define h5tpriv_get_elem_idx( entity_id )	\
	( entity_id & H5T_ELEM_MASK )

#define H5T_BOUNDARY_ELEM_FLAG 1
#define H5T_BOUNDARY_FACET_FLAG 2


#endif
