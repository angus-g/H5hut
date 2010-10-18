#ifndef __H5T_CORE_H
#define __H5T_CORE_H

#include <limits.h>
#include <stdint.h>

#include "h5t_adjacencies.h"
#include "h5t_inquiry.h"
#include "h5t_map.h"
#include "h5t_openclose.h"
#include "h5t_readwrite.h"
#include "h5t_ref_elements.h"
#include "h5t_retrieve.h"
#include "h5t_storemesh.h"
#include "h5t_tags.h"


#if !(defined(ULLONG_MAX))
#define ULLONG_MAX  (ULONG_MAX * 2ULL + 1ULL)
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
#define H5T_ETYPE_MASK		( 7ull << (sizeof(h5_id_t)*8-4) )
#define H5T_COMPONENT_MASK	(~H5T_ELEM_MASK)
#define H5T_COMPONENT_ID_MASK	(15ull << (sizeof(h5_id_t)*7) )

#define H5T_ETYPE_VERTEX	((h5_id_t)1)
#define H5T_ETYPE_EDGE		((h5_id_t)2)
#define H5T_ETYPE_TRIANGLE	((h5_id_t)3)
#define H5T_ETYPE_TET		((h5_id_t)4)

#define h5tpriv_set_entity_type( type, elem_idx )		\
	(							\
		((h5_id_t)(type) << (sizeof(h5_id_t)*8-4)) |	\
		((h5_id_t)(elem_idx))				\
		)
#define h5tpriv_get_entity_type( entity_id )			\
	((entity_id & H5T_ETYPE_MASK) >> (sizeof(h5_id_t)*8-4))


#define h5tpriv_build_id( type, comp_idx, elem_idx )		\
	(							\
		((h5_id_t)(type) << (sizeof(h5_id_t)*8-4)) |	\
		((h5_id_t)(comp_idx) << (sizeof(h5_id_t)*7)) |	\
		((h5_id_t)(elem_idx) & H5T_ELEM_MASK)		\
		)

#define h5tpriv_build_vertex_id( comp_idx, elem_idx )			\
	( h5tpriv_build_id ( H5T_ETYPE_VERTEX, comp_idx, elem_idx ) )

#define h5tpriv_build_edge_id( comp_idx, elem_idx )			\
	( h5tpriv_build_id ( H5T_ETYPE_EDGE, comp_idx, elem_idx ) )

#define h5tpriv_build_triangle_id( comp_idx, elem_idx )	 \
	( h5tpriv_build_id ( H5T_ETYPE_TRIANGLE, comp_idx, elem_idx ) )

#define h5tpriv_build_elem_id( elem_idx )				\
	( h5tpriv_build_id ( f->t->mesh_type, 0, elem_idx ) )

#define h5tpriv_get_face_idx( entity_id )				\
	( (entity_id & H5T_COMPONENT_ID_MASK) >> (sizeof(h5_id_t)*7) )

#define h5tpriv_get_elem_idx( entity_id )		\
	( entity_id & H5T_ELEM_MASK )

#endif
