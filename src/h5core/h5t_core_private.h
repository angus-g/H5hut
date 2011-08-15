#ifndef __H5T_CORE_PRIVATE_H
#define __H5T_CORE_PRIVATE_H

#define H5T_CONTAINER_GRPNAME	"Topo"
#define TETRAHEDRAL_MESHES_GRPNAME "TetMeshes"
#define TRIANGLE_MESHES_GRPNAME "TriangleMeshes"

#include "h5t_tags_private.h"

#include "h5t_types_private.h"

#include "h5t_access_private.h"
#include "h5t_adjacencies_private.h"
#include "h5t_hsearch_private.h"
#include "h5t_map_private.h"
#include "h5t_model_private.h"
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

#define BITS_OF(x)		(sizeof(x)*CHAR_BIT)

#define H5T_TYPE_VERTEX		(1<<4)
#define H5T_TYPE_EDGE		(2<<4)
#define H5T_TYPE_TRIANGLE	(3<<4)
#define H5T_TYPE_TET		(4<<4)

#define H5T_FACE_MASK		(0x0f)
#define H5T_TYPE_MASK		(0x70)

#if 0
enum elem_types {
	vertex = 1,	// 1 vertex
	edge,		// 2 vertices
	triangle,	// 3 vertices
	quadrangle,	// 4 vertices
	tetrahedron,	// 4 vertices
	pyramid,	// 5 vertices
	prism,		// 6 vertices
	hexahedron	// 8 vertices
};
#endif

#define h5tpriv_build_entity_id( type, face_idx, elem_idx )	\
	(((type) | (face_idx)) << (BITS_OF(elem_idx)-8) | (elem_idx))

#define h5tpriv_build_vertex_id( face_idx, elem_idx )			\
	(h5tpriv_build_entity_id (H5T_TYPE_VERTEX, face_idx, elem_idx))

#define h5tpriv_build_edge_id( face_idx, elem_idx )			\
	(h5tpriv_build_entity_id (H5T_TYPE_EDGE, face_idx, elem_idx))

#define h5tpriv_build_triangle_id( face_idx, elem_idx )			\
	(h5tpriv_build_entity_id (H5T_TYPE_TRIANGLE, face_idx, elem_idx))

#define h5tpriv_build_tet_id( face_idx, elem_idx )			\
	(h5tpriv_build_entity_id (H5T_TYPE_TET, face_idx, elem_idx))

#define h5tpriv_get_entity_type( entity_id )	\
	((entity_id >> (BITS_OF(entity_id)-8)) & H5T_TYPE_MASK)

#define h5tpriv_get_face_idx( entity_id )				\
	(((entity_id) >> (BITS_OF(entity_id)-8)) & H5T_FACE_MASK)

#define h5tpriv_get_face_id( entity_id )				\
	(((entity_id) >> (BITS_OF(entity_id)-8)) & (H5T_TYPE_MASK|H5T_FACE_MASK))

#define h5tpriv_get_elem_idx( entity_id )			\
	(((entity_id) << 8) >> 8)

#define H5T_BOUNDARY_ELEM_FLAG 1
#define H5T_BOUNDARY_FACET_FLAG 2

#endif
