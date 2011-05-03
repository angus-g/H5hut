/******	RETRIEVAL routines **************************************************/
/*
  Copyright 2007-2008
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Benedikt Oswald;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Achim Gsell
  
  Warning
	This code is under development.
 
 */

#include "h5core/h5_core.h"
#include "H5Fed.h"

/*!
  Begin traverse over all entities on this compute node. 
  Initialize internal data structures.

  \remark
  Entities might be on processor boundaries! Therefore the same entity might be
  processed on several compute nodes.

  \param[in]	f	file handle
  \param[in]	codim	co-dimension of entity to traverse

  \return	H5_SUCCESS or error code
*/

h5t_iterator_p
H5FedBeginTraverseEntities (
	h5_file_t* const f,
	const int codim
	) {
	H5_API_ENTER2 (h5t_iterator_p, "f=0x%p, codim=%d", f, codim);
	h5t_iterator_p iter;
	TRY2 (iter = h5_calloc (1, sizeof (*iter)));
	TRY2 (h5t_init_leaf_iterator (f, iter, codim));
	H5_API_RETURN (iter);
}

h5t_iterator_p
H5FedBeginTraverseBoundaryFaces (
	h5_file_t* const f,
	const int codim
	) {
	H5_API_ENTER2 (h5t_iterator_p, "f=0x%p, codim=%d", f, codim);
	h5t_iterator_p iter;
	TRY2 (iter = h5_calloc (1, sizeof (*iter)));
	TRY2 (h5t_init_boundary_face_iterator (f, iter, codim));
	H5_API_RETURN (iter);
}

/*!
  Get next local entity ID.

  \param[in]		f	file handle
  \param[in/out]	iter	iterator

  \return		Local entity ID
  \return		-1, if done
  \return		error code on error
  */
h5_loc_id_t
H5FedTraverseEntities (
	h5_file_t* const f,
	h5t_iterator_t* iter
	) {
	H5_API_ENTER2 (h5_loc_id_t, "f=0x%p, iter=0x%p", f, iter);
	H5_API_RETURN (h5t_iterate_entities (f, iter));
}

/*!
  End of traversing. Release internal data structures.

  \param[in]	f	File handle

  \return	H5_SUCCESS or error code
 
 */
h5_err_t
H5FedEndTraverseEntities (
	h5_file_t* const f,
	h5t_iterator_t* iter
	) {
	H5_API_ENTER2 (h5_err_t, "f=0x%p, iter=0x%p", f, iter);
	H5_API_RETURN (h5t_release_entity_iterator (f, iter));
}


/*!
  Get coordinates of vertex given by local index

  \param[in]	f		file handle
  \param[in]	vertex_idx	local index of vertex
  \param[out]	P		3-dimensional coordinates

  \return	H5_SUCCESS or error code
  */
h5_err_t
H5FedGetVertexCoordsByIndex (
	h5_file_t* const f,
	h5_loc_idx_t vertex_index,
	h5_float64_t P[3]
	) {
	H5_API_ENTER3 (h5_err_t, 
		       "f=0x%p, vertex_index=%lld, P=0x%p",
		       f, (long long)vertex_index, P);
	H5_API_RETURN (h5t_get_vertex_coords_by_index (f, vertex_index, P));
}

h5_err_t
H5FedGetVertexCoordsByID (
	h5_file_t* const f,
	h5_loc_id_t vertex_id,
	h5_float64_t P[3]
	) {
	H5_API_ENTER3 (h5_err_t, 
		       "f=0x%p, vertex_id=%lld, P=0x%p",
		       f, (long long)vertex_id, P);
	H5_API_RETURN (h5t_get_vertex_coords_by_id (f, vertex_id, P));
}

h5_err_t
H5FedGetVertexIndicesOfEdge (
	h5_file_t* const f,
	h5_loc_id_t entity_id,
	h5_loc_idx_t* vertex_indices
	) {
	H5_API_ENTER3 (h5_err_t,
		       "f=0x%p, entity_id=%lld, vertex_indices=0x%p",
		       f, (long long)entity_id, vertex_indices);
	H5_API_RETURN (h5t_get_vertex_indices_of_edge (f, entity_id, vertex_indices));
}

h5_err_t
H5FedGetVertexIndicesOfTriangle (
	h5_file_t* const f,
	h5_loc_id_t entity_id,
	h5_loc_idx_t* vertex_indices
	) {
	H5_API_ENTER3 (h5_err_t,
		       "f=0x%p, entity_id=%lld, vertex_indices=0x%p",
		       f, (long long)entity_id, vertex_indices);
	H5_API_RETURN (h5t_get_vertex_indices_of_triangle (f, entity_id, vertex_indices));
}

h5_err_t
H5FedGetVertexIndicesOfTet (
	h5_file_t* const f,
	h5_loc_id_t entity_id,
	h5_loc_idx_t* vertex_indices
	) {
	H5_API_ENTER3 (h5_err_t,
		       "f=0x%p, entity_id=%lld, vertex_indices=0x%p",
		       f, (long long)entity_id, vertex_indices);
	H5_API_RETURN (h5t_get_vertex_indices_of_tet (f, entity_id, vertex_indices));
}

h5_err_t
H5FedGetVertexIndicesOfEntity (
	h5_file_t* const f,
	h5_loc_id_t entity_id,
	h5_loc_idx_t* vertex_indices
	) {
	H5_API_ENTER3 (h5_err_t,
		       "f=0x%p, entity_id=%lld, vertex_indices=0x%p",
		       f, (long long)entity_id, vertex_indices);
	H5_API_RETURN (h5t_get_vertex_indices_of_entity (f, entity_id, vertex_indices));
}


