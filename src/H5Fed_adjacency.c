/*
  Copyright 2006-2007
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Benedikt Oswald;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Achim Gsell
  
  Warning
	This code is under development.
 
 */

#include <hdf5.h>
#include "h5_core/h5_types.h"
#include "h5_core/h5_core.h"
#include "H5Fed.h"

/******	UPWARD ADJACENCY routines *********************************************/

/*!
  \return	number of upward adjacent edges 
 */
h5_err_t
H5FedGetEdgesUpAdjacentToVertex (
	h5_file_t * const f,
	const h5_id_t local_vid,
	h5_idlist_t **list
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_edges_upadjacent_to_vertex ( f, local_vid, list );
}

h5_err_t
H5FedGetTrianglesUpAdjacentToVertex (
	h5_file_t * const f,
	const h5_id_t local_vid,
	h5_idlist_t **list
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_triangles_upadjacent_to_vertex ( f, local_vid, list );
}

h5_err_t
H5FedGetTetsUpAdjacentToVertex (
	h5_file_t * const f,
	const h5_id_t local_vid,
	h5_idlist_t **list
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_tets_upadjacent_to_vertex ( f, local_vid, list );
}

h5_err_t
H5FedGetTrianglesUpAdjacentToEdge (
	h5_file_t * const f,
	const h5_id_t local_kid,
	h5_idlist_t **list
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_triangles_upadjacent_to_edge ( f, local_kid, list );
}

h5_err_t
H5FedGetTetsUpAdjacentToEdge (
	h5_file_t * const f,
	const h5_id_t local_kid,
	h5_idlist_t **list
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_tets_upadjacent_to_edge ( f, local_kid, list );
}

h5_err_t
H5FedGetTetsUpAdjacentToTriangle (
	h5_file_t * const f,
	const h5_id_t local_did,
	h5_idlist_t **list
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_tets_upadjacent_to_triangle ( f, local_did, list );
}

h5_err_t
H5FedGetVerticesDownAdjacentToEdge (
	h5_file_t * const f,
	const h5_id_t local_kid,
	h5_idlist_t **list
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_vertices_downadjacent_to_edge ( f, local_kid, list );
}

h5_err_t
H5FedGetVerticesDownAdjacentToTriangle (
	h5_file_t * const f,
	const h5_id_t local_did,
	h5_idlist_t **list
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_vertices_downadjacent_to_triangle ( f, local_did, list );
}

h5_err_t
H5FedGetVerticesDownAdjacentToTet (
	h5_file_t * const f,
	const h5_id_t local_tid,
	h5_idlist_t **list
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_vertices_downadjacent_to_tet ( f, local_tid, list );
}

h5_err_t
H5FedGetEdgesDownAdjacentToTriangle (
	h5_file_t * const f,
	const h5_id_t local_did,
	h5_idlist_t **list
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_edges_downadjacent_to_triangle ( f, local_did, list );
}

h5_err_t
H5FedGetEdgesDownAdjacentToTet (
	h5_file_t * const f,
	const h5_id_t local_tid,
	h5_idlist_t **list
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_edges_downadjacent_to_tet ( f, local_tid, list );
}

h5_err_t
H5FedGetTrianglesDownAdjacentToTet (
	h5_file_t * const f,
	const h5_id_t local_tid,
	h5_idlist_t **list
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_triangles_downadjacent_to_tet ( f, local_tid, list );
}

h5_err_t
H5FedReleaseListOfAdjacencies (
	h5_file_t * const f,
	h5_idlist_t **list
	) {
	SET_FNAME ( f, __func__ );
	return _h5_free_idlist ( f, list );
}
