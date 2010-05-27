/*
  Copyright 2006-2010
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Achim Gsell
  
  Warning
	This code is under development.
*/

#include <time.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

h5_err_t
h5t_get_edges_upadjacent_to_vertex (
	h5_file_t* const f,
	const h5_id_t local_vid,
	h5_idlist_t** list
	) {
	return (*f->t->methods.adjacency->get_edges_upadjacent_to_vertex)(
		f, local_vid, list);
}

h5_err_t
h5t_get_triangles_upadjacent_to_vertex (
	h5_file_t* const f,
	const h5_id_t local_id,
	h5_idlist_t** list
	) {
	return (*f->t->methods.adjacency->get_triangles_upadjacent_to_vertex)(
		f, local_id, list);
}

h5_err_t
h5t_get_tets_upadjacent_to_vertex (
	h5_file_t* const f,
	const h5_id_t local_id,
	h5_idlist_t** list
	) {
	return (*f->t->methods.adjacency->get_tets_upadjacent_to_vertex)(
		f, local_id, list);
}

h5_err_t
h5t_get_triangles_upadjacent_to_edge (
	h5_file_t* const f,
	const h5_id_t local_id,
	h5_idlist_t** list
	) {
	return (*f->t->methods.adjacency->get_triangles_upadjacent_to_edge)(
		f, local_id, list);
}

h5_err_t
h5t_get_tets_upadjacent_to_edge (
	h5_file_t* const f,
	const h5_id_t local_id,
	h5_idlist_t** list
	) {
	return (*f->t->methods.adjacency->get_tets_upadjacent_to_edge)(
		f, local_id, list);
}

h5_err_t
h5t_get_tets_upadjacent_to_triangle (
	h5_file_t* const f,
	const h5_id_t local_id,
	h5_idlist_t** list
	) {
	return (*f->t->methods.adjacency->get_tets_upadjacent_to_triangle)(
		f, local_id, list);
}

h5_err_t
h5t_get_vertices_downadjacent_to_edge (
	h5_file_t* const f,
	const h5_id_t local_id,
	h5_idlist_t** list
	) {
	return (*f->t->methods.adjacency->get_vertices_downadjacent_to_edge)(
		f, local_id, list);
}

h5_err_t
h5t_get_vertices_downadjacent_to_triangle (
	h5_file_t* const f,
	const h5_id_t local_id,
	h5_idlist_t** list
	) {
	return (*f->t->methods.adjacency->get_vertices_downadjacent_to_triangle)(
		f, local_id, list);
}

h5_err_t
h5t_get_vertices_downadjacent_to_tet (
	h5_file_t* const f,
	const h5_id_t local_id,
	h5_idlist_t** list
	) {
	return (*f->t->methods.adjacency->get_vertices_downadjacent_to_tet)(
		f, local_id, list);
}

h5_err_t
h5t_get_edges_downadjacent_to_triangle (
	h5_file_t* const f,
	const h5_id_t local_id,
	h5_idlist_t** list
	) {
	return (*f->t->methods.adjacency->get_edges_downadjacent_to_triangle)(
		f, local_id, list);
}

h5_err_t
h5t_get_edges_downadjacent_to_tet (
	h5_file_t* const f,
	const h5_id_t local_id,
	h5_idlist_t** list
	) {
	return (*f->t->methods.adjacency->get_edges_downadjacent_to_tet)(
		f, local_id, list);
}

h5_err_t
h5t_get_triangles_downadjacent_to_tet (
	h5_file_t* const f,
	const h5_id_t local_id,
	h5_idlist_t** list
	) {
	return (*f->t->methods.adjacency->get_triangles_downadjacent_to_tet)(
		f, local_id, list);
}

h5_err_t
h5t_release_list_of_adjacencies (
	h5_file_t* const f,
	h5_idlist_t** list
	) {
	TRY( h5priv_free_idlist (f, list) );
	return H5_SUCCESS;
}


