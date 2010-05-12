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

/*
  compute T(V) from current level up to highest levels.
*/
static h5_err_t
compute_elems_of_vertices (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t elem_idx = (t->cur_level <= 0) ?
		0 : t->num_elems[t->cur_level-1];
	h5_elem_ldta_t *el = &t->elems_ldta[elem_idx];
	h5_id_t num_elems = t->num_elems[t->num_levels-1];
	for (;elem_idx < num_elems; elem_idx++, el++) {
		int face_idx;
		int num_faces = t->ref_element->num_faces[0];
		for (face_idx = 0; face_idx < num_faces; face_idx++) {
			h5_id_t vidx = el->local_vids[face_idx];
			TRY( h5priv_append_to_idlist (
				     f,
				     &t->vertices_data[vidx].tv,
				     h5tpriv_build_vertex_id (
					     face_idx, elem_idx)) );
		}
	}
	return H5_SUCCESS;
}

/*
  Compute T(E) from current level up to highest levels.
 */
static h5_err_t
compute_elems_of_edges (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t elem_idx = (t->cur_level <= 0) ?
		0 : t->num_elems[t->cur_level-1];
	h5_elem_ldta_t *el = &t->elems_ldta[elem_idx];
	h5_id_t num_elems = t->num_elems[t->num_levels-1];
	h5_idlist_t *retval = NULL;
	TRY( h5tpriv_resize_te_htab (f, 4*(num_elems-elem_idx)) );
	for (;elem_idx < num_elems; elem_idx++, el++) {
		int face_idx;
		int num_faces = t->ref_element->num_faces[1];
		for (face_idx = 0; face_idx < num_faces; face_idx++) {
			TRY ( h5tpriv_search_te2 (
				      f, face_idx, elem_idx, &retval ) );
		}
	}
	return H5_SUCCESS;
}

static h5_err_t
rebuild_internal_structs (
	h5_file_t * const f
	) {
	clock_t t1 = clock();
	TRY ( compute_elems_of_vertices ( f ) );
	clock_t t2 = clock();
	fprintf ( stderr, "compute_elems_of_vertices(): %f\n",
		  (float)(t2-t1)/CLOCKS_PER_SEC );
	t1 = clock();
	TRY ( compute_elems_of_edges ( f ) );
	t2 = clock();
	fprintf ( stderr, "compute_elems_of_edge(): %f\n",
		  (float)(t2-t1)/CLOCKS_PER_SEC );

	return H5_SUCCESS;
}

static h5_err_t
compute_children_of_edge (
	h5_file_t * const f,
	h5_id_t kid,
	h5_idlist_t *children
	) {
	h5t_fdata_t *t = f->t;
	h5_idlist_t *te;

	TRY ( h5tpriv_find_te2 (
		      f,
		      h5tpriv_get_face_idx ( kid ),
		      h5tpriv_get_elem_idx ( kid ),
		      &te ) 
		);
	h5_id_t *edge = te->items;
	h5_id_t *end = te->items+te->num_items;
	for ( ; edge < end; edge++ ) {
		h5_id_t elem_idx = h5tpriv_get_elem_idx ( *edge );
		h5_id_t face_idx =  h5tpriv_get_face_idx ( *edge );
		h5_elem_ldta_t *el = &t->elems_ldta[elem_idx];
		if ( h5tpriv_elem_is_on_cur_level ( f, el ) == H5_OK ) {
			TRY ( h5priv_append_to_idlist (
				      f, children, *edge )
				);
		} else {
			h5_id_t kids[2];
			TRY ( t->methods.store->get_direct_children_of_edge (
				      f,
				      face_idx,
				      el->local_child_eid,
				      kids ) );
			TRY ( compute_children_of_edge (
				      f, kids[0], children ) );
			TRY ( compute_children_of_edge (
				      f, kids[1], children ) );
		}
	}
	return H5_SUCCESS;
}

/* 
   Compute all sections of an edge.
*/
static h5_err_t
compute_sections_of_edge (
	h5_file_t * const f,
	h5_id_t kid,
	h5_idlist_t *children
	) {
	h5t_fdata_t *t = f->t;
	h5_idlist_t *te;

	TRY ( h5tpriv_find_te2 (
		      f,
		      h5tpriv_get_face_idx ( kid ),
		      h5tpriv_get_elem_idx ( kid ),
		      &te )
		);
	h5_id_t *edge = te->items;
	h5_id_t *end = te->items+te->num_items;
	int refined = 0;
	for ( ; edge < end; edge++ ) {
		h5_id_t eid = h5tpriv_get_elem_idx ( *edge );
		h5_id_t face_id =  h5tpriv_get_face_idx ( *edge );
		h5_elem_ldta_t *el = &t->elems_ldta[eid];
		if ( ! h5tpriv_elem_is_on_cur_level ( f, el ) == H5_OK ) {
			refined = 1;
			h5_id_t kids[2];
			TRY ( t->methods.store->get_direct_children_of_edge (
				      f,
				      face_id,
				      el->local_child_eid,
				      kids ) );
			TRY ( compute_sections_of_edge (
				      f, kids[0], children ) );
			TRY ( compute_sections_of_edge (
				      f, kids[1], children ) );
		}
	}
	if ( ! refined ) {
		TRY ( h5priv_append_to_idlist ( f, children, te->items[0] ) );
	}
	return H5_SUCCESS;
}


/*
  map edge ID to unique ID
  if unique ID not in list: add
*/
static h5_err_t
add_edge (
	h5_file_t * const f,
	h5_idlist_t *list,
	h5_id_t face_idx,
	h5_id_t elem_idx
	) {
	h5_idlist_t *te;
	TRY ( h5tpriv_find_te2 ( f, face_idx, elem_idx, &te ) );
	TRY ( h5priv_search_idlist ( f, list, te->items[0] ) );
	return H5_SUCCESS;
}


static h5_err_t
get_edges_upadjacent_to_vertex (
	h5_file_t * const f,
	const h5_id_t entity_id,
	h5_idlist_t **list
	) {
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	
	h5t_fdata_t *t = f->t;
	h5_idlist_t *tv = &t->vertices_data[entity_id].tv;
	h5_size_t i;

	h5_id_t *vertex_idp = tv->items;
	for ( i = 0; i < tv->num_items; i++, vertex_idp++ ) {
		h5_id_t elem_idx = h5tpriv_get_elem_idx ( *vertex_idp );
		h5_id_t face_idx = h5tpriv_get_face_idx ( *vertex_idp );
		h5_elem_ldta_t *el = &t->elems_ldta[elem_idx];

		if ( h5tpriv_elem_is_on_cur_level ( f, el ) == H5_NOK ) {
			continue;
		}
		/*
		  upward adjacend edges to vertices according reference element
		 */
		int map[3][2] = { {0,1}, // edge 0, 1 are adjacent to vertex 0
				  {0,2}, // edge 0, 2 are adjacent to vertex 1
				  {1,2}  // edge 1, 2 are adjacent to vertex 2
		};
		TRY ( add_edge ( f, *list, map[face_idx][0], elem_idx ) );
		TRY ( add_edge ( f, *list, map[face_idx][1], elem_idx ) );
	}
	return H5_SUCCESS;
}

static h5_err_t
get_triangles_upadjacent_to_vertex (
	h5_file_t * const f,
	const h5_id_t entity_id,
	h5_idlist_t **list
	) {
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	
	h5t_fdata_t *t = f->t;
	h5_idlist_t *tv = &t->vertices_data[entity_id].tv;
	h5_size_t i;
	h5_id_t *vertex_id = tv->items;
	for ( i = 0; i < tv->num_items; i++, vertex_id++ ) {
		h5_id_t elem_id = h5tpriv_get_elem_idx ( *vertex_id );
		h5_elem_ldta_t *el = &t->elems_ldta[elem_id];

		if ( h5tpriv_elem_is_on_cur_level ( f, el ) == H5_NOK ) {
			continue;
		}
		TRY ( h5priv_search_idlist ( f, *list, elem_id ) );
	}
	return H5_SUCCESS;
}

static h5_err_t
get_triangles_upadjacent_to_edge (
	h5_file_t * const f,
	const h5_id_t entity_id,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( h5priv_alloc_idlist ( f, &children, 8 ) );
	TRY ( compute_children_of_edge ( f, entity_id, children ) );
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	h5_id_t *edge_id = children->items;
	h5_id_t *end = children->items+children->num_items;
	for ( ; edge_id < end; edge_id++ ) {
		h5_id_t elem_id = h5tpriv_get_elem_idx ( *edge_id );
		TRY ( h5priv_search_idlist ( f, *list, elem_id ) );
	}
	TRY ( h5priv_free_idlist( f, &children ) );

	return H5_SUCCESS;
}

static h5_err_t
get_vertices_downadjacent_to_edge (
	h5_file_t * const f,
	const h5_id_t entity_id,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( h5priv_alloc_idlist ( f, &children, 8 ) );
	TRY( compute_sections_of_edge ( f, entity_id, children ) );
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	int i;
	h5_id_t *edge_id = children->items;
	for ( i = 0; i < children->num_items; i++, edge_id++ ) {
		h5_id_t vids[2];
		TRY ( h5t_get_vertex_indices_of_edge ( f, *edge_id, vids ) );
		TRY ( h5priv_search_idlist ( f, *list, vids[0] ) );
		TRY ( h5priv_search_idlist ( f, *list, vids[1] ) );
	}
	TRY ( h5priv_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

/*
  Compute downward adjacent vertices of all edges of triangle.
 */
static h5_err_t
get_vertices_downadjacent_to_triangle (
	h5_file_t* const f,
	const h5_id_t entity_id,
	h5_idlist_t** list
	) {
	h5_idlist_t* children;
	TRY ( h5priv_alloc_idlist ( f, &children, 8 ) );

	h5_id_t elem_idx = h5tpriv_get_elem_idx ( entity_id );
	// loop over all edges of triangle
	h5_id_t face_idx;
	h5_id_t num_faces = f->t->ref_element->num_faces[1];
	for (face_idx = 0; face_idx < num_faces; face_idx++) {
		TRY( compute_sections_of_edge (
			     f,
			     h5tpriv_build_edge_id (face_idx, elem_idx),
			     children) );
	}
	TRY ( h5priv_alloc_idlist (f, list, 8) );
	h5_id_t *edge_idp = children->items;
	int i;
	for ( i = 0; i < children->num_items; i++, edge_idp++ ) {
		h5_id_t vids[2];
		TRY ( h5t_get_vertex_indices_of_edge ( f, *edge_idp, vids ) );
		TRY ( h5priv_search_idlist ( f, *list, vids[0] ) );
		TRY ( h5priv_search_idlist ( f, *list, vids[1] ) );
	}
	TRY ( h5priv_free_idlist(f, &children) );
	return H5_SUCCESS;
}

static h5_err_t
get_edges_downadjacent_to_triangle (
	h5_file_t* const f,
	const h5_id_t entity_id,
	h5_idlist_t** list
	) {
	h5_idlist_t* children;
	TRY ( h5priv_alloc_idlist (f, &children, 8) );

	h5_id_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	// loop over all edges of triangle
	h5_id_t face_idx;
	h5_id_t num_faces = f->t->ref_element->num_faces[1];
	for ( face_idx = 0; face_idx < num_faces; face_idx++ ) {
		TRY( compute_sections_of_edge (
			     f,
			     h5tpriv_build_edge_id (face_idx, elem_idx),
			     children) );
	}
	TRY ( h5priv_alloc_idlist (f, list, 8) );
	h5_id_t *edge_idp = children->items;
	int i;
	for (i = 0; i < children->num_items; i++, edge_idp++) {
		TRY ( h5priv_search_idlist (f, *list, *edge_idp) );
	}
	TRY ( h5priv_free_idlist(f, &children) );
	return H5_SUCCESS;
}


static h5_err_t
release_internal_structs (
	h5_file_t * const f
	) {
	/* TO BE WRITTEN @@@ */
	return H5_SUCCESS;
}

struct h5t_adjacency_methods h5tpriv_trim_adjacency_methods = {
	rebuild_internal_structs,
	release_internal_structs,
	get_edges_upadjacent_to_vertex,
	get_triangles_upadjacent_to_vertex,
	NULL,
	get_triangles_upadjacent_to_edge,
	NULL,
	NULL,
	get_vertices_downadjacent_to_edge,
	get_vertices_downadjacent_to_triangle,
	NULL,
	get_edges_downadjacent_to_triangle,
	NULL,
	NULL
};

