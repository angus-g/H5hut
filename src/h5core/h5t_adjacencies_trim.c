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
#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

static inline h5_err_t
alloc_tv (
	h5_file_t* const f,
	const h5_loc_idx_t level_idx

	) {
	h5t_fdata_t* t = f->t;
	h5_loc_idx_t idx = (level_idx <= 0) ? 0 : t->num_vertices[level_idx-1];
	h5_loc_idx_t last = t->num_vertices[t->num_levels-1];

	h5t_adjacencies_t* adj = &t->adjacencies;
	adj->tv.size = last;
	size_t size = last * sizeof(adj->tv.v[0]);
	TRY( adj->tv.v = h5priv_alloc (f, adj->tv.v, size) );
	size = (last-idx) * sizeof(adj->tv.v[0]);
	memset (&adj->tv.v[idx], 0, size);

	return H5_SUCCESS;
}

static inline h5_err_t
release_tv (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;
	h5t_adjacencies_t* adj = &t->adjacencies;
	if (adj->tv.v == NULL) return H5_SUCCESS;

	h5_loc_idx_t idx = 0;
	h5_loc_idx_t last = t->num_vertices[t->num_levels-1];
	for (; idx < last; idx++) {
		TRY( h5priv_free_idlist_items (f, &adj->tv.v[idx]) );
	}
	TRY( h5priv_free (f, adj->tv.v) );
	adj->tv.v = NULL;
	return H5_SUCCESS;
}

/*
  compute T(V) from current level up to highest levels.
*/
static inline h5_err_t
compute_elems_of_vertices (
	h5_file_t* const f,
	const h5_id_t from_lvl
	) {
	/* expand structure */
	TRY( alloc_tv (f, from_lvl) );

	/* loop over all elements in current level */
	h5t_fdata_t *t = f->t;
	h5_loc_idx_t idx = (from_lvl <= 0) ? 0 : t->num_elems[from_lvl-1];
	h5_loc_idx_t last = t->num_elems[t->num_levels-1];
	h5_triangle_t *el = &t->loc_elems.tris[idx];
	for (;idx < last; idx++, el++) {
		int face_idx;
		int num_faces = h5tpriv_ref_elem_get_num_vertices (t);
		for (face_idx = 0; face_idx < num_faces; face_idx++) {
			h5_id_t vidx = el->vertex_indices[face_idx];
			TRY( h5priv_append_to_idlist (
				     f,
				     &t->adjacencies.tv.v[vidx],
				     h5tpriv_build_vertex_id (
					     face_idx, idx)) );
		}
	}
	return H5_SUCCESS;
}

static inline h5_err_t
release_te (
	h5_file_t* const f
	) {
#pragma unused f
	// @@@ TBD @@@
	return H5_SUCCESS;
}

/*
  Compute T(E) from current level up to highest levels.
 */
static inline h5_err_t
compute_elems_of_edges (
	h5_file_t* const f,
	const h5_id_t from_lvl
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t elem_idx = (from_lvl <= 0) ? 0 : t->num_elems[from_lvl-1];
	h5_id_t num_elems = t->num_elems[t->num_levels-1];
	h5_idlist_t *retval = NULL;
	TRY( h5tpriv_resize_te_htab (f, 4*(num_elems-elem_idx)) );
	for (;elem_idx < num_elems; elem_idx++) {
		int face_idx;
		int num_faces = h5tpriv_ref_elem_get_num_edges (t);
		for (face_idx = 0; face_idx < num_faces; face_idx++) {
			TRY ( h5tpriv_search_te2 (
				      f, face_idx, elem_idx, &retval ) );
		}
	}
	return H5_SUCCESS;
}

static inline h5_err_t
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
		h5_generic_elem_t *el = (h5_generic_elem_t*)&t->loc_elems.tris[elem_idx];
		if ( h5tpriv_elem_is_on_cur_level ( f, el ) == H5_OK ) {
			TRY ( h5priv_append_to_idlist (
				      f, children, *edge )
				);
		} else {
			h5_id_t kids[2];
			TRY ( h5tpriv_get_direct_children_of_edge (
				      f,
				      face_idx,
				      el->child_idx,
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
static inline h5_err_t
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
		h5_generic_elem_t *el = (h5_generic_elem_t*)&t->loc_elems.tris[eid];
		if ( ! h5tpriv_elem_is_on_cur_level ( f, el ) == H5_OK ) {
			refined = 1;
			h5_id_t kids[2];
			TRY ( h5tpriv_get_direct_children_of_edge (
				      f,
				      face_id,
				      el->child_idx,
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
static inline h5_err_t
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


static inline h5_err_t
get_edges_uadj_to_vertex (
	h5_file_t * const f,
	const h5_id_t entity_id,
	h5_idlist_t **list
	) {
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	
	h5t_fdata_t* t = f->t;
	h5_loc_idx_t idx;
	TRY( h5t_get_vertex_index_of_vertex (f, entity_id, &idx) );
	h5_idlist_t* tv = &t->adjacencies.tv.v[idx];

	h5_size_t i;
	h5_id_t* vertex_idp = tv->items;
	for ( i = 0; i < tv->num_items; i++, vertex_idp++ ) {
		h5_id_t elem_idx = h5tpriv_get_elem_idx ( *vertex_idp );
		h5_id_t face_idx = h5tpriv_get_face_idx ( *vertex_idp );
		h5_generic_elem_t* el = (h5_generic_elem_t*)&t->loc_elems.tris[elem_idx];

		if ( h5tpriv_elem_is_on_cur_level ( f, el ) == H5_NOK ) {
			continue;
		}
		h5_loc_idx_t edge_idx;
		edge_idx = h5tpriv_get_edge_connected_to_vertex (t->ref_elem, face_idx, 0);
		TRY ( add_edge ( f, *list, edge_idx, elem_idx ) );
		edge_idx = h5tpriv_get_edge_connected_to_vertex (t->ref_elem, face_idx, 1);
		TRY ( add_edge ( f, *list, edge_idx, elem_idx ) );
	}
	return H5_SUCCESS;
}

static inline h5_err_t
get_triangles_uadj_to_vertex (
	h5_file_t * const f,
	const h5_id_t entity_id,
	h5_idlist_t **list
	) {
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	
	h5t_fdata_t *t = f->t;
	h5_loc_idx_t idx;
	TRY( h5t_get_vertex_index_of_vertex (f, entity_id, &idx) );
	h5_idlist_t* tv = &t->adjacencies.tv.v[idx];

	h5_size_t i;
	h5_id_t *vertex_idp = tv->items;
	for ( i = 0; i < tv->num_items; i++, vertex_idp++ ) {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx ( *vertex_idp );
		h5_generic_elem_t* el = (h5_generic_elem_t*)&t->loc_elems.tris[elem_idx];

		if ( h5tpriv_elem_is_on_cur_level ( f, el ) == H5_NOK ) {
			continue;
		}
		TRY ( h5priv_search_idlist ( f, *list, elem_idx ) );
	}
	return H5_SUCCESS;
}

static inline h5_err_t
get_triangles_uadj_to_edge (
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

static inline h5_err_t
get_vertices_dadj_to_edge (
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
static inline h5_err_t
get_vertices_dadj_to_triangle (
	h5_file_t* const f,
	const h5_id_t entity_id,
	h5_idlist_t** list
	) {
	h5_idlist_t* children;
	TRY ( h5priv_alloc_idlist ( f, &children, 8 ) );

	h5_id_t elem_idx = h5tpriv_get_elem_idx ( entity_id );
	// loop over all edges of triangle
	h5_id_t face_idx;
	h5_id_t num_faces = h5tpriv_ref_elem_get_num_edges (f->t);
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

static inline h5_err_t
get_edges_dadj_to_triangle (
	h5_file_t* const f,
	const h5_id_t entity_id,
	h5_idlist_t** list
	) {
	h5_idlist_t* children;
	TRY ( h5priv_alloc_idlist (f, &children, 8) );

	h5_id_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	// loop over all edges of triangle
	h5_id_t face_idx;
	h5_id_t num_faces = h5tpriv_ref_elem_get_num_edges (f->t);
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

static inline h5_err_t
dim_error(
	h5_file_t* const f,
	const h5_int32_t dim
	) {
	return h5_error (
		f,
		H5_ERR_INVAL,
		"Illegal dimension %ld", (long)dim);
}

static inline h5_err_t
get_adjacencies_to_vertex (
	h5_file_t* const f,
	const h5_id_t entity_id,
	const h5_int32_t dim,
	h5_idlist_t** list
	) {
	switch (dim) {
	case 1:
		return get_edges_uadj_to_vertex(f, entity_id, list);
	case 2:
		return get_triangles_uadj_to_vertex(f, entity_id, list);
	default:
		return dim_error (f, dim);
	}
}

static inline h5_err_t
get_adjacencies_to_edge (
	h5_file_t* const f,
	const h5_id_t entity_id,
	const h5_int32_t dim,
	h5_idlist_t** list
	) {
	switch (dim) {
	case 0:
		return get_vertices_dadj_to_edge(f, entity_id, list);
	case 2:
		return get_triangles_uadj_to_edge(f, entity_id, list);
	default:
		return dim_error (f, dim);
	}
}

static inline h5_err_t
get_adjacencies_to_triangle (
	h5_file_t* const f,
	const h5_id_t entity_id,
	const h5_int32_t dim,
	h5_idlist_t** list
	) {
	switch (dim) {
	case 0:
		return get_vertices_dadj_to_triangle(f, entity_id, list);
	case 1:
		return get_edges_dadj_to_triangle(f, entity_id, list);
	default:
		return dim_error (f, dim);
	}
}

static h5_err_t
get_adjacencies (
	h5_file_t* const f,
	const h5_id_t entity_id,
	const h5_int32_t dim,
	h5_idlist_t** list
	) {
	h5_id_t entity_type = h5tpriv_get_entity_type (entity_id);
	switch (entity_type) {
	case H5T_ETYPE_VERTEX:
		return get_adjacencies_to_vertex (f, entity_id, dim, list);
	case H5T_ETYPE_EDGE:
		return get_adjacencies_to_edge (f, entity_id, dim, list);
	case H5T_ETYPE_TRIANGLE:
		return get_adjacencies_to_triangle (f, entity_id, dim, list);
	default:
		break;
	}
	return h5_error_internal (f, __FILE__, __func__, __LINE__);
}

static inline h5_err_t
update_internal_structs (
	h5_file_t* const f,
	const h5_id_t from_lvl
	) {
	clock_t t1 = clock();
	TRY( compute_elems_of_vertices (f, from_lvl) );
	clock_t t2 = clock();
	fprintf (stderr, "compute_elems_of_vertices(): %f\n",
		 (float)(t2-t1)/CLOCKS_PER_SEC);
	t1 = clock();
	TRY( compute_elems_of_edges (f, from_lvl ) );
	t2 = clock();
	fprintf (stderr, "compute_elems_of_edge(): %f\n",
		 (float)(t2-t1)/CLOCKS_PER_SEC);

	return H5_SUCCESS;
}

static inline h5_err_t
release_internal_structs (
	h5_file_t* const f
	) {
	TRY( release_tv (f) );
	TRY( release_te (f) );

	return H5_SUCCESS;
}

struct h5t_adjacency_methods h5tpriv_trim_adjacency_methods = {
	update_internal_structs,
	release_internal_structs,
	get_adjacencies
};

