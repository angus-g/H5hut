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
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;
	h5_loc_idx_t num_vertices = t->num_vertices[t->num_levels-1];

	h5t_adjacencies_t* adj = &t->adjacencies;
	// allocate one ID list per vertex
	TRY( adj->tv.v = h5priv_calloc (f, num_vertices, sizeof(*adj->tv.v)) );

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
	const h5t_lvl_idx_t from_lvl
	) {
	/* expand structure */
	TRY( alloc_tv (f) );

	/* loop over all elements in current level */
	h5t_fdata_t* t = f->t;
	h5_loc_idx_t idx = (from_lvl <= 0) ? 0 : t->num_elems[from_lvl-1];
	h5_loc_idx_t last = t->num_elems[t->num_levels-1];
	h5_loc_tet_t *el = &t->loc_elems.tets[idx];
	for (;idx < last; idx++, el++) {
		int face_idx;
		int num_faces = h5tpriv_ref_elem_get_num_vertices(t);
		for (face_idx = 0; face_idx < num_faces; face_idx++) {
			h5_loc_idx_t vidx = el->vertex_indices[face_idx];
			TRY( h5priv_append_to_idlist (
				     f,
				     &t->adjacencies.tv.v[vidx],
				     h5tpriv_build_vertex_id (face_idx, idx)) );
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
	const h5t_lvl_idx_t from_lvl
	) {
	h5t_fdata_t *t = f->t;
	h5_loc_idx_t elem_idx = (from_lvl <= 0) ? 0 : t->num_elems[from_lvl-1];
	h5_loc_idx_t num_elems = t->num_elems[t->num_levels-1];
	h5_idlist_t *retval = NULL;
	TRY( h5tpriv_resize_te_htab (f, 4*(num_elems-elem_idx)) );
	for (;elem_idx < num_elems; elem_idx++) {
		int face_idx;
		int num_faces = h5tpriv_ref_elem_get_num_edges(t);
		for (face_idx = 0; face_idx < num_faces; face_idx++) {
			TRY( h5tpriv_search_te2 (
				     f, face_idx, elem_idx, &retval) );
		}
	}
	return H5_SUCCESS;
}

static inline h5_err_t
release_td (
	h5_file_t* const f
	) {
#pragma unused f
	// @@@ TBD @@@
	return H5_SUCCESS;
}

static inline h5_err_t
compute_elems_of_triangles (
	h5_file_t* const f,
	const h5t_lvl_idx_t from_lvl
	) {
	h5t_fdata_t* t = f->t;
	h5_loc_idx_t elem_idx = (from_lvl <= 0) ? 0 : t->num_elems[from_lvl-1];
	h5_loc_idx_t num_elems = t->num_elems[t->num_levels-1];
	h5_idlist_t *retval = NULL;
	TRY( h5tpriv_resize_td_htab (f, 4*(num_elems-elem_idx)) );
	for (;elem_idx < num_elems; elem_idx++) {
		int face_idx;
		int num_faces = h5tpriv_ref_elem_get_num_edges (t);
		for (face_idx = 0; face_idx < num_faces; face_idx++) {
			TRY( h5tpriv_search_td2 (
				     f, face_idx, elem_idx, &retval) );
		}
	}
	return H5_SUCCESS;
}

static inline h5_err_t
compute_children_of_edge (
	h5_file_t* const f,
	h5_loc_id_t kid,
	h5_idlist_t* children
	) {
	h5t_fdata_t* t = f->t;
	h5_idlist_t* te;

	TRY( h5tpriv_find_te2 (
		     f,
		     h5tpriv_get_face_idx (kid),
		     h5tpriv_get_elem_idx (kid),
		     &te ) 
		);
	h5_loc_id_t* edge = te->items;
	h5_loc_id_t* end = te->items+te->num_items;
	for (; edge < end; edge++) {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (*edge);
		h5_loc_idx_t face_idx =  h5tpriv_get_face_idx (*edge);
		h5_loc_tet_t* tet = &t->loc_elems.tets[elem_idx];
		if (h5tpriv_elem_is_on_cur_level (f, (h5_generic_loc_elem_t*)tet) == H5_OK ) {
			TRY( h5priv_append_to_idlist (f, children, *edge) );
		} else {
			h5_loc_id_t kids[2];
			TRY( h5tpriv_get_direct_children_of_edge (
				      f,
				      face_idx,
				      tet->child_idx,
				      kids) );
			TRY( compute_children_of_edge (f, kids[0], children) );
			TRY( compute_children_of_edge (f, kids[1], children) );
		}
	}
	return H5_SUCCESS;
}

/* 
   Compute all sections of an edge.
*/
static inline h5_err_t
compute_sections_of_edge (
	h5_file_t* const f,
	h5_loc_id_t kid,
	h5_idlist_t* children
	) {
	h5t_fdata_t* t = f->t;
	h5_idlist_t* te;

	TRY( h5tpriv_find_te2 (
		     f,
		     h5tpriv_get_face_idx (kid),
		     h5tpriv_get_elem_idx (kid),
		     &te ) );
	h5_loc_id_t* edge = te->items;
	h5_loc_id_t* end = te->items+te->num_items;
	int refined = 0;
	for (; edge < end; edge++) {
		h5_loc_idx_t eid = h5tpriv_get_elem_idx (*edge);
		h5_loc_idx_t face_id =  h5tpriv_get_face_idx (*edge);
		h5_generic_loc_elem_t* tet = (h5_generic_loc_elem_t*)&t->loc_elems.tets[eid];
		if (! h5tpriv_elem_is_on_cur_level (f, tet) == H5_OK) {
			refined = 1;
			h5_loc_id_t kids[2];
			TRY( h5tpriv_get_direct_children_of_edge (
				     f,
				     face_id,
				     tet->child_idx,
				     kids) );
			TRY( compute_sections_of_edge (f, kids[0], children) );
			TRY( compute_sections_of_edge (f, kids[1], children) );
		}
	}
	if (! refined) {
		TRY( h5priv_append_to_idlist (f, children, te->items[0]) );
	}
	return H5_SUCCESS;
}

/*
  Compute direct children of a triangle.

  Face 0	      Face 1		  Face 2	      Face 3	    
  1		      1	      	          2		      1	    
   +		       +	      	   +		       +	    
   |\		       |\	      	   |\		       |\	    
   | \		       | \	      	   | \		       | \	    
   |  \		       |  \	      	   |  \		       |  \	    
   |   \	       |   \	      	   |   \	       |   \	    
  4+----+7	      4+----+8     	  5+----+9	      7+----+8     
   |\   |\	       |\   |\     	   |\   |\	       |\   |\     
   | \  | \	       | \  | \    	   | \  | \	       | \  | \    
   |  \ |  \	       |  \ |  \   	   |  \ |  \	       |  \ |  \   
   |   \|   \	       |   \|   \  	   |   \|   \	       |   \|   \  
   +----+----+	       +----+----+ 	   +----+----+	       +----+----+ 
  0     5     2	      0     6     3	  0     6     3	      2     9     3

  z ^                 z ^                 y ^                 z ^
    |                   |                   |                   |
    +-->                +-->                +-->                +-->
       y                    x                  x                   x

  Triangle: face idx, #child

  [0,4,5]: 0, 0       [0,4,6]: 1, 0       [0,5,6]: 2, 0       [1,7,8]: 3, 1
  [1,4,7]: 0, 1       [1,4,8]: 1, 1       [2,5,9]: 2, 2       [2,7,9]: 3, 2
  [2,5,7]: 0, 2       [3,6,8]: 1, 3       [3,6,9]: 2, 3       [3,8,9]: 3, 3
  [4,5,7]: 0, 5       [4,6,8]: 2, 4       [5,6,9]: 1, 7       [7,8,9]: 3, 6

*/
static inline h5_err_t
compute_direct_children_of_triangle (
	h5_file_t* const f,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx,
	h5_loc_id_t children[4]
	) {
	h5_loc_idx_t map[4][4][2] = {
		{{0,0},{0,1},{0,2},{0,5}},
		{{1,0},{1,1},{1,3},{2,4}},
		{{2,0},{2,2},{2,3},{1,7}},
		{{3,1},{3,2},{3,3},{3,6}}
	};
	int num_faces = h5tpriv_ref_elem_get_num_facets (f->t);
	if ((face_idx < 0) || (face_idx >= num_faces)) {
		return h5_error_internal (f, __FILE__, __func__, __LINE__); 
	}
	children[0] = h5tpriv_build_edge_id (
		map[face_idx][0][0], elem_idx+map[face_idx][0][1]);
	children[1] = h5tpriv_build_edge_id (
		map[face_idx][1][0], elem_idx+map[face_idx][1][1]);
	children[2] = h5tpriv_build_edge_id (
		map[face_idx][2][0], elem_idx+map[face_idx][2][1]);
	children[3] = h5tpriv_build_edge_id (
		map[face_idx][3][0], elem_idx+map[face_idx][3][1]);
	return H5_SUCCESS;
}

static inline h5_err_t
compute_children_of_triangle (
	h5_file_t* const f,
	h5_loc_id_t did,
	h5_idlist_t* children 
	) {

	h5t_fdata_t* t = f->t;
	h5_idlist_t* td;

	TRY( h5tpriv_find_td2 (
		     f,
		     h5tpriv_get_face_idx (did),
		     h5tpriv_get_elem_idx (did),
		     &td) );
	h5_loc_id_t* tri = td->items;
	h5_loc_id_t* end = td->items+td->num_items;
	for (; tri < end; tri++) {
		h5_loc_idx_t eid = h5tpriv_get_elem_idx (*tri);
		h5_loc_idx_t face_idx =  h5tpriv_get_face_idx (*tri);
		h5_generic_loc_elem_t* tet = (h5_generic_loc_elem_t*)&t->loc_elems.tets[eid];
		if (h5tpriv_elem_is_on_cur_level (f, tet) == H5_OK) {
			TRY( h5priv_append_to_idlist (f, children, *tri) );
		} else {
			h5_loc_id_t dids[4];
			TRY( compute_direct_children_of_triangle (
				      f,
				      face_idx,
				      tet->child_idx,
				      dids) );
			TRY( compute_children_of_triangle (f, dids[0], children) );
			TRY( compute_children_of_triangle (f, dids[1], children) );
			TRY( compute_children_of_triangle (f, dids[2], children) );
			TRY( compute_children_of_triangle (f, dids[3], children) );
		}
	}
	return H5_SUCCESS;
}

static inline h5_err_t
compute_sections_of_triangle (
	h5_file_t* const f,
	h5_loc_id_t did,
	h5_idlist_t* children
	) {
	h5t_fdata_t* t = f->t;
	h5_idlist_t* td;

	TRY( h5tpriv_find_td2 (
		     f,
		     h5tpriv_get_face_idx (did),
		     h5tpriv_get_elem_idx (did), &td) );
	h5_loc_id_t* tri = td->items;
	h5_loc_id_t* end = td->items+td->num_items;
	int refined = 0;
	for (; tri < end; tri++) {
		h5_loc_idx_t eid = h5tpriv_get_elem_idx (*tri);
		h5_loc_idx_t face_idx =  h5tpriv_get_face_idx (*tri);
		h5_generic_loc_elem_t* tet = (h5_generic_loc_elem_t*)&t->loc_elems.tets[eid];
		if (! h5tpriv_elem_is_on_cur_level (f, tet) == H5_OK) {
			refined = 1;
			h5_loc_id_t dids[4];
			TRY( compute_direct_children_of_triangle (
				     f,
				     face_idx,
				     tet->child_idx,
				     dids) );
			TRY( compute_sections_of_triangle (f, dids[0], children) );
			TRY( compute_sections_of_triangle (f, dids[1], children) );
			TRY( compute_sections_of_triangle (f, dids[2], children) );
			TRY( compute_sections_of_triangle (f, dids[3], children) );
		}
	}
	if (! refined) {
		TRY( h5priv_append_to_idlist (f, children, td->items[0]) );
	}
	return H5_SUCCESS;
}
/*
  map edge ID to unique ID
  if unique ID not in list: add
*/
static inline h5_err_t
add_edge (
	h5_file_t* const f,
	h5_idlist_t* list,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx
	) {
	h5_idlist_t* te;
	TRY( h5tpriv_find_te2 (f, face_idx, elem_idx, &te) );
	TRY( h5priv_search_idlist (f, list, te->items[0]) );
	return H5_SUCCESS;
}

/*
  Get upward adjacent edges to vertex given by ID.
 */
static inline h5_err_t
get_edges_uadj_to_vertex (
	h5_file_t* const f,
	const h5_loc_id_t id,
	h5_idlist_t** list
	) {
	TRY( h5priv_alloc_idlist (f, list, 8) );
	
	h5t_fdata_t* const t = f->t;
	h5_loc_idx_t idx;
	TRY( h5t_get_vertex_index_of_vertex (f, id, &idx) );
	h5_idlist_t* tv = &t->adjacencies.tv.v[idx];
	h5_size_t i;

	h5_loc_id_t* vidp = tv->items;	// ptr to upward adjacent elements
	for (i = 0; i < tv->num_items; i++, vidp++) {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (*vidp);
		h5_loc_idx_t face_idx = h5tpriv_get_face_idx (*vidp);
		h5_generic_loc_elem_t* tet = (h5_generic_loc_elem_t*)&t->loc_elems.tets[elem_idx];

		if (h5tpriv_elem_is_on_cur_level (f, tet) == H5_NOK ) {
			continue;
		}
		h5_loc_idx_t edge_idx;
		edge_idx = h5tpriv_get_edge_connected_to_vertex (t->ref_elem, face_idx, 0);
		TRY( add_edge (f, *list, edge_idx, elem_idx) );
		edge_idx = h5tpriv_get_edge_connected_to_vertex (t->ref_elem, face_idx, 1);
		TRY( add_edge (f, *list, edge_idx, elem_idx) );
		edge_idx = h5tpriv_get_edge_connected_to_vertex (t->ref_elem, face_idx, 2);
		TRY( add_edge (f, *list, edge_idx, elem_idx) );
	}
	return H5_SUCCESS;
}

static inline h5_err_t
add_triangle (
	h5_file_t* const f,
	h5_idlist_t* list,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx
	) {
	h5_idlist_t* td;
	TRY( h5tpriv_find_td2 (f, face_idx, elem_idx, &td) );
	TRY( h5priv_search_idlist (f, list, td->items[0]) );

	return H5_SUCCESS;
}

static inline h5_err_t
get_triangles_uadj_to_vertex (
	h5_file_t * const f,
	const h5_loc_id_t id,
	h5_idlist_t **list
	) {
	/*
	  Pre-allocate a list with 8 items. Why 8? Why not?
	  ;-) 
	  We should change it when we have something better.
	 */
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	
	h5t_fdata_t* t = f->t;
	h5_loc_idx_t idx;
	TRY( h5t_get_vertex_index_of_vertex (f, id, &idx) );
	h5_idlist_t* tv = &t->adjacencies.tv.v[idx];

	h5_size_t i;
	h5_loc_id_t* vidp = tv->items;
	for (i = 0; i < tv->num_items; i++, vidp++) {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (*vidp);
		h5_loc_idx_t face_idx = h5tpriv_get_face_idx (*vidp);
		h5_generic_loc_elem_t* tet = (h5_generic_loc_elem_t*)&t->loc_elems.tets[elem_idx];

		if (h5tpriv_elem_is_on_cur_level (f, tet) == H5_NOK) {
			continue;
		}
		h5_loc_idx_t facet_idx;
		facet_idx = h5tpriv_get_triangles_connected_to_vertex (t->ref_elem, face_idx, 0);
		TRY( add_triangle (f, *list, facet_idx, elem_idx) );
		facet_idx = h5tpriv_get_triangles_connected_to_vertex (t->ref_elem, face_idx, 1);
		TRY( add_triangle (f, *list, facet_idx, elem_idx) );
		facet_idx = h5tpriv_get_triangles_connected_to_vertex (t->ref_elem, face_idx, 2);
		TRY( add_triangle (f, *list, facet_idx, elem_idx) );
	}
	return H5_SUCCESS;
}

static inline h5_err_t
get_tets_uadj_to_vertex (
	h5_file_t* const f,
	const h5_loc_id_t id,
	h5_idlist_t** list
	) {
	TRY( h5priv_alloc_idlist (f, list, 8) );
	
	h5t_fdata_t* t = f->t;
	h5_loc_idx_t idx;
	TRY( h5t_get_vertex_index_of_vertex (f, id, &idx) );
	h5_idlist_t* tv = &t->adjacencies.tv.v[idx];
	h5_size_t i;

	h5_loc_id_t* vidp = tv->items;
	for (i = 0; i < tv->num_items; i++, vidp++) {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (*vidp);
		h5_generic_loc_elem_t* tet = (h5_generic_loc_elem_t*)&t->loc_elems.tets[elem_idx];

		if (h5tpriv_elem_is_on_cur_level (f, tet) == H5_NOK) {
			continue;
		}
		// add to result
		TRY( h5priv_search_idlist (f, *list, elem_idx) );
	}
	return H5_SUCCESS;
}

static inline h5_err_t
get_triangles_uadj_to_edge (
	h5_file_t* const f,
	const h5_loc_id_t kid,
	h5_idlist_t** list
	) {
	h5_idlist_t* children;
	TRY( h5priv_alloc_idlist (f, &children, 8) );
	TRY( compute_children_of_edge (f, kid, children) );
	TRY( h5priv_alloc_idlist (f, list, 8) );
	h5_loc_id_t* edge = children->items;
	h5_loc_id_t* end = children->items+children->num_items;
	int map[6][2] = { {2,3}, {0,3}, {1,3}, {1,2}, {0,2}, {0,1} };
	for (; edge < end; edge++) {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (*edge);
		h5_loc_idx_t face_idx = h5tpriv_get_face_idx (*edge);
		TRY( add_triangle (f, *list, map[face_idx][0], elem_idx) );
		TRY( add_triangle (f, *list, map[face_idx][1], elem_idx) );
	}
	TRY( h5priv_free_idlist ( f, &children) );

	return H5_SUCCESS;
}

static inline h5_err_t
get_tets_uadj_to_edge (
	h5_file_t* const f,
	const h5_loc_id_t kid,
	h5_idlist_t** list
	) {
	h5_idlist_t* children;
	TRY( h5priv_alloc_idlist (f, &children, 8) );
	TRY( compute_children_of_edge (f, kid, children) );
	TRY( h5priv_alloc_idlist (f, list, 8) );
	int i;
	h5_loc_id_t* kidp = children->items;
	for (i = 0; i < children->num_items; i++, kidp++) {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (*kidp);
		TRY( h5priv_search_idlist (f, *list, elem_idx) );
	}
	TRY( h5priv_free_idlist (f, &children) );
	return H5_SUCCESS;
}

static inline h5_err_t
get_tets_uadj_to_triangle (
	h5_file_t* const f,
	const h5_loc_id_t did,
	h5_idlist_t** list
	) {
	h5_idlist_t* children;
	TRY( h5priv_alloc_idlist (f, &children, 8) );
	TRY( compute_children_of_triangle (f, did, children) );
	TRY( h5priv_alloc_idlist (f, list, 8) );
	int i;
	h5_loc_id_t *didp = children->items;
	for (i = 0; i < children->num_items; i++ , didp++) {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (*didp);
		TRY( h5priv_search_idlist (f, *list, elem_idx) );
	}
	TRY( h5priv_free_idlist (f, &children) );
	return H5_SUCCESS;
}

static inline h5_err_t
get_vertices_dadj_to_edge (
	h5_file_t* const f,
	const h5_loc_id_t kid,
	h5_idlist_t** list
	) {
	h5_idlist_t* children;
	TRY( h5priv_alloc_idlist (f, &children, 8) );
	TRY( compute_sections_of_edge (f, kid, children) );
	TRY( h5priv_alloc_idlist (f, list, 8) );
	int i;
	h5_loc_id_t* kidp = children->items;
	for (i = 0; i < children->num_items; i++, kidp++) {
		h5_loc_idx_t vertex_indices[2];
		TRY( h5t_get_vertex_indices_of_edge (f, *kidp, vertex_indices) );
		TRY( h5priv_search_idlist (f, *list, vertex_indices[0]) );
		TRY( h5priv_search_idlist (f, *list, vertex_indices[1]) );
	}
	TRY( h5priv_free_idlist(f, &children) );
	return H5_SUCCESS;
}

/*
  Compute downward adjacent vertices of all edges of triangle.
 */
static inline h5_err_t
get_vertices_dadj_to_triangle (
	h5_file_t* const f,
	const h5_loc_id_t did,
	h5_idlist_t** list
	) {
	h5_idlist_t* children;
	TRY( h5priv_alloc_idlist (f, &children, 8) );

	int map[4][3] = { {1,4,5}, {2,3,5}, {0,3,4}, {0,1,2} };
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (did);
	h5_loc_idx_t face_idx = h5tpriv_get_face_idx (did);

	int i;
	for (i = 0; i < 3; i++) {
		TRY( compute_sections_of_edge (
			     f,
			     h5tpriv_build_edge_id (map[face_idx][i], elem_idx),
			     children) );
	}
	TRY( h5priv_alloc_idlist (f, list, 8) );
	h5_loc_id_t* kid = children->items;
	for (i = 0; i < children->num_items; i++, kid++) {
		h5_loc_idx_t vertex_indices[2];
		TRY( h5t_get_vertex_indices_of_edge (f, *kid, vertex_indices) );
		TRY( h5priv_search_idlist (f, *list, vertex_indices[0]) );
		TRY( h5priv_search_idlist (f, *list, vertex_indices[1]) );
	}
	TRY( h5priv_free_idlist(f, &children) );
	return H5_SUCCESS;
}

/*
  Compute downward adjacent vertices to tetrahedron given by ID.
 */
static inline h5_err_t
get_vertices_dadj_to_tet (
	h5_file_t* const f,
	const h5_loc_id_t eid,
	h5_idlist_t** list
	) {
	h5_idlist_t* children;
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (eid);
	TRY( h5priv_alloc_idlist (f, &children, 8) );

	int i;
	for (i = 0; i < 6; i++) {
		TRY( compute_sections_of_edge (
			     f,
			     h5tpriv_build_edge_id ( i, elem_idx ),
			     children) );
	}
	TRY( h5priv_alloc_idlist (f, list, 8) );
	h5_loc_id_t *kid = children->items;
	for (i = 0; i < children->num_items; i++, kid++) {
		h5_loc_idx_t vertex_indices[2];
		TRY( h5t_get_vertex_indices_of_edge (f, *kid, vertex_indices) );
		TRY( h5priv_search_idlist (f, *list, vertex_indices[0]) );
		TRY( h5priv_search_idlist (f, *list, vertex_indices[1]) );
	}
	TRY( h5priv_free_idlist(f, &children) );
	return H5_SUCCESS;
}

static inline h5_err_t
get_edges_dadj_to_triangle (
	h5_file_t* const f,
	const h5_loc_id_t did,
	h5_idlist_t** list
	) {
	h5_idlist_t* children;
	TRY( h5priv_alloc_idlist (f, &children, 8) );

	int map[4][3] = { {1,4,5}, {2,3,5}, {0,3,4}, {0,1,2} };
	h5_loc_idx_t face_idx = h5tpriv_get_face_idx (did);
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (did);

	int i;
	for (i = 0; i < 3; i++) {
		TRY( compute_sections_of_edge (
			     f,
			     h5tpriv_build_edge_id ( map[face_idx][i], elem_idx ),
			     children) );
	}
	TRY( h5priv_alloc_idlist (f, list, 8) );
	h5_loc_id_t *kid = children->items;
	for (i = 0; i < children->num_items; i++, kid++) {
		TRY( h5priv_search_idlist (f, *list, *kid) );
	}
	TRY( h5priv_free_idlist (f, &children) );
	return H5_SUCCESS;
}

static inline h5_err_t
get_edges_dadj_to_tet (
	h5_file_t* const f,
	const h5_loc_id_t eid,
	h5_idlist_t** list
	) {
	h5_idlist_t* children;
	TRY( h5priv_alloc_idlist (f, &children, 8) );
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (eid);

	int i;
	for (i = 0; i < 6; i++) {
		TRY( compute_sections_of_edge (
			     f,
			     h5tpriv_build_edge_id ( i, elem_idx ),
			     children) );
	}
	TRY( h5priv_alloc_idlist (f, list, 8) );
	h5_loc_id_t* kid = children->items;
	for (i = 0; i < children->num_items; i++, kid++) {
		TRY( h5priv_search_idlist (f, *list, *kid) );
	}
	TRY( h5priv_free_idlist (f, &children) );
	return H5_SUCCESS;
}

static inline h5_err_t
get_triangles_dadj_to_tet (
	h5_file_t* const f,
	const h5_loc_id_t eid,
	h5_idlist_t** list
	) {
	h5_idlist_t* children;
	TRY( h5priv_alloc_idlist (f, &children, 8) );
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (eid);

	int i;
	for (i = 0; i < 4; i++) {
		TRY( compute_sections_of_triangle (
			     f,
			     h5tpriv_build_edge_id ( i, elem_idx ),
			     children) );
	}
	TRY( h5priv_alloc_idlist (f, list, 8) );
	h5_loc_id_t* did = children->items;
	for (i = 0; i < children->num_items; i++, did++) {
		TRY( h5priv_search_idlist (f, *list, *did) );
	}
	TRY( h5priv_free_idlist (f, &children) );
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
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_idlist_t** list
	) {
	switch (dim) {
	case 1:
		return get_edges_uadj_to_vertex(f, entity_id, list);
	case 2:
		return get_triangles_uadj_to_vertex(f, entity_id, list);
	case 3:
		return get_tets_uadj_to_vertex(f, entity_id, list);
	default:
		return dim_error (f, dim);
	}
}

static inline h5_err_t
get_adjacencies_to_edge (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_idlist_t** list
	) {
	switch (dim) {
	case 0:
		return get_vertices_dadj_to_edge(f, entity_id, list);
	case 2:
		return get_triangles_uadj_to_edge(f, entity_id, list);
	case 3:
		return get_tets_uadj_to_edge(f, entity_id, list);
	default:
		return dim_error (f, dim);
	}
}

static inline h5_err_t
get_adjacencies_to_triangle (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_idlist_t** list
	) {
	switch (dim) {
	case 0:
		return get_vertices_dadj_to_triangle(f, entity_id, list);
	case 1:
		return get_edges_dadj_to_triangle(f, entity_id, list);
	case 3:
		return get_tets_uadj_to_triangle(f, entity_id, list);
	default:
		return dim_error (f, dim);
	}
}

static inline h5_err_t
get_adjacencies_to_tet (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_idlist_t** list
	) {
	switch (dim) {
	case 0:
		return get_vertices_dadj_to_tet(f, entity_id, list);
	case 1:
		return get_edges_dadj_to_tet(f, entity_id, list);
	case 2:
		return get_triangles_dadj_to_tet(f, entity_id, list);
	default:
		return dim_error (f, dim);
	}
}

static h5_err_t
get_adjacencies (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_idlist_t** list
	) {
	h5_loc_id_t entity_type = h5tpriv_get_entity_type (entity_id);
	switch (entity_type) {
	case H5T_ETYPE_VERTEX:
		return get_adjacencies_to_vertex (f, entity_id, dim, list);
	case H5T_ETYPE_EDGE:
		return get_adjacencies_to_edge (f, entity_id, dim, list);
	case H5T_ETYPE_TRIANGLE:
		return get_adjacencies_to_triangle (f, entity_id, dim, list);
	case H5T_ETYPE_TET:
		return get_adjacencies_to_tet (f, entity_id, dim, list);
	default:
		break;
	}
	return h5_error_internal (f, __FILE__, __func__, __LINE__);
}


static h5_err_t
update_internal_structs (
	h5_file_t* const f,
	const h5t_lvl_idx_t from_lvl
	) {
	clock_t t1 = clock();
	TRY( compute_elems_of_vertices (f, from_lvl) );
	clock_t t2 = clock();
	fprintf (stderr, "compute_tets_of_vertices(): %f\n",
		 (float)(t2-t1)/CLOCKS_PER_SEC);
	t1 = clock();
	TRY( compute_elems_of_edges (f, from_lvl) );
	t2 = clock();
	fprintf (stderr, "compute_tets_of_edge(): %f\n",
		 (float)(t2-t1)/CLOCKS_PER_SEC);
	t1 = clock();
	TRY( compute_elems_of_triangles (f, from_lvl) );
	t2 = clock();
	fprintf (stderr, "compute_tets_of_triangle(): %f\n",
		 (float)(t2-t1)/CLOCKS_PER_SEC);

	return H5_SUCCESS;
}

static h5_err_t
release_internal_structs (
	h5_file_t * const f
	) {
	TRY( release_tv (f) );
	TRY( release_te (f) );
	TRY( release_td (f) );

	return H5_SUCCESS;
}

struct h5t_adjacency_methods h5tpriv_tetm_adjacency_methods = {
	update_internal_structs,
	release_internal_structs,
	get_adjacencies,
};

