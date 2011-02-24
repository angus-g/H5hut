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

/*
  Allocate structure keeping the upward adjacent elements for each vertex.
 */
static inline h5_err_t
alloc_tv (
	h5_file_t* const f,
	const h5t_lvl_idx_t from_lvl
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;
	h5_loc_idx_t num_vertices = t->num_vertices[t->num_leaf_levels-1];

	h5t_adjacencies_t* adj = &t->adjacencies;
	// allocate ptr to ID-list per vertex
	TRY( adj->tv.v = h5_alloc (adj->tv.v, num_vertices*sizeof(*adj->tv.v)) );

	size_t i = from_lvl <= 0 ? 0 : t->num_vertices[from_lvl-1];
	bzero (adj->tv.v+i, (num_vertices-i)*sizeof(*adj->tv.v));

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Release structure keeping the upward adjacent elements for each vertex.
 */
static inline h5_err_t
release_tv (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;
	h5t_adjacencies_t* adj = &t->adjacencies;
	if (adj->tv.v == NULL)
		H5_PRIV_FUNC_LEAVE (H5_SUCCESS);

	h5_loc_idx_t vertex_idx = 0;
	h5_loc_idx_t last = t->num_vertices[t->num_leaf_levels-1];
	for (; vertex_idx < last; vertex_idx++) {
		TRY( h5priv_free_idlist (&adj->tv.v[vertex_idx]) );
	}
	TRY( h5_free (adj->tv.v) );
	adj->tv.v = NULL;
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Compute upward adjacent elements for each vertex.
*/
static inline h5_err_t
compute_elems_of_vertices (
	h5_file_t* const f,
	const h5t_lvl_idx_t from_lvl
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	/* expand structure */
	TRY( alloc_tv (f, from_lvl) );

	/* loop over all elements in current level */
	h5t_fdata_t* t = f->t;
	h5_loc_idx_t elem_idx = (from_lvl <= 0) ? 0:t->num_elems[from_lvl-1];
	h5_loc_idx_t last = (t->num_leaf_levels < 0) ? 0:t->num_elems[t->num_leaf_levels-1];
	for (;elem_idx < last; elem_idx++) {
		int face_idx;
		int num_faces = h5tpriv_ref_elem_get_num_vertices(t);
		for (face_idx = 0; face_idx < num_faces; face_idx++) {
			TRY( h5tpriv_search_tv2 (f, face_idx, elem_idx, NULL) );
		}
	}
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Compute upward adjacent elements for each edge.
 */
static inline h5_err_t
compute_elems_of_edges (
	h5_file_t* const f,
	const h5t_lvl_idx_t from_lvl
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;
	h5_loc_idx_t elem_idx = (from_lvl <= 0) ? 0 : t->num_elems[from_lvl-1];
	h5_loc_idx_t num_elems = t->num_elems[t->num_leaf_levels-1];
	TRY( h5tpriv_resize_te_htab (f, 4*(num_elems-elem_idx)) );
	for (;elem_idx < num_elems; elem_idx++) {
		int face_idx;
		int num_faces = h5tpriv_ref_elem_get_num_edges(t);
		for (face_idx = 0; face_idx < num_faces; face_idx++) {
			TRY( h5tpriv_search_te2 (f, face_idx, elem_idx, NULL) );
		}
	}
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Compute upward adjacent elements for each triangle.
 */
static inline h5_err_t
compute_elems_of_triangles (
	h5_file_t* const f,
	const h5t_lvl_idx_t from_lvl
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;
	h5_loc_idx_t elem_idx = (from_lvl <= 0) ? 0 : t->num_elems[from_lvl-1];
	h5_loc_idx_t num_elems = t->num_elems[t->num_leaf_levels-1];
	TRY( h5tpriv_resize_td_htab (f, 4*(num_elems-elem_idx)) );
	for (;elem_idx < num_elems; elem_idx++) {
		int face_idx;
		int num_faces = h5tpriv_ref_elem_get_num_edges (t);
		for (face_idx = 0; face_idx < num_faces; face_idx++) {
			TRY( h5tpriv_search_td2 (f, face_idx, elem_idx, NULL) );
		}
	}
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Compute the children o an edge
 */
static inline h5_err_t
compute_children_of_edge (
	h5_file_t* const f,
	h5_loc_id_t entity_id,
	h5_loc_idlist_t** children
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* te;
	TRY( h5tpriv_find_te (f, entity_id, &te ) );
	h5_loc_id_t* edge_idp = te->items;
	h5_loc_id_t* end = te->items+te->num_items;
	do {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (*edge_idp);
		h5_loc_idx_t face_idx = h5tpriv_get_face_idx (*edge_idp);
		h5_loc_tet_t* elem = &f->t->loc_elems.tets[elem_idx];
		if (h5tpriv_is_leaf_elem (f, elem)) {
			TRY( h5priv_insert_idlist (children, *edge_idp, -1) );
		} else {
			h5_loc_id_t edge_ids[2];
			TRY( h5tpriv_get_direct_children_of_edge (
				      f,
				      face_idx,
				      elem->child_idx,
				      edge_ids) );
			TRY( compute_children_of_edge (f, edge_ids[0], children) );
			TRY( compute_children_of_edge (f, edge_ids[1], children) );
		}
	} while (++edge_idp < end);
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/* 
   Compute all sections of an edge.
*/
static inline h5_err_t
compute_sections_of_edge (
	h5_file_t* const f,
	h5_loc_id_t entity_id,
	h5_loc_idlist_t** children
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* te;
	TRY( h5tpriv_find_te (f, entity_id, &te) );
	h5_loc_id_t* edge_idp = te->items;
	h5_loc_id_t* end = te->items+te->num_items;
	int refined = 0;
	do {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (*edge_idp);
		h5_loc_idx_t face_idx =  h5tpriv_get_face_idx (*edge_idp);
		h5_loc_tet_t* elem = &f->t->loc_elems.tets[elem_idx];
		if (!h5tpriv_is_leaf_elem (f, elem)) {
			refined = 1;
			h5_loc_id_t edge_ids[2];
			TRY( h5tpriv_get_direct_children_of_edge (
				     f,
				     face_idx,
				     elem->child_idx,
				     edge_ids) );
			TRY( compute_sections_of_edge (f, edge_ids[0], children) );
			TRY( compute_sections_of_edge (f, edge_ids[1], children) );
		}
	} while (++edge_idp < end);
	if (!refined) {
		TRY( h5priv_insert_idlist (children, te->items[0], -1) );
	}
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
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
	h5_loc_idx_t face_idx,		// in
	h5_loc_idx_t elem_idx,		// in
	h5_loc_id_t* children		// out
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idx_t map[4][4][2] = {
		{{0,0},{0,1},{0,2},{0,5}},
		{{1,0},{1,1},{1,3},{2,4}},
		{{2,0},{2,2},{2,3},{1,7}},
		{{3,1},{3,2},{3,3},{3,6}}
	};
	int num_faces = h5tpriv_ref_elem_get_num_facets (f->t);
	if ((face_idx < 0) || (face_idx >= num_faces)) {
		H5_PRIV_FUNC_LEAVE (h5_error_internal ());
	}
	children[0] = h5tpriv_build_triangle_id (
		map[face_idx][0][0], elem_idx+map[face_idx][0][1]);
	children[1] = h5tpriv_build_triangle_id (
		map[face_idx][1][0], elem_idx+map[face_idx][1][1]);
	children[2] = h5tpriv_build_triangle_id (
		map[face_idx][2][0], elem_idx+map[face_idx][2][1]);
	children[3] = h5tpriv_build_triangle_id (
		map[face_idx][3][0], elem_idx+map[face_idx][3][1]);
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
compute_children_of_triangle (
	h5_file_t* const f,
	h5_loc_id_t entity_id,
	h5_loc_idlist_t** children 
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* td;
	TRY( h5tpriv_find_td (f, entity_id, &td) );
	h5_loc_id_t* triangle_idp = td->items;
	h5_loc_id_t* end = td->items+td->num_items;
	do {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (*triangle_idp);
		h5_loc_idx_t face_idx = h5tpriv_get_face_idx (*triangle_idp);
		h5_loc_tet_t* elem = &f->t->loc_elems.tets[elem_idx];
		if (h5tpriv_is_leaf_elem (f, elem)) {
			TRY( h5priv_insert_idlist (children, *triangle_idp, -1) );
		} else {
			h5_loc_id_t triangle_ids[4] = {-1,-1,-1,-1};
			TRY( compute_direct_children_of_triangle (
				      f,
				      face_idx,
				      elem->child_idx,
				      triangle_ids) );
			TRY( compute_children_of_triangle (f, triangle_ids[0], children) );
			TRY( compute_children_of_triangle (f, triangle_ids[1], children) );
			TRY( compute_children_of_triangle (f, triangle_ids[2], children) );
			TRY( compute_children_of_triangle (f, triangle_ids[3], children) );
		}
	} while (++triangle_idp < end);
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
compute_sections_of_triangle (
	h5_file_t* const f,
	h5_loc_id_t entity_id,
	h5_loc_idlist_t** children
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* td;
	TRY( h5tpriv_find_td (f, entity_id, &td) );
	h5_loc_id_t* triangle_idp = td->items;
	h5_loc_id_t* end = td->items+td->num_items;
	int refined = 0;
	do {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (*triangle_idp);
		h5_loc_idx_t face_idx = h5tpriv_get_face_idx (*triangle_idp);
		h5_loc_tet_t* elem = &f->t->loc_elems.tets[elem_idx];
		if (!h5tpriv_is_leaf_elem (f, elem)) {
			refined = 1;
			h5_loc_id_t triangle_ids[4] = {-1,-1,-1,-1};
			TRY( compute_direct_children_of_triangle (
				     f,
				     face_idx,
				     elem->child_idx,
				     triangle_ids) );
			TRY( compute_sections_of_triangle (f, triangle_ids[0], children) );
			TRY( compute_sections_of_triangle (f, triangle_ids[1], children) );
			TRY( compute_sections_of_triangle (f, triangle_ids[2], children) );
			TRY( compute_sections_of_triangle (f, triangle_ids[3], children) );
		}
	} while (++triangle_idp < end);
	if (! refined) {
		TRY( h5priv_insert_idlist (children, td->items[0], -1) );
	}
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Add unique ID of vertex given by face and element index to list.
*/
static inline h5_err_t
add_vertex2 (
	h5_file_t* const f,	// in
	h5_loc_idlist_t** list,	// out
	h5_loc_idx_t face_idx,	// in
	h5_loc_idx_t elem_idx	// in
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* tv;
	TRY( h5tpriv_find_tv2 (f, face_idx, elem_idx, &tv) );
	TRY( h5priv_search_idlist (list, tv->items[0]) );
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Add unique ID of edge given by ID or face and element index.
*/
static inline h5_err_t
add_edge (
	h5_file_t* const f,	// in
	h5_loc_idlist_t** list,	// out
	h5_loc_id_t entity_id	// in
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* te;
	TRY( h5tpriv_find_te (f, entity_id, &te) );
	TRY( h5priv_search_idlist (list, te->items[0]) );
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
add_edge2 (
	h5_file_t* const f,	// in
	h5_loc_idlist_t** list,	// out
	h5_loc_idx_t face_idx,	// in
	h5_loc_idx_t elem_idx	// in
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* te;
	TRY( h5tpriv_find_te2 (f, face_idx, elem_idx, &te) );
	TRY( h5priv_search_idlist (list, te->items[0]) );
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Add unique ID of triangle given by ID or face and element index.
*/
static inline h5_err_t
add_triangle (
	h5_file_t* const f,	// in
	h5_loc_idlist_t** list,	// out
	h5_loc_idx_t entity_id // in
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* td;
	TRY( h5tpriv_find_td (f, entity_id, &td) );
	TRY( h5priv_search_idlist (list, td->items[0]) );

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
add_triangle2 (
	h5_file_t* const f,	// in
	h5_loc_idlist_t** list,	// out
	h5_loc_idx_t face_idx,	// in
	h5_loc_idx_t elem_idx	// in
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* td;
	TRY( h5tpriv_find_td2 (f, face_idx, elem_idx, &td) );
	TRY( h5priv_search_idlist (list, td->items[0]) );

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
add_elem2 (
	h5_loc_idlist_t** list,	// out
	h5_loc_idx_t elem_idx	// in
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_id_t elem_id = h5tpriv_build_tet_id (0, elem_idx);
	TRY( h5priv_search_idlist (list, elem_id) );

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Get upward adjacent edges to vertex given by ID.
 */
static inline h5_err_t
get_edges_uadj_to_vertex (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idlist_t** list
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idx_t vertex_idx;
	TRY( h5t_get_vertex_index_of_vertex (f, entity_id, &vertex_idx) );
	h5_loc_idlist_t* tv = f->t->adjacencies.tv.v[vertex_idx];

	TRY( h5priv_alloc_idlist (list, 8) );
	h5_loc_id_t* vertex_idp = tv->items;
	h5_loc_id_t* end = vertex_idp + tv->num_items;
	do {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (*vertex_idp);
		h5_loc_idx_t face_idx = h5tpriv_get_face_idx (*vertex_idp);
		h5_loc_tet_t* elem = &f->t->loc_elems.tets[elem_idx];

		if (!h5tpriv_is_leaf_elem (f, elem)) {
			continue;
		}
		TRY( add_edge2 (f, list,
			       h5tpriv_ref_elem_get_edge_idx (
				       f->t, 0, face_idx, 0),
				elem_idx) );
		TRY( add_edge2 (f, list,
			       h5tpriv_ref_elem_get_edge_idx (
				       f->t, 0, face_idx, 1),
			       elem_idx) );
		TRY( add_edge2 (f, list,
			       h5tpriv_ref_elem_get_edge_idx (
				       f->t, 0, face_idx, 2),
			       elem_idx) );
	} while (++vertex_idp < end);
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
get_triangles_uadj_to_vertex (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idlist_t** list
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	/* get list of all tetrahedra connected to given vertex
	   Note: this list may include tetrahedra which are not in
	   the (current) leaf grid */
	h5_loc_idx_t vertex_idx;
	TRY( h5t_get_vertex_index_of_vertex (f, entity_id, &vertex_idx) );
	h5_loc_idlist_t* tv = f->t->adjacencies.tv.v[vertex_idx];
	
	// build list of upward adjacent triangles
	TRY( h5priv_alloc_idlist (list, 8) );
	h5_loc_id_t* vertex_idp = tv->items;
	h5_loc_id_t* end = vertex_idp + tv->num_items;
	do {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (*vertex_idp);
		h5_loc_idx_t face_idx = h5tpriv_get_face_idx (*vertex_idp);
		h5_loc_tet_t* elem = &f->t->loc_elems.tets[elem_idx];
		if (!h5tpriv_is_leaf_elem (f, elem)) {
			continue;
		}
		TRY( add_triangle2 (f, list,
				   h5tpriv_ref_elem_get_triangle_idx (
					   f->t, 0, face_idx, 0),
				   elem_idx) );
		TRY( add_triangle2 (f, list,
				   h5tpriv_ref_elem_get_triangle_idx (
					   f->t, 0, face_idx, 1),
				   elem_idx) );
		TRY( add_triangle2 (f, list,
				   h5tpriv_ref_elem_get_triangle_idx (
					   f->t, 0, face_idx, 2),
				   elem_idx) );
	} while (++vertex_idp < end);
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
get_tets_uadj_to_vertex (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idlist_t** list
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idx_t vertex_idx;
	TRY( h5t_get_vertex_index_of_vertex (f, entity_id, &vertex_idx) );
	h5_loc_idlist_t* tv = f->t->adjacencies.tv.v[vertex_idx];

	TRY( h5priv_alloc_idlist (list, 8) );
	h5_loc_id_t* vertex_idp = tv->items;
	h5_loc_id_t* end = vertex_idp + tv->num_items;
	do {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (*vertex_idp);
		h5_loc_tet_t* elem = &f->t->loc_elems.tets[elem_idx];
		if (!h5tpriv_is_leaf_elem (f, elem)) {
			continue;
		}
		TRY( add_elem2 (list, elem_idx) );
	} while (++vertex_idp < end);
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
get_triangles_uadj_to_edge (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idlist_t** list
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* children;
	TRY( h5priv_alloc_idlist (&children, 8) );
	TRY( compute_children_of_edge (f, entity_id, &children) );
	TRY( h5priv_alloc_idlist (list, 8) );
	h5_loc_id_t* edge_idp = children->items;
	h5_loc_id_t* end = children->items+children->num_items;
	do {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (*edge_idp);
		h5_loc_idx_t face_idx = h5tpriv_get_face_idx (*edge_idp);
		TRY( add_triangle2 (f, list,
				   h5tpriv_ref_elem_get_triangle_idx (
					   f->t, 1, face_idx, 0),
				   elem_idx) );
		TRY( add_triangle2 (f, list,
				   h5tpriv_ref_elem_get_triangle_idx (
					   f->t, 1, face_idx, 1),
				   elem_idx) );
	} while (++edge_idp < end);
	TRY (h5priv_free_idlist (&children));

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
get_tets_uadj_to_edge (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idlist_t** list
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* children;
	TRY( h5priv_alloc_idlist (&children, 8) );
	TRY( compute_children_of_edge (f, entity_id, &children) );
	TRY( h5priv_alloc_idlist (list, 8) );
	h5_loc_id_t* edge_idp = children->items;
	h5_loc_id_t* end = children->items+children->num_items;
	do {
		TRY( add_elem2 (list, h5tpriv_get_elem_idx (*edge_idp)) );
	} while (++edge_idp < end);
	TRY( h5priv_free_idlist (&children) );
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
get_tets_uadj_to_triangle (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idlist_t** list
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* children;
	TRY( h5priv_alloc_idlist (&children, 8) );
	TRY( compute_children_of_triangle (f, entity_id, &children) );
	TRY( h5priv_alloc_idlist (list, 8) );
	h5_loc_id_t* triangle_idp = children->items;
	h5_loc_id_t* end = triangle_idp + children->num_items;
	do {
		TRY( add_elem2 (list, h5tpriv_get_elem_idx (*triangle_idp)) );
	} while (++triangle_idp < end);
	TRY( h5priv_free_idlist (&children) );
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
get_vertices_dadj_to_edge (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idlist_t** list
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* children;
	TRY( h5priv_alloc_idlist (&children, 8) );
	TRY( compute_sections_of_edge (f, entity_id, &children) );
	// build list of unique vertex IDs
	TRY( h5priv_alloc_idlist (list, 8) );
	h5_loc_id_t* edge_idp = children->items;
	h5_loc_id_t* end = edge_idp + children->num_items;
	do {
		h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (*edge_idp);
		h5_loc_idx_t face_idx = h5tpriv_get_face_idx (*edge_idp);

		TRY( add_vertex2 (f, list,
				 h5tpriv_ref_elem_get_vertex_idx (
					 f->t, 1, face_idx, 0),
				 elem_idx) );
		TRY( add_vertex2 (f, list,
				 h5tpriv_ref_elem_get_vertex_idx (
					 f->t, 1, face_idx, 1),
				 elem_idx) );
	} while (++edge_idp < end);
	TRY( h5priv_free_idlist(&children) );
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Compute downward adjacent vertices of all edges of triangle.
 */
static inline h5_err_t
get_vertices_dadj_to_triangle (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idlist_t** list
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* children;
	TRY( h5priv_alloc_idlist (&children, 8) );
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	h5_loc_idx_t face_idx = h5tpriv_get_face_idx (entity_id);
	h5_loc_idx_t edge_idx;
	int i;
	// loop over all vertices of given triangle
	for (i = 0; i < 3; i++) {
		edge_idx = h5tpriv_ref_elem_get_edge_idx (f->t, 2, face_idx, i);
		TRY( compute_sections_of_edge (
			     f,
			     h5tpriv_build_edge_id (edge_idx, elem_idx),
			     &children) );
	}
	// build list of unique vertex IDs
	TRY( h5priv_alloc_idlist (list, 8) );
	h5_loc_id_t* edge_idp = children->items;
	h5_loc_id_t* end = edge_idp + children->num_items;
	do {
		elem_idx = h5tpriv_get_elem_idx (*edge_idp);
		edge_idx = h5tpriv_get_face_idx (*edge_idp);

		TRY( add_vertex2 (f, list,
				 h5tpriv_ref_elem_get_vertex_idx (
					 f->t, 1, edge_idx, 0),
				 elem_idx) );
		TRY( add_vertex2 (f, list,
				 h5tpriv_ref_elem_get_vertex_idx (
					 f->t, 1, edge_idx, 1),
				 elem_idx) );
	} while (++edge_idp < end );
	TRY( h5priv_free_idlist(&children) );
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Compute downward adjacent vertices to tetrahedron given by ID.
 */
static inline h5_err_t
get_vertices_dadj_to_tet (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idlist_t** list
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* children;
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	TRY( h5priv_alloc_idlist (&children, 8) );
	// loop over all edges of tetrahedron
	h5_loc_idx_t edge_idx = h5tpriv_ref_elem_get_num_edges(f->t);
	while (--edge_idx >= 0) {
		TRY( compute_sections_of_edge (
			     f,
			     h5tpriv_build_edge_id ( edge_idx, elem_idx ),
			     &children) );
	}
	// build list of unique vertex IDs
	TRY( h5priv_alloc_idlist (list, 8) );
	h5_loc_id_t* edge_idp = children->items;
	h5_loc_id_t* end = edge_idp + children->num_items;
	do {
		elem_idx = h5tpriv_get_elem_idx (*edge_idp);
		edge_idx = h5tpriv_get_face_idx (*edge_idp);

		TRY( add_vertex2 (f, list,
				 h5tpriv_ref_elem_get_vertex_idx (
					 f->t, 1, edge_idx, 0),
				 elem_idx) );
		TRY( add_vertex2 (f, list,
				 h5tpriv_ref_elem_get_vertex_idx (
					 f->t, 1, edge_idx, 1),
				 elem_idx) );
	} while (++edge_idp < end);
	TRY( h5priv_free_idlist(&children) );
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
get_edges_dadj_to_triangle (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idlist_t** list
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* children;
	TRY( h5priv_alloc_idlist (&children, 8) );
	h5_loc_idx_t face_idx = h5tpriv_get_face_idx (entity_id);
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	// loop over all three edges of triangle
	h5_loc_idx_t i;
	for (i = 0; i < 3; i++) {
		TRY( compute_sections_of_edge (
			     f,
			     h5tpriv_build_edge_id (
				     h5tpriv_ref_elem_get_edge_idx (
					     f->t, 2, face_idx, i),	     
				     elem_idx ),
			     &children) );
	}
	// build list of unique edge IDs
	TRY( h5priv_alloc_idlist (list, 8) );
	h5_loc_id_t* edge_idp = children->items;
	h5_loc_id_t* end = edge_idp + children->num_items;
	do {
		TRY( add_edge (f, list, *edge_idp) );
	} while (++edge_idp < end);
	TRY( h5priv_free_idlist (&children) );
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
get_edges_dadj_to_tet (
	h5_file_t* const f,
	const h5_loc_id_t elem_id,
	h5_loc_idlist_t** list
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* children;
	TRY( h5priv_alloc_idlist (&children, 8) );
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (elem_id);

	// loop over all edges of tet
	h5_loc_idx_t edge_idx = h5tpriv_ref_elem_get_num_edges(f->t);
	while (--edge_idx >= 0) {
		TRY( compute_sections_of_edge (
			     f,
			     h5tpriv_build_edge_id (edge_idx, elem_idx),
			     &children) );
	}
	// build list of unique edge IDs
	TRY( h5priv_alloc_idlist (list, 8) );
	h5_loc_id_t* edge_idp = children->items;
	h5_loc_id_t* end = edge_idp + children->num_items;
	do {
		TRY( add_edge (f, list, *edge_idp) );
	} while (++edge_idp < end);
	TRY( h5priv_free_idlist (&children) );
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
get_triangles_dadj_to_tet (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idlist_t** list
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5_loc_idlist_t* children;
	TRY( h5priv_alloc_idlist (&children, 8) );
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	// loop over all triangle of element
	h5_loc_idx_t triangle_idx = h5tpriv_ref_elem_get_num_facets(f->t);
	while (--triangle_idx >= 0) {
		TRY( compute_sections_of_triangle (
			     f,
			     h5tpriv_build_edge_id (triangle_idx, elem_idx),
			     &children) );
	}
	TRY( h5priv_alloc_idlist (list, 8) );
	h5_loc_id_t* triangle_idp = children->items;
	h5_loc_id_t* end = triangle_idp + children->num_items;
	do {
		TRY( add_triangle (f, list, *triangle_idp) );
	} while (++triangle_idp < end);
	TRY( h5priv_free_idlist (&children) );
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
dim_error(
	const h5_int32_t dim
	) {
	return h5_error (
		H5_ERR_INVAL,
		"Illegal dimension %ld", (long)dim);
}

static inline h5_err_t
get_adjacencies_to_vertex (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_loc_idlist_t** list
	) {
	switch (dim) {
	case 1:
		return get_edges_uadj_to_vertex(f, entity_id, list);
	case 2:
		return get_triangles_uadj_to_vertex(f, entity_id, list);
	case 3:
		return get_tets_uadj_to_vertex(f, entity_id, list);
	default:
		return dim_error (dim);
	}
}

static inline h5_err_t
get_adjacencies_to_edge (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_loc_idlist_t** list
	) {
	switch (dim) {
	case 0:
		return get_vertices_dadj_to_edge(f, entity_id, list);
	case 2:
		return get_triangles_uadj_to_edge(f, entity_id, list);
	case 3:
		return get_tets_uadj_to_edge(f, entity_id, list);
	default:
		return dim_error (dim);
	}
}

static inline h5_err_t
get_adjacencies_to_triangle (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_loc_idlist_t** list
	) {
	switch (dim) {
	case 0:
		return get_vertices_dadj_to_triangle(f, entity_id, list);
	case 1:
		return get_edges_dadj_to_triangle(f, entity_id, list);
	case 3:
		return get_tets_uadj_to_triangle(f, entity_id, list);
	default:
		return dim_error (dim);
	}
}

static inline h5_err_t
get_adjacencies_to_tet (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_loc_idlist_t** list
	) {
	switch (dim) {
	case 0:
		return get_vertices_dadj_to_tet(f, entity_id, list);
	case 1:
		return get_edges_dadj_to_tet(f, entity_id, list);
	case 2:
		return get_triangles_dadj_to_tet(f, entity_id, list);
	default:
		return dim_error (dim);
	}
}

static h5_err_t
get_adjacencies (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_loc_idlist_t** list
	) {
	h5_loc_id_t entity_type = h5tpriv_get_entity_type (entity_id);
	switch (entity_type) {
	case H5T_TYPE_VERTEX:
		return get_adjacencies_to_vertex (f, entity_id, dim, list);
	case H5T_TYPE_EDGE:
		return get_adjacencies_to_edge (f, entity_id, dim, list);
	case H5T_TYPE_TRIANGLE:
		return get_adjacencies_to_triangle (f, entity_id, dim, list);
	case H5T_TYPE_TET:
		return get_adjacencies_to_tet (f, entity_id, dim, list);
	default:
		break;
	}
	return h5_error_internal ();
}


static h5_err_t
update_internal_structs (
	h5_file_t* const f,
	const h5t_lvl_idx_t from_lvl
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
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

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
release_internal_structs (
	h5_file_t * const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;
	TRY( release_tv (f) );
	TRY( h5priv_hdestroy (&t->adjacencies.te_hash) );
	TRY( h5priv_hdestroy (&t->adjacencies.td_hash) );	
	bzero (&t->adjacencies, sizeof (t->adjacencies));
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

struct h5t_adjacency_methods h5tpriv_tetm_adjacency_methods = {
	update_internal_structs,
	release_internal_structs,
	get_adjacencies,
};

