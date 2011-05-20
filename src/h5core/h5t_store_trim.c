#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

static h5_err_t
alloc_triangles (
	h5_file_t * const f,
	const size_t cur,
	const size_t new
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t *t = f->t;

	/* alloc mem for local data of elements */
	TRY ( t->loc_elems.tris = h5_alloc (
		      t->loc_elems.tris,
		      new * sizeof (t->loc_elems.tris[0]) ) );
	memset (
		t->loc_elems.tris + cur,
		-1,
		(new-cur) * sizeof (t->loc_elems.tris[0]) );

	/* alloc mem for global to local ID mapping */
	TRY (h5priv_alloc_idxmap (&t->map_elem_g2l, new));

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
get_direct_children_of_edge (
	h5_file_t * const f,
	const h5_loc_idx_t face_idx,
	const h5_loc_idx_t elem_idx,
	h5_loc_id_t children[2]
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	/*
	  Please note: The face index of the children and the father is
	  always the same. The only think we have to know, is the offset
	  to the element index of the first child. This is either 0, 1 or
	  2. The third child is an "inner" child which doesn't superpose edges
	  of the parent.
	  
	  The direct children of edge 0 of an element are edge 0 of the
	  first child and edge 0 of the second child, giving the offset 0
	  and 1 for this edge.
	 */
	int off[3][2] = { {0,1}, // edge 0
			  {0,2}, // edge 1
			  {1,2}  // edge 2
	};
	h5_loc_idx_t num_faces = h5tpriv_ref_elem_get_num_edges (f->t);
	if ((face_idx < 0) || (face_idx >= num_faces)) {
		H5_PRIV_FUNC_LEAVE (h5_error_internal ());
	}
	children[0] = h5tpriv_build_edge_id (face_idx, elem_idx+off[face_idx][0]);
	children[1] = h5tpriv_build_edge_id (face_idx, elem_idx+off[face_idx][1]);
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Bisect edge and return local vertex index of the bisecting point.
*/
static h5_loc_idx_t
bisect_edge (
	h5_file_t* const f,
	const h5_loc_idx_t face_idx,
	const h5_loc_idx_t elem_idx
	) {
	H5_PRIV_FUNC_ENTER (h5_loc_idx_t);
	h5t_fdata_t* const t = f->t;
	h5_loc_idlist_t* retval;
	/*
	  get all elements sharing the given edge
	 */
	TRY( h5tpriv_find_te2 (f, face_idx, elem_idx, &retval) );
	/*
	  check wether one of the found elements has been refined
	 */
	size_t i;
	for (i = 0; i < retval->num_items; i++) {
		h5_loc_idx_t idx = h5tpriv_get_elem_idx (retval->items[i]);
		h5_loc_idx_t child_idx = h5tpriv_get_loc_elem_child_idx (f, idx);
		if (child_idx >= 0) {
			/*
			  this element has been refined!
			  return bisecting point
			 */
			h5_loc_id_t kids[2];
			TRY( get_direct_children_of_edge (
				      f,
				      h5tpriv_get_face_idx (retval->items[i]),
				      child_idx,
				      kids ) );

			h5_loc_idx_t edge0[2], edge1[2];
			TRY( h5t_get_vertex_indices_of_edge ( f, kids[0], edge0 ) );
			TRY( h5t_get_vertex_indices_of_edge ( f, kids[1], edge1 ) );
			if ((edge0[0] == edge1[0]) || (edge0[0] == edge1[1])) {
				H5_PRIV_FUNC_LEAVE (edge0[0]);
			} else {
				H5_PRIV_FUNC_LEAVE (edge0[1]);
			}
		}
	}
	/*
	  None of the elements has been refined -> add new vertex.
	 */
	h5_loc_idx_t indices[2];
	TRY( h5t_get_vertex_indices_of_edge2 (f, face_idx, elem_idx, indices) );
	h5_float64_t* P0 = t->vertices[indices[0]].P;
	h5_float64_t* P1 = t->vertices[indices[1]].P;
	h5_float64_t P[3];

	P[0] = (P0[0] + P1[0]) / 2.0;
	P[1] = (P0[1] + P1[1]) / 2.0;
	P[2] = (P0[2] + P1[2]) / 2.0;

	H5_PRIV_FUNC_RETURN (h5t_store_vertex (f, -1, P));
}

/*
  Please read note about number of new vertices in tetrahedral
  mesh implementation.
 */
static h5_err_t
pre_refine_triangle (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_err_t, "f=0x%p", f);
	unsigned int num_elems_to_refine = f->t->marked_entities->num_items;
	TRY (h5t_begin_store_vertices (f, num_elems_to_refine*2 + 64));
	TRY (h5t_begin_store_elems (f, num_elems_to_refine*4));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*!
  Refine triangle \c local_eid

  \return Local index of first new triangle or \c H5_ERR
*/
static h5_loc_idx_t
refine_triangle (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	H5_PRIV_FUNC_ENTER (h5_loc_idx_t);
	h5t_fdata_t* const t = f->t;
	h5_loc_idx_t vertices[6];	// local vertex indices
	h5_loc_idx_t elem_idx_of_first_child;
	h5_loc_triangle_t* el = &t->loc_elems.tris[elem_idx];

	if (el->child_idx >= 0)
		H5_PRIV_FUNC_LEAVE (
			h5_error (
				H5_ERR_INVAL,
				"Element %lld already refined.",
				(long long)elem_idx));
			
	vertices[0] = el->vertex_indices[0];
	vertices[1] = el->vertex_indices[1];
	vertices[2] = el->vertex_indices[2];

	vertices[3] = bisect_edge (f, 0, elem_idx);
	vertices[4] = bisect_edge (f, 1, elem_idx);
	vertices[5] = bisect_edge (f, 2, elem_idx);

	h5_loc_idx_t new_elem[3];

	new_elem[0] = vertices[0]; // V[0] < V[3] , V[4]
	new_elem[1] = vertices[3]; 
	new_elem[2] = vertices[4];
	TRY( elem_idx_of_first_child = h5t_store_elem (f, elem_idx, new_elem) );

	new_elem[0] = vertices[3];  // V[3] < V[1] , V[5]
	new_elem[1] = vertices[1]; 
	new_elem[2] = vertices[5];
	TRY( h5t_store_elem (f, elem_idx, new_elem) );

	new_elem[0] = vertices[4];  // V[4] < V[5] , V[2]
	new_elem[1] = vertices[5]; 
	new_elem[2] = vertices[2];
	TRY( h5t_store_elem (f, elem_idx, new_elem) );

	new_elem[0] = vertices[3];  // V[3] < V[4] , V[5]
	new_elem[1] = vertices[4];
	new_elem[2] = vertices[5];
	TRY( h5t_store_elem (f, elem_idx, new_elem) );

	t->loc_elems.tris[elem_idx].child_idx = elem_idx_of_first_child;
	t->num_elems_on_leaf_level[t->leaf_level]--;

	H5_PRIV_FUNC_RETURN (elem_idx_of_first_child);
}

static inline h5_loc_idx_t
compute_neighbor_of_face (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx,
	const h5_loc_idx_t face_idx
	) {
	H5_PRIV_FUNC_ENTER (h5_loc_idx_t);
	h5t_fdata_t * const t = f->t;
	h5_loc_idlist_t* te;
	h5_loc_idx_t neighbor_idx = -2;

	do {
		TRY( h5tpriv_find_te2 (
			     f,
			     face_idx,
			     elem_idx,
			     &te) );
		if (te == NULL) {
			H5_PRIV_FUNC_LEAVE (h5_error_internal ());
		}
		if (te->num_items == 1) {
			// neighbor is coarser or face is on the boundary
			elem_idx = t->loc_elems.tris[elem_idx].parent_idx;
			if (elem_idx == -1) {
				// we are on the level of the macro grid
				neighbor_idx = -1;
			}
		} else if (te->num_items == 2) {
			// neighbor has same level of coarsness
			if (h5tpriv_get_elem_idx(te->items[0]) == elem_idx) {
				neighbor_idx = h5tpriv_get_elem_idx (te->items[1]);
			} else {
				neighbor_idx = h5tpriv_get_elem_idx (te->items[0]);
			}
			
		} else {
			H5_PRIV_FUNC_LEAVE (h5_error_internal ());
		}
	} while (neighbor_idx < -1);
	H5_PRIV_FUNC_RETURN (neighbor_idx);
}

/*
  Compute neighbors for elements on given level.
 */
static inline h5_err_t
compute_neighbors_of_elems (
	h5_file_t* const f,
	h5t_lvl_idx_t level
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t * const t = f->t;
	if (level < 0 || level >= t->num_leaf_levels) {
		H5_PRIV_FUNC_LEAVE (
			h5_error (
				H5_ERR_INVAL,
				"level idx %lld out of bound, must be in [%lld,%lld]",
				(long long)level,
				(long long)0,
				(long long)t->num_leaf_levels));
	}
	h5_loc_idx_t elem_idx = level == 0 ? 0 : t->num_elems[level-1];
	const h5_loc_idx_t last_idx = t->num_elems[level] - 1;
	h5_loc_triangle_t *el = &t->loc_elems.tris[elem_idx];
	while (elem_idx <= last_idx) {
		h5_loc_idx_t face_idx = 0;
		for (; face_idx < 3; face_idx++) {
			el->neighbor_indices[face_idx] = 
				compute_neighbor_of_face (f, elem_idx, face_idx);
		}
		elem_idx++;
		el++;
	}

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
end_store_elems (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;

	TRY( h5tpriv_update_adjacency_structs (f, t->leaf_level) );
	TRY( compute_neighbors_of_elems (f, t->leaf_level) );
	TRY( h5tpriv_init_geom_boundary_info (f, t->leaf_level) );
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

struct h5t_store_methods h5tpriv_trim_store_methods = {
	alloc_triangles,
	pre_refine_triangle,
	refine_triangle,
	end_store_elems,
	get_direct_children_of_edge
};
