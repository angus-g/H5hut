#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

static h5_err_t
alloc_triangles (
	h5t_mesh_t* const m,
	const size_t cur,
	const size_t new
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p, cur=%zu, new=%zu", m, cur, new);

	/* alloc mem for local data of elements */
	TRY ( m->loc_elems.tris = h5_alloc (
		      m->loc_elems.tris,
		      new * sizeof (m->loc_elems.tris[0]) ) );
	memset (
		m->loc_elems.tris + cur,
		-1,
		(new-cur) * sizeof (m->loc_elems.tris[0]) );

	/* alloc mem for global to local ID mapping */
	TRY (h5priv_alloc_idxmap (&m->map_elem_g2l, new));

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Bisect edge and return local vertex index of the bisecting point.
*/
static h5_loc_idx_t
bisect_edge (
	h5t_mesh_t* const m,
	const h5_loc_idx_t face_idx,
	const h5_loc_idx_t elem_idx
	) {
	H5_PRIV_FUNC_ENTER (h5_loc_idx_t, "m=%p, face_idx=%d, elem_idx=%d",
			    m, face_idx, elem_idx);
	h5_loc_idlist_t* retval;
	// get all elements sharing the given edge
	TRY (h5tpriv_find_te2 (m, face_idx, elem_idx, &retval));
	// check wether one of the found elements has been refined
	size_t i;
	for (i = 0; i < retval->num_items; i++) {
		h5_loc_id_t kids[2] = {-1,-1};
		TRY (h5tpriv_get_loc_entity_children (m, retval->items[i], kids));
		if (kids[0] >= 0) {
			// element has been refined, return bisecting point
			h5_loc_idx_t edge0[2], edge1[2];
			TRY (h5t_get_vertex_indices_of_edge (m, kids[0], edge0));
			TRY (h5t_get_vertex_indices_of_edge (m, kids[1], edge1));
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
	TRY( h5t_get_vertex_indices_of_edge2 (m, face_idx, elem_idx, indices) );
	h5_float64_t* P0 = m->vertices[indices[0]].P;
	h5_float64_t* P1 = m->vertices[indices[1]].P;
	h5_float64_t P[3];

	P[0] = (P0[0] + P1[0]) / 2.0;
	P[1] = (P0[1] + P1[1]) / 2.0;
	P[2] = (P0[2] + P1[2]) / 2.0;

	H5_PRIV_FUNC_RETURN (h5t_store_vertex (m, -1, P));
}

/*
  Please read note about number of new vertices in tetrahedral
  mesh implementation.
 */
static h5_err_t
pre_refine_triangle (
	h5t_mesh_t* const m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);
	unsigned int num_elems_to_refine = m->marked_entities->num_items;
	TRY (h5t_begin_store_vertices (m, num_elems_to_refine*2 + 64));
	TRY (h5t_begin_store_elems (m, num_elems_to_refine*4));
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*!
  Refine triangle \c local_eid

  \return Local index of first new triangle or \c H5_ERR
*/
static h5_loc_idx_t
refine_triangle (
	h5t_mesh_t* const m,
	const h5_loc_idx_t elem_idx
	) {
	H5_PRIV_FUNC_ENTER (h5_loc_idx_t, "m=%p, elem_idx=%d", m, elem_idx);
	h5_loc_idx_t vertices[6];	// local vertex indices
	h5_loc_idx_t elem_idx_of_first_child;
	h5_loc_triangle_t* el = &m->loc_elems.tris[elem_idx];

	if (el->child_idx >= 0)
		H5_PRIV_FUNC_LEAVE (
			h5_error (
				H5_ERR_INVAL,
				"Element %lld already refined.",
				(long long)elem_idx));
			
	vertices[0] = el->vertex_indices[0];
	vertices[1] = el->vertex_indices[1];
	vertices[2] = el->vertex_indices[2];

	vertices[3] = bisect_edge (m, 0, elem_idx);
	vertices[4] = bisect_edge (m, 1, elem_idx);
	vertices[5] = bisect_edge (m, 2, elem_idx);

	h5_loc_idx_t new_elem[3];

	new_elem[0] = vertices[0]; // V[0] < V[3] , V[4]
	new_elem[1] = vertices[3]; 
	new_elem[2] = vertices[4];
	TRY( elem_idx_of_first_child = h5t_store_elem (m, elem_idx, new_elem) );

	new_elem[0] = vertices[3];  // V[3] < V[1] , V[5]
	new_elem[1] = vertices[1]; 
	new_elem[2] = vertices[5];
	TRY( h5t_store_elem (m, elem_idx, new_elem) );

	new_elem[0] = vertices[4];  // V[4] < V[5] , V[2]
	new_elem[1] = vertices[5]; 
	new_elem[2] = vertices[2];
	TRY( h5t_store_elem (m, elem_idx, new_elem) );

	new_elem[0] = vertices[3];  // V[3] < V[4] , V[5]
	new_elem[1] = vertices[4];
	new_elem[2] = vertices[5];
	TRY( h5t_store_elem (m, elem_idx, new_elem) );

	m->loc_elems.tris[elem_idx].child_idx = elem_idx_of_first_child;
	m->num_leaf_elems[m->leaf_level]--;

	H5_PRIV_FUNC_RETURN (elem_idx_of_first_child);
}

static inline h5_loc_idx_t
compute_neighbor_of_face (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx,
	const h5_loc_idx_t face_idx
	) {
	H5_PRIV_FUNC_ENTER (h5_loc_idx_t, "m=%p, elem_idx=%d, face_idx=%d",
			     m, elem_idx, face_idx);
	h5_loc_idlist_t* te;
	h5_loc_idx_t neighbor_idx = -2;

	do {
		TRY( h5tpriv_find_te2 (
			     m,
			     face_idx,
			     elem_idx,
			     &te) );
		if (te == NULL) {
			H5_PRIV_FUNC_LEAVE (h5_error_internal ());
		}
		if (te->num_items == 1) {
			// neighbor is coarser or face is on the boundary
			elem_idx = m->loc_elems.tris[elem_idx].parent_idx;
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
	h5t_mesh_t* const m,
	h5t_lvl_idx_t level
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p, level=%d", m, level);
	if (level < 0 || level >= m->num_leaf_levels) {
		H5_PRIV_FUNC_LEAVE (
			h5_error (
				H5_ERR_INVAL,
				"level idx %lld out of bound, must be in [%lld,%lld]",
				(long long)level,
				(long long)0,
				(long long)m->num_leaf_levels));
	}
	h5_loc_idx_t elem_idx = level == 0 ? 0 : m->num_elems[level-1];
	const h5_loc_idx_t last_idx = m->num_elems[level] - 1;
	h5_loc_triangle_t *el = &m->loc_elems.tris[elem_idx];
	while (elem_idx <= last_idx) {
		h5_loc_idx_t face_idx = 0;
		for (; face_idx < 3; face_idx++) {
			el->neighbor_indices[face_idx] = 
				compute_neighbor_of_face (m, elem_idx, face_idx);
		}
		elem_idx++;
		el++;
	}

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
end_store_elems (
	h5t_mesh_t* const m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);

	TRY( h5tpriv_update_adjacency_structs (m, m->leaf_level) );
	TRY( compute_neighbors_of_elems (m, m->leaf_level) );
	TRY( h5tpriv_init_geom_boundary_info (m, m->leaf_level) );
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

struct h5t_store_methods h5tpriv_trim_store_methods = {
	alloc_triangles,
	pre_refine_triangle,
	refine_triangle,
	end_store_elems,
};
