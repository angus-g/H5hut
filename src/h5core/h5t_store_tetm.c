#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

static h5_err_t
alloc_tets (
	h5_file_t* const f,
	const size_t cur,
	const size_t new
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t *t = f->t;

	/* alloc mem for local data of elements */
	TRY ( t->loc_elems.tets = h5_alloc (
		      t->loc_elems.tets,
		      new * sizeof (t->loc_elems.tets[0]) ) );
	memset (
		t->loc_elems.tets + cur,
		-1,
		(new-cur) * sizeof (t->loc_elems.tets[0]) );

	/* alloc mem for global to local ID mapping */
	TRY (h5priv_alloc_idxmap (&t->map_elem_g2l, new));

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Return the two direct children of the edge given by face and
  element index of first child.
  Note: the semantic of elem_idx is a bit confusing.
  
 */
static h5_err_t
get_direct_children_of_edge (
	h5_file_t* const f,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx,
	h5_loc_id_t children[2]
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	/*
	  Please read the note about the offsets in the corresponding file
	  for triangle meshes.
	 */
	int offs[6][2] = { {0,1}, // edge 0
			   {0,2}, // edge 1
			   {1,2}, // edge 2
			   {0,3}, // edge 3
			   {1,3}, // edge 4
			   {2,3}  // edge 5
	};
	h5_loc_idx_t num_faces = h5tpriv_ref_elem_get_num_edges (f->t);
	if ((face_idx < 0) || (face_idx >= num_faces)) {
		H5_PRIV_FUNC_LEAVE (h5_error_internal ());
	}
	children[0] = h5tpriv_build_edge_id (face_idx, elem_idx+offs[face_idx][0]);
	children[1] = h5tpriv_build_edge_id (face_idx, elem_idx+offs[face_idx][1]);
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*!
  Refine edge. Store vertex, if new.

  Function can be used with tetrahedral and triangle meshes.

  \return local index of vertex
*/
static h5_loc_idx_t
bisect_edge (
	h5_file_t* const f,
	const h5_loc_idx_t face_idx,
	const h5_loc_idx_t elem_idx
	) {
	H5_PRIV_FUNC_ENTER (h5_loc_idx_t);
	h5t_fdata_t* t = f->t;
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
			h5_loc_idx_t face_id = h5tpriv_get_face_idx (
				retval->items[i]);
			h5_loc_id_t kids[2];
			h5_loc_idx_t edge0[2], edge1[2];
			TRY( get_direct_children_of_edge (
				     f,
				     face_id,
				     child_idx,
				     kids) );
			TRY( h5t_get_vertex_indices_of_edge (f, kids[0], edge0) );
			TRY( h5t_get_vertex_indices_of_edge (f, kids[1], edge1) );
			if ((edge0[0] == edge1[0]) || (edge0[0] == edge1[1])) {
				H5_PRIV_FUNC_LEAVE (edge0[0]); // return idx of first vertex of edge
			} else {
				H5_PRIV_FUNC_LEAVE (edge0[1]); // return idx of second vertex of edge
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

	H5_PRIV_FUNC_RETURN (h5t_store_vertex (f, -1, P));  // return idx of new vertex
}

/*
  When calling this function, we know the number of elements to refine. But
  we don't now the number of new vertices we will get. We have to compute
  this number or just to guess it.

  Let n be the number of elements to refine and l the number of disconnected
  areas to be refined.

  For triangle grids the upper limit of new vertices is 3n and the lower limit
  2n + 1. The exact number is 2n + l.

  For tetrahedral grids the upper limit is 6n and the lower limit is 3n+3. 
  The exact number is 3n + 3l.

  To get the real number of vertices to add, we either have to compute the
  number of disconnected areas (which is quiet expensive), try to guess it
  (which is impossible) or just set a limit. In most cases the number of
  disconnected areas will be "small".

  For the time being we set the maximum number of disconnected areas to 64.
 */
static h5_err_t
pre_refine_tet (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_err_t, "f=0x%p", f);
	unsigned int num_elems_to_refine = f->t->marked_entities->num_items;
	TRY (h5t_begin_store_vertices (f, num_elems_to_refine*3 + 192));
	TRY (h5t_begin_store_elems (f, num_elems_to_refine*8));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*!
  Refine tetrahedron \c elem_idx

  \return Local id of first new tetrahedron or \c -1
*/
static h5_loc_idx_t
refine_tet (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	H5_PRIV_FUNC_ENTER (h5_loc_idx_t);
	h5t_fdata_t* t = f->t;
	h5_loc_idx_t vertices[10];
	h5_loc_idx_t elem_idx_of_first_child;
	h5_loc_tet_t* el = &t->loc_elems.tets[elem_idx];

	if ( el->child_idx >= 0 )
		H5_PRIV_FUNC_LEAVE (
			h5_error (
				H5_ERR_INVAL,
				"Tetrahedron %lld already refined.",
				(long long)elem_idx ));
	vertices[0] = el->vertex_indices[0];
	vertices[1] = el->vertex_indices[1];
	vertices[2] = el->vertex_indices[2];
	vertices[3] = el->vertex_indices[3];

	vertices[4] = bisect_edge (f, 0, elem_idx);
	vertices[5] = bisect_edge (f, 1, elem_idx);
	vertices[6] = bisect_edge (f, 3, elem_idx);
	vertices[7] = bisect_edge (f, 2, elem_idx);
	vertices[8] = bisect_edge (f, 4, elem_idx);
	vertices[9] = bisect_edge (f, 5, elem_idx);

	/* 
	   add new tets
	*/
	h5_loc_idx_t new_elem[4];

	new_elem[0] = vertices[0]; // child 0
	new_elem[1] = vertices[4];
	new_elem[2] = vertices[5];
	new_elem[3] = vertices[6];
	TRY( elem_idx_of_first_child = h5t_store_elem (f, elem_idx, new_elem) );

	new_elem[0] = vertices[4]; // child 1
	new_elem[1] = vertices[1];
	new_elem[2] = vertices[7];
	new_elem[3] = vertices[8];
	TRY( h5t_store_elem (f, elem_idx, new_elem) );

	new_elem[0] = vertices[5]; // child 2
	new_elem[1] = vertices[7];
	new_elem[2] = vertices[2];
	new_elem[3] = vertices[9];
	TRY( h5t_store_elem (f, elem_idx, new_elem) );

	new_elem[0] = vertices[6]; // child 3
	new_elem[1] = vertices[8];
	new_elem[2] = vertices[9];
	new_elem[3] = vertices[3];
	TRY( h5t_store_elem (f, elem_idx, new_elem) );

	new_elem[0] = vertices[4]; // child 4
	new_elem[1] = vertices[5];
	new_elem[2] = vertices[6];
	new_elem[3] = vertices[8];
	TRY( h5t_store_elem (f, elem_idx, new_elem) );

	new_elem[0] = vertices[4]; // child 5
	new_elem[1] = vertices[5];
	new_elem[2] = vertices[7];
	new_elem[3] = vertices[8];
	TRY( h5t_store_elem (f, elem_idx, new_elem) );

	new_elem[0] = vertices[5]; // child 6
	new_elem[1] = vertices[7];
	new_elem[2] = vertices[8];
	new_elem[3] = vertices[9];
	TRY( h5t_store_elem (f, elem_idx, new_elem) );

	new_elem[0] = vertices[5]; // child 7
	new_elem[1] = vertices[6];
	new_elem[2] = vertices[8];
	new_elem[3] = vertices[9];
	TRY( h5t_store_elem (f, elem_idx, new_elem) );

	t->loc_elems.tets[elem_idx].child_idx = elem_idx_of_first_child;
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
	h5_loc_idlist_t* td;
	h5_loc_idx_t neighbor_idx = -2;

	do {
		TRY( h5tpriv_find_td2 (
			     f,
			     face_idx,
			     elem_idx,
			     &td) );
		if (td == NULL) {
			H5_PRIV_FUNC_LEAVE (h5_error_internal ());
		}
		if (td->num_items == 1) {
			// neighbor is coarser or face is on the boundary
			elem_idx = t->loc_elems.tets[elem_idx].parent_idx;
			if (elem_idx == -1) {
				// we are on the level of the macro grid
				neighbor_idx = -1;
			}
		} else if (td->num_items == 2) {
			// neighbor has same level of coarsness
			if (h5tpriv_get_elem_idx(td->items[0]) == elem_idx) {
				neighbor_idx = h5tpriv_get_elem_idx (td->items[1]);
			} else {
				neighbor_idx = h5tpriv_get_elem_idx (td->items[0]);
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
	h5_loc_tet_t *el = &t->loc_elems.tets[elem_idx];
	while (elem_idx <= last_idx) {
		h5_loc_idx_t face_idx = 0;
		for (; face_idx < 4; face_idx++) {
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
	h5t_fdata_t* const t = f->t;
	TRY( h5tpriv_update_adjacency_structs (f, t->leaf_level) );
	TRY( compute_neighbors_of_elems (f, t->leaf_level) );
	TRY( h5tpriv_init_geom_boundary_info (f, t->leaf_level) );
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

struct h5t_store_methods h5tpriv_tetm_store_methods = {
	alloc_tets,
	pre_refine_tet,
	refine_tet,
	end_store_elems,
	get_direct_children_of_edge
};
