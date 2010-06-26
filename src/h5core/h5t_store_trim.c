#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

static h5_err_t
alloc_triangles (
	h5_file_t * const f,
	const size_t cur,
	const size_t new
	) {
	h5t_fdata_t *t = f->t;
	const size_t nvertices = 3;

	/* alloc mem for elements */
	TRY ( t->elems.tris = h5priv_alloc (
		      f,
		      t->elems.tris,
		      new * sizeof(t->elems.tris[0]) ) );
	memset (
		t->elems.tris + cur,
		-1,
		(new-cur) * sizeof(t->elems.tris[0]) );

	/* alloc mem for local data of elements */
	TRY ( t->elems_ldta = h5priv_alloc (
		      f,
		      t->elems_ldta,
		      new * sizeof (t->elems_ldta[0]) ) );
	memset (
		t->elems_ldta + cur,
		-1,
		(new-cur) * sizeof (t->elems_ldta[0]) );

	/* alloc mem for local vertex IDs of elements */
	TRY ( t->elems_lvids = h5priv_alloc (
		      f,
		      t->elems_lvids,
		      new * sizeof(t->elems_lvids[0]) * nvertices ) );
	memset (
		t->elems_lvids + cur * sizeof(t->elems_lvids[0]) * nvertices,
		-1,
		(new-cur) * sizeof(t->elems_lvids[0]) * nvertices );

	/* re-init pointer to local vertex id in local data structure */
	h5_id_t *p = t->elems_lvids;
	h5_id_t id;
	for ( id = 0; id < new; id++, p+=nvertices ) {
		t->elems_ldta[id].local_vertex_indices = p;
	}

	/* alloc mem for global to local ID mapping */
	TRY ( h5priv_alloc_idmap ( f, &t->map_elem_g2l, new ) );

	return  H5_SUCCESS;
}

static h5_err_t
get_direct_children_of_edge (
	h5_file_t * const f,
	const h5_id_t face_idx,
	const h5_id_t elem_idx, // index of the first child!
	h5_id_t	children[2]
	) {
	int num_faces = f->t->ref_element->num_faces[1];
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

	if ( ( face_idx < 0 ) || ( face_idx >= num_faces ) ) {
		return  h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
	children[0] = h5tpriv_build_edge_id ( face_idx, elem_idx+off[face_idx][0] );
	children[1] = h5tpriv_build_edge_id ( face_idx, elem_idx+off[face_idx][1] );
	return H5_SUCCESS;
}

static h5_id_t
bisect_edge (
	h5_file_t * const f,
	h5_id_t face_idx,
	h5_id_t elem_idx
	) {
	h5t_fdata_t *t = f->t;
	h5_idlist_t *retval;
	/*
	  get all elements sharing the given edge
	 */
	TRY( h5tpriv_find_te2 (f, face_idx, elem_idx, &retval) );
	/*
	  check wether one of the found elements has been refined
	 */
	size_t i;
	for ( i = 0; i < retval->num_items; i++ ) {
		h5_id_t local_id = h5tpriv_get_elem_idx ( retval->items[i] );
		h5_elem_ldta_t *tet = &t->elems_ldta[local_id];
		if ( tet->local_child_idx >= 0 ) {
			/*
			  this element has been refined!
			  return bisecting point
			 */
			h5_id_t	face_id = h5tpriv_get_face_idx (
				retval->items[i] );
			h5_id_t kids[2], edge0[2], edge1[2];
			TRY ( get_direct_children_of_edge (
				      f,
				      face_id,
				      tet->local_child_idx,
				      kids ) );
			TRY ( h5t_get_vertex_indices_of_edge ( f, kids[0], edge0 ) );
			TRY ( h5t_get_vertex_indices_of_edge ( f, kids[1], edge1 ) );
			if ( (edge0[0] == edge1[0]) || (edge0[0] == edge1[1]) )
				return edge0[0];
			else
				return edge0[1];
		}
	}
	/*
	  None of the elements has been refined -> add new vertex.
	 */
	h5_id_t vindices[2];
	TRY( h5t_get_vertex_indices_of_edge2 (f, face_idx, elem_idx, vindices) );
	h5_float64_t *P0 = t->vertices[vindices[0]].P;
	h5_float64_t *P1 = t->vertices[vindices[1]].P;
	h5_float64_t P[3];

	P[0] = ( P0[0] + P1[0] ) / 2.0;
	P[1] = ( P0[1] + P1[1] ) / 2.0;
	P[2] = ( P0[2] + P1[2] ) / 2.0;

	return h5t_store_vertex ( f, -1, P );
}

/*!
  Refine triangle \c local_eid

  \return Local id of first new triangle or \c -1
*/
static h5_id_t
refine_triangle (
	h5_file_t* const f,
	const h5_id_t elem_idx
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t vertices[6];	// local vertex indices
	h5_id_t elem_idx_of_first_child;
	h5_elem_ldta_t *el = &t->elems_ldta[elem_idx];

	if ( el->local_child_idx >= 0 )
		return h5_error (
			f,
			H5_ERR_INVAL,
			"Element %lld already refined.",
			(long long)elem_idx );

	vertices[0] = el->local_vertex_indices[0];
	vertices[1] = el->local_vertex_indices[1];
	vertices[2] = el->local_vertex_indices[2];

	vertices[3] = bisect_edge( f, 0, elem_idx );
	vertices[4] = bisect_edge( f, 1, elem_idx );
	vertices[5] = bisect_edge( f, 2, elem_idx );

	h5_id_t new_elem[3];

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

	t->elems.tris[elem_idx].global_child_idx = elem_idx_of_first_child;
	t->elems_ldta[elem_idx].local_child_idx = elem_idx_of_first_child;
	t->num_elems_on_level[t->cur_level]--;

	return elem_idx_of_first_child;
}

struct h5t_store_methods h5tpriv_trim_store_methods = {
	alloc_triangles,
	refine_triangle,
	get_direct_children_of_edge
};
