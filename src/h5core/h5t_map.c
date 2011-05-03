#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*
  Mapping of global to local id's:

  Before adding a new level or closing the mesh, we must define global id's
  for the vertices and elements. This we have to do only for the last stored
  level.
*/  


/*!
  Compare to vertices given by their 3-dimensional coordinates
*/
static int
cmp_vertices (
	h5_float64_t P0[3],
	h5_float64_t P1[3]
	) {
	int i;
	for (i = 0; i < 3; i++) {
		h5_int64_t diff = h5priv_fcmp (P0[i], P1[i], 10);
		if (diff < 0)		return -1;
		else if (diff > 0)	return 1;
	}
	return 0;
}

/*!
  Sort (small) array of local vertex indices geometrically. 
 */
h5_err_t
h5tpriv_sort_local_vertex_indices (
	h5_file_t* const f,
	h5_loc_idx_t* const indices,	/* IN/OUT: local vertex indices */	
	const h5_size_t size		/* size of array */
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;

	h5_size_t i;
	for (i = 1; i < size; ++i) {
		h5_loc_idx_t idx = indices[i];

		h5_size_t j = i;
		while ((j >= 1 ) && cmp_vertices (
			       t->vertices[idx].P,
			       t->vertices[indices[j-1]].P 
			       ) < 0 ) {
			indices[j] = indices[j-1];
			--j;
		}
		indices[j] = idx;
	}
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

/*!
  Map a global vertex index to corresponding local index.
*/
h5_loc_idx_t
h5t_map_global_vertex_idx2local (
	h5_file_t* const f,
	const h5_glb_idx_t glb_idx
	) {
	H5_CORE_API_ENTER2 (h5_loc_idx_t,
			    "f=0x%p, glb_idx=%llu",
			    f, glb_idx);
	if (glb_idx < 0) return -1;
	
	h5_loc_idx_t loc_idx = h5priv_search_idxmap (&f->t->map_vertex_g2l, glb_idx);
	if (loc_idx < 0) {
		H5_CORE_API_LEAVE (h5tpriv_error_global_id_nexist ("vertex", glb_idx));
	}
	H5_CORE_API_RETURN (loc_idx);
}

h5_err_t
h5t_map_global_vertex_indices2local (
	h5_file_t* const f,
	const h5_glb_idx_t* const glb_indices,
	const h5_size_t size,
	h5_loc_idx_t* const loc_indices
	) {
	H5_CORE_API_ENTER4 (h5_err_t,
			    "f=0x%p, glb_indices=0x%p, size=%llu, loc_indices=0x%p",
			    f, glb_indices, size, loc_indices);
	h5_size_t i;
	for (i = 0; i < size; i++) {
		TRY (loc_indices[i] =
		      h5t_map_global_vertex_idx2local (f, glb_indices[i]));
	}
	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*!
  Get local element idx of element given by its global idx.

  \param[in]	f		File handle
  \param[in]	glb_idx		Global element index

  \return	Local element index or error code.
*/
h5_loc_idx_t
h5t_map_glb_elem_idx2loc (
	h5_file_t* const f,
	const h5_glb_idx_t glb_idx
	) {
	H5_CORE_API_ENTER2 (h5_loc_idx_t,
			    "f=0x%p, glb_idx=%llu",
			    f, glb_idx);
	if (glb_idx < 0) H5_CORE_API_LEAVE (-1);

	h5_loc_idx_t loc_idx = h5priv_search_idxmap (&f->t->map_elem_g2l, glb_idx);
	if (loc_idx < 0) 
		H5_CORE_API_LEAVE (h5tpriv_error_global_id_nexist ("elem", glb_idx));
	H5_CORE_API_RETURN (loc_idx);
}

h5_err_t
h5t_map_glb_elem_indices2loc (
	h5_file_t* const f,
	const h5_glb_idx_t*  glb_indices,
	const h5_size_t size,
	h5_loc_idx_t* loc_indices
	) {
	H5_CORE_API_ENTER4 (h5_err_t,
			    "f=0x%p, glb_indices=0x%p, size=%llu, loc_indices=0x%p",
			    f, glb_indices, size, loc_indices);
	const h5_glb_idx_t*  end = glb_indices+size;

	while (glb_indices < end) {
		TRY (*loc_indices =
		     h5t_map_glb_elem_idx2loc (f, *glb_indices));
		loc_indices++;
		glb_indices++;
	}
	H5_CORE_API_RETURN (H5_SUCCESS);
}


/*
  rebuild mapping of global vertex indices to their local indices
 */
h5_err_t
h5tpriv_rebuild_vertex_indices_mapping (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_loc_idx_t, "f=0x%p", f);
	h5t_fdata_t* t = f->t;
	if (t->num_leaf_levels <= 0) H5_PRIV_API_LEAVE (H5_SUCCESS);

	h5_loc_idx_t loc_idx = t->leaf_level > 0 ? t->num_vertices[t->leaf_level-1] : 0;
	h5_loc_idx_t num_loc_vertices = t->num_vertices[t->num_leaf_levels-1];
	h5_idxmap_el_t *item = &t->map_vertex_g2l.items[loc_idx];

	for (; loc_idx < num_loc_vertices; loc_idx++, item++) {
		item->glb_idx = t->vertices[loc_idx].idx; 
		item->loc_idx = loc_idx;
		t->map_vertex_g2l.num_items++;
	}
	h5priv_sort_idxmap (&t->map_vertex_g2l);
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

/*
  Rebuild mapping of global element indices to their local indices.
*/
h5_err_t
h5tpriv_rebuild_elem_indices_mapping (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_loc_idx_t, "f=0x%p", f);
	h5t_fdata_t* t = f->t;
	if (t->num_leaf_levels <= 0) H5_PRIV_API_LEAVE (H5_SUCCESS);

	h5_loc_idx_t loc_idx = t->leaf_level > 0 ? t->num_elems[t->leaf_level-1] : 0;
	h5_loc_idx_t num_loc_elems = t->num_elems[t->num_leaf_levels-1];
	h5_idxmap_el_t *item = &t->map_elem_g2l.items[loc_idx];

	for (; loc_idx < num_loc_elems; loc_idx++, item++) {
		item->glb_idx = h5tpriv_get_loc_elem_glb_idx (f, loc_idx);
		item->loc_idx = loc_idx;
		t->map_elem_g2l.num_items++;
	}
	h5priv_sort_idxmap (&t->map_elem_g2l);
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

/*
  Get local vertex indices of entity given by it's local ID.
 */
h5_err_t
h5t_get_vertex_indices_of_entity (
	h5_file_t* const f,		// in
	const h5_loc_id_t entity_id,	// in
	h5_loc_idx_t* vertex_indices   	// out
	) {
	H5_CORE_API_ENTER3 (h5_loc_idx_t,
			    "f=0x%p, entity_id=%llu, vertex_indices=0x%p",
			    f,
			    (long long unsigned)entity_id,
			    vertex_indices);
	h5_loc_idx_t type = h5tpriv_get_entity_type (entity_id);
	h5_loc_idx_t face_idx = h5tpriv_get_face_idx (entity_id);
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	int dim = 0;
	switch (type) {
	case H5T_TYPE_VERTEX:   dim = 0; break;
	case H5T_TYPE_EDGE:     dim = 1; break;
	case H5T_TYPE_TRIANGLE: dim = 2; break;
	case H5T_TYPE_TET:      dim = 3; break;
	default:
		H5_CORE_API_LEAVE (h5_error_internal ());
	}
	H5_CORE_API_RETURN (h5t_get_vertex_indices_of_entity2 (f, dim, face_idx, elem_idx, vertex_indices));
}

/*
  Get local vertex indices of entity given by it's face and local element index.
 */
h5_err_t
h5t_get_vertex_indices_of_entity2 (
	h5_file_t* const f,		// [in]
	const int dim,			// [in] dimension
	const h5_loc_idx_t face_idx,	// [in] vertex index according ref. element
	const h5_loc_idx_t elem_idx,	// [in] local element index
	h5_loc_idx_t* vertex_indices   	// [out]
	) {
	H5_CORE_API_ENTER5 (h5_err_t,
			    "f=0x%p, dim=%d, face_idx=%llu, elem_idx=%llu, vertex_indices=0x%p",
			    f, dim,
			    (long long unsigned)face_idx,
			    (long long unsigned)elem_idx,
			    vertex_indices);
	h5_loc_idx_t* indices = h5tpriv_get_loc_elem_vertex_indices (f, elem_idx);
	const h5t_ref_elem_t* ref_elem = f->t->ref_elem;
	int num_vertices = ref_elem->num_vertices_of_face[dim][face_idx];
	int i;
	for (i = 0; i < num_vertices; i++) {
		int idx = h5tpriv_ref_elem_get_vertex_idx(f->t, dim, face_idx, i);
		vertex_indices[i] = indices[idx];
	}
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5t_get_vertex_index_of_vertex (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idx_t* vertex_index
	) {
	H5_CORE_API_ENTER3 (h5_loc_idx_t,
			    "f=0x%p, entity_id=%llu, vertex_index=%llu",
			    f,
			    (long long unsigned)entity_id,
			    (long long unsigned)*vertex_index);
	h5_loc_idx_t face_idx = h5tpriv_get_face_idx (entity_id);
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	H5_CORE_API_RETURN (h5t_get_vertex_index_of_vertex2 (
				    f, face_idx, elem_idx, vertex_index));
}

h5_err_t
h5t_get_vertex_index_of_vertex2 (
	h5_file_t* const f,
	const h5_loc_idx_t face_idx,	// vertex index according ref. element
	const h5_loc_idx_t elem_idx,	// local element index
	h5_loc_idx_t* vertex_indices	// OUT: vertex ID's
	) {
	H5_CORE_API_ENTER4 (h5_err_t,
			    "f=0x%p, face_idx=%llu, elem_idx=%llu, vertex_indices=0x%p",
			    f,
			    (long long unsigned)face_idx,
			    (long long unsigned)elem_idx,
			    vertex_indices);
	vertex_indices[0] = h5tpriv_get_loc_elem_vertex_idx (f, elem_idx, face_idx); 
	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*
  Get the local ID of the vertices of an elemet.
 */
h5_err_t
h5t_get_vertex_indices_of_edge (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idx_t* vertex_indices
	) {
	H5_CORE_API_ENTER3 (h5_loc_idx_t,
			    "f=0x%p, entity_id=%llu, vertex_indices=0x%p",
			    f,
			    (long long unsigned)entity_id,
			    vertex_indices);
	h5_loc_idx_t face_idx = h5tpriv_get_face_idx (entity_id);
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	H5_CORE_API_RETURN (h5t_get_vertex_indices_of_edge2 (
				    f, face_idx, elem_idx, vertex_indices));
}

/*!
  Get local vertex indices of an edge. The edge is specified by the local 
  element index and the face number of the edge according the reference
  element.

  This function can be used with tetrahedral and triangle meshes.
 */
h5_err_t
h5t_get_vertex_indices_of_edge2 (
	h5_file_t* const f,
	const h5_loc_idx_t face_idx,	// edge index according ref. element
	const h5_loc_idx_t elem_idx,	// local element index
	h5_loc_idx_t* vertex_indices	// OUT: vertex indices
	) {
	H5_CORE_API_ENTER4 (h5_err_t,
			    "f=0x%p, face_idx=%llu, elem_idx=%llu, vertex_indices=0x%p",
			    f,
			    (long long unsigned)face_idx,
			    (long long unsigned)elem_idx,
			    vertex_indices);
	const h5_loc_idx_t* indices = h5tpriv_get_loc_elem_vertex_indices (f, elem_idx);

	h5_loc_idx_t idx;
	idx = h5tpriv_ref_elem_get_vertex_idx (f->t, 1, face_idx, 0); 
 	vertex_indices[0] = indices[idx];
	idx = h5tpriv_ref_elem_get_vertex_idx (f->t, 1, face_idx, 1);
	vertex_indices[1] = indices[idx];
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5t_get_vertex_indices_of_triangle (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idx_t* vertex_indices
	) {
	H5_CORE_API_ENTER3 (h5_loc_idx_t,
			    "f=0x%p, entity_id=%llu, vertex_indices=0x%p",
			    f,
			    (long long unsigned)entity_id,
			    vertex_indices);
	h5_loc_idx_t face_idx = h5tpriv_get_face_idx (entity_id);
	h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	H5_CORE_API_RETURN (h5t_get_vertex_indices_of_triangle2 (
				    f, face_idx, elem_idx, vertex_indices));
}

h5_err_t
h5t_get_vertex_indices_of_triangle2 (
	h5_file_t* const f,
	const h5_loc_idx_t face_idx,
	const h5_loc_idx_t elem_idx,
	h5_loc_idx_t* vertex_indices
	) {
	H5_CORE_API_ENTER4 (h5_err_t,
			    "f=0x%p, face_idx=%llu, elem_idx=%llu, vertex_indices=0x%p",
			    f,
			    (long long unsigned)face_idx,
			    (long long unsigned)elem_idx,
			    vertex_indices);
	const h5_loc_idx_t* indices = h5tpriv_get_loc_elem_vertex_indices (f, elem_idx);

	h5_loc_idx_t idx;
	idx = h5tpriv_ref_elem_get_vertex_idx (f->t, 2, face_idx, 0); 
 	vertex_indices[0] = indices[idx];
	idx = h5tpriv_ref_elem_get_vertex_idx (f->t, 2, face_idx, 1);
	vertex_indices[1] = indices[idx];
	idx = h5tpriv_ref_elem_get_vertex_idx (f->t, 2, face_idx, 2);
	vertex_indices[2] = indices[idx];
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5t_get_vertex_indices_of_tet (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idx_t* vertex_indices
	) {
	H5_CORE_API_ENTER3 (h5_loc_idx_t,
			    "f=0x%p, entity_id=%llu, vertex_indices=0x%p",
			    f,
			    (long long unsigned)entity_id,
			    vertex_indices);

	const h5_loc_idx_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	const h5_loc_idx_t* indices = h5tpriv_get_loc_elem_vertex_indices (
		f, elem_idx);

	h5_loc_idx_t idx;
	idx = h5tpriv_ref_elem_get_vertex_idx (f->t, 3, 0, 0); 
 	vertex_indices[0] = indices[idx];
	idx = h5tpriv_ref_elem_get_vertex_idx (f->t, 3, 0, 1);
	vertex_indices[1] = indices[idx];
	idx = h5tpriv_ref_elem_get_vertex_idx (f->t, 3, 0, 2);
	vertex_indices[2] = indices[idx];
	idx = h5tpriv_ref_elem_get_vertex_idx (f->t, 3, 0, 3);
	vertex_indices[3] = indices[idx];
	H5_CORE_API_RETURN (H5_SUCCESS);
}
