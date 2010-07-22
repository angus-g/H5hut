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

static int
qsort_cmp_vertices (
	void* _f,
	const void* _local_vid1,
	const void* _local_vid2
	) {
	h5_file_t* f = (h5_file_t*)_f;
	h5_id_t local_vid1 = *(h5_id_t*)_local_vid1;
	h5_id_t local_vid2 = *(h5_id_t*)_local_vid2;

	return cmp_vertices (
		f->t->vertices[local_vid1].P,
		f->t->vertices[local_vid2].P );
}

/*!
  Sort vertices. Store local id's in a sorted array for binary search. 
*/
h5_err_t
h5tpriv_sort_vertices (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;
	if (t->num_levels <= 0) return H5_SUCCESS;

	h5_id_t local_vid = t->cur_level > 0 ?
		t->num_vertices[t->cur_level-1] : 0;

	h5_id_t num_vertices = t->num_vertices[t->num_levels-1];
	for (; local_vid < num_vertices; local_vid++) {
		t->sorted_lvertices.items[local_vid] = local_vid;
	}
	t->sorted_lvertices.num_items = num_vertices;

	h5priv_qsort_r (
		t->sorted_lvertices.items,
		num_vertices,
		sizeof (t->sorted_lvertices.items[0]),
		f,
		qsort_cmp_vertices);

	return H5_SUCCESS;
}


/*!
  Return local vertex id of a vertex given by its coordinates.

  \return	local vertex id if found
  \return	else negativ value
 */
h5_id_t
h5tpriv_get_local_vid (
	h5_file_t* const f,
	h5_float64_t P[3]
	) {
	h5t_fdata_t*t = f->t;
	register h5_id_t low = 0;
	register h5_id_t high = t->sorted_lvertices.num_items - 1;
	while (low <= high) {
		register int mid = (low + high) / 2;

		h5_id_t local_vid = t->sorted_lvertices.items[mid];
		h5_float64_t *P1 = t->vertices[local_vid].P;
	        int diff = cmp_vertices ( P, P1 );
           	if ( diff < 0 )
               		high = mid - 1;
           	else if ( diff > 0 )
               		low = mid + 1;
           	else
               		return t->sorted_lvertices.items[mid]; // found
       	}
       	return -(low+1);  // not found
}

/*!
  compare two elems given by their local id
*/
static int
cmp_elems (
	h5_file_t* const  f,
	const h5_id_t elem_idx1,
	const h5_id_t elem_idx2
	) {
	h5t_fdata_t* t = f->t;
	int num_vertices = t->ref_elem->num_faces[0];
	h5_id_t* indices1 = h5tpriv_get_loc_elem_vertex_indices (f, elem_idx1);
	h5_id_t* indices2 = h5tpriv_get_loc_elem_vertex_indices (f, elem_idx2);

	int i;
	for (i = 0; i < num_vertices; i++) {
		h5_float64_t* v1 = t->vertices[indices1[i]].P;
		h5_float64_t* v2 = t->vertices[indices2[i]].P;
		int r = cmp_vertices (v1, v2);
		if (r < 0)
			return -1;
		else if (r > 0)
			return 1;
	}
	return 0;
}

static int
cmp_elems1 (
	h5_file_t* f,
	h5_id_t	elem_idx1,
	h5_id_t elem_idx2
	) {
	h5t_fdata_t *t = f->t;
	int num_vertices = t->ref_elem->num_faces[0];
	h5_id_t* indices1 = h5tpriv_get_loc_elem_vertex_indices (f, elem_idx1);
	h5_id_t* indices2 = h5tpriv_get_loc_elem_vertex_indices (f, elem_idx2);

	int imap[] = { 1, 0, 2, 3 };
	int i;
	for ( i = 0; i < num_vertices; i++ ) {
		h5_float64_t* v1 = t->vertices[ indices1[imap[i]] ].P;
		h5_float64_t* v2 = t->vertices[ indices2[imap[i]] ].P;
		int r = cmp_vertices (v1, v2);
		if (r < 0)
			return -1;
		else if (r > 0)
			return 1;
	}
	return 0;
}


static int
qsort_cmp_elems0 (
	void* _f,
	const void* _local_eid1,
	const void* _local_eid2
	) {
	h5_file_t* f = (h5_file_t*)_f;
	h5_id_t local_eid1 = *(h5_id_t*)_local_eid1;
	h5_id_t local_eid2 = *(h5_id_t*)_local_eid2;
	return cmp_elems (f, local_eid1, local_eid2);
}

static int
qsort_cmp_elems1 (
	void* _f,
	const void* _local_eid1,
	const void* _local_eid2
	) {
	h5_file_t* f = (h5_file_t*)_f;
	h5_id_t local_eid1 = *(h5_id_t*)_local_eid1;
	h5_id_t local_eid2 = *(h5_id_t*)_local_eid2;
	return cmp_elems1 (f, local_eid1, local_eid2);
}

/*!
  Sort elements. Store local id's in a sorted array so we can run a
  binary search. 
*/
h5_err_t
h5tpriv_sort_elems (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;
	if (t->num_levels <= 0) return H5_SUCCESS;
	h5_id_t local_eid = t->cur_level > 0 ? t->num_elems[t->cur_level-1] : 0;
	h5_size_t num_elems = t->num_elems[t->num_levels-1];

	int k;
	h5_id_t i;
	for (k = 0; k < 2; k++) {
		TRY( h5priv_alloc_idlist_items (f, &t->sorted_elems[k], num_elems) );
		for (i = local_eid; i < num_elems; i++) {
			t->sorted_elems[k].items[i] = i;
		}
		t->sorted_elems[k].num_items = num_elems;
	}

	h5priv_qsort_r (
		t->sorted_elems[0].items,
		num_elems,
		sizeof (t->sorted_elems[0].items[0]),
		f,    
		qsort_cmp_elems0);
	h5priv_qsort_r (
		t->sorted_elems[1].items,
		num_elems,
		sizeof (t->sorted_elems[1].items[0]),
		f,
		qsort_cmp_elems1);

	return H5_SUCCESS;
}

/*!
  Sort (small) array of local vertex indices geometrically. 
 */
h5_err_t
h5tpriv_sort_local_vertex_indices (
	h5_file_t* const f,
	h5_id_t* const indices,		/* IN/OUT: local vertex indices */	
	const h5_size_t size		/* size of array */
	) {
	h5t_fdata_t* t = f->t;

	h5_size_t i;
	for (i = 1; i < size; ++i) {
		h5_id_t idx = indices[i];

		h5_id_t j = i;
		while ((j >= 1 ) && cmp_vertices (
			       t->vertices[idx].P,
			       t->vertices[indices[j-1]].P 
			       ) < 0 ) {
			indices[j] = indices[j-1];
			--j;
		}
		indices[j] = idx;
	}
	return H5_SUCCESS;
}

/*!
  Map a global vertex index to corresponding local index.
*/
h5_id_t
h5t_map_global_vertex_idx2local (
	h5_file_t* const f,
	const h5_id_t global_idx
	) {
	h5t_fdata_t* t = f->t;

	h5_id_t local_idx = h5priv_search_idmap (&t->map_vertex_g2l, global_idx);
	if (local_idx < 0) 
		return h5tpriv_error_global_id_nexist (f, "vertex", global_idx);
	return local_idx;
}

h5_err_t
h5t_map_global_vertex_indices2local (
	h5_file_t* const f,
	const h5_id_t* const global_indices,
	const h5_id_t size,
	h5_id_t* const local_indices
	) {
	h5_id_t i;
	for (i = 0; i < size; i++) {
		TRY( (local_indices[i] = h5t_map_global_vertex_idx2local (
				f, global_indices[i])) );
	}
	return H5_SUCCESS;
}

/*!
  Get local element idx of element given by its global idx.

  \param[in]	f		File handle
  \param[in]	global_idx	Global element id

  \return	Local element id or error code.
*/
h5_id_t
h5t_map_global_elem_idx2local (
	h5_file_t* const f,
	const h5_id_t global_idx
	) {
	h5t_fdata_t* t = f->t;
	h5_id_t local_idx = h5priv_search_idmap (&t->map_elem_g2l, global_idx);
	if (local_idx < 0) 
		return h5tpriv_error_global_id_nexist (f, "elem", global_idx);
	return local_idx;
}

h5_err_t
h5tpriv_rebuild_global_2_local_map_of_vertices (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;
	if (t->num_levels <= 0) return H5_SUCCESS;
	h5_id_t local_vid = t->cur_level > 0 ?
		t->num_vertices[t->cur_level-1] : 0;
	for (; local_vid < t->num_vertices[t->num_levels-1]; local_vid++) {
		t->map_vertex_g2l.items[local_vid].global_id =
			t->vertices[local_vid].global_idx; 
		t->map_vertex_g2l.items[local_vid].local_id = local_vid;
		t->map_vertex_g2l.num_items++;
	}
	h5priv_sort_idmap (&t->map_vertex_g2l);

	return H5_SUCCESS;
}

h5_err_t
h5tpriv_rebuild_global_2_local_map_of_elems (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;
	if (t->num_levels <= 0) return H5_SUCCESS;

	h5_id_t idx = t->cur_level > 0 ? t->num_elems[t->cur_level-1] : 0;
	h5_id_t num_elems = t->num_elems[t->num_levels-1];
	h5_idmap_el_t *item = &t->map_elem_g2l.items[idx];

	for (; idx < num_elems; idx++, item++) {
		item->global_id = h5tpriv_get_glb_elem_idx (f, idx);
		item->local_id = idx;
		t->map_elem_g2l.num_items++;
	}
	h5priv_sort_idmap (&t->map_elem_g2l);

	return H5_SUCCESS;
}


h5_err_t
h5t_get_vertex_indices_of_entity (
	h5_file_t* const f,		// in
	const h5_id_t entity_id,	// in
	h5_id_t* vertex_indices		// out
	) {
	static int map_entity_type_to_dimension[] = {
		-1,
		[H5_OID_VERTEX] = 0,
		[H5_OID_EDGE] = 1,
		[H5_OID_TRIANGLE] = 2,
		[H5_OID_TETRAHEDRON] = 3
	};

	h5_id_t entity_type = h5tpriv_get_entity_type (entity_id);
	h5_id_t face_idx = h5tpriv_get_face_idx (entity_id);
	h5_id_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	int dim =  map_entity_type_to_dimension[entity_type];
	h5_id_t* indices = h5tpriv_get_loc_elem_vertex_indices (f, elem_idx);
	const h5t_ref_elem_t* ref_elem = f->t->ref_elem;
	int num_vertices = ref_elem->num_vertices_of_face[dim][face_idx];
	int i;
	for (i = 0; i < num_vertices; i++) {
		int idx = ref_elem->map[dim][face_idx][i];
		vertex_indices[i] = indices[idx];
	}
	return H5_SUCCESS;
}

h5_err_t
h5t_get_vertex_index_of_vertex (
	h5_file_t* const f,
	const h5_id_t entity_id,
	h5_id_t* vertex_index
	) {
	h5_id_t face_idx = h5tpriv_get_face_idx (entity_id);
	h5_id_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	return h5t_get_vertex_index_of_vertex2 (
		f, face_idx, elem_idx, vertex_index);
}

h5_err_t
h5t_get_vertex_index_of_vertex2 (
	h5_file_t* const f,
	const h5_id_t face_idx,		// vertex index according ref. element
	const h5_id_t elem_idx,		// local element index
	h5_id_t* vertex_indices		// OUT: vertex ID's
	) {
	vertex_indices[0] = h5tpriv_get_loc_elem_vertex_idx (f, elem_idx, face_idx); 
	return H5_SUCCESS;
}

/*
  Get the local ID of the vertices of an elemet.
 */
h5_err_t
h5t_get_vertex_indices_of_edge (
	h5_file_t* const f,
	const h5_id_t entity_id,
	h5_id_t* vertex_indices
	) {
	h5_id_t face_idx = h5tpriv_get_face_idx (entity_id);
	h5_id_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	return h5t_get_vertex_indices_of_edge2 (
		f, face_idx, elem_idx, vertex_indices);
}

/*!
  Get local vertex ID's of an edge. The edge is specified by the local 
  element index and the sub-entity ID of the edge according the reference
  element.

  This function can be used with tetrahedral and triangle meshes.
 */
h5_err_t
h5t_get_vertex_indices_of_edge2 (
	h5_file_t* const f,
	const h5_id_t face_idx,		// edge index according ref. element
	const h5_id_t elem_idx,		// local element index
	h5_id_t* vertex_indices		// OUT: vertex ID's
	) {
	const h5_id_t* indices = h5tpriv_get_loc_elem_vertex_indices (f, elem_idx);
	const h5t_ref_elem_t* ref_elem = f->t->ref_elem;

 	vertex_indices[0] = indices[ ref_elem->map[1][face_idx][0] ];
	vertex_indices[1] = indices[ ref_elem->map[1][face_idx][1] ];
	return H5_SUCCESS;
}

h5_err_t
h5t_get_vertex_indices_of_triangle (
	h5_file_t* const f,
	const h5_id_t entity_id,
	h5_id_t* vertex_indices
	) {
	h5_id_t face_idx = h5tpriv_get_face_idx (entity_id);
	h5_id_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	return h5t_get_vertex_indices_of_triangle2 (
		f, face_idx, elem_idx, vertex_indices);
}

h5_err_t
h5t_get_vertex_indices_of_triangle2 (
	h5_file_t* const f,
	const h5_id_t face_idx,
	const h5_id_t elem_idx,
	h5_id_t* vertex_indices
	) {
	const h5_id_t* indices = h5tpriv_get_loc_elem_vertex_indices (f, elem_idx);
	const h5t_ref_elem_t* ref_elem = f->t->ref_elem;

 	vertex_indices[0] = indices[ ref_elem->map[2][face_idx][0] ];
	vertex_indices[1] = indices[ ref_elem->map[2][face_idx][1] ];
	vertex_indices[2] = indices[ ref_elem->map[2][face_idx][2] ];

	return H5_SUCCESS;
}

h5_err_t
h5t_get_vertex_indices_of_tet (
	h5_file_t* const f,
	const h5_id_t entity_id,
	h5_id_t* vertex_indices
	) {
	const h5_id_t elem_idx = h5tpriv_get_elem_idx (entity_id);
	const h5_id_t* indices = h5tpriv_get_loc_elem_vertex_indices (f, elem_idx);
	const h5t_ref_elem_t* ref_elem = f->t->ref_elem;

 	vertex_indices[0] = indices[ ref_elem->map[3][0][0] ];
	vertex_indices[1] = indices[ ref_elem->map[3][0][1] ];
	vertex_indices[2] = indices[ ref_elem->map[3][0][2] ];
	vertex_indices[3] = indices[ ref_elem->map[3][0][3] ];

	return H5_SUCCESS;
}
