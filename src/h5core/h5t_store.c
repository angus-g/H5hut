#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*!
  Add new mesh

  \return mesh id
*/
h5_id_t
h5t_add_tetrahedral_mesh (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_id_t, "f=0x%p", f);
	h5_id_t mesh_id = 0;
	TRY (mesh_id = h5t_open_tetrahedral_mesh (f, -1)); 
	TRY (h5t_add_level (f));
	f->t->mesh_changed = 1;
	H5_CORE_API_RETURN (mesh_id);
}

h5_id_t
h5t_add_triangle_mesh (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_id_t, "f=0x%p", f);
	h5_id_t mesh_id = 0;
	TRY (mesh_id = h5t_open_triangle_mesh (f, -1)); 
	TRY (h5t_add_level (f));
	f->t->mesh_changed = 1;
	H5_CORE_API_RETURN (mesh_id);
}

/*
  Assign unique global indices to vertices.
*/
static h5_err_t
assign_global_vertex_indices (
	h5_file_t* const f
	) {
	h5t_fdata_t* const t = f->t;

	if (t->leaf_level < 0) return H5_SUCCESS; /* no level defined */

	/*
	  simple in serial runs: global_id = local_id
	*/
	h5_loc_idx_t local_idx = (t->leaf_level == 0) ?
		0 : t->num_vertices[t->leaf_level-1];
	for (local_idx = 0;
	     local_idx < t->num_vertices[t->num_leaf_levels-1];
	     local_idx++) {
		t->vertices[local_idx].idx = local_idx;
	}

	return H5_SUCCESS;
}

/*!
 Assign unique global indices to new elements.
*/
static h5_err_t
assign_glb_elem_indices (
	h5_file_t* const f
	) {
	h5t_fdata_t* const t = f->t;

	if (t->leaf_level < 0) return H5_SUCCESS; /* no level defined */

	/*
	  simple in serial runs: global index = local index
	*/
	h5_loc_idx_t loc_idx = (t->leaf_level == 0) ? 0 : t->num_elems[t->leaf_level-1];
	
	for (; loc_idx < t->num_elems[t->leaf_level]; loc_idx++) {
		h5tpriv_set_loc_elem_glb_idx (f, loc_idx, loc_idx);
	}

	return H5_SUCCESS;
}



h5t_lvl_idx_t
h5t_add_level (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5t_lvl_idx_t, "f=0x%p", f);
	h5t_fdata_t* const t = f->t;

	if (f->mode == H5_O_RDONLY) {
		H5_CORE_API_LEAVE (h5priv_handle_file_mode_error(f->mode));
	}

	/* t->num_leaf_levels will be set to zero on file creation(!) */
	if ((t->cur_mesh < 0) || (t->num_leaf_levels == -1)) {
		H5_CORE_API_LEAVE (h5tpriv_error_undef_mesh ());
	}
	t->leaf_level = t->num_leaf_levels++;
	t->num_loaded_levels = t->num_leaf_levels;
	t->dsinfo_num_vertices.dims[0] = t->num_leaf_levels;
	t->dsinfo_num_elems.dims[0] = t->num_leaf_levels;
	t->dsinfo_num_elems_on_leaf_level.dims[0] = t->num_leaf_levels;

	ssize_t num_bytes = t->num_leaf_levels*sizeof (h5_size_t);
	TRY (t->num_vertices = h5_alloc (t->num_vertices, num_bytes));
	t->num_vertices[t->leaf_level] = -1;

	TRY (t->num_elems = h5_alloc (t->num_elems, num_bytes));
	t->num_elems[t->leaf_level] = -1;
	TRY ( t->num_elems_on_leaf_level = h5_alloc (
		     t->num_elems_on_leaf_level, num_bytes));
	t->num_elems_on_leaf_level[t->leaf_level] = -1;

	if (t->leaf_level == 0) {
		/* nothing stored yet */
		t->last_stored_vid = -1;
		t->last_stored_eid = -1;
	}

	H5_CORE_API_RETURN (t->leaf_level);
}

/*!
  Allocate memory for (more) vertices.
*/
h5_err_t
h5t_begin_store_vertices (
	h5_file_t* const f,
	const h5_size_t num
	) {
	H5_CORE_API_ENTER2 (h5_err_t,
			   "f=0x%p, num=%llu",
			   f, (long long unsigned)num);
	h5t_fdata_t* const t = f->t;

	if (t->leaf_level < 0) {
		H5_CORE_API_LEAVE (h5tpriv_error_undef_level());
	}
	h5_size_t cur_num_vertices = (t->leaf_level > 0 ?
				      t->num_vertices[t->leaf_level-1] : 0);
	t->num_vertices[t->leaf_level] = cur_num_vertices+num;
	t->dsinfo_vertices.dims[0] = cur_num_vertices+num;
	H5_CORE_API_RETURN (h5tpriv_alloc_num_vertices (f, cur_num_vertices+num));
}

h5_loc_idx_t
h5t_store_vertex (
	h5_file_t* const f,		/*!< file handle		*/
	const h5_glb_idx_t glb_id,	/*!< global vertex id from mesher or -1	*/
	const h5_float64_t P[3]		/*!< coordinates		*/
	) {
	H5_CORE_API_ENTER3 (h5_loc_idx_t,
			    "f=0x%p, glb=id=%lld, P=0x%p",
			    f,
			    (long long unsigned)glb_id,
			    P);
	h5t_fdata_t* const t = f->t;
	
	/*
	  more than allocated
	*/
	if (t->last_stored_vid+1 >= t->num_vertices[t->leaf_level]) 
		H5_CORE_API_LEAVE (HANDLE_H5_OVERFLOW_ERR(
					   t->num_vertices[t->leaf_level]));
	
	/*
	  missing call to add the first level
	 */
	if (t->leaf_level < 0)
		H5_CORE_API_LEAVE (h5tpriv_error_undef_level());

	h5_loc_idx_t local_idx = ++t->last_stored_vid;
	h5_loc_vertex_t *vertex = &t->vertices[local_idx];
	vertex->idx = glb_id;     /* ID from mesher, replaced later!*/
	memcpy (&vertex->P, P, sizeof (vertex->P));
	H5_CORE_API_RETURN (local_idx);
}

h5_err_t
h5t_end_store_vertices (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_err_t, "f=0x%p", f);
	h5t_fdata_t* const t = f->t;

	t->num_vertices[t->leaf_level] = t->last_stored_vid+1;
	TRY (assign_global_vertex_indices (f));
	TRY (h5tpriv_rebuild_vertex_indices_mapping (f));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*!
  Initialize everything so that we can begin to store elements.

  \param[in]	f	file handle
  \param[in]	num	number of elements to add
 */
h5_err_t
h5t_begin_store_elems (
	h5_file_t* const f,
	const h5_size_t num
	) {
	H5_CORE_API_ENTER2 (h5_err_t,
			   "f=0x%p, num=%llu",
			   f, (long long unsigned)num);
	h5t_fdata_t* const t = f->t;

	size_t cur = t->leaf_level > 0 ? t->num_elems[t->leaf_level-1] : 0;
	size_t new = num + cur;
	t->num_elems[t->leaf_level] = new;
	t->dsinfo_elems.dims[0] = new;

	t->num_elems_on_leaf_level[t->leaf_level] = t->leaf_level > 0 ?
		num + t->num_elems_on_leaf_level[t->leaf_level-1] : num;
	/*
	  We allocate a hash table for a minimum of 2^21 edges to
	  avoid resizing.
	 */
	size_t nel = 2097152 > 5*new ? 2097152 : 5*new;
	TRY (h5tpriv_resize_te_htab (f, nel));
	H5_CORE_API_RETURN (h5tpriv_alloc_elems (f, cur, new));
}


/*!
  Store element. The vertices are given via their local indices.

  \param[in]	f			File handle.
  \param[in]	elem_idx_of_parent	Local indexd of the parent element
					or \c -1.
  \param[in]	vertices		Local vertex indices defining the
					tetrahedron.
 */
h5_loc_idx_t
h5t_store_elem (
	h5_file_t* const f,
	const h5_loc_idx_t parent_idx,
	const h5_loc_idx_t* vertex_indices
	) {
	H5_CORE_API_ENTER3 (h5_loc_idx_t,
			    "f=0x%p, parent_idx=%lld, vertex_indices=0x%p",
			    f,
			    (long long)parent_idx,
			    vertex_indices);
	h5t_fdata_t* t = f->t;

	/* level set? */
	if (t->leaf_level < 0)
		H5_CORE_API_LEAVE (
			h5tpriv_error_undef_level());

	/*  more than allocated? */
	if ( t->last_stored_eid+1 >= t->num_elems[t->leaf_level] ) 
		H5_CORE_API_LEAVE (
			HANDLE_H5_OVERFLOW_ERR (t->num_elems[t->leaf_level]));

	/* check parent id */
	if ((t->leaf_level == 0 && parent_idx != -1) ||
	    (t->leaf_level >  0 && parent_idx < 0) ||
	    (t->leaf_level >  0
	     && parent_idx >= t->num_elems[t->leaf_level-1])
		) {
		H5_CORE_API_LEAVE (
			HANDLE_H5_PARENT_ID_ERR (parent_idx));
	}

	/* store elem data (but neighbors) */
	h5_loc_idx_t elem_idx = ++t->last_stored_eid;
	h5tpriv_set_loc_elem_parent_idx (f, elem_idx, parent_idx);
	h5tpriv_set_loc_elem_child_idx (f, elem_idx, -1);
	h5tpriv_set_loc_elem_level_idx (f, elem_idx, t->leaf_level);

	// get ptr to local vertices store
	h5_loc_idx_t* loc_vertex_indices = h5tpriv_get_loc_elem_vertex_indices (
		f, elem_idx);
	int num_vertices = h5tpriv_ref_elem_get_num_vertices (t);
	memcpy (loc_vertex_indices, vertex_indices,
		sizeof (*vertex_indices)*num_vertices);
	h5tpriv_sort_local_vertex_indices (f, loc_vertex_indices, num_vertices);

	/* add edges to map  edges -> elements */
	h5_loc_idx_t face_idx;
	int num_faces = h5tpriv_ref_elem_get_num_edges (t);
	for (face_idx = 0; face_idx < num_faces; face_idx++) {
		// add edges to neighbour struct
		TRY (h5tpriv_search_te2 (f, face_idx, elem_idx, NULL));
	}
	H5_CORE_API_RETURN (elem_idx);
}

h5_err_t
h5t_end_store_elems (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_err_t, "f=0x%p", f);
	h5t_fdata_t* const t = f->t;

	t->num_elems[t->leaf_level] = t->last_stored_eid+1;

	/* assign global indices to new indices */
	TRY (assign_glb_elem_indices (f));

	/* rebuild map: global index -> local_index */
	TRY (h5tpriv_rebuild_elem_indices_mapping (f));

	/* mesh specific finalize */
	TRY (t->methods.store->end_store_elems (f));

	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*
  Mark entity for further processing (e.g. refinement). 
 */
h5_err_t
h5t_mark_entity (
	h5_file_t* const f,
	const h5_loc_id_t entity_id
	) {
	H5_CORE_API_ENTER2 (h5_err_t, "f=0x%p, entity_id=%llu",
			    f, (long long unsigned)entity_id);
	h5t_fdata_t* const t = f->t;
	H5_CORE_API_RETURN (h5priv_insert_idlist (&t->marked_entities, entity_id, -1));
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
h5_err_t
h5t_pre_refine (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_err_t, "f=0x%p", f);
	h5t_fdata_t* const t = f->t;
	unsigned int num_elems_to_refine = t->marked_entities->num_items;
	unsigned int num_elems_to_add = 0;
	unsigned int num_vertices_to_add = 0;

	switch (t->mesh_type) {
	case H5_OID_TETRAHEDRON:
		num_vertices_to_add = num_elems_to_refine*3 + 192;
		num_elems_to_add = num_elems_to_refine*8;
		break;
	case H5_OID_TRIANGLE:
		num_vertices_to_add = num_elems_to_refine*2 + 64;
		num_elems_to_add = num_elems_to_refine*4;
		break;
	default:
		H5_CORE_API_LEAVE (h5_error_internal ());
	}
	TRY (h5t_begin_store_vertices (f, num_vertices_to_add));
	TRY (h5t_begin_store_elems (f, num_elems_to_add));

	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*
  Refine previously marked elements.
*/
h5_err_t
h5t_refine_marked_elems (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_err_t, "f=0x%p", f);
	h5t_fdata_t* const t = f->t;
	int i;
	for (i = 0; i < t->marked_entities->num_items; i++) {
		TRY (h5tpriv_refine_elem (f, t->marked_entities->items[i]));
	}
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5t_post_refine (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_err_t, "f=0x%p", f);
	h5t_fdata_t* const t = f->t;
	TRY (h5t_end_store_vertices (f));
	TRY (h5t_end_store_elems (f));
	H5_CORE_API_RETURN (h5priv_free_idlist (&t->marked_entities));
}


h5_err_t
h5t_begin_refine_elems (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_err_t, "f=0x%p", f);
	h5t_fdata_t* const t = f->t;

	/*
	  Pre-allocate space for items to avoid allocating small pieces of
	  memory.
	*/
	TRY (h5priv_alloc_idlist (&t->marked_entities, 2048));
	H5_CORE_API_RETURN (H5_SUCCESS);
}


h5_err_t
h5t_end_refine_elems (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_err_t, "f=0x%p", f);
	TRY (h5t_pre_refine (f));
	TRY (h5t_refine_marked_elems (f));
	TRY (h5t_post_refine (f));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5t_create_index_set (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_err_t, "f=0x%p", f);
	int codim;
	int dim = h5tpriv_ref_elem_get_dim (f->t);
	// todo: check tagset already exist
	TRY (h5t_add_mtagset (f, "__IndexSet__", H5_INT64_T));

	for (codim = 0; codim <= dim; codim++) {
		h5_glb_idx_t idx = 0;
		h5t_leaf_iterator_t it;
		h5_glb_id_t entity_id;
		TRY (h5t_init_leaf_iterator (f, (h5t_iterator_t*)&it, codim));
		while ((entity_id = it.iter(f, (h5t_iterator_t*)&it)) >= 0) {
			TRY (h5t_set_mtag_by_name (f, "__IndexSet__", entity_id, 1, &idx));
		}
	}
	H5_CORE_API_RETURN (H5_SUCCESS);
}
