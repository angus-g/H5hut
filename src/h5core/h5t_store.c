#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*!
  Add new mesh

  \return mesh id
*/
h5_id_t
h5t_add_mesh (
	h5_file_t* const f,
	const h5_oid_t mesh_type
	) {
	h5_id_t mesh_id = 0;

	TRY( (mesh_id = h5t_open_mesh (f, -1, mesh_type)) ); 
	TRY( h5t_add_level (f) );
	
	f->t->mesh_changed = 1;

	return mesh_id;
}

/*

 * Assign unique global indices to vertices.
*/
static h5_err_t
assign_global_vertex_indices (
	h5_file_t* const f
	) {
	h5t_fdata_t* const t = f->t;

	if (t->cur_level < 0) return H5_SUCCESS; /* no level defined */

	/*
	  simple in serial runs: global_id = local_id
	*/
	h5_id_t local_idx = (t->cur_level == 0) ? 0 : t->num_vertices[t->cur_level-1];
	for (local_idx = 0;
	     local_idx < t->num_vertices[t->num_levels-1];
	     local_idx++) {
		t->vertices[local_idx].global_idx = local_idx;
	}

	return H5_SUCCESS;
}

/*!
 Assign unique global indices to new elements.
*/
static h5_err_t
assign_global_elem_indices (
	h5_file_t* const f
	) {
	h5t_fdata_t* const t = f->t;

	if (t->cur_level < 0) return H5_SUCCESS; /* no level defined */

	/*
	  simple in serial runs: global index = local index
	*/
	h5_id_t local_idx = (t->cur_level == 0) ? 0 : t->num_elems[t->cur_level-1];
	int num_vertices = t->ref_elem->num_faces[0];
	
	for (; local_idx < t->num_elems[t->cur_level]; local_idx++) {
		h5_generic_elem_t *loc_elem = h5tpriv_get_loc_elem (f, local_idx);
		h5_generic_elem_t *glb_elem = h5tpriv_get_glb_elem (f, local_idx);

		glb_elem->idx = local_idx;
		glb_elem->parent_idx = loc_elem->parent_idx;
		glb_elem->child_idx = loc_elem->child_idx;

		h5_id_t* glb_indices = h5tpriv_get_glb_elem_vertex_indices (f, local_idx);
		h5_id_t* loc_indices = h5tpriv_get_loc_elem_vertex_indices (f, local_idx);

		memcpy (glb_indices, loc_indices, num_vertices*sizeof(*glb_indices));

		glb_indices = h5tpriv_get_glb_elem_neighbor_indices (f, local_idx);
		loc_indices = h5tpriv_get_loc_elem_neighbor_indices (f, local_idx);

		memcpy (glb_indices, loc_indices, num_vertices*sizeof(*glb_indices));

	}

	return H5_SUCCESS;
}

h5_id_t
h5t_add_level (
	h5_file_t* const f
	) {
	h5t_fdata_t* const t = f->t;

	if (f->mode == H5_O_RDONLY) {
		return H5_ERR_INVAL;
	}

	/* t->num_levels will be set to zero on file creation(!) */
	if ((t->cur_mesh < 0) || (t->num_levels == -1)) {
		return h5tpriv_error_undef_mesh (f);
	}
	t->cur_level = t->num_levels++;
	t->num_loaded_levels = t->num_levels;
	t->dsinfo_num_vertices.dims[0] = t->num_levels;
	t->dsinfo_num_elems.dims[0] = t->num_levels;
	t->dsinfo_num_elems_on_level.dims[0] = t->num_levels;

	ssize_t num_bytes = t->num_levels*sizeof (h5_size_t);
	TRY( t->num_vertices = h5priv_alloc (f, t->num_vertices, num_bytes) );
	t->num_vertices[t->cur_level] = -1;

	TRY( t->num_elems = h5priv_alloc (f, t->num_elems, num_bytes) );
	t->num_elems[t->cur_level] = -1;
	TRY( t->num_elems_on_level = h5priv_alloc (
		     f, t->num_elems_on_level, num_bytes) );
	t->num_elems_on_level[t->cur_level] = -1;

	if (t->cur_level == 0) {
		/* nothing stored yet */
		t->last_stored_vid = -1;
		t->last_stored_eid = -1;
	}

	return t->cur_level;
}

/*!
  Allocate memory for (more) vertices.
*/
h5_err_t
h5t_begin_store_vertices (
	h5_file_t* const f,
	const h5_size_t num
	) {
	h5t_fdata_t* const t = f->t;

	if (t->cur_level < 0) {
		return h5tpriv_error_undef_level(f);
	}
	h5_size_t cur_num_vertices = (t->cur_level > 0 ?
				      t->num_vertices[t->cur_level-1] : 0);
	t->num_vertices[t->cur_level] = cur_num_vertices+num;
	t->dsinfo_vertices.dims[0] = cur_num_vertices+num;
	return h5tpriv_alloc_num_vertices (f, cur_num_vertices+num);
}

h5_id_t
h5t_store_vertex (
	h5_file_t* const f,		/*!< file handle		*/
	const h5_id_t global_idx,     	/*!< global vertex id or -1	*/
	const h5_float64_t P[3]		/*!< coordinates		*/
	) {
	h5t_fdata_t* const t = f->t;
	
	/*
	  more than allocated
	*/
	if (t->last_stored_vid+1 >= t->num_vertices[t->cur_level]) 
		return HANDLE_H5_OVERFLOW_ERR(
			f, "vertex", t->num_vertices[t->cur_level]);
	
	/*
	  missing call to add the first level
	 */
	if (t->cur_level < 0)
		return h5tpriv_error_undef_level(f);

	h5_id_t local_idx = ++t->last_stored_vid;
	h5_vertex_t *vertex = &t->vertices[local_idx];
	vertex->global_idx = global_idx;     /* ID from mesher, replaced later!*/
	memcpy (&vertex->P, P, sizeof (vertex->P));
	return local_idx;
}

h5_err_t
h5t_end_store_vertices (
	h5_file_t* const f
	) {
	h5t_fdata_t* const t = f->t;

	t->num_vertices[t->cur_level] = t->last_stored_vid+1;
	TRY( assign_global_vertex_indices (f) );
	TRY( h5tpriv_sort_vertices (f) );
	TRY( h5tpriv_rebuild_global_2_local_map_of_vertices (f) );
	return H5_SUCCESS;
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
	h5t_fdata_t* const t = f->t;

	size_t cur = t->cur_level > 0 ? t->num_elems[t->cur_level-1] : 0;
	size_t new = num + cur;
	t->num_elems[t->cur_level] = new;
	t->dsinfo_elems.dims[0] = new;

	t->num_elems_on_level[t->cur_level] = t->cur_level > 0 ?
		num + t->num_elems_on_level[t->cur_level-1] : num;
	/*
	  We allocate a hash table for a minimum of 2^21 edges to
	  avoid resizing.
	 */
	size_t nel = 2097152 > 5*new ? 2097152 : 5*new;
	TRY( h5tpriv_resize_te_htab (f, nel) );
	return (*t->methods.store->alloc_elems) (f, cur, new);
}


/*!
  Store element. The vertices are given via their local indices.

  \param[in]	f			File handle.
  \param[in]	elem_idx_of_parent	Local indexd of the parent element
					or \c -1.
  \param[in]	vertices		Local vertex indices defining the
					tetrahedron.
 */
h5_id_t
h5t_store_elem (
	h5_file_t* const f,
	const h5_id_t parent_idx,
	const h5_id_t* vertex_indices
	) {
	h5t_fdata_t* t = f->t;

	/* level set? */
	if (t->cur_level < 0)
		return h5tpriv_error_undef_level(f);

	/*  more than allocated? */
	if ( t->last_stored_eid+1 >= t->num_elems[t->cur_level] ) 
		return HANDLE_H5_OVERFLOW_ERR(
			f, h5tpriv_map_oid2str(t->mesh_type),
			t->num_elems[t->cur_level] );

	/* check parent id */
	if ((t->cur_level == 0 && parent_idx != -1) ||
	    (t->cur_level >  0 && parent_idx < 0) ||
	    (t->cur_level >  0
	     && parent_idx >= t->num_elems[t->cur_level-1])
		) {
		return HANDLE_H5_PARENT_ID_ERR (
			f, h5tpriv_map_oid2str (t->mesh_type), parent_idx);
	}

	/* store elem data (but neighbors) */
	h5_id_t elem_idx = ++t->last_stored_eid;
	h5tpriv_set_loc_elem_parent_idx (f, elem_idx, parent_idx);
	h5tpriv_set_loc_elem_child_idx (f, elem_idx, -1);
	h5tpriv_set_loc_elem_level_idx (f, elem_idx, t->cur_level);

	h5_id_t* loc_elem_vertex_indices = h5tpriv_get_loc_elem_vertex_indices (f, elem_idx);
	int num_vertices = t->ref_elem->num_faces[0];
	memcpy (loc_elem_vertex_indices, vertex_indices, sizeof (*vertex_indices) * num_vertices);
	h5tpriv_sort_local_vertex_indices (f, loc_elem_vertex_indices, num_vertices);

	/* add edges to directory which maps edges to elements */
	h5_id_t face_idx;
	int num_faces = t->ref_elem->num_faces[1];
	h5_idlist_t* retval;
	for (face_idx = 0; face_idx < num_faces; face_idx++) {
		// add edges to neighbour struct
		TRY( h5tpriv_search_te2 (
			      f,
			      face_idx,
			      elem_idx,
			      &retval) );
	}
	return elem_idx;
}

h5_err_t
h5t_end_store_elems (
	h5_file_t* const f
	) {
	h5t_fdata_t* const t = f->t;

	t->num_elems[t->cur_level] = t->last_stored_eid+1;
	TRY( assign_global_elem_indices (f) );

	TRY( h5tpriv_sort_elems (f) );
	TRY( h5tpriv_rebuild_global_2_local_map_of_elems (f) );

	TRY( (t->methods.store->end_store_elems)(f) );

	return H5_SUCCESS;
}

/*
  Mark entity for further processing (e.g. refinement). 
 */
h5_err_t
h5t_mark_entity (
	h5_file_t* const f,
	const h5_id_t entity_id
	) {
	h5t_fdata_t* const t = f->t;
	return h5priv_append_to_idlist (f, &t->marked_entities, entity_id);
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
	h5t_fdata_t* const t = f->t;
	unsigned int num_elems_to_refine = t->marked_entities.num_items;
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
		return h5_error_internal (f, __FILE__, __func__, __LINE__);
	}
	TRY( h5t_begin_store_vertices (f, num_vertices_to_add) );
	TRY( h5t_begin_store_elems (f, num_elems_to_add) );

	return H5_SUCCESS;
}

/*
  Refine previously marked elements.
*/
h5_err_t
h5t_refine (
	h5_file_t* const f
	) {
	h5t_fdata_t* const t = f->t;
	int i;
	for (i = 0; i < t->marked_entities.num_items; i++) {
		TRY( h5t_refine_elem (f, t->marked_entities.items[i]) );
	}
	return H5_SUCCESS;
}

h5_err_t
h5t_post_refine (
	h5_file_t* const f
	) {
	h5t_fdata_t* const t = f->t;
	TRY( h5t_end_store_vertices (f) );
	TRY( h5t_end_store_elems (f) );
	return h5priv_free_idlist_items (f, &t->marked_entities);
}


h5_err_t
h5t_begin_refine_elems (
	h5_file_t* const f
	) {
	h5t_fdata_t* const t = f->t;

	/*
	  Pre-allocate space for items to avoid allocating small pieces of
	  memory.
	*/
	TRY( h5priv_alloc_idlist_items (f, &t->marked_entities, 2048) );
	return H5_SUCCESS;
}


/*!
  Refine element \c elem_id. Actually we set a mark only ...

  \return local id of first new element or \c -1
*/
h5_id_t
h5t_refine_elem (
	h5_file_t* const f,
	const h5_id_t elem_id
	) {
	return h5t_mark_entity (f, elem_id);
}

h5_err_t
h5t_end_refine_elems (
	h5_file_t* const f
	) {
	TRY( h5t_pre_refine (f) );
	TRY( h5t_refine (f) );
	TRY( h5t_post_refine (f) );

	return H5_SUCCESS;
}





  
