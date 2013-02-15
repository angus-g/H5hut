#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*
  Assign unique global indices to vertices.
*/
static h5_err_t
assign_global_vertex_indices (
	h5t_mesh_t* const m
	) {
	if (m->leaf_level < 0) return H5_SUCCESS; /* no level defined */

	/*
	  simple in serial runs: global_id = local_id
	*/
	h5_loc_idx_t local_idx = (m->leaf_level == 0) ?
		0 : m->num_vertices[m->leaf_level-1];
	for (local_idx = 0;
	     local_idx < m->num_vertices[m->num_leaf_levels-1];
	     local_idx++) {
		m->vertices[local_idx].idx = local_idx;
	}

	return H5_SUCCESS;
}

/*!
 Assign unique global indices to new elements.
*/
static h5_err_t
assign_glb_elem_indices (
	h5t_mesh_t* const m
	) {
	if (m->leaf_level < 0) return H5_SUCCESS; /* no level defined */

	/*
	  simple in serial runs: global index = local index
	*/
	h5_loc_idx_t loc_idx = (m->leaf_level == 0) ? 0 : m->num_elems[m->leaf_level-1];
	
	for (; loc_idx < m->num_elems[m->leaf_level]; loc_idx++) {
		h5tpriv_set_loc_elem_glb_idx (m, loc_idx, loc_idx);
	}

	return H5_SUCCESS;
}



h5t_lvl_idx_t
h5t_add_level (
	h5t_mesh_t* const m
	) {
	H5_CORE_API_ENTER (h5t_lvl_idx_t, "m=%p", m);
	CHECK_WRITABLE_MODE(m->f);

	m->leaf_level = m->num_leaf_levels++;
	m->num_loaded_levels = m->num_leaf_levels;

	ssize_t num_bytes = m->num_leaf_levels*sizeof (h5_size_t);
	TRY (m->num_vertices = h5_alloc (m->num_vertices, num_bytes));
	m->num_vertices[m->leaf_level] = -1;

	TRY (m->num_elems = h5_alloc (m->num_elems, num_bytes));
	m->num_elems[m->leaf_level] = -1;
	TRY ( m->num_leaf_elems = h5_alloc (
		     m->num_leaf_elems, num_bytes));
	m->num_leaf_elems[m->leaf_level] = -1;

	if (m->leaf_level == 0) {
		/* nothing stored yet */
		m->last_stored_vid = -1;
		m->last_stored_eid = -1;
	}

	H5_CORE_API_RETURN (m->leaf_level);
}

/*!
  Allocate memory for (more) vertices.
*/
h5_err_t
h5t_begin_store_vertices (
	h5t_mesh_t* const m,
	const h5_size_t num
	) {
	H5_CORE_API_ENTER (h5_err_t, "m=%p, num=%llu", m, (long long unsigned)num);
	if (m->leaf_level < 0) {
		H5_CORE_API_LEAVE (h5tpriv_error_undef_level());
	}
	h5_size_t cur_num_vertices = (m->leaf_level > 0 ?
				      m->num_vertices[m->leaf_level-1] : 0);
	m->num_vertices[m->leaf_level] = cur_num_vertices+num;
	m->dsinfo_vertices.dims[0] = cur_num_vertices+num;
	H5_CORE_API_RETURN (h5tpriv_alloc_num_vertices (m, cur_num_vertices+num));
}

h5_loc_idx_t
h5t_store_vertex (
	h5t_mesh_t* const m,		/*!< file handle		*/
	const h5_glb_idx_t glb_id,	/*!< global vertex id from mesher or -1	*/
	const h5_float64_t P[3]		/*!< coordinates		*/
	) {
	H5_CORE_API_ENTER (h5_loc_idx_t,
			   "m=%p, glb=id=%lld, P=%p",
			   m,
			   (long long)glb_id,
			   P);
	
	/*
	  more than allocated
	*/
	if (m->last_stored_vid+1 >= m->num_vertices[m->leaf_level]) 
		H5_CORE_API_LEAVE (HANDLE_H5_OVERFLOW_ERR(
					   m->num_vertices[m->leaf_level]));
	
	/*
	  missing call to add the first level
	 */
	if (m->leaf_level < 0)
		H5_CORE_API_LEAVE (h5tpriv_error_undef_level());

	h5_loc_idx_t local_idx = ++m->last_stored_vid;
	h5_loc_vertex_t *vertex = &m->vertices[local_idx];
	vertex->idx = glb_id;     /* ID from mesher, replaced later!*/
	memcpy (&vertex->P, P, sizeof (vertex->P));
	H5_CORE_API_RETURN (local_idx);
}

h5_err_t
h5t_end_store_vertices (
	h5t_mesh_t* const m
	) {
	H5_CORE_API_ENTER (h5_err_t, "m=%p", m);
	
	m->num_vertices[m->leaf_level] = m->last_stored_vid+1;
	TRY (assign_global_vertex_indices (m));
	TRY (h5tpriv_rebuild_vertex_indices_mapping (m));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*!
  Initialize everything so that we can begin to store elements.

  \param[in]	f	file handle
  \param[in]	num	number of elements to add
 */
h5_err_t
h5t_begin_store_elems (
	h5t_mesh_t* const m,
	const h5_size_t num
	) {
	H5_CORE_API_ENTER (h5_err_t,
			   "m=%p, num=%llu",
			   m, (long long unsigned)num);

	size_t cur = m->leaf_level > 0 ? m->num_elems[m->leaf_level-1] : 0;
	size_t new = num + cur;
	m->num_elems[m->leaf_level] = new;
	m->dsinfo_elems.dims[0] = new;

	m->num_leaf_elems[m->leaf_level] = m->leaf_level > 0 ?
		num + m->num_leaf_elems[m->leaf_level-1] : num;
	/*
	  We allocate a hash table for a minimum of 2^21 edges to
	  avoid resizing.
	 */
	size_t nel = 2097152 > 5*new ? 2097152 : 5*new;
	TRY (h5tpriv_resize_te_htab (m, nel));
	H5_CORE_API_RETURN (h5tpriv_alloc_elems (m, cur, new));
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
	h5t_mesh_t* const m,
	const h5_loc_idx_t parent_idx,
	const h5_loc_idx_t* vertex_indices
	) {
	H5_CORE_API_ENTER (h5_loc_idx_t,
			   "m=%p, parent_idx=%lld, vertex_indices=%p",
			   m,
			   (long long)parent_idx,
			   vertex_indices);

	/* level set? */
	if (m->leaf_level < 0)
		H5_CORE_API_LEAVE (
			h5tpriv_error_undef_level());

	/*  more than allocated? */
	if ( m->last_stored_eid+1 >= m->num_elems[m->leaf_level] ) 
		H5_CORE_API_LEAVE (
			HANDLE_H5_OVERFLOW_ERR (m->num_elems[m->leaf_level]));

	/* check parent id */
	if ((m->leaf_level == 0 && parent_idx != -1) ||
	    (m->leaf_level >  0 && parent_idx < 0) ||
	    (m->leaf_level >  0
	     && parent_idx >= m->num_elems[m->leaf_level-1])
		) {
		H5_CORE_API_LEAVE (
			HANDLE_H5_PARENT_ID_ERR (parent_idx));
	}

	/* store elem data (but neighbors) */
	h5_loc_idx_t elem_idx = ++m->last_stored_eid;
	h5tpriv_set_loc_elem_parent_idx (m, elem_idx, parent_idx);
	h5tpriv_set_loc_elem_child_idx (m, elem_idx, -1);
	h5tpriv_set_loc_elem_level_idx (m, elem_idx, m->leaf_level);

	// get ptr to local vertices store
	h5_loc_idx_t* loc_vertex_indices = h5tpriv_get_loc_elem_vertex_indices (
		m, elem_idx);
	int num_vertices = h5tpriv_ref_elem_get_num_vertices (m);
	memcpy (loc_vertex_indices, vertex_indices,
		sizeof (*vertex_indices)*num_vertices);
	h5tpriv_sort_local_vertex_indices (m, loc_vertex_indices, num_vertices);

	/* add edges to map  edges -> elements */
	h5_loc_idx_t face_idx;
	int num_faces = h5tpriv_ref_elem_get_num_edges (m);
	for (face_idx = 0; face_idx < num_faces; face_idx++) {
		// add edges to neighbour struct
		TRY (h5tpriv_search_te2 (m, face_idx, elem_idx, NULL));
	}
	H5_CORE_API_RETURN (elem_idx);
}

h5_err_t
h5t_end_store_elems (
	h5t_mesh_t* const m
	) {
	H5_CORE_API_ENTER (h5_err_t, "m=%p", m);

	m->num_elems[m->leaf_level] = m->last_stored_eid+1;
        if (m->leaf_level == 0) {
                m->num_leaf_elems[0] = m->num_elems[0];
        }

	/* assign global indices to new indices */
	TRY (assign_glb_elem_indices (m));

	/* rebuild map: global index -> local_index */
	TRY (h5tpriv_rebuild_elem_indices_mapping (m));

	/* mesh specific finalize */
	TRY (m->methods.store->end_store_elems (m));

	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*
  Mark entity for further processing (e.g. refinement). 
 */
h5_err_t
h5t_mark_entity (
	h5t_mesh_t* const m,
	const h5_loc_id_t entity_id
	) {
	H5_CORE_API_ENTER (h5_err_t, "m=%p, entity_id=%llu",
			   m, (long long unsigned)entity_id);
	H5_CORE_API_RETURN (h5priv_insert_idlist (&m->marked_entities, entity_id, -1));
}

h5_err_t
h5t_pre_refine (
	h5t_mesh_t* const m
	) {
	H5_CORE_API_ENTER (h5_err_t, "m=%p", m);
	H5_CORE_API_RETURN (m->methods.store->pre_refine (m));
}

/*
  Refine previously marked elements.
*/
h5_err_t
h5t_refine_marked_elems (
	h5t_mesh_t* const m
	) {
	H5_CORE_API_ENTER (h5_err_t, "m=%p", m);
	int i;
	for (i = 0; i < m->marked_entities->num_items; i++) {
		TRY (h5tpriv_refine_elem (m, m->marked_entities->items[i]));
	}
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5t_post_refine (
	h5t_mesh_t* const m
	) {
	H5_CORE_API_ENTER (h5_err_t, "m=%p", m);
	TRY (h5t_end_store_vertices (m));
	TRY (h5t_end_store_elems (m));
	H5_CORE_API_RETURN (h5priv_free_idlist (&m->marked_entities));
}

h5_err_t
h5t_begin_refine_elems (
	h5t_mesh_t* const m
	) {
	H5_CORE_API_ENTER (h5_err_t, "m=%p", m);

	/*
	  Pre-allocate space for items to avoid allocating small pieces of
	  memory.
	*/
	TRY (h5priv_alloc_idlist (&m->marked_entities, 2048));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5t_end_refine_elems (
	h5t_mesh_t* const m
	) {
	H5_CORE_API_ENTER (h5_err_t, "m=%p", m);
	TRY (h5t_pre_refine (m));
	TRY (h5t_refine_marked_elems (m));
	TRY (h5t_post_refine (m));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

#if 0
// index set for DUNE
h5_err_t
h5t_create_index_set (
	h5t_mesh_t* const m
	) {
	H5_CORE_API_ENTER (h5_err_t, "m=%p", m);
	int codim;
	int dim = h5tpriv_ref_elem_get_dim (m);
	// todo: check tagset already exist
	TRY (h5t_add_mtagset (m, "__IndexSet__", H5_INT64_T));

	for (codim = 0; codim <= dim; codim++) {
		h5_glb_idx_t idx = 0;
		h5t_leaf_iterator_t it;
		h5_glb_id_t entity_id;
		TRY (h5t_init_leaf_iterator ((h5t_iterator_t*)&it, m, codim));
		while ((entity_id = it.iter(f, (h5t_iterator_t*)&it)) >= 0) {
			TRY (h5t_set_mtag_by_name (f, "__IndexSet__", entity_id, 1, &idx));
		}
	}
	H5_CORE_API_RETURN (H5_SUCCESS);
}
#endif
