#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*
  Skip elements which have been refined on a level <= the current one.
*/
static h5_err_t
skip_to_next_elem_on_level (
	h5_file_t* f,
	h5t_mesh_iterator_t* iter
	) {
	h5_generic_loc_elem_t* el;
	do {
		iter->elem_idx++;
		if (iter->elem_idx >= f->t->num_elems[iter->level_idx]) {
			return H5_NOK;
		}
		el = h5tpriv_get_loc_elem (f, iter->elem_idx);
	} while (h5tpriv_elem_is_on_level (f, el, iter->level_idx) == H5_NOK);
	return H5_SUCCESS;
}

static h5_loc_id_t
iterate_elems (
	h5_file_t* const f,
	h5t_mesh_iterator_t* iter
	) {
	if ( skip_to_next_elem_on_level (f, iter) == H5_NOK) {
		h5_debug ( f, "Traversing done!" );
		return H5_NOK;
	}
	return h5tpriv_build_elem_id ( iter->elem_idx );
}

static h5_loc_id_t
iterate_boundary_elems (
	h5_file_t* const f,
	h5t_mesh_iterator_t* iter
	) {
	do {
		if ( skip_to_next_elem_on_level (f, iter) == H5_NOK) {
			h5_debug ( f, "Traversing done!" );
			return H5_NOK;
		}
	} while (!h5tpriv_is_boundary_elem (f, iter->elem_idx));
	return h5tpriv_build_elem_id ( iter->elem_idx );
}



/*
  Iterate boundary facets (co-dim 1 entities).
 */
static h5_loc_id_t
iterate_boundary_facets (
	h5_file_t* const f,
	h5t_mesh_iterator_t* iter
	) {
	int num_facets = h5tpriv_ref_elem_get_num_facets (iter) - 1;
	int dim = h5tpriv_ref_elem_get_dim (iter) - iter->codim;
	do {
		if (iter->face_idx >= num_facets) {
			h5_loc_id_t elem_id;
			TRY( elem_id = iterate_boundary_elems (f, iter) );
			if (elem_id == H5_NOK) {
				return H5_NOK;	// done!
			}
			iter->elem_idx = h5tpriv_get_elem_idx (elem_id);
			iter->face_idx = 0;
		} else {
			iter->face_idx++;
		}
	} while (! h5tpriv_is_boundary_facet (f, iter->elem_idx, iter->face_idx));
	int type = h5tpriv_ref_elem_get_entity_type (iter->ref_elem, dim);
	return h5tpriv_build_entity_id (type, iter->face_idx, iter->elem_idx );
}

/*!
  Travere entities with co-dim > 0
*/
static h5_loc_id_t
iterate_faces (
	h5_file_t* const f,
	h5t_mesh_iterator_t* iter
	) {
	h5_idlist_t* entry;
	int dim = h5tpriv_ref_elem_get_dim (iter) - iter->codim;
	int num_faces = h5tpriv_ref_elem_get_num_faces (iter, dim) - 1;
	int i = -1;
	do {
		if (iter->face_idx >= num_faces) {
			if (skip_to_next_elem_on_level (f, iter) == H5_NOK) {
				h5_debug (f, "Traversing done!");
				return H5_NOK;
			}
			iter->face_idx = 0;
		} else {
			iter->face_idx++;
		}
		// Skip already visited faces:

		//  1. Get list of all elements with this face. Actually we
		//     retrieve a list of entities sorted by the element index.
		TRY( (iter->find)(f, iter->face_idx, iter->elem_idx, &entry) );

		// 2. Go to first element in list which is on current level
		i = -1;
		h5_generic_loc_elem_t *el;
		do {
			i++;
			h5_loc_idx_t idx = h5tpriv_get_elem_idx (entry->items[i]);
			el = h5tpriv_get_loc_elem (f, idx);
		} while (h5tpriv_elem_is_on_level (f, el, iter->level_idx) == H5_NOK);

		// 3. Face already visited if 
	} while (iter->elem_idx > h5tpriv_get_elem_idx(entry->items[i]));
	/*
	  note: in above test iter->elem_idx is always greater or equal to the
	  compared index. It cannot be smaller since iter->elem_idx is on the
	  current level and the element index of entry->items[i] is the smallest
	  element index with the given face on the current level.
	*/
	return entry->items[0];
}

/*
  Iterate boundary faces
 */
static h5_loc_id_t
iterate_boundary_faces (
	h5_file_t* const f,
	h5t_mesh_iterator_t* iter
	) {
	// TODO!!!
	int dim = h5tpriv_ref_elem_get_dim (iter) - iter->codim;
	int num_faces = h5tpriv_ref_elem_get_num_faces (iter, dim) - 1;
	do {
		// iterate to next boundary face
		do {
			// first iterate over all faces of element, if done
			// goto next element
			if (iter->face_idx >= num_faces) {
				h5_loc_id_t elem_id;
				TRY( elem_id = iterate_boundary_elems (f, iter) );
				if (elem_id == H5_NOK) {
					return H5_NOK;	// done!
				}
				iter->face_idx = 0;
			} else {
				iter->face_idx++;
			}
		} while (! h5tpriv_is_boundary_face (
				 f, dim, iter->elem_idx, iter->face_idx));
		// Skip already visited faces
	} while (0);
	return h5_error_internal (f, __FILE__, __func__, __LINE__);
}

static h5_loc_id_t
iterate_tags (
	h5_file_t* const f,
	h5t_tag_iterator_t* iter
	) {
	UNUSED_ARGUMENT (f);
#if 0
	h5t_tagsel_t* tags;
	do {
		if (iter->subentity_idx >= 14) {
			iter->elem_idx++;
			iter->subentity_idx = 0;
		} else {
			iter->subentity_idx++;
		}
		tags = iter->tagset->elems[iter->elem_idx];
	} while ((tags == NULL) || (tags->idx[iter->subentity_idx]));
#endif	
	return 0;
}

h5_err_t
h5t_init_mesh_iterator (
	h5_file_t* f,
	h5t_iterator_t* iter,
	int codim
	) {
	h5t_mesh_iterator_t* it = (h5t_mesh_iterator_t*)iter;
	it->face_idx = 999;
	it->elem_idx = -1;
	it->codim = codim;
	it->level_idx = f->t->cur_level;
	it->ref_elem = f->t->ref_elem;

	if (it->codim > 0) {
		it->iter = iterate_faces;
	} else if (it->codim == 0) {
		it->iter = iterate_elems;
	}
	return H5_SUCCESS;
}

h5_err_t
h5t_create_mesh_iterator (
	h5_file_t* f,
	h5t_iterator_t** iter,
	int codim
	) {
	h5t_mesh_iterator_t* it;
	TRY( it = h5priv_calloc (f, 1, sizeof (h5t_mesh_iterator_t)) );
	TRY( h5t_init_mesh_iterator (f, (h5t_iterator_t*)it, codim) );
	*iter =  (h5t_iterator_t*)it;
	return H5_SUCCESS;
}

h5_err_t
h5t_create_boundary_face_iterator (
	h5_file_t* f,
	h5t_iterator_t** iter,
	int codim
	) {
	h5t_mesh_iterator_t* it;
	TRY( it = h5priv_calloc (f, 1, sizeof (h5t_mesh_iterator_t)) );
	it->face_idx = 999; // just a high enough number
	it->elem_idx = -1;
	it->codim = codim;
	it->level_idx = f->t->cur_level;
	it->ref_elem = f->t->ref_elem;

	if (it->codim <= 0 || it->codim > it->ref_elem->dim) {
		return h5tpriv_inval_codim (f, codim, 1, it->ref_elem->dim);
	} else if (it->codim == 1) {
		it->iter = iterate_boundary_facets;
	}
	else if (it->codim > 1) {
		it->iter = iterate_boundary_faces;
	}
	*iter =  (h5t_iterator_t*)it;
	return H5_SUCCESS;
}

h5_err_t
h5t_create_mtag_iterator (
	h5_file_t* f,
	h5t_iterator_t** iter,
	const char* name
	) {
	h5t_tag_iterator_t* it;
	TRY( it = h5priv_calloc (f, 1, sizeof (h5t_tag_iterator_t)) );
	h5t_open_mtagset (f, name, &it->tagset);
	it->elem_idx = -1;
	it->subentity_idx = 999;
	it->level_idx = f->t->cur_level;
	it->iter = iterate_tags;
	*iter =  (h5t_iterator_t*)it;
	return H5_SUCCESS;
}

h5_err_t
h5t_release_entity_iterator (
	h5_file_t* const f,
	h5t_iterator_t* iter
	) {
	return h5priv_free (f, iter);
}

h5_loc_id_t
h5t_iterate_entities (
	h5_file_t* const f,
	h5t_iterator_t* iter
	) {
	return (iter->iter (f, iter));
}
		   
h5_err_t
h5t_end_iterate_entities (
	h5_file_t* const f,
	h5t_iterator_t* iter
	) {
	UNUSED_ARGUMENT (f);
	h5t_mesh_iterator_t* it = (h5t_mesh_iterator_t*)iter;
	it->face_idx = -1;
	it->elem_idx = -1;
	it->codim = -1;
	it->ref_elem = NULL;
	it->find = NULL;
	it->iter = NULL;
	return H5_SUCCESS;
}

h5_err_t
h5t_get_vertex_coords_by_index (
	h5_file_t* const f,
	h5_loc_idx_t vertex_index,
	h5_float64_t P[3]
	) {
	h5_loc_vertex_t *vertex = &f->t->vertices[vertex_index];
	memcpy ( P, &vertex->P, sizeof ( vertex->P ) );
	return H5_SUCCESS;
}

h5_err_t
h5t_get_vertex_coords_by_id (
	h5_file_t* const f,
	h5_loc_id_t vertex_id,
	h5_float64_t P[3]
	) {
	h5_loc_idx_t vertex_index;
	h5t_get_vertex_index_of_vertex (f, vertex_id, &vertex_index );
	h5t_get_vertex_coords_by_index (f, vertex_index, P);
	return H5_SUCCESS;
}

