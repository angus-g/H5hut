#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*
  Skip elements which have been refined on a level <= the current one.
*/
static h5_err_t
iter_leaf_elem_idx (
	h5_file_t* f,
	h5t_leaf_iterator_t* iter
	) {
	h5_generic_loc_elem_t* el;
	do {
		iter->elem_idx++;
		if (iter->elem_idx >= f->t->num_elems[iter->leaf_level]) {
			return H5_NOK;
		}
		el = h5tpriv_get_loc_elem (f, iter->elem_idx);
	} while (h5tpriv_elem_is_on_level (f, el, iter->leaf_level) == H5_NOK);
	return H5_SUCCESS;
}

static h5_loc_id_t
iterate_leaf_elems (
	h5_file_t* const f,
	union h5t_iterator* iter
	) {
	h5t_leaf_iterator_t* it = (h5t_leaf_iterator_t*)&iter->leaf;
	if ( iter_leaf_elem_idx (f, it) == H5_NOK) {
		h5_debug ( f, "Traversing done!" );
		return H5_NOK;
	}
	int dim = it->ref_elem->dim;
	h5_loc_id_t type_id = it->ref_elem->entity_types[dim];
	return h5tpriv_build_entity_id (type_id, 0, it->elem_idx );
}

static h5_loc_id_t
iterate_boundary_elems (
	h5_file_t* const f,
	h5t_iterator_t* iter
	) {
	h5t_leaf_iterator_t* it = (h5t_leaf_iterator_t*)&iter->leaf;
	do {
		if ( iter_leaf_elem_idx (f, it) == H5_NOK) {
			h5_debug ( f, "Traversing done!" );
			return H5_NOK;
		}
	} while (!h5tpriv_is_boundary_elem (f, it->elem_idx));
	int dim = it->ref_elem->dim;
	h5_loc_id_t type_id = it->ref_elem->entity_types[dim];
	return h5tpriv_build_entity_id (type_id, 0, it->elem_idx );
}



/*
  Iterate boundary facets (co-dim 1 entities).
 */
static h5_loc_id_t
iterate_boundary_facets (
	h5_file_t* const f,
	h5t_iterator_t* iter
	) {
	h5t_leaf_iterator_t* it = (h5t_leaf_iterator_t*)&iter->leaf;
	int num_facets = h5tpriv_ref_elem_get_num_facets (it) - 1;
	int dim = h5tpriv_ref_elem_get_dim (it) - it->codim;
	do {
		if (it->face_idx >= num_facets) {
			h5_loc_id_t elem_id;
			TRY( elem_id = iterate_boundary_elems (f, iter) );
			if (elem_id == H5_NOK) {
				return H5_NOK;	// done!
			}
			it->elem_idx = h5tpriv_get_elem_idx (elem_id);
			it->face_idx = 0;
		} else {
			it->face_idx++;
		}
	} while (! h5tpriv_is_boundary_facet (f, it->elem_idx, it->face_idx));
	int type = h5tpriv_ref_elem_get_entity_type (it->ref_elem, dim);
	return h5tpriv_build_entity_id (type, it->face_idx, it->elem_idx );
}

/*!
  Travere entities with co-dim > 0
*/
static h5_loc_id_t
iterate_leaf_faces (
	h5_file_t* const f,
	h5t_iterator_t* iter
	) {
	h5t_leaf_iterator_t* it = (h5t_leaf_iterator_t*)iter;
	h5_idlist_t* entry;
	int dim = h5tpriv_ref_elem_get_dim (it) - it->codim;
	int num_faces = h5tpriv_ref_elem_get_num_faces (it, dim) - 1;
	int i = -1;
	do {
		if (it->face_idx >= num_faces) {
			if (iter_leaf_elem_idx (f, it) == H5_NOK) {
				h5_debug (f, "Traversing done!");
				return H5_NOK;
			}
			it->face_idx = 0;
		} else {
			it->face_idx++;
		}
		// Skip already visited faces:

		//  1. Get list of all elements with this face. Actually we
		//     retrieve a list of entities sorted by the element index.
		TRY( (it->find)(f, it->face_idx, it->elem_idx, &entry) );

		// 2. Go to first element in list which is on current level
		i = -1;
		h5_generic_loc_elem_t *el;
		do {
			i++;
			h5_loc_idx_t idx = h5tpriv_get_elem_idx (entry->items[i]);
			el = h5tpriv_get_loc_elem (f, idx);
		} while (h5tpriv_elem_is_on_level (f, el, it->leaf_level) == H5_NOK);

		// 3. Face already visited if 
	} while (it->elem_idx > h5tpriv_get_elem_idx(entry->items[i]));
	/*
	  note: in above test it->elem_idx is always greater or equal to the
	  compared index. It cannot be smaller since it->elem_idx is on the
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
	h5t_iterator_t* iter
	) {
	h5t_leaf_iterator_t* it = (h5t_leaf_iterator_t*)iter;
	// TODO!!!
	int dim = h5tpriv_ref_elem_get_dim (it) - it->codim;
	int num_faces = h5tpriv_ref_elem_get_num_faces (it, dim) - 1;
	do {
		// iterate to next boundary face
		do {
			// first iterate over all faces of element, if done
			// goto next element
			if (it->face_idx >= num_faces) {
				h5_loc_id_t elem_id;
				TRY( elem_id = iterate_boundary_elems (f, iter) );
				if (elem_id == H5_NOK) {
					return H5_NOK;	// done!
				}
				it->face_idx = 0;
			} else {
				it->face_idx++;
			}
		} while (! h5tpriv_is_boundary_face (
				 f, dim, it->elem_idx, it->face_idx));
		// Skip already visited faces
	} while (0);
	return h5_error_internal (f, __FILE__, __func__, __LINE__);
}

static h5_loc_id_t
iterate_tags (
	h5_file_t* const f,
	h5t_iterator_t* iter
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
h5t_init_leaf_iterator (
	h5_file_t* f,
	h5t_iterator_t* iter,
	int codim
	) {
	H5_CORE_API_ENTER;
	h5_err_t h5err = H5_SUCCESS;
	h5t_leaf_iterator_t* it = (h5t_leaf_iterator_t*)&iter->leaf;
	it->face_idx = 999;
	it->elem_idx = -1;
	it->codim = codim;
	it->leaf_level = f->t->leaf_level;
	it->ref_elem = f->t->ref_elem;

	if (it->codim > 0) {
		it->iter = iterate_leaf_faces;
	} else if (it->codim == 0) {
		it->iter = iterate_leaf_elems;
	}
	H5_CORE_API_RETURN (h5err);
}

h5_err_t
h5t_init_boundary_face_iterator (
	h5_file_t* f,
	h5t_iterator_t* iter,
	int codim
	) {
	H5_CORE_API_ENTER;
	h5_err_t h5err = H5_SUCCESS;
	h5t_leaf_iterator_t* it;
	it->face_idx = 999; // something > max number of faces
	it->elem_idx = -1;
	it->codim = codim;
	it->leaf_level = f->t->leaf_level;
	it->ref_elem = f->t->ref_elem;

	if (it->codim <= 0 || it->codim > it->ref_elem->dim) {
		return h5tpriv_inval_codim (f, codim, 1, it->ref_elem->dim);
	} else if (it->codim == 1) {
		it->iter = iterate_boundary_facets;
	}
	else if (it->codim > 1) {
		it->iter = iterate_boundary_faces;
	}
	H5_CORE_API_RETURN (h5err);
}

h5_err_t
h5t_init_mtag_iterator (
	h5_file_t* f,
	h5t_iterator_t* iter,
	const char* name
	) {
	H5_CORE_API_ENTER;
	h5_err_t h5err = H5_SUCCESS;
	h5t_tag_iterator_t* it = (h5t_tag_iterator_t*)iter;
	TRY2( h5err = h5t_open_mtagset (f, name, &it->tagset) );
	it->elem_idx = -1;
	it->subentity_idx = 999;
	it->level_idx = f->t->leaf_level;
	it->iter = iterate_tags;
	h5err = H5_SUCCESS;
	H5_CORE_API_RETURN (h5err);
}

h5_err_t
h5t_release_entity_iterator (
	h5_file_t* const f,
	h5t_iterator_t* iter
	) {
	H5_CORE_API_ENTER;
	h5_err_t h5err = h5_free (f, iter);
	H5_CORE_API_RETURN (h5err);
}

h5_loc_id_t
h5t_iterate_entities (
	h5_file_t* const f,
	h5t_iterator_t* iter
	) {
	H5_CORE_API_ENTER;
	h5t_generic_iterator_t* it = (h5t_generic_iterator_t*)iter;
	h5_err_t h5err = it->iter (f, iter);
	H5_CORE_API_RETURN (h5err);
}
		   
h5_err_t
h5t_end_iterate_entities (
	h5_file_t* const f,
	h5t_iterator_t* iter
	) {
	UNUSED_ARGUMENT (f);
	H5_CORE_API_ENTER;
	h5_err_t h5err = H5_SUCCESS;
	bzero (iter, sizeof(*iter));
	h5t_leaf_iterator_t* it = (h5t_leaf_iterator_t*)iter;
	it->face_idx = -1;
	it->elem_idx = -1;
	it->codim = -1;
	H5_CORE_API_RETURN (h5err);
}

h5_err_t
h5t_get_vertex_coords_by_index (
	h5_file_t* const f,
	h5_loc_idx_t vertex_index,
	h5_float64_t P[3]
	) {
	H5_CORE_API_ENTER;
	h5_err_t h5err = H5_SUCCESS;
	h5_loc_vertex_t *vertex = &f->t->vertices[vertex_index];
	memcpy ( P, &vertex->P, sizeof ( vertex->P ) );
	H5_CORE_API_RETURN (h5err);
}

h5_err_t
h5t_get_vertex_coords_by_id (
	h5_file_t* const f,
	h5_loc_id_t vertex_id,
	h5_float64_t P[3]
	) {
	H5_CORE_API_ENTER;
	h5_err_t h5err = H5_SUCCESS;
	h5_loc_idx_t vertex_index;
	TRY2( h5err = h5t_get_vertex_index_of_vertex (f, vertex_id, &vertex_index) );
	TRY2( h5t_get_vertex_coords_by_index (f, vertex_index, P) );
	H5_CORE_API_RETURN (h5err);
}

