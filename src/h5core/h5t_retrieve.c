#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

#define num_elems_on_cur_level	f->t->num_elems[f->t->cur_level]

/*
  Skip elements which have been refined on a level <= the current one.
*/
h5_err_t
h5tpriv_skip_to_next_elem_on_level (
	h5_file_t* f,
	h5t_entity_iterator_t* iter
	) {
	h5_generic_loc_elem_t* el;
	do {
		iter->elem_idx++;
		if (iter->elem_idx >= num_elems_on_cur_level) {
			return H5_NOK;
		}
		el = h5tpriv_get_loc_elem (f, iter->elem_idx);
	} while (h5tpriv_elem_is_on_cur_level (f, el) == H5_NOK);
	return H5_SUCCESS;
}


/*
  Test whether given element is on current level. This is the case, if
  - the level_id of the element is <= the current level
  - and, if any, the direct children is on a level > the current level
*/
h5_err_t
h5tpriv_elem_is_on_cur_level (
	h5_file_t* const f,
	h5_generic_loc_elem_t *el // ptr to local element
	) {
	h5t_fdata_t* t = f->t;
	if ( (el->level_idx > t->cur_level) ||
	     (el->child_idx >= 0 && el->child_idx < num_elems_on_cur_level) ) {
		return H5_NOK;
	}
	return H5_SUCCESS;
}


static h5_loc_id_t
iterate_elems (
	h5_file_t* const f,
	h5t_entity_iterator_t* iter
	) {
	if ( h5tpriv_skip_to_next_elem_on_level (f, iter) == H5_NOK) {
		h5_debug ( f, "Traversing done!" );
		return H5_NOK;
	}
	return h5tpriv_build_elem_id ( iter->elem_idx );
}

static h5_loc_id_t
iterate_boundary_elems (
	h5_file_t* const f,
	h5t_entity_iterator_t* iter
	) {
	do {
		if ( h5tpriv_skip_to_next_elem_on_level (f, iter) == H5_NOK) {
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
	h5t_entity_iterator_t* iter
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
	return h5tpriv_build_id (type, iter->face_idx, iter->elem_idx );
}

/*!
  Travere entities with co-dim > 0
*/
static h5_loc_id_t
iterate_faces (
	h5_file_t* const f,
	h5t_entity_iterator_t* iter
	) {
	h5_idlist_t* entry;
	int dim = h5tpriv_ref_elem_get_dim (iter) - iter->codim;
	int num_faces = h5tpriv_ref_elem_get_num_faces (iter, dim) - 1;
	int i = -1;
	do {
		if (iter->face_idx >= num_faces) {
			if (h5tpriv_skip_to_next_elem_on_level (f, iter) == H5_NOK) {
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
		} while (h5tpriv_elem_is_on_cur_level (f, el) == H5_NOK);

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
	h5t_entity_iterator_t* iter
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
		} while (! h5tpriv_is_boundary_face (f, dim, iter->elem_idx, iter->face_idx));
		// Skip already visited faces
	} while (0);
	return h5_error_internal (f, __FILE__, __func__, __LINE__);
}

h5_err_t
h5t_alloc_entity_iterator (
	h5_file_t* f,
	h5t_entity_iterator_t** iter,
	int codim
	) {
	TRY( *iter = h5priv_alloc (f, NULL, sizeof (h5t_entity_iterator_t)) );
	return h5t_begin_iterate_entities (f, *iter, codim);
}

h5_err_t
h5t_alloc_boundary_face_iterator (
	h5_file_t* f,
	h5t_entity_iterator_t** iter,
	int codim
	) {
	TRY( *iter = h5priv_alloc (f, NULL, sizeof (h5t_entity_iterator_t)) );
	return h5t_begin_iterate_boundary_faces (f, *iter, codim);
}

h5_err_t
h5t_release_entity_iterator (
	h5_file_t* const f,
	h5t_entity_iterator_t* iter
	) {
	return h5priv_free (f, iter);
}

h5_err_t
h5t_begin_iterate_entities (
	h5_file_t* f,
	h5t_entity_iterator_t* iter,
	const int codim
	) {
	iter->face_idx = -1;
	iter->elem_idx = -1;
	iter->codim = codim;
	iter->ref_elem = f->t->ref_elem;

	if (iter->codim > 0) {
		iter->iter = iterate_faces;
	} else if (iter->codim == 0) {
		iter->iter = iterate_elems;
	}
	return h5tpriv_init_iterator (f, iter, codim);
}

h5_err_t
h5t_begin_iterate_boundary_faces (
	h5_file_t* f,
	h5t_entity_iterator_t* iter,
	const int codim
	) {
	iter->face_idx = 999; // just a high enough number
	iter->elem_idx = -1;
	iter->codim = codim;
	iter->ref_elem = f->t->ref_elem;

	if (iter->codim <= 0 || iter->codim > iter->ref_elem->dim) {
		return h5_error (f, H5_ERR_INVAL,
				 "Co-dimension requested %d, but must be between %d and %d",
				 codim, 1, iter->ref_elem->dim);
	} else if (iter->codim == 1) {
		iter->iter = iterate_boundary_facets;
	}
	else if (iter->codim > 1) {
		iter->iter = iterate_boundary_faces;
	}
	return H5_SUCCESS;
}

h5_loc_id_t
h5t_iterate_entities (
	h5_file_t* const f,
	h5t_entity_iterator_t* iter
	) {
	return (iter->iter (f, iter));
}
		   
h5_err_t
h5t_end_iterate_entities (
	h5_file_t* const f,
	h5t_entity_iterator_t* iter
	) {
#pragma unused f
	iter->face_idx = -1;
	iter->elem_idx = -1;
	iter->codim = -1;
	iter->ref_elem = NULL;
	iter->find = NULL;
	iter->iter = NULL;
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

