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
	h5_generic_elem_t* el;
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
	h5_generic_elem_t *el // ptr to local element
	) {
	h5t_fdata_t* t = f->t;
	if ( (el->idx > t->cur_level) ||
	     (el->child_idx >= 0 && el->child_idx < num_elems_on_cur_level) ) {
		return H5_NOK;
	}
	return H5_SUCCESS;
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
	return h5tpriv_init_iterator (f, iter, codim);
}

/*!
  Travere entities with co-dim > 0
*/
static h5_id_t
iterate_faces (
	h5_file_t* const f,
	h5t_entity_iterator_t* iter
	) {
	h5_idlist_t* entry;
	h5_size_t i;
	int dim = h5tpriv_ref_elem_get_dim (iter) - iter->codim;
	int num_faces = h5tpriv_ref_elem_get_num_faces (iter, dim) - 1;
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
		/*
		  get list of all elements with this face
		  and skip to first element in list which is on
		  current level
		*/
		TRY( (iter->find)(f, iter->face_idx,
				  iter->elem_idx, &entry) );
		i = -1;
		h5_generic_elem_t *el;
		do {
			i++;
			h5_id_t idx = h5tpriv_get_elem_idx (entry->items[i]);
			el = h5tpriv_get_loc_elem (f, idx);
		} while (h5tpriv_elem_is_on_cur_level (f, el) == H5_NOK);
	} while (iter->elem_idx != h5tpriv_get_elem_idx(entry->items[i]));
	
	return entry->items[0];
}

static h5_id_t
iterate_elems (
	h5_file_t* const f,
	h5t_entity_iterator_t*iter
	) {
	if ( h5tpriv_skip_to_next_elem_on_level (f, iter) == H5_NOK) {
		h5_debug ( f, "Traversing done!" );
		return H5_NOK;
	}
	return h5tpriv_build_elem_id ( iter->elem_idx );
}

h5_id_t
h5t_iterate_entities (
	h5_file_t* const f,
	h5t_entity_iterator_t* iter
	) {
	if (iter->codim > 0) {
		return iterate_faces (f, iter);
	} else if (iter->codim == 0) {
		return iterate_elems (f, iter);
	} else {
		return h5_error_internal (f, __FILE__, __func__, __LINE__);
	}
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
	return H5_SUCCESS;
}
		   
h5_err_t
h5t_get_vertex_coords_by_index (
	h5_file_t* const f,
	h5_id_t vertex_index,
	h5_float64_t P[3]
	) {
	h5_vertex_t *vertex = &f->t->vertices[vertex_index];
	memcpy ( P, &vertex->P, sizeof ( vertex->P ) );
	return H5_SUCCESS;
}

h5_err_t
h5t_get_vertex_coords_by_id (
	h5_file_t* const f,
	h5_id_t vertex_id,
	h5_float64_t P[3]
	) {
	h5_id_t vertex_index;
	h5t_get_vertex_index_of_vertex (f, vertex_id, &vertex_index );
	h5t_get_vertex_coords_by_index (f, vertex_index, P);
	return H5_SUCCESS;
}

