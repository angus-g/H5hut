#ifndef __H5T_RETRIEVE_PRIVATE_H
#define __H5T_RETRIEVE_PRIVATE_H

#include <assert.h>

struct h5t_retrieve_methods {
	h5_err_t (*init_entity_iterator)(
		h5_file_t* const, h5t_iterator_t*, const int);
};

extern struct h5t_retrieve_methods h5tpriv_trim_retrieve_methods;
extern struct h5t_retrieve_methods h5tpriv_tetm_retrieve_methods;

/*
  Test whether given element is on current level. This is the case, if
  - the level_id of the element is <= the current level
  - and, if any, the direct children is on a level > the current level
*/
static inline h5_err_t
h5tpriv_is_leaf_elem (
	h5_file_t* const f,
	h5_generic_loc_elem_t *el // ptr to local element
	) {
	h5t_fdata_t* t = f->t;
	if ( (el->level_idx > t->leaf_level) ||
	     (el->child_idx >= 0 && el->child_idx < f->t->num_elems[f->t->leaf_level]) ) {
		return H5_NOK;
	}
	return H5_SUCCESS;
}

static inline h5_err_t
h5tpriv_elem_is_on_level (
	h5_file_t* const f,
	h5_generic_loc_elem_t *el, // ptr to local element
	const h5t_lvl_idx_t level_idx
	) {
	assert ( level_idx < f->t->num_leaf_levels );
	if ( (el->level_idx > level_idx) ||
	     (el->child_idx >= 0 && el->child_idx < f->t->num_elems[level_idx]) ) {
		return H5_NOK;
	}
	return H5_SUCCESS;
}

static inline h5_err_t
h5tpriv_init_entity_iterator (
	h5_file_t* f,
	h5t_iterator_t* const iter,
	const int codim
	) {
	return (*f->t->methods.retrieve->init_entity_iterator) (f, iter, codim);
}

#endif
