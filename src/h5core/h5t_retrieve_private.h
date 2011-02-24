#ifndef __H5T_RETRIEVE_PRIVATE_H
#define __H5T_RETRIEVE_PRIVATE_H

#include <assert.h>

struct h5t_retrieve_methods {
	h5_err_t (*init_entity_iterator)(
		h5t_iterator_t*, const int);
};

extern struct h5t_retrieve_methods h5tpriv_trim_retrieve_methods;
extern struct h5t_retrieve_methods h5tpriv_tetm_retrieve_methods;

/*
  Test whether given element is on current level. This is the case, if
  - the level_id of the element is <= the current level
  - and, if any, the direct children is on a level > the current level
*/
#define h5tpriv_is_leaf_elem(f, el)					\
	( (el->level_idx <= f->t->leaf_level) &&			\
	  (el->child_idx < 0 || el->child_idx >= f->t->num_elems[f->t->leaf_level]) )

static inline h5_err_t
h5tpriv_init_entity_iterator (
	h5_file_t* f,
	h5t_iterator_t* const iter,
	const int codim
	) {
	return (*f->t->methods.retrieve->init_entity_iterator) (iter, codim);
}

#endif
