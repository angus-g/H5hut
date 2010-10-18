#ifndef __H5T_RETRIEVE_PRIVATE_H
#define __H5T_RETRIEVE_PRIVATE_H

h5_err_t
h5tpriv_skip_to_next_elem_on_level (
	h5_file_t* f,
	h5t_entity_iterator_t* iter
	);

h5_err_t
h5tpriv_elem_is_on_cur_level (
	h5_file_t* const f,
	h5_generic_loc_elem_t* el
	);

extern struct h5t_retrieve_methods h5tpriv_trim_retrieve_methods;
extern struct h5t_retrieve_methods h5tpriv_tetm_retrieve_methods;

static inline h5_err_t
h5tpriv_init_iterator (
	h5_file_t* f,
	h5t_entity_iterator_t* const iter,
	const int codim
	) {
	return (*f->t->methods.retrieve->init_iterator) (f, iter, codim);
}

#endif
