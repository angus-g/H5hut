#ifndef __H5T_RETRIEVE_PRIVATE_H
#define __H5T_RETRIEVE_PRIVATE_H

h5_err_t
h5tpriv_skip_to_next_elem_on_level (
	h5_file_t* f,
	h5t_entity_iterator_t* iter
	);

h5_err_t
h5tpriv_elem_is_on_cur_level (
	h5_file_t * const f,
	h5_elem_ldta_t *el_dta 
	);

#endif
