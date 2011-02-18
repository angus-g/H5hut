#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

static h5_err_t
begin_iterate_entities (
	h5_file_t* f,
	h5t_iterator_t* const iter,
	const int codim
	) {
	h5t_leaf_iterator_t* it = (h5t_leaf_iterator_t*)&iter->leaf;
	switch (it->ref_elem->dim - codim) {
	case 0: // iterate vertices
		it->find = h5tpriv_find_tv2;
		break;
	case 1: // iterate edges
		it->find = h5tpriv_find_te2;
		break;
	case 2: // iterate faces
		it->find = h5tpriv_find_td2;
		break;
	case 3: // iterate elems
		it->find = NULL;
		break;
	default:
		return h5_error_internal (__FILE__, __func__, __LINE__);
	}
	return H5_SUCCESS;
}

struct h5t_retrieve_methods h5tpriv_tetm_retrieve_methods = {
	begin_iterate_entities,
};
	
