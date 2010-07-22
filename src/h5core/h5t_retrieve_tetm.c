#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

static h5_err_t
begin_iterate_entities (
	h5_file_t* f,
	h5t_entity_iterator_t* const iter,
	const int codim
	) {
	iter->face_idx = -1;
	iter->elem_idx = -1;
	iter->codim = codim;
	iter->ref_elem = f->t->ref_elem;
	switch (iter->ref_elem->dim - codim) {
	case 0: // iterate vertices
		iter->find = h5tpriv_find_tv2;
		return h5tpriv_skip_to_next_elem_on_level (f, iter);
	case 1: // iterate edges
		iter->find = h5tpriv_find_te2;
		return h5tpriv_skip_to_next_elem_on_level (f, iter);
	case 2: // iterate faces
		iter->find = h5tpriv_find_td2;
		return h5tpriv_skip_to_next_elem_on_level (f, iter);
	case 3: // iterate elems
		iter->find = NULL;
		return H5_SUCCESS;
	default:
		return h5_error_internal (f, __FILE__, __func__, __LINE__);
	}
}

struct h5t_retrieve_methods h5tpriv_trim_retrieve_methods = {
	begin_iterate_entities,
};
	
