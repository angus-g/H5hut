#ifndef __H5T_STOREMESH_PRIVATE_H
#define __H5T_STOREMESH_PRIVATE_H

extern struct h5t_store_methods h5tpriv_trim_store_methods;
extern struct h5t_store_methods h5tpriv_tetm_store_methods;

static inline h5_err_t
h5tpriv_get_direct_children_of_edge (
	h5_file_t* const f,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx,
	h5_loc_id_t children[2]
	) {
	return (*f->t->methods.store->get_direct_children_of_edge) (
		f, face_idx, elem_idx, children);
}

static inline h5_err_t
h5tpriv_alloc_elems (
	h5_file_t* const f,
	const size_t cur,
	const size_t new
	) {
	return (*f->t->methods.store->alloc_elems) (f, cur, new);
}

static inline h5_loc_idx_t
h5tpriv_refine_elem (
	h5_file_t * const f,
	const h5_loc_idx_t elem_idx
	) {
	return (*f->t->methods.store->refine_elem)(f, elem_idx);
}

#endif
