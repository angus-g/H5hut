#ifndef __H5T_STOREMESH_PRIVATE_H
#define __H5T_STOREMESH_PRIVATE_H

struct h5t_store_methods {
	h5_err_t (*alloc_elems)(h5t_mesh_t* const, const size_t, const size_t);
	h5_err_t (*pre_refine)(h5t_mesh_t* const);
	h5_loc_idx_t (*refine_elem)(h5t_mesh_t* const, const h5_loc_idx_t);
	h5_err_t (*end_store_elems)(h5t_mesh_t* const);
};

extern struct h5t_store_methods h5tpriv_trim_store_methods;
extern struct h5t_store_methods h5tpriv_tetm_store_methods;

static inline h5_err_t
h5tpriv_alloc_elems (
	h5t_mesh_t* const m,
	const size_t cur,
	const size_t new
	) {
	return (*m->methods.store->alloc_elems) (m, cur, new);
}

static inline h5_loc_idx_t
h5tpriv_refine_elem (
	h5t_mesh_t * const m,
	const h5_loc_idx_t elem_idx
	) {
	return (*m->methods.store->refine_elem)(m, elem_idx);
}

#endif
