#ifndef __H5T_ADJACENCIES_PRIVATE_H
#define __H5T_ADJACENCIES_PRIVATE_H

struct h5t_adjacency_methods {
	h5_err_t (*update_internal_structs)(h5_file_t* const, h5t_lvl_idx_t);
	h5_err_t (*release_internal_structs)(h5_file_t* const);
	h5_err_t (*get_adjacencies)(
		h5_file_t * const,
		const h5_loc_id_t, const h5_int32_t, h5_loc_idlist_t**);
};

extern struct h5t_adjacency_methods h5tpriv_trim_adjacency_methods;
extern struct h5t_adjacency_methods h5tpriv_tetm_adjacency_methods;

static inline h5_err_t
h5tpriv_get_adjacencies (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_loc_idlist_t** list
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	if (f->t->methods.adjacency == NULL) {
		H5_PRIV_API_LEAVE (h5_error_internal ());
	}
	H5_PRIV_API_RETURN (f->t->methods.adjacency->get_adjacencies(
				    f, entity_id, dim, list));
}

static inline h5_err_t
h5tpriv_release_adjacency_structs (
	h5_file_t* const f
	) {
	h5_debug ("%s ()", __func__);
	if (f->t->methods.adjacency == NULL) {
		return 0;
	}
	return (*f->t->methods.adjacency->release_internal_structs)(f);
}

static inline h5_err_t
h5tpriv_update_adjacency_structs (
	h5_file_t* const f,
	const h5t_lvl_idx_t level_id
	) {
	h5_debug ("%s (%lld)", __func__, (long long)level_id);
	return (*f->t->methods.adjacency->update_internal_structs)(f, level_id);
}

#endif
