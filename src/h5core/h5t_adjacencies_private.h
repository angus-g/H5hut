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
	H5_PRIV_API_ENTER4 (h5_err_t,
			    "f=0x%p, entity_id=%lld, dim=%d, list=0x%p",
			    f, (long long)entity_id, dim, list);
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
	H5_PRIV_API_ENTER1 (h5_err_t, "f=0x%p", f);
	if (f->t->methods.adjacency == NULL) {
		H5_PRIV_API_LEAVE (H5_OK);
	}
	H5_PRIV_API_RETURN (f->t->methods.adjacency->release_internal_structs(f));
}

static inline h5_err_t
h5tpriv_update_adjacency_structs (
	h5_file_t* const f,
	const h5t_lvl_idx_t level_id
	) {
	H5_PRIV_API_ENTER2 (h5_err_t, "f=0x%p, level_id=%d", f, level_id);
	if (f->t->methods.adjacency == NULL) {
		H5_PRIV_API_LEAVE (H5_OK);
	}
	H5_PRIV_API_RETURN (f->t->methods.adjacency->update_internal_structs(f, level_id));
}

#endif
