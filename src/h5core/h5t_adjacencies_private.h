#ifndef __H5T_ADJACENCIES_PRIVATE_H
#define __H5T_ADJACENCIES_PRIVATE_H

struct h5t_adjacency_methods {
	h5_err_t (*update_internal_structs)(h5t_mesh_t* const, h5t_lvl_idx_t);
	h5_err_t (*release_internal_structs)(h5t_mesh_t* const);
	h5_err_t (*get_adjacencies)(
		h5t_mesh_t* const,
		const h5_loc_id_t, const h5_int32_t, h5_loc_idlist_t**);
};

extern struct h5t_adjacency_methods h5tpriv_trim_adjacency_methods;
extern struct h5t_adjacency_methods h5tpriv_tetm_adjacency_methods;

static inline h5_err_t
h5tpriv_get_adjacencies (
	h5t_mesh_t* const m,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_loc_idlist_t** list
	) {
	H5_PRIV_API_ENTER (h5_err_t,
			   "m=%p, entity_id=%lld, dim=%d, list=%p",
			   m, (long long)entity_id, dim, list);
	if (m->methods.adjacency == NULL) {
		H5_PRIV_API_LEAVE (h5_error_internal ());
	}
	H5_PRIV_API_RETURN (m->methods.adjacency->get_adjacencies(
				    m, entity_id, dim, list));
}

static inline h5_err_t
h5tpriv_release_adjacency_structs (
	h5t_mesh_t* const m
	) {
	H5_PRIV_API_ENTER (h5_err_t, "m=%p", m);
	if (m->methods.adjacency == NULL) {
		H5_PRIV_API_LEAVE (H5_OK);
	}
	H5_PRIV_API_RETURN (m->methods.adjacency->release_internal_structs(m));
}

static inline h5_err_t
h5tpriv_update_adjacency_structs (
	h5t_mesh_t* const m,
	const h5t_lvl_idx_t level_id
	) {
	H5_PRIV_API_ENTER (h5_err_t, "m=%p, level_id=%d", m, level_id);
	if (m->methods.adjacency == NULL) {
		H5_PRIV_API_LEAVE (H5_OK);
	}
	H5_PRIV_API_RETURN (m->methods.adjacency->update_internal_structs(m, level_id));
}

#endif
