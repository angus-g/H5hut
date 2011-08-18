#ifndef __H5FED_ADJACENCY_H
#define __H5FED_ADJACENCY_H

#ifdef __cplusplus
extern "C" {
#endif

static inline h5_err_t
H5FedGetAdjacencies (
	h5t_mesh_t* const m,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_loc_idlist_t** list
	) {
	H5_API_ENTER (h5_err_t,
		      "m=%p, entity_id=%lld, dim=%d, list=%p",
		      m, (long long)entity_id, dim, list);
	H5_API_RETURN (h5t_get_adjacencies (m, entity_id, dim, list));
}

static inline h5_err_t
H5FedReleaseListOfAdjacencies (
	h5t_mesh_t* const m,
	h5_loc_idlist_t** list
	) {
	H5_API_ENTER (h5_err_t, "f=%p, list=%p", m, list);
	H5_API_RETURN (h5t_release_list_of_adjacencies (m, list));
}

#ifdef __cplusplus
}
#endif

#endif
