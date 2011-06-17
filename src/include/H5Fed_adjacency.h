#ifndef __H5FED_ADJACENCY_H
#define __H5FED_ADJACENCY_H

#ifdef __cplusplus
extern "C" {
#endif

static inline h5_err_t
H5FedGetAdjacencies (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_loc_idlist_t** list
	) {
	H5_API_ENTER4 (h5_err_t,
		       "f=0x%p, entity_id=%lld, dim=%d, list=0x%p",
		       f, (long long)entity_id, dim, list);
	H5_API_RETURN (h5t_get_adjacencies (f, entity_id, dim, list));
}

static inline h5_err_t
H5FedReleaseListOfAdjacencies (
	h5_file_t* const f,
	h5_loc_idlist_t** list
	) {
	H5_API_ENTER2 (h5_err_t,
		       "f=0x%p, list=0x%p",
		       f, list);
	H5_API_RETURN (h5t_release_list_of_adjacencies (f, list));
}

#ifdef __cplusplus
}
#endif

#endif
