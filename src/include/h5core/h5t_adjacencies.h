#ifndef __H5T_ADJACENCIES_H
#define __H5T_ADJACENCIES_H

#ifdef __cplusplus
extern "C" {
#endif

h5_err_t
h5t_get_adjacencies (
	h5t_mesh_t* const m,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_loc_idlist_t **list
	);

h5_err_t
h5t_release_list_of_adjacencies (
	h5t_mesh_t* const m,
	h5_loc_idlist_t **list
	);

h5_err_t
h5t_find_te2 (
	h5t_mesh_t* const m,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx,
	h5_loc_idlist_t** retval
	);

#ifdef __cplusplus
}
#endif

#endif
