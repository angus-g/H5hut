#ifndef __H5T_MAP_PRIVATE_H
#define __H5T_MAP_PRIVATE_H

h5_err_t
h5tpriv_sort_local_vertex_indices (
	h5t_mesh_t* const m,
	h5_loc_idx_t * const indices,
	const h5_size_t size
	);

h5_loc_idx_t
h5tpriv_get_local_vid (
	h5t_mesh_t* const m,
	h5_float64_t P[3]
	);

h5_err_t
h5tpriv_rebuild_vertex_indices_mapping (
	h5t_mesh_t* const m
	);

h5_err_t
h5tpriv_rebuild_elem_indices_mapping (
	h5t_mesh_t* const m
	);

#endif
