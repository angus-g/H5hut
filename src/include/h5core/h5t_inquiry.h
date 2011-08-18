#ifndef __H5T_INQUIRY_H
#define __H5T_INQUIRY_H

#ifdef __cplusplus
extern "C" {
#endif

h5_ssize_t
h5t_get_num_tetmeshes (
	h5_file_t * const f
	);

h5_ssize_t
h5t_get_num_trimeshes (
	h5_file_t * const f
	);

h5_ssize_t
h5t_get_num_leaf_levels (
	h5t_mesh_t* const m
	);

h5_ssize_t
h5t_get_num_elems (
	h5t_mesh_t* const m,
	const h5_id_t cnode
	);

h5_ssize_t
h5t_get_num_vertices (
	h5t_mesh_t* const m,
	const h5_id_t cnode
	);

h5t_lvl_idx_t
h5t_get_level (
	h5t_mesh_t* const m
	);

#ifdef __cplusplus
}
#endif

#endif
