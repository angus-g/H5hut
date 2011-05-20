#ifndef __H5T_INQUIRY_H
#define __H5T_INQUIRY_H

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
	h5_file_t * const f
	);

h5_ssize_t
h5t_get_num_elems (
	h5_file_t * const f,
	const h5_id_t cnode
	);

h5_ssize_t
h5t_get_num_vertices (
	h5_file_t * const f,
	const h5_id_t cnode
	);

h5t_lvl_idx_t
h5t_get_level (
	h5_file_t * const f
	);
#endif
