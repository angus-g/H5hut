#ifndef __H5T_MODEL_H
#define __H5T_MODEL_H

h5_err_t h5t_open_tetrahedral_mesh (
	h5_file_t * const f,
	const h5_id_t id
	);

h5_err_t h5t_open_triangle_mesh (
	h5_file_t * const f,
	const h5_id_t id
	);

h5_id_t
h5t_add_tetrahedral_mesh (
	h5_file_t * const f
	);

h5_id_t
h5t_add_triangle_mesh (
	h5_file_t * const f
	);

h5_err_t h5t_set_level (
	h5_file_t * const f,
	const h5t_lvl_idx_t id
	);
h5_err_t h5t_close_mesh (
	h5_file_t * const f
	);

#endif
