#ifndef __H5T_STOREMESH_H
#define __H5T_STOREMESH_H

h5_id_t
h5t_add_level (
	h5_file_t * const f
	);

h5_id_t
h5t_store_vertex (
	h5_file_t * const f,
	const h5_id_t global_id,
	const h5_float64_t P[3]
	);

h5_err_t
h5_add_num_tets (
	h5_file_t * const f,
	const h5_size_t num
	);

h5_err_t
h5_add_num_triangles (
	h5_file_t * const f,
	const h5_size_t num
	);

h5_id_t
h5t_store_tet (
	h5_file_t * const f,
	const h5_id_t global_id,
	const h5_id_t parent_id,
	const h5_id_t vids[4]
	);

h5_id_t
h5t_store_triangle (
	h5_file_t * const f,
	const h5_id_t global_id,
	const h5_id_t parent_id,
	const h5_id_t vids[3]
	);
#endif
