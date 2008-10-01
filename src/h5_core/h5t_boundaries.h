#ifndef __H5T_BOUNDARIES_H
#define __H5T_BOUNDARIES_H

h5_err_t
h5t_open_boundary (
	h5_file_t * const f,
	const h5_id_t boundary_id
	);

h5_err_t
h5t_close_boundary (
	h5_file_t * const f
	);

h5_err_t
h5t_add_num_boundaryfaces (
	h5_file_t * const f,
	const h5_id_t num
	);

h5_id_t
h5t_get_num_boundaryfaces (
	h5_file_t * const f
	);

h5_id_t
h5t_store_boundaryface (
	h5_file_t * const f,
	h5_id_t * const vertices
	);

h5_id_t
h5t_store_boundaryface_global_id (
	h5_file_t * const f,
	const h5_id_t global_fid
	);

h5_id_t
h5t_store_boundaryface_local_id (
	h5_file_t * const f,
	const h5_id_t local_fid
	);

h5_err_t
h5t_start_traverse_boundary_faces (
	h5_file_t * const f
	);

h5_id_t
h5t_traverse_boundary_faces (
	h5_file_t * const f,
	h5_id_t * const global_id,
	h5_id_t * const parent_id,
	h5_id_t vertex_ids[]
	);
#endif
