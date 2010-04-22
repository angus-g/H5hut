#ifndef __H5T_STOREMESH_H
#define __H5T_STOREMESH_H

h5_id_t
h5t_add_mesh (
	h5_file_t * const f,
	const h5_oid_t mesh_type
	);

h5_id_t
h5t_add_level (
	h5_file_t * const f
	);

h5_size_t
h5t_begin_store_vertices (
	h5_file_t * const f,
	const h5_size_t num
	);

h5_id_t
h5t_store_vertex (
	h5_file_t * const f,
	const h5_id_t mesher_vid,
	const h5_float64_t P[3]
	);

h5_err_t
h5t_end_store_vertices (
	h5_file_t * const f
	);

h5_err_t
h5t_begin_store_elems (
	h5_file_t * const f,
	const h5_size_t num
	);

h5_id_t
h5t_store_elem    (
	h5_file_t * const f,
	const h5_id_t local_parent_eid,
	const h5_id_t local_vids[]
	);

h5_err_t
h5t_end_store_elems (
	h5_file_t * const f
	);

h5_err_t
h5t_begin_refine_elems (
	h5_file_t * const f,
	const h5_size_t num_elems_to_refine
	);

h5_id_t
h5t_refine_elem (
	h5_file_t * const f,
	const h5_id_t local_eid
	);

h5_err_t
h5t_end_refine_elems (
	h5_file_t * const f
	);
#endif
