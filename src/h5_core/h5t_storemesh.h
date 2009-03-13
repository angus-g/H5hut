#ifndef __H5T_STOREMESH_H
#define __H5T_STOREMESH_H

h5_id_t
h5t_add_mesh (
	h5_file_t * const f,
	const h5_size_t num_elems,
	const h5_oid_t mesh_type
	);

h5_id_t
h5t_add_level (
	h5_file_t * const f,
	const h5_size_t num_vertices,
	const h5_size_t num_elems
	);

h5_id_t
h5t_store_vertex (
	h5_file_t * const f,
	const h5_id_t mesher_vid,
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
h5t_store_elem    (
	h5_file_t * const f,
	const h5_id_t local_parent_eid,
	const h5_id_t local_vids[]
	);

h5_id_t
_h5t_store_triangle (
	h5_file_t * const f,
	const h5_id_t local_parent_eid,
	const h5_id_t vids[3]
	);

h5_id_t
_h5t_store_tet (
	h5_file_t * const f,
	const h5_id_t local_parent_eid,
	const h5_id_t vids[4]
	);

h5_id_t
h5t_refine_elem (
	h5_file_t * const f,
	const h5_id_t local_eid
	);

h5_id_t
_h5t_refine_triangle (
	h5_file_t * const f,
	const h5_id_t local_eid
	);

h5_id_t
_h5t_refine_tet (
	h5_file_t * const f,
	const h5_id_t local_eid
	);
#endif
