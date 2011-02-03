#ifndef __H5T_STOREMESH_H
#define __H5T_STOREMESH_H

h5_id_t
h5t_add_mesh (
	h5_file_t * const f,
	const h5_oid_t mesh_type
	);

h5t_lvl_idx_t
h5t_add_level (
	h5_file_t * const f
	);

h5_err_t
h5t_begin_store_vertices (
	h5_file_t * const f,
	const h5_size_t num
	);

h5_loc_id_t
h5t_store_vertex (
	h5_file_t * const f,
	const h5_glb_id_t glb_id,
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

h5_loc_idx_t
h5t_store_elem    (
	h5_file_t * const f,
	const h5_loc_idx_t parent_idx,
	const h5_loc_idx_t* vertex_indices
	);

h5_err_t
h5t_end_store_elems (
	h5_file_t * const f
	);

h5_err_t
h5t_begin_refine_elems (
	h5_file_t * const f
	);

h5_err_t
h5t_refine_marked_elems (
	h5_file_t * const f
	);

h5_err_t
h5t_end_refine_elems (
	h5_file_t * const f
	);

h5_err_t
h5t_mark_entity (
	h5_file_t* const f,
	const h5_loc_id_t entity_id
	);

h5_err_t
h5t_pre_refine (
	h5_file_t* const f
	);

h5_err_t
h5t_refine (
	h5_file_t* const f
	);

h5_err_t
h5t_post_refine (
	h5_file_t* const f
	);
#endif
