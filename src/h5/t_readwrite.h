#ifndef __T_READWRITE_H
#define __T_READWRITE_H

h5_err_t
_h5t_init_step (
	h5_file * f
	);

h5_err_t
_h5t_close_step (
	h5_file * f
	);

h5_err_t
_h5t_close_mesh (
	h5_file * f
	);

h5_size_t
h5t_get_num_meshes (
	h5_file * f,
	const enum h5_mesh_types type
	);

h5_err_t
h5t_open_mesh (
	h5_file * f,
	h5_id_t id,
	const enum h5_mesh_types type
	);

h5_id_t
h5t_add_mesh (
	h5_file * f
	);

h5_size_t
h5t_get_num_levels (
	h5_file * f
	);

h5_err_t
h5t_open_level (
	h5_file * f,
	const h5_id_t id
	);

h5_id_t
h5t_get_level (
	h5_file * f
	);

h5_id_t
h5t_add_level (
	h5_file * f
	);

h5_size_t
h5t_add_num_vertices (
	h5_file * f,
	const h5_size_t num
	);

h5_id_t
h5t_store_vertex (
	h5_file * f,
	const h5_id_t id,
	const h5_float64_t P[3]
	);

h5_size_t
h5t_get_num_vertices (
	h5_file * f
	);

h5_err_t
h5t_start_traverse_vertices (
	h5_file * f
	);

h5_id_t
h5t_traverse_vertices (
	h5_file * f,
	h5_id_t * const  id,
	h5_float64_t P[3]
	);

h5_size_t
h5t_add_num_entities (
	h5_file * f,
	const h5_size_t num
	) ;

h5_size_t
h5t_get_num_entities (
	h5_file * f
	);

h5_id_t
h5t_store_tet (
	h5_file * f,
	const h5_id_t id,
	const h5_id_t parent_id,
	const h5_id_t vertex_ids[4]
	);

h5_id_t
h5t_store_triangle (
	h5_file * f,
	const h5_id_t id,
	const h5_id_t parent_id,
	const h5_id_t vertex_ids[3]
	);

h5_err_t
h5t_start_traverse_tets (
	h5_file * f
	);

h5_id_t
h5t_traverse_tets (
	h5_file * f,
	h5_id_t * const id,
	h5_id_t * const parent_id,
	h5_id_t vertex_ids[4]
	);


h5_err_t
h5t_start_traverse_triangles (
	h5_file * f
	);

h5_id_t
h5t_traverse_triangles (
	h5_file * f,
	h5_id_t * const id,
	h5_id_t * const parent_id,
	h5_id_t vertex_ids[3]
	);

#endif
