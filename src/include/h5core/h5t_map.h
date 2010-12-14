#ifndef __H5T_MAP_H
#define __H5T_MAP_H

h5_id_t
h5t_map_global_vertex_idx2local (
	h5_file_t * const f,
	h5_glb_idx_t glb_idx
	);

h5_err_t
h5t_map_global_vertex_indices2local (
	h5_file_t* f,
	const h5_glb_id_t* const glb_indices,
	const h5_size_t size,
	h5_loc_idx_t* const loc_indices
	);

h5_loc_idx_t
h5t_map_glb_elem_idx2loc (
	h5_file_t * const f,
	const h5_glb_idx_t glb_idx
	);

h5_err_t
h5t_map_glb_elem_indices2loc (
	h5_file_t * const f,
	const h5_glb_idx_t* glb_indices,
	const h5_size_t size,
	h5_loc_idx_t* loc_indices
	);

h5_err_t
h5t_get_vertex_index_of_vertex (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_id_t* vertex_index
	);

h5_err_t
h5t_get_vertex_index_of_vertex2 (
	h5_file_t* const f,
	const h5_loc_idx_t face_idx,
	const h5_loc_idx_t elem_idx,
	h5_loc_idx_t* vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_edge (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idx_t *vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_edge2 (
	h5_file_t* const f,
	const h5_loc_idx_t face_idx,
	const h5_loc_idx_t elem_id,
	h5_loc_idx_t* vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_triangle (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idx_t* vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_triangle_cclockwise (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idx_t* vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_triangle2 (
	h5_file_t* const f,
	const h5_loc_idx_t face_idx,
	const h5_loc_idx_t elem_idx,
	h5_loc_idx_t* vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_tet (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	h5_loc_idx_t *vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_entity (
	h5_file_t * const f,
	const h5_loc_id_t entity_id,
	h5_loc_idx_t *vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_entity2 (
	h5_file_t* const f,
	const int dim,
	const h5_loc_idx_t face_idx,
	const h5_loc_idx_t elem_idx,
	h5_loc_idx_t* vertex_indices
	);

#endif
