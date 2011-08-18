#ifndef __H5T_MAP_H
#define __H5T_MAP_H

#ifdef __cplusplus
extern "C" {
#endif

h5_loc_idx_t
h5t_map_global_vertex_idx2local (
	h5t_mesh_t* const m,
	h5_glb_idx_t glb_idx
	);

h5_err_t
h5t_map_global_vertex_indices2local (
	h5t_mesh_t* f,
	const h5_glb_id_t* const glb_indices,
	const h5_size_t size,
	h5_loc_idx_t* const loc_indices
	);

h5_loc_idx_t
h5t_map_glb_elem_idx2loc (
	h5t_mesh_t* const m,
	const h5_glb_idx_t glb_idx
	);

h5_err_t
h5t_map_glb_elem_indices2loc (
	h5t_mesh_t* const m,
	const h5_glb_idx_t* glb_indices,
	const h5_size_t size,
	h5_loc_idx_t* loc_indices
	);

h5_err_t
h5t_get_vertex_index_of_vertex (
	h5t_mesh_t* const m,
	const h5_loc_id_t entity_id,
	h5_loc_idx_t* vertex_index
	);

h5_err_t
h5t_get_vertex_index_of_vertex2 (
	h5t_mesh_t* const m,
	const h5_loc_idx_t face_idx,
	const h5_loc_idx_t elem_idx,
	h5_loc_idx_t* vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_edge (
	h5t_mesh_t* const m,
	const h5_loc_id_t entity_id,
	h5_loc_idx_t *vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_edge2 (
	h5t_mesh_t* const m,
	const h5_loc_idx_t face_idx,
	const h5_loc_idx_t elem_id,
	h5_loc_idx_t* vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_triangle (
	h5t_mesh_t* const m,
	const h5_loc_id_t entity_id,
	h5_loc_idx_t* vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_triangle2 (
	h5t_mesh_t* const m,
	const h5_loc_idx_t face_idx,
	const h5_loc_idx_t elem_idx,
	h5_loc_idx_t* vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_tet (
	h5t_mesh_t* const m,
	const h5_loc_id_t entity_id,
	h5_loc_idx_t *vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_entity (
	h5t_mesh_t* const m,
	const h5_loc_id_t entity_id,
	h5_loc_idx_t *vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_entity2 (
	h5t_mesh_t* const m,
	const int dim,
	const h5_loc_idx_t face_idx,
	const h5_loc_idx_t elem_idx,
	h5_loc_idx_t* vertex_indices
	);

#ifdef __cplusplus
}
#endif

#endif
