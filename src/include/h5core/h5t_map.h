#ifndef __H5T_MAP_H
#define __H5T_MAP_H

h5_id_t
h5t_map_global_vertex_idx2local (
	h5_file_t * const f,
	h5_id_t global_idx
	);

h5_err_t
h5t_map_global_vertex_indices2local (
	h5_file_t* f,
	const h5_id_t* const glb_indices,
	const h5_id_t size,
	h5_id_t* const loc_indices
	);

h5_id_t
h5t_map_local_vid2global (
	h5_file_t *f,
	h5_id_t local_vid
	);

h5_id_t
h5t_map_global_elem_idx2local (
	h5_file_t * const f,
	const h5_id_t global_idx
	);

h5_id_t
h5t_map_global_elem_indices2local (
	h5_file_t * const f,
	const h5_id_t* glb_indices,
	const h5_id_t size,
	h5_id_t* loc_indices
	);

h5_id_t
h5tpriv_get_local_vid (
	h5_file_t * const f,
	h5_float64_t P[3]
	);

h5_id_t
h5t_get_local_eid (
	h5_file_t *f,
	h5_id_t * const local_vids
	);

h5_err_t
h5t_get_vertex_index_of_vertex (
	h5_file_t* const f,
	const h5_id_t entity_id,
	h5_id_t* vertex_index
	);

h5_err_t
h5t_get_vertex_index_of_vertex2 (
	h5_file_t* const f,
	const h5_id_t face_idx,
	const h5_id_t elem_idx,
	h5_id_t* vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_edge (
	h5_file_t* const f,
	const h5_id_t entity_id,
	h5_id_t *vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_edge2 (
	h5_file_t* const f,
	const h5_id_t face_idx,
	const h5_id_t elem_id,
	h5_id_t* vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_triangle (
	h5_file_t* const f,
	const h5_id_t entity_id,
	h5_id_t* vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_triangle2 (
	h5_file_t* const f,
	const h5_id_t face_idx,
	const h5_id_t elem_idx,
	h5_id_t* vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_tet (
	h5_file_t* const f,
	const h5_id_t entity_id,
	h5_id_t *vertex_indices
	);

h5_err_t
h5t_get_vertex_indices_of_entity (
	h5_file_t * const f,
	const h5_id_t entity_id,
	h5_id_t *vertex_indices
	);

#endif
