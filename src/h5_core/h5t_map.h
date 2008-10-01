#ifndef __T_MAP_H
#define __T_MAP_H

h5_id_t
h5t_map_global_vertex_id2local (
	h5_file_t *f,
	h5_id_t global_vid
	);

h5_err_t
h5t_map_global_vertex_ids2local (
	h5_file_t *f,
	const h5_id_t * const global_vids,
	const h5_id_t size,
	h5_id_t * const local_vids
	);

h5_id_t
h5t_map_local_vertex_id2global (
	h5_file_t *f,
	h5_id_t local_vid
	);

h5_id_t
h5t_map_local_entity_id2global (
	h5_file_t *f,
	h5_id_t local_eid
	);

h5_id_t
h5t_map_global_entity_id2local (
	h5_file_t * const f,
	const h5_id_t global_eid
	);

h5_id_t
h5t_map_local_triangle_id2global (
	h5_file_t * const f,
	const h5_id_t local_tid
	);

h5_id_t
h5t_map_global_triangle_id2local (
	h5_file_t * const f,
	const h5_id_t global_tid
	);

h5_id_t
h5t_get_local_entity_id (
	h5_file_t *f,
	h5_id_t * const local_vids
	);

h5_id_t
h5t_get_global_entity_id (
	h5_file_t *f,
	const h5_id_t * const global_vids
	);

h5_id_t
h5t_get_local_triangle_id  (
	h5_file_t * const f,
	h5_id_t * const local_vids
	);

h5_id_t
h5t_get_global_triangle_id (
	h5_file_t * const f,
	h5_id_t * const global_vids
	);

#endif
