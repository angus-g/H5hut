#ifndef __H5T_MAP_H
#define __H5T_MAP_H

h5_id_t
h5t_map_global_vid2local (
	h5_file_t * const f,
	h5_id_t global_vid
	);

h5_err_t
h5t_map_global_vids2local (
	h5_file_t *f,
	const h5_id_t * const global_vids,
	const h5_id_t size,
	h5_id_t * const local_vids
	);

h5_id_t
h5t_map_local_vid2global (
	h5_file_t *f,
	h5_id_t local_vid
	);

h5_id_t
h5t_map_local_eid2global (
	h5_file_t *f,
	h5_id_t local_eid
	);

h5_id_t
h5t_map_global_eid2local (
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
h5t_get_local_vids_of_edge (
	h5_file_t * const f,
	const h5_id_t id,
	h5_id_t *edge
	);

h5_err_t
h5t_get_local_vids_of_edge2 (
	h5_file_t * const f,
	const h5_id_t face,
	const h5_id_t id,
	h5_id_t *vids
	);

h5_err_t
h5t_get_local_vids_of_triangle (
	h5_file_t * const f,
	const h5_id_t id,
	h5_id_t *vids
	);

h5_err_t
h5t_get_local_vids_of_triangle2 (
	h5_file_t * const f,
	const h5_id_t face,
	const h5_id_t id,
	h5_id_t *vids
	);

h5_err_t
h5t_get_local_vids_of_tet (
	h5_file_t * const f,
	const h5_id_t id,
	h5_id_t *vids
	);


#endif
