#ifndef __H5T_MAP_PRIVATE_H
#define __H5T_MAP_PRIVATE_H

h5_err_t
_h5t_sort_global_vids (
	h5_file_t * const f,
	h5_id_t * const global_vids,
	const h5_size_t size
	);

h5_err_t
_h5t_sort_local_vids (
	h5_file_t * const f,
	h5_id_t * const local_vids,
	const h5_size_t size
	);

h5_err_t
_h5t_sort_vertices (
	h5_file_t * const f
	);

h5_err_t
_h5t_sort_elems (
	h5_file_t * const f
	);

h5_err_t
_h5t_rebuild_global_2_local_map_of_vertices (
	h5_file_t * const f
	);

h5_err_t
_h5t_rebuild_global_2_local_map_of_elems (
	h5_file_t * const f
	);

h5_id_t *
_h5t_get_local_vids_of_edge (
	const h5_elem_ldta_t *tet,
	const h5_id_t face_id,
	h5_2id_t edge
	);

h5_id_t *
_h5t_get_local_vids_of_triangle (
	const h5_elem_ldta_t *tet,
	const h5_id_t face_id,
	h5_2id_t tri
	);

#endif
