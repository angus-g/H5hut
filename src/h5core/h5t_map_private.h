#ifndef __H5T_MAP_PRIVATE_H
#define __H5T_MAP_PRIVATE_H

h5_err_t
h5tpriv_sort_local_vertex_indices (
	h5_file_t * const f,
	h5_id_t * const indices,
	const h5_size_t size
	);

h5_err_t
h5tpriv_sort_vertices (
	h5_file_t * const f
	);

h5_err_t
h5tpriv_sort_loc_elems (
	h5_file_t * const f
	);

h5_err_t
h5tpriv_rebuild_global_2_local_map_of_vertices (
	h5_file_t * const f
	);

h5_err_t
h5tpriv_rebuild_global_2_local_map_of_elems (
	h5_file_t * const f
	);

#endif
