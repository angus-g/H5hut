#ifndef __T_MAP_PRIVATE_H
#define __T_MAP_PRIVATE_H

h5_err_t
_h5t_sort_global_vertex_ids (
	h5_file * const f,
	h5_id_t * const global_vids,
	const h5_size_t size
	);

h5_err_t
_h5t_sort_local_vertex_ids (
	h5_file * const f,
	h5_id_t * const local_vids,
	const h5_size_t size
	);

#endif
