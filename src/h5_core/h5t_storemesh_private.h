#ifndef __H5T_STOREMESH_PRIVATE_H
#define __H5T_STOREMESH_PRIVATE_H

h5_err_t
_h5t_alloc_num_vertices (
	h5_file_t * const f,
	const h5_size_t num_vertices
	);

h5_err_t
_h5t_alloc_num_elems (
	h5_file_t * const f,
	const size_t cur_num_elems,
	const size_t new_num_elems
	);

h5_id_t
_h5t_store_elem (
	h5_file_t * const f,
	const h5_id_t local_parent_eid,
	const h5_id_t vids[]
	);


h5_err_t
_h5t_close_level (
	h5_file_t * const f
	);
h5_id_t
_h5t_refine_triangle (
	h5_file_t * const f,
	const h5_id_t local_eid
	);

h5_id_t
_h5t_refine_tet (
	h5_file_t * const f,
	const h5_id_t local_eid
	);

#endif
