#ifndef __H5T_STOREMESH_PRIVATE_H
#define __H5T_STOREMESH_PRIVATE_H

h5_err_t
_h5t_alloc_num_vertices (
	h5_file_t * const f,
	const h5_size_t num_vertices
	);

h5_size_t
_h5t_add_num_vertices (
	h5_file_t * const f,
	const h5_size_t num
	);

h5_err_t
_h5t_add_num_elems (
	h5_file_t * const f,
	const h5_size_t num
	);

h5_err_t
_h5t_alloc_num_elems (
	h5_file_t * const f,
	const size_t cur_num_elems,
	const size_t new_num_elems
	);
#endif
