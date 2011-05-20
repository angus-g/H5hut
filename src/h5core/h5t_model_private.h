#ifndef __H5T_MODEL_PRIVATE_H
#define __H5T_MODEL_PRIVATE_H

h5_err_t
h5tpriv_open_file (
	h5_file_t * const f
	);
h5_err_t
h5tpriv_close_file (
	h5_file_t * const f
	);
h5_err_t
h5tpriv_close_step (
	h5_file_t * const f
	);
h5_err_t
h5tpriv_init_fdata (
	h5_file_t * const f
	);
h5_err_t
h5tpriv_init_step (
	h5_file_t * const f
	);
h5_err_t
h5tpriv_alloc_num_vertices (
	h5_file_t * const f,
	const h5_size_t num_vertices
	);
h5_err_t
h5tpriv_alloc_tris (
	h5_file_t * const f,
	const size_t cur,
	const size_t new_size
	);
h5_err_t
h5tpriv_alloc_tets (
	h5_file_t * const f,
	const size_t cur,
	const size_t new_size
	);

#endif
