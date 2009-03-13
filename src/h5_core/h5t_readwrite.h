#ifndef __H5T_READWRITE_H
#define __H5T_READWRITE_H

h5_err_t
_h5t_write_mesh (
	h5_file_t * f
	);

h5_err_t
_h5t_read_vertices (
	h5_file_t * f
	);

h5_err_t
h5t_start_traverse_vertices (
	h5_file_t * f
	);

h5_id_t
h5t_traverse_vertices (
	h5_file_t * f,
	h5_id_t * const  id,
	h5_float64_t P[3]
	);

h5_size_t
h5t_add_num_tets (
	h5_file_t * f,
	const h5_size_t num
	) ;

h5_size_t
h5t_add_num_triangles (
	h5_file_t * f,
	const h5_size_t num
	) ;

h5_size_t
h5t_add_num_elems (
	h5_file_t * f,
	const h5_size_t num
	) ;

h5_err_t
_h5t_read_elems (
	h5_file_t * f
	);

h5_err_t
h5t_start_traverse_tets (
	h5_file_t * f
	);

h5_id_t
h5t_traverse_tets (
	h5_file_t * f,
	h5_id_t * const id,
	h5_id_t * const parent_id,
	h5_id_t vids[4]
	);


h5_err_t
h5t_start_traverse_triangles (
	h5_file_t * f
	);

h5_id_t
h5t_traverse_triangles (
	h5_file_t * f,
	h5_id_t * const id,
	h5_id_t * const parent_id,
	h5_id_t vids[3]
	);

#endif
