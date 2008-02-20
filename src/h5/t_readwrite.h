#ifndef __T_READWRITE_H
#define __T_READWRITE_H

h5_err_t
_h5t_init_step (
	h5_file * f
	);

h5_err_t
_h5t_close_step (
	h5_file * f
	);


h5_id_t
H5t_add_mesh (
	h5_file * f
	);

h5_id_t
H5t_add_level (
	h5_file * f
	);

h5_size_t
H5t_add_num_vertices (
	h5_file * f,
	const h5_size_t num
	);

h5_id_t
H5t_store_vertex (
	h5_file * f,
	const h5_id_t id,
	const h5_float64_t P[3]
	);

h5_id_t
H5t_get_num_vertices (
	h5_file * f
	);

h5_id_t
H5t_get_vertex (
	h5_file * f,
	h5_id_t * const  id,
	h5_float64_t * const P[3]
	);

h5_size_t
H5t_add_num_tets (
	h5_file * f,
	const h5_size_t num
	) ;

h5_size_t
H5t_get_num_tets (
	h5_file * f
	);

h5_id_t
H5t_store_tet (
	h5_file * f,
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	const h5_id_t parent_id,	/*!< global parent id
					     if level \c >0 else \c -1	*/
	const h5_id_t vertex_ids[4]	/*!< tuple with vertex id's	*/
	);

h5_id_t
H5t_get_tet (
	h5_file * f,
	h5_id_t * const id,
	h5_id_t * const parent_id,
	h5_id_t * const vertex_ids[4]
	);

h5_id_t
_h5t_open_mesh (
	h5_file * f
	);

h5_id_t
_h5t_close_mesh (
	h5_file * f
	);

#endif
