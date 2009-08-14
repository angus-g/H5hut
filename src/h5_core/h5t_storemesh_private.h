#ifndef __H5T_STOREMESH_PRIVATE_H
#define __H5T_STOREMESH_PRIVATE_H

h5_err_t
_h5t_alloc_num_vertices (
	h5_file_t * const f,
	const h5_size_t num_vertices
	);

h5_err_t
_h5t_alloc_tris (
	h5_file_t * const f,
	const size_t cur,
	const size_t new
	);

h5_err_t
_h5t_alloc_tets (
	h5_file_t * const f,
	const size_t cur,
	const size_t new
	);

h5_id_t
_h5t_store_tri (
	h5_file_t * const f,
	const h5_id_t local_parent_eid,
	const h5_id_t vids[]
	);

h5_id_t
_h5t_store_tet (
	h5_file_t * const f,
	const h5_id_t local_parent_eid,
	const h5_id_t vids[]
	);

h5_err_t
_h5t_close_level (
	h5_file_t * const f
	);

h5_err_t
_h5t_compute_direct_children_of_edge (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5_id_t	kids[2]
	);

h5_id_t
_h5t_refine_tri (
	h5_file_t * const f,
	const h5_id_t local_eid
	);

h5_id_t
_h5t_refine_tet (
	h5_file_t * const f,
	const h5_id_t local_eid
	);

#endif
