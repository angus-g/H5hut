#ifndef __H5T_RETRIEVE_H
#define __H5T_RETRIEVE_H

typedef struct h5t_iterator h5t_iterator_t;

h5_err_t
h5t_init_mesh_iterator (
	h5_file_t* f,
	h5t_iterator_t* iter,
	const int codim
	);

h5_err_t
h5t_create_mesh_iterator (
	h5_file_t* f,
	h5t_iterator_t** iter,
	const int codim
	);

h5_err_t
h5t_create_boundary_face_iterator (
	h5_file_t* f,
	h5t_iterator_t** iter,
	const int codim
	);

h5_err_t
h5t_create_mtag_iterator (
	h5_file_t* f,
	h5t_iterator_t** iter,
	const char* name
	);

h5_err_t
h5t_release_entity_iterator (
	h5_file_t* const f,
	h5t_iterator_t* iter
	);

h5_id_t
h5t_iterate_entities (
	h5_file_t * const f,
	h5t_iterator_t *iter
	);

h5_err_t
h5t_end_iterate_entities (
	h5_file_t * const f,
	h5t_iterator_t *iter
	);


h5_err_t
h5t_get_vertex_coords_by_index (
	h5_file_t* const f,
	h5_loc_idx_t vertex_idx,
	h5_float64_t P[3]
	);

h5_err_t
h5t_get_vertex_coords_by_id (
	h5_file_t* const f,
	h5_loc_id_t vertex_id,
	h5_float64_t P[3]
	);
#endif
