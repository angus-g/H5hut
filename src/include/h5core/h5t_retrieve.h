#ifndef __H5T_RETRIEVE_H
#define __H5T_RETRIEVE_H

typedef struct {
	h5_id_t elem_idx;	// local element id
	h5_id_t	face_idx;	// face id according reference element
	int codim;		// dimension of entities to traverse
	const h5t_ref_elem_t* ref_elem; // pointer to reference element
	h5_err_t (*find)(h5_file_t *const f,
			 h5_id_t face_idx,
			 h5_id_t elem_idx,
			 h5_idlist_t **retval);
} h5t_entity_iterator_t;

h5_err_t
h5t_alloc_entity_iterator (
	h5_file_t* f,
	h5t_entity_iterator_t** iter,
	int codim
	);

h5_err_t
h5t_release_entity_iterator (
	h5_file_t* const f,
	h5t_entity_iterator_t* iter
	);

h5_err_t
h5t_begin_iterate_entities (
	h5_file_t* f,
	h5t_entity_iterator_t* iter,
	int codim
	);

h5_id_t
h5t_iterate_entities (
	h5_file_t * const f,
	h5t_entity_iterator_t *iter
	);

h5_err_t
h5t_end_iterate_entities (
	h5_file_t * const f,
	h5t_entity_iterator_t *iter
	);


h5_err_t
h5t_get_vertex_coords_by_index (
	h5_file_t* const f,
	h5_id_t vertex_index,
	h5_float64_t P[3]
	);

h5_err_t
h5t_get_vertrex_coords_by_id (
	h5_file_t* const f,
	h5_id_t vertex_id,
	h5_float64_t P[3]
	);
#endif
