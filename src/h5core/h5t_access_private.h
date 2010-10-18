#ifndef __H5T_ACCESS_PRIVATE_H
#define __H5T_ACCESS_PRIVATE_H

extern struct h5t_access_methods h5tpriv_access_trim_methods;
extern struct h5t_access_methods h5tpriv_access_tetm_methods;

static inline h5_generic_loc_elem_t*
h5tpriv_get_loc_elem (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	return (*f->t->methods.access->get_loc_elem)(f, elem_idx);
}

static inline h5_loc_idx_t
h5tpriv_get_loc_elem_parent_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	return (*f->t->methods.access->get_loc_elem_parent_idx)(f, elem_idx);
}

static inline h5_loc_idx_t
h5tpriv_set_loc_elem_parent_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx,
	const h5_loc_idx_t parent_idx
	) {
	return (*f->t->methods.access->set_loc_elem_parent_idx)(
		f, elem_idx, parent_idx);
}

static inline h5_loc_idx_t
h5tpriv_get_loc_elem_child_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	return (*f->t->methods.access->get_loc_elem_child_idx)(
		f, elem_idx);
}

static inline h5_loc_idx_t
h5tpriv_set_loc_elem_child_idx (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t child_idx) {
	return (*f->t->methods.access->set_loc_elem_child_idx)(
		f, elem_idx, child_idx);
}

static inline h5_id_t
h5tpriv_get_loc_elem_level_idx (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx
	) {
	return (*f->t->methods.access->get_loc_elem_level_idx)(
		f, elem_idx);
}

static inline h5_id_t
h5tpriv_set_loc_elem_level_idx (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx,
	h5_id_t lvl_idx
	) {
	return (*f->t->methods.access->set_loc_elem_level_idx)(
		f, elem_idx, lvl_idx);
}

static inline h5_loc_idx_t*
h5tpriv_get_loc_elem_vertex_indices (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx
	) {
	return (*f->t->methods.access->get_loc_elem_vertex_indices)(
		f, elem_idx);
}

static inline h5_loc_idx_t
h5tpriv_get_loc_elem_vertex_idx (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t face_idx
	) {
	return (*f->t->methods.access->get_loc_elem_vertex_idx)(
	f, elem_idx, face_idx);
}

static inline h5_loc_idx_t
h5tpriv_set_loc_elem_vertex_idx (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t vertex_idx
	) {
	return (*f->t->methods.access->set_loc_elem_vertex_idx)(
		f, elem_idx, face_idx, vertex_idx);
}

static inline h5_loc_idx_t*
h5tpriv_get_loc_elem_neighbor_indices (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx
	) {
	return (*f->t->methods.access->get_loc_elem_neighbor_indices)(
		f, elem_idx);
}

static inline h5_loc_idx_t
h5tpriv_get_loc_elem_neighbor_idx (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t face_idx
	) {
	return (*f->t->methods.access->get_loc_elem_neighbor_idx)(
		f, elem_idx, face_idx);
}

static inline h5_loc_idx_t
h5tpriv_set_loc_elem_neighbor_idx (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t idx
	) {
	return (*f->t->methods.access->set_loc_elem_neighbor_idx)(
		f, elem_idx, face_idx, idx);
}

static inline h5_generic_glb_elem_t*
h5tpriv_get_glb_elem (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx
	) {
	return (*f->t->methods.access->get_glb_elem)(
		f, elem_idx);
}

static inline h5_glb_idx_t
h5tpriv_get_glb_elem_idx (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx
	) {
	return (*f->t->methods.access->get_glb_elem_idx)(
		f, elem_idx);
}

static inline h5_glb_idx_t
h5tpriv_set_glb_elem_idx (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx,
	h5_glb_idx_t idx
	) {
	return (*f->t->methods.access->set_glb_elem_idx)(
		f, elem_idx, idx);
}

static inline h5_glb_idx_t
h5tpriv_get_glb_elem_parent_idx (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx
	) {
	return (*f->t->methods.access->get_glb_elem_parent_idx)(
		f, elem_idx);
}

static inline h5_glb_idx_t
h5tpriv_set_glb_elem_parent_idx (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx,
	h5_glb_idx_t idx
	) {
	return (*f->t->methods.access->set_glb_elem_parent_idx)(
		f, elem_idx, idx);
}

static inline h5_glb_idx_t
h5tpriv_get_glb_elem_child_idx(
	h5_file_t* const f,
	h5_loc_idx_t elem_idx
	) {
	return (*f->t->methods.access->get_glb_elem_child_idx)(
		f, elem_idx);
}

static inline h5_glb_idx_t
h5tpriv_set_glb_elem_child_idx (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx,
	h5_glb_idx_t idx
	) {
	return (*f->t->methods.access->set_glb_elem_child_idx)(
		f, elem_idx, idx);
}

static inline h5_glb_idx_t*
h5tpriv_get_glb_elem_vertex_indices (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx
	) {
	return (*f->t->methods.access->get_glb_elem_vertex_indices)(
		f, elem_idx);
}

static inline h5_glb_idx_t
h5tpriv_get_glb_elem_vertex_idx(
	h5_file_t* const f,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t face_idx
	) {
	return (*f->t->methods.access->get_glb_elem_vertex_idx)(
		f, elem_idx, face_idx);
}

static inline h5_glb_idx_t
 h5tpriv_set_glb_elem_vertex_idx(
	h5_file_t* const f,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t face_idx,
	h5_glb_idx_t idx
	) {
	return (*f->t->methods.access->set_glb_elem_vertex_idx)(
		f, elem_idx, face_idx, idx);
}

static inline h5_glb_idx_t*
h5tpriv_get_glb_elem_neighbor_indices(
	h5_file_t* const f,
	h5_loc_idx_t elem_idx
	) {
	return (*f->t->methods.access->get_glb_elem_neighbor_indices)(
		f, elem_idx);
}

static inline h5_glb_idx_t
h5tpriv_get_glb_elem_neighbor_idx (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t face_idx
	) {
	return (*f->t->methods.access->get_glb_elem_neighbor_idx)(
		f, elem_idx, face_idx);
}

static inline h5_glb_idx_t
h5tpriv_set_glb_elem_neighbor_idx (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t face_idx,
	h5_glb_idx_t idx
	) {
	return (*f->t->methods.access->set_glb_elem_neighbor_idx)(
		f, elem_idx, face_idx, idx);
}

#endif
