#ifndef __H5T_ACCESS_PRIVATE_H
#define __H5T_ACCESS_PRIVATE_H

struct h5t_access_methods {
	h5_generic_loc_elem_t* (*get_loc_elem)(
		h5t_mesh_t* const, const h5_loc_idx_t);
	h5_glb_idx_t (*get_loc_elem_glb_idx) (
		h5t_mesh_t* const, const h5_loc_idx_t);
	h5_glb_idx_t (*set_loc_elem_glb_idx) (
		h5t_mesh_t* const, const h5_loc_idx_t, const h5_glb_idx_t);
	h5_loc_idx_t (*get_loc_elem_parent_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t);
	h5_loc_idx_t (*set_loc_elem_parent_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t, const h5_loc_idx_t);
	h5_loc_id_t (*get_loc_elem_child_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t);
	h5_loc_id_t (*set_loc_elem_child_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t, const h5_loc_idx_t);
	h5t_lvl_idx_t (*get_loc_elem_level_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t);
	h5t_lvl_idx_t (*set_loc_elem_level_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t, const h5t_lvl_idx_t);
	h5_loc_idx_t* (*get_loc_elem_vertex_indices)(
		h5t_mesh_t* const, const h5_loc_idx_t);
	h5_loc_idx_t (*get_loc_elem_vertex_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t, const h5_loc_idx_t);
	h5_loc_idx_t (*set_loc_elem_vertex_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t, const h5_loc_idx_t, const h5_loc_idx_t);
	h5_loc_idx_t* (*get_loc_elem_neighbor_indices)(
		h5t_mesh_t* const, const h5_loc_idx_t);
	h5_loc_idx_t (*get_loc_elem_neighbor_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t, const h5_loc_idx_t);
	h5_loc_idx_t (*set_loc_elem_neighbor_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t, const h5_loc_idx_t, const h5_loc_idx_t);
	h5_loc_id_t (*get_loc_entity_parent)(
		h5t_mesh_t* const, h5_loc_id_t);
	h5_err_t (*get_loc_entity_children)(
		h5t_mesh_t* const, const h5_loc_id_t, h5_loc_id_t*);
	h5_generic_glb_elem_t* (*get_glb_elem)(
		h5t_mesh_t* const, const h5_loc_idx_t);
	h5_glb_id_t (*get_glb_elem_idx)(
		h5t_mesh_t* const, const h5_loc_id_t);
	h5_glb_id_t (*set_glb_elem_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t, const h5_glb_idx_t);
	h5_glb_idx_t (*get_glb_elem_parent_idx)(
		h5t_mesh_t* const, const h5_loc_id_t);
	h5_glb_idx_t (*set_glb_elem_parent_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t, const h5_glb_idx_t);
	h5_glb_idx_t (*get_glb_elem_child_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t);
	h5_glb_idx_t (*set_glb_elem_child_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t, const h5_glb_idx_t);
	h5_glb_idx_t* (*get_glb_elem_vertex_indices)(
		h5t_mesh_t* const, const h5_loc_idx_t);
	h5_glb_idx_t (*get_glb_elem_vertex_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t, const h5_loc_idx_t);
	h5_glb_id_t (*set_glb_elem_vertex_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t, const h5_loc_idx_t, const h5_glb_idx_t);
	h5_glb_idx_t* (*get_glb_elem_neighbor_indices)(
		h5t_mesh_t* const, const h5_loc_idx_t);
	h5_glb_idx_t (*get_glb_elem_neighbor_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t, const h5_loc_idx_t);
	h5_glb_idx_t (*set_glb_elem_neighbor_idx)(
		h5t_mesh_t* const, const h5_loc_idx_t, const h5_loc_idx_t, const h5_glb_idx_t);
	h5_err_t (*set_boundary_elem_flag)(h5t_mesh_t* const, const h5_loc_idx_t);
	h5_err_t (*clear_boundary_elem_flag)(h5t_mesh_t* const, const h5_loc_idx_t);
	h5_err_t (*set_boundary_facet_flag)(h5t_mesh_t* const, const h5_loc_idx_t);
	h5_err_t (*clear_boundary_facet_flag)(h5t_mesh_t* const, const h5_loc_idx_t);
	int (*is_boundary_elem)(h5t_mesh_t* const, const h5_loc_idx_t);
	int (*is_boundary_facet)(h5t_mesh_t* const, const h5_loc_idx_t, const h5_loc_idx_t);
	int (*is_boundary_face)(h5t_mesh_t* const, const int, const h5_loc_idx_t, const h5_loc_idx_t);
};

extern struct h5t_access_methods h5tpriv_access_trim_methods;
extern struct h5t_access_methods h5tpriv_access_tetm_methods;

static inline h5_generic_loc_elem_t*
h5tpriv_get_loc_elem (
	h5t_mesh_t* const m,
	const h5_loc_idx_t elem_idx
	) {
	return (*m->methods.access->get_loc_elem)(m, elem_idx);
}

static inline h5_glb_idx_t
h5tpriv_set_loc_elem_glb_idx (
	h5t_mesh_t* const m,
	h5_loc_idx_t loc_elem_idx,
	h5_glb_idx_t glb_elem_idx
	) {
	return (*m->methods.access->set_loc_elem_glb_idx)(m, loc_elem_idx, glb_elem_idx);
}

static inline h5_glb_idx_t
h5tpriv_get_loc_elem_glb_idx (
	h5t_mesh_t* const m,
	h5_loc_idx_t loc_elem_idx
	) {
	return (*m->methods.access->get_loc_elem_glb_idx)(m, loc_elem_idx);
}

static inline h5_loc_idx_t
h5tpriv_get_loc_elem_parent_idx (
	h5t_mesh_t* const m,
	const h5_loc_idx_t elem_idx
	) {
	return (*m->methods.access->get_loc_elem_parent_idx)(m, elem_idx);
}

static inline h5_loc_idx_t
h5tpriv_set_loc_elem_parent_idx (
	h5t_mesh_t* const m,
	const h5_loc_idx_t elem_idx,
	const h5_loc_idx_t parent_idx
	) {
	return (*m->methods.access->set_loc_elem_parent_idx)(
		m, elem_idx, parent_idx);
}

static inline h5_loc_idx_t
h5tpriv_get_loc_elem_child_idx (
	h5t_mesh_t* const m,
	const h5_loc_idx_t elem_idx
	) {
	return (*m->methods.access->get_loc_elem_child_idx)(m, elem_idx);
}

static inline h5_loc_idx_t
h5tpriv_set_loc_elem_child_idx (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t child_idx) {
	return (*m->methods.access->set_loc_elem_child_idx)(
		m, elem_idx, child_idx);
}

static inline h5t_lvl_idx_t
h5tpriv_get_loc_elem_level_idx (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx
	) {
	return (*m->methods.access->get_loc_elem_level_idx)(
		m, elem_idx);
}

static inline h5t_lvl_idx_t
h5tpriv_set_loc_elem_level_idx (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx,
	h5t_lvl_idx_t lvl_idx
	) {
	return (*m->methods.access->set_loc_elem_level_idx)(
		m, elem_idx, lvl_idx);
}

static inline h5_loc_idx_t*
h5tpriv_get_loc_elem_vertex_indices (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx
	) {
	return (*m->methods.access->get_loc_elem_vertex_indices)(
		m, elem_idx);
}

static inline h5_loc_idx_t
h5tpriv_get_loc_elem_vertex_idx (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t face_idx
	) {
	return (*m->methods.access->get_loc_elem_vertex_idx)(
		m, elem_idx, face_idx);
}

static inline h5_loc_idx_t
h5tpriv_set_loc_elem_vertex_idx (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t vertex_idx
	) {
	return (*m->methods.access->set_loc_elem_vertex_idx)(
		m, elem_idx, face_idx, vertex_idx);
}

static inline h5_loc_idx_t*
h5tpriv_get_loc_elem_neighbor_indices (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx
	) {
	return (*m->methods.access->get_loc_elem_neighbor_indices)(
		m, elem_idx);
}

static inline h5_loc_idx_t
h5tpriv_get_loc_elem_neighbor_idx (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t face_idx
	) {
	return (*m->methods.access->get_loc_elem_neighbor_idx)(
		m, elem_idx, face_idx);
}

static inline h5_loc_idx_t
h5tpriv_set_loc_elem_neighbor_idx (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t idx
	) {
	return (*m->methods.access->set_loc_elem_neighbor_idx)(
		m, elem_idx, face_idx, idx);
}

static inline h5_loc_id_t
h5tpriv_get_loc_entity_parent (
	h5t_mesh_t* const m,
	h5_loc_id_t entity_id
	) {
	return m->methods.access->get_loc_entity_parent (m, entity_id);
}

static inline h5_err_t
h5tpriv_get_loc_entity_children (
	h5t_mesh_t* const m,
	const h5_loc_id_t elem_id,
	h5_loc_id_t* const children
	) {
	return (*m->methods.access->get_loc_entity_children) (m, elem_id, children);
}

static inline h5_generic_glb_elem_t*
h5tpriv_get_glb_elem (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx
	) {
	return (*m->methods.access->get_glb_elem)(
		m, elem_idx);
}

static inline h5_glb_idx_t
h5tpriv_get_glb_elem_idx (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx
	) {
	return (*m->methods.access->get_glb_elem_idx)(
		m, elem_idx);
}

static inline h5_glb_idx_t
h5tpriv_set_glb_elem_idx (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx,
	h5_glb_idx_t idx
	) {
	return (*m->methods.access->set_glb_elem_idx)(
		m, elem_idx, idx);
}

static inline h5_glb_idx_t
h5tpriv_get_glb_elem_parent_idx (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx
	) {
	return (*m->methods.access->get_glb_elem_parent_idx)(
		m, elem_idx);
}

static inline h5_glb_idx_t
h5tpriv_set_glb_elem_parent_idx (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx,
	h5_glb_idx_t idx
	) {
	return (*m->methods.access->set_glb_elem_parent_idx)(
		m, elem_idx, idx);
}

static inline h5_glb_idx_t
h5tpriv_get_glb_elem_child_idx(
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx
	) {
	return (*m->methods.access->get_glb_elem_child_idx)(
		m, elem_idx);
}

static inline h5_glb_idx_t
h5tpriv_set_glb_elem_child_idx (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx,
	h5_glb_idx_t idx
	) {
	return (*m->methods.access->set_glb_elem_child_idx)(
		m, elem_idx, idx);
}

static inline h5_glb_idx_t*
h5tpriv_get_glb_elem_vertex_indices (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx
	) {
	return (*m->methods.access->get_glb_elem_vertex_indices)(
		m, elem_idx);
}

static inline h5_glb_idx_t
h5tpriv_get_glb_elem_vertex_idx(
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t face_idx
	) {
	return (*m->methods.access->get_glb_elem_vertex_idx)(
		m, elem_idx, face_idx);
}

static inline h5_glb_idx_t
 h5tpriv_set_glb_elem_vertex_idx(
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t face_idx,
	h5_glb_idx_t idx
	) {
	return (*m->methods.access->set_glb_elem_vertex_idx)(
		m, elem_idx, face_idx, idx);
}

static inline h5_glb_idx_t*
h5tpriv_get_glb_elem_neighbor_indices(
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx
	) {
	return (*m->methods.access->get_glb_elem_neighbor_indices)(
		m, elem_idx);
}

static inline h5_glb_idx_t
h5tpriv_get_glb_elem_neighbor_idx (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t face_idx
	) {
	return (*m->methods.access->get_glb_elem_neighbor_idx)(
		m, elem_idx, face_idx);
}

static inline h5_glb_idx_t
h5tpriv_set_glb_elem_neighbor_idx (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t face_idx,
	h5_glb_idx_t idx
	) {
	return (*m->methods.access->set_glb_elem_neighbor_idx)(
		m, elem_idx, face_idx, idx);
}

static inline h5_err_t
h5tpriv_set_boundary_elem_flag (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx
	) {
	return (*m->methods.access->set_boundary_elem_flag)(m, elem_idx);
}

static inline h5_err_t
h5tpriv_clear_boundary_elem_flag (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx
	) {
	return (*m->methods.access->clear_boundary_elem_flag)(m, elem_idx);
}

static inline int
h5tpriv_is_boundary_elem (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx
	) {
	return (*m->methods.access->is_boundary_elem)(m, elem_idx);
}

static inline int
h5tpriv_is_boundary_facet (
	h5t_mesh_t* const m,
	h5_loc_idx_t elem_idx,
	h5_loc_idx_t facet_idx
	) {
	return (*m->methods.access->is_boundary_facet)(m, elem_idx, facet_idx);
}

static inline int
h5tpriv_is_boundary_face (
	h5t_mesh_t* const m,
	const int dim,
	const h5_loc_idx_t elem_idx,
	const h5_loc_idx_t facet_idx
	) {
	return (*m->methods.access->is_boundary_face)(m, dim, elem_idx, facet_idx);
}

#endif
