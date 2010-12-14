#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*** op's on local elements ***/
static h5_generic_loc_elem_t*
get_loc_elem (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	return (h5_generic_loc_elem_t*)&f->t->loc_elems.tris[elem_idx];
}

static h5_loc_idx_t
get_loc_elem_parent_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	return f->t->loc_elems.tris[elem_idx].parent_idx;
}

static h5_loc_idx_t
set_loc_elem_parent_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx,
	const h5_loc_idx_t parent_idx
	) {
	f->t->loc_elems.tris[elem_idx].parent_idx = parent_idx;
	return parent_idx;
}

static h5_loc_idx_t
get_loc_elem_child_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	return f->t->loc_elems.tris[elem_idx].child_idx;
}

static h5_loc_idx_t
set_loc_elem_child_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx,
	const h5_loc_idx_t child_idx
	) {
	f->t->loc_elems.tris[elem_idx].child_idx = child_idx;
	return child_idx;
}

static h5t_lvl_idx_t
get_loc_elem_level_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	return f->t->loc_elems.tris[elem_idx].level_idx;
}

static h5t_lvl_idx_t
set_loc_elem_level_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx,
	const h5t_lvl_idx_t level_idx
	) {
	f->t->loc_elems.tris[elem_idx].level_idx = level_idx;
	return level_idx;
}

static h5_loc_idx_t*
get_loc_elem_vertex_indices (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	return f->t->loc_elems.tris[elem_idx].vertex_indices;
}

static h5_loc_idx_t
get_loc_elem_vertex_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx,
	const h5_loc_idx_t face_idx
	) {
	return f->t->loc_elems.tris[elem_idx].vertex_indices[face_idx];
}

static h5_loc_idx_t
set_loc_elem_vertex_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx,
	const h5_loc_idx_t face_idx,
	const h5_loc_idx_t vertex_idx
	) {
	f->t->loc_elems.tris[elem_idx].vertex_indices[face_idx] = vertex_idx;
	return vertex_idx;
}

static h5_loc_idx_t*
get_loc_elem_neighbor_indices (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	return f->t->loc_elems.tris[elem_idx].neighbor_indices;
}

static h5_loc_idx_t
get_loc_elem_neighbor_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx,
	const h5_loc_idx_t face_idx
	) {
	return f->t->loc_elems.tris[elem_idx].neighbor_indices[face_idx];
}

static h5_loc_idx_t
set_loc_elem_neighbor_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx,
	const h5_loc_idx_t face_idx,
	const h5_loc_idx_t neighbor_idx
	) {
	f->t->loc_elems.tris[elem_idx].neighbor_indices[face_idx] = neighbor_idx;
	return neighbor_idx;
}


/*** op's on global elements ***/
static h5_generic_glb_elem_t*
get_glb_elem (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	return (h5_generic_glb_elem_t*)&f->t->glb_elems.tris[elem_idx];
}

static h5_glb_idx_t
get_glb_elem_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	return f->t->glb_elems.tris[elem_idx].idx;
}

static h5_glb_idx_t
set_glb_elem_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx,
	const h5_glb_idx_t idx
	) {
	f->t->glb_elems.tris[elem_idx].idx = idx;
	return idx;
}

static h5_glb_idx_t
get_glb_elem_parent_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	return f->t->glb_elems.tris[elem_idx].parent_idx;
}

static h5_glb_idx_t
set_glb_elem_parent_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx,
	const h5_glb_id_t parent_idx
	) {
	f->t->glb_elems.tris[elem_idx].parent_idx = parent_idx;
	return parent_idx;
}

static h5_glb_idx_t
get_glb_elem_child_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	return f->t->glb_elems.tris[elem_idx].child_idx;
}

static h5_glb_idx_t
set_glb_elem_child_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx,
	const h5_glb_idx_t child_idx
	) {
	f->t->glb_elems.tris[elem_idx].child_idx = child_idx;
	return child_idx;
}

static h5_glb_idx_t*
get_glb_elem_vertex_indices (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	return f->t->glb_elems.tris[elem_idx].vertex_indices;
}

static h5_glb_idx_t
get_glb_elem_vertex_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx,
	const h5_loc_idx_t face_idx
	) {
	return f->t->glb_elems.tris[elem_idx].vertex_indices[face_idx];
}

static h5_glb_idx_t
set_glb_elem_vertex_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx,
	const h5_loc_idx_t face_idx,
	const h5_glb_idx_t vertex_idx
	) {
	f->t->glb_elems.tris[elem_idx].vertex_indices[face_idx] = vertex_idx;
	return vertex_idx;
}

static h5_glb_idx_t*
get_glb_elem_neighbor_indices (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	return f->t->glb_elems.tris[elem_idx].neighbor_indices;
}

static h5_glb_idx_t
get_glb_elem_neighbor_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx,
	const h5_loc_idx_t face_idx
	) {
	return f->t->glb_elems.tris[elem_idx].neighbor_indices[face_idx];
}

static h5_glb_idx_t
set_glb_elem_neighbor_idx (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx,
	const h5_loc_idx_t face_idx,
	const h5_glb_idx_t neighbor_idx
	) {
	f->t->glb_elems.tris[elem_idx].neighbor_indices[face_idx] = neighbor_idx;
	return neighbor_idx;
}

static h5_err_t
set_boundary_elem_flag (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx
	) {
	f->t->loc_elems.tris[elem_idx].flags |= H5T_BOUNDARY_ELEM_FLAG;
	return H5_SUCCESS;
}

static h5_err_t
set_boundary_facet_flag (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx
	) {
	f->t->loc_elems.tris[elem_idx].flags |= H5T_BOUNDARY_FACET_FLAG;
	return H5_SUCCESS;
}

static h5_err_t
clear_boundary_elem_flag (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx
	) {
	f->t->loc_elems.tris[elem_idx].flags &= ~H5T_BOUNDARY_ELEM_FLAG;
	return H5_SUCCESS;
}

static h5_err_t
clear_boundary_facet_flag (
	h5_file_t* const f,
	h5_loc_idx_t elem_idx
	) {
	f->t->loc_elems.tris[elem_idx].flags &= ~H5T_BOUNDARY_FACET_FLAG;
	return H5_SUCCESS;
}

static int
is_boundary_elem  (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx
	) {
	return (f->t->loc_elems.tris[elem_idx].flags & H5T_BOUNDARY_ELEM_FLAG) ? 1 : 0;
}

static int
is_boundary_facet (
	h5_file_t* const f,
	const h5_loc_idx_t elem_idx,
	const h5_loc_idx_t facet_idx
	) {
	return (f->t->loc_elems.tris[elem_idx].neighbor_indices[facet_idx] == -1);
}

static int
is_boundary_face (
	h5_file_t* const f,
	const int dim,
	const h5_loc_idx_t elem_idx,
	const h5_loc_idx_t facet_idx
	) {
	UNUSED_ARGUMENT (f);
	UNUSED_ARGUMENT (dim);
	UNUSED_ARGUMENT (elem_idx);
	UNUSED_ARGUMENT (facet_idx);
	return h5_error_internal (f, __FILE__, __func__, __LINE__);
}

struct h5t_access_methods h5tpriv_access_trim_methods = {
	get_loc_elem,
	get_loc_elem_parent_idx,
	set_loc_elem_parent_idx,
	get_loc_elem_child_idx,
	set_loc_elem_child_idx,
	get_loc_elem_level_idx,
	set_loc_elem_level_idx,
	get_loc_elem_vertex_indices,
	get_loc_elem_vertex_idx,
	set_loc_elem_vertex_idx,
	get_loc_elem_neighbor_indices,
	get_loc_elem_neighbor_idx,
	set_loc_elem_neighbor_idx,
	get_glb_elem,
	get_glb_elem_idx,
	set_glb_elem_idx,
	get_glb_elem_parent_idx,
	set_glb_elem_parent_idx,
	get_glb_elem_child_idx,
	set_glb_elem_child_idx,
	get_glb_elem_vertex_indices,
	get_glb_elem_vertex_idx,
	set_glb_elem_vertex_idx,
	get_glb_elem_neighbor_indices,
	get_glb_elem_neighbor_idx,
	set_glb_elem_neighbor_idx,
	set_boundary_elem_flag,
	clear_boundary_elem_flag,
	set_boundary_facet_flag,
	clear_boundary_facet_flag,
	is_boundary_elem,
	is_boundary_facet,
	is_boundary_face,
};
	
