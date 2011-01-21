#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*
  initialize local element structure
*/
static h5_err_t
init_loc_elems_struct (
	h5_file_t* const f,
	const h5t_lvl_idx_t from_lvl
	) {
	h5t_fdata_t* const t = f->t;
	h5_loc_idx_t elem_idx = 0;
	const h5_loc_idx_t num_elems = t->num_elems[t->num_leaf_levels-1];
	h5t_lvl_idx_t level_idx = 0;
	int num_vertices = h5tpriv_ref_elem_get_num_vertices (t);
	int num_facets = h5tpriv_ref_elem_get_num_facets (t);
	h5_loc_triangle_t* loc_elem = t->loc_elems.tris;
	h5_glb_triangle_t* glb_elem = t->glb_elems.tris;

	for (elem_idx = (from_lvl <= 0) ? 0 : t->num_elems[from_lvl-1];
	     elem_idx < num_elems; elem_idx++, loc_elem++, glb_elem++) {
		// global element index
		loc_elem->glb_idx = glb_elem->idx;
		// local parent index
		TRY( loc_elem->parent_idx =
		     h5t_map_glb_elem_idx2loc (f, glb_elem->parent_idx) );

		// local child index
		TRY( loc_elem->child_idx =
		     h5t_map_glb_elem_idx2loc (f, glb_elem->child_idx) );

		// level idx
		if (elem_idx >= t->num_elems[level_idx]) {
			level_idx++;
		}
		loc_elem->level_idx = level_idx;

		// refinement level
		loc_elem->refinement_level = 0;
		h5_loc_triangle_t* elem = loc_elem;
		while (elem->parent_idx > 0) {
			elem = t->loc_elems.tris + elem->parent_idx;
			loc_elem->refinement_level++;
		}

		// vertex indices
		TRY( h5t_map_global_vertex_indices2local (
			     f,
			     glb_elem->vertex_indices,
			     num_vertices,
			     loc_elem->vertex_indices) );

		// neighbor indices
		TRY( h5t_map_glb_elem_indices2loc (
			     f,
			     glb_elem->neighbor_indices,
			     num_facets,
			     loc_elem->neighbor_indices) );

	}
	return H5_SUCCESS;
}

static h5_err_t
init_geom_boundary_info (
	h5_file_t* const f,
	const h5t_lvl_idx_t from_lvl
	) {
	h5t_fdata_t* const t = f->t;
	h5_loc_idx_t elem_idx = 0;
	const h5_loc_idx_t num_elems = t->num_elems[t->num_leaf_levels-1];
	int num_facets = h5tpriv_ref_elem_get_num_facets (t);
	h5_loc_triangle_t* loc_elem = t->loc_elems.tris;
	h5_glb_triangle_t* glb_elem = t->glb_elems.tris;

	for (elem_idx = (from_lvl <= 0) ? 0 : t->num_elems[from_lvl-1];
	     elem_idx < num_elems; elem_idx++, loc_elem++, glb_elem++) {
		// on boundary?
		int i;
		for (i=0; i < num_facets; i++) {
			if (loc_elem->neighbor_indices[i] == -1) {
				loc_elem->flags |= H5T_BOUNDARY_ELEM_FLAG;
				break;
			}
		}
		if (i == num_facets) {
			continue; // no facet on boundary
		}
	}
	return H5_SUCCESS;
}

struct h5t_read_methods h5tpriv_read_trim_methods = {
	init_loc_elems_struct,
	init_geom_boundary_info,
};
