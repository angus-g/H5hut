#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*
  initialize local element structure
*/
static h5_err_t
init_loc_elems_struct (
	h5_file_t* const f
	) {
	h5t_fdata_t* const t = f->t;
	h5_loc_idx_t idx = 0;
	const h5_loc_idx_t num_elems = t->num_elems[t->num_levels-1];
	h5_id_t level_idx = 0;
	int num_vertices = h5tpriv_ref_elem_get_num_vertices (t);
	int num_edges = h5tpriv_ref_elem_get_num_edges (t);
	h5_loc_triangle_t* loc_elem = t->loc_elems.tris;
	h5_glb_triangle_t* glb_elem = t->glb_elems.tris;

	for (idx = 0; idx < num_elems; idx++, loc_elem++, glb_elem++) {
		// local parent index
		TRY( loc_elem->parent_idx =
		     h5t_map_glb_elem_idx2loc (f, glb_elem->parent_idx) );

		// local child index
		TRY( loc_elem->child_idx =
		     h5t_map_glb_elem_idx2loc (f, glb_elem->child_idx) );

		// level idx
		if (idx >= t->num_elems[level_idx]) {
			level_idx++;
		}
		loc_elem->idx = level_idx;

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
			     num_edges,
			     loc_elem->neighbor_indices) );
	}
	return H5_SUCCESS;
}

struct h5t_read_methods h5tpriv_read_trim_methods = {
	init_loc_elems_struct
};
