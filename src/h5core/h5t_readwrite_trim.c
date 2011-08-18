#include <string.h>
#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*
  initialize local element structure
*/
static h5_err_t
init_loc_elems_struct (
	h5t_mesh_t* const m,
	const h5t_lvl_idx_t from_lvl
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p, from_lvl=%d", m, from_lvl);
	h5_loc_idx_t elem_idx = 0;
	const h5_loc_idx_t num_elems = m->num_elems[m->num_leaf_levels-1];
	h5t_lvl_idx_t level_idx = 0;
	int num_vertices = h5tpriv_ref_elem_get_num_vertices (m);
	int num_facets = h5tpriv_ref_elem_get_num_facets (m);
	h5_loc_triangle_t* loc_elem = m->loc_elems.tris;
	h5_glb_triangle_t* glb_elem = m->glb_elems.tris;

	for (elem_idx = (from_lvl <= 0) ? 0 : m->num_elems[from_lvl-1];
	     elem_idx < num_elems; elem_idx++, loc_elem++, glb_elem++) {
		// global element index
		loc_elem->glb_idx = glb_elem->idx;
		// local parent index
		TRY( loc_elem->parent_idx =
		     h5t_map_glb_elem_idx2loc (m, glb_elem->parent_idx) );

		// local child index
		TRY( loc_elem->child_idx =
		     h5t_map_glb_elem_idx2loc (m, glb_elem->child_idx) );

		// level idx
		if (elem_idx >= m->num_elems[level_idx]) {
			level_idx++;
		}
		loc_elem->level_idx = level_idx;

		// refinement level
		loc_elem->refinement_level = 0;
		h5_loc_triangle_t* elem = loc_elem;
		while (elem->parent_idx > 0) {
			elem = m->loc_elems.tris + elem->parent_idx;
			loc_elem->refinement_level++;
		}

		// vertex indices
		TRY( h5t_map_global_vertex_indices2local (
			     m,
			     glb_elem->vertex_indices,
			     num_vertices,
			     loc_elem->vertex_indices) );

		// neighbor indices
		TRY( h5t_map_glb_elem_indices2loc (
			     m,
			     glb_elem->neighbor_indices,
			     num_facets,
			     loc_elem->neighbor_indices) );

	}
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
init_geom_boundary_info (
	h5t_mesh_t* const m,
	const h5t_lvl_idx_t from_lvl
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p, from_lvl=%d", m, from_lvl);
	h5_loc_idx_t elem_idx = 0;
	const h5_loc_idx_t num_elems = m->num_elems[m->num_leaf_levels-1];
	int num_facets = h5tpriv_ref_elem_get_num_facets (m);
	h5_loc_triangle_t* loc_elem = m->loc_elems.tris;
	h5_glb_triangle_t* glb_elem = m->glb_elems.tris;

	for (elem_idx = (from_lvl <= 0) ? 0 : m->num_elems[from_lvl-1];
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
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Alloc mem for elements
*/
static h5_err_t
alloc_glb_elems_struct (
	h5t_mesh_t* const m,
	h5_loc_idx_t num_elems
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p, num_elems=%d", m, num_elems);

	TRY ( m->glb_elems.tris = h5_calloc (
		      num_elems,
		      sizeof(m->glb_elems.tris[0]) ) );
	memset (
		m->glb_elems.tris,
		-1,
		(num_elems) * sizeof(m->glb_elems.tris[0]) );
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static h5_err_t
init_glb2loc_elem_map (
	h5t_mesh_t* const m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);

	if (m->num_leaf_levels <= 0)
		H5_PRIV_FUNC_LEAVE (H5_SUCCESS);

	h5_loc_idx_t loc_idx = 0;
	h5_loc_idx_t num_loc_elems = m->num_elems[m->num_leaf_levels-1];
	h5_idxmap_el_t* item = &m->map_elem_g2l.items[loc_idx];
	h5_glb_triangle_t* elem = m->glb_elems.tris;

	for (; loc_idx < num_loc_elems; elem++, loc_idx++, item++) {
		item->glb_idx = elem->idx;
		item->loc_idx = loc_idx;
		m->map_elem_g2l.num_items++;
	}
	h5priv_sort_idxmap (&m->map_elem_g2l);

	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Setup data structure to be written on disk. We always write the hole mesh. 
*/
static h5_err_t
init_glb_elems_struct (
	h5t_mesh_t* const m
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t, "m=%p", m);
	h5_loc_idx_t num_elems = m->num_elems[m->num_leaf_levels-1];

	// simple in serial runs: global index = local index
	h5_loc_triangle_t* loc_elem = m->loc_elems.tris;
	h5_glb_triangle_t* glb_elem = m->glb_elems.tris;
	h5_loc_triangle_t* end = loc_elem + num_elems;

	while (loc_elem < end) {
		glb_elem->idx = loc_elem->glb_idx;
		glb_elem->parent_idx = loc_elem->parent_idx;
		glb_elem->child_idx = loc_elem->child_idx;
		int i;
		for (i = 0; i < 3; i++) {
			glb_elem->vertex_indices[i] = loc_elem->vertex_indices[i];
			glb_elem->neighbor_indices[i] = loc_elem->neighbor_indices[i];
		}
		loc_elem++;
		glb_elem++;
	}
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

struct h5t_read_methods h5tpriv_read_trim_methods = {
	init_loc_elems_struct,
	init_geom_boundary_info,
	alloc_glb_elems_struct,
	init_glb2loc_elem_map,
	init_glb_elems_struct,
};
