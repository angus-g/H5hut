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
	h5t_lvl_idx_t level_idx = 0;
	int num_vertices = h5tpriv_ref_elem_get_num_vertices (t);
	int num_facets = h5tpriv_ref_elem_get_num_faces (t,1);
	h5_loc_triangle_t* loc_elem = t->loc_elems.tris;
	h5_glb_triangle_t* glb_elem = t->glb_elems.tris;

	for (idx = 0; idx < num_elems; idx++, loc_elem++, glb_elem++) {
		// global element index
		loc_elem->glb_idx = glb_elem->idx;
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
		loc_elem->level_idx = level_idx;

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
		// on boundary?
		int i;
		for (i=0; i < num_facets; i++) {
			if (loc_elem->neighbor_indices[i] == -1) {
				loc_elem->flags |= H5T_BOUNDARY_ELEM_FLAG;
				break;
			}
		}
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
	const h5_loc_idx_t num_elems = t->num_elems[t->num_levels-1];
	int num_facets = h5tpriv_ref_elem_get_num_faces (t, 1);
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

#if 0
		// mark elements which are edge- or vertex- adjacent to the boundary
		// are there more facets on the boundary?
		int j;
		for (j = i+1; j < num_facets; j++) {
			if (loc_elem->neighbor_indices[j] == -1) {
				break; // found another boundary facet
			}
		}
		// get vertex ID's of vertices on boundary
		h5_loc_idx_t vertex_indices[4];
		if (j < num_facets) {
			// all vertices on boundary
			vertex_indices[0] = h5tpriv_get_loc_elem_vertex_idx (f, elem_idx, 0);
			vertex_indices[1] = h5tpriv_get_loc_elem_vertex_idx (f, elem_idx, 1);
			vertex_indices[2] = h5tpriv_get_loc_elem_vertex_idx (f, elem_idx, 2);
			num_vertices = 3;
		} else {
			// two vertices on boundary
			// get vertices of edge i
			h5_loc_idx_t face_idx;
			face_idx = t->ref_elem->map[1][i][0];
			vertex_indices[0] = h5tpriv_get_loc_elem_vertex_idx (f, elem_idx, face_idx);
			face_idx = t->ref_elem->map[1][i][1];
			vertex_indices[1] = h5tpriv_get_loc_elem_vertex_idx (f, elem_idx, face_idx);
			num_vertices = 2;
		}
		// mark elements
		for (i = 0; i < num_vertices; i++) {
			// get upward adjacent elements
			const h5_loc_idx_t vertex_idx = vertex_indices[i];
			h5_idlist_t* list = &t->adjacencies.tv.v[vertex_idx];
			// set flag
			for (j=0; j < list->num_items; j++) {
				h5_loc_idx_t idx = h5tpriv_get_elem_idx (list->items[j]);
				t->loc_elems.tris[idx].flags |= H5T_BOUNDARY_ELEM_FLAG;
			}
		}
#endif
	}
	return H5_SUCCESS;
}

struct h5t_read_methods h5tpriv_read_trim_methods = {
	init_loc_elems_struct,
	init_geom_boundary_info,
};
