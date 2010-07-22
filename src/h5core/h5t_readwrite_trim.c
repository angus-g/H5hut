#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*
  setup structure "elems_ldta" with local indices for each element:
  - translate the global vertex id's of each element to their
    local id's
  - translate the global parent id of each element to the
    corresponding local id.
*/
static h5_err_t
init_loc_elems_struct (
	h5_file_t* const f
	) {
	h5t_fdata_t* const t = f->t;
	h5_id_t idx = 0;
	const h5_id_t num_elems = t->num_elems[t->num_levels-1];
	h5_id_t level_id = 0;
	int num_vertices = t->ref_elem->num_faces[0];
	h5_triangle_t* loc_elems = t->loc_elems.tris;
	h5_triangle_t* glb_elems = t->glb_elems.tris;

	for (idx = 0; idx < num_elems; idx++) {
		
		// local idx
		// local parent index
		// level idx

		// vertex indices
		// neighbor indices
	}
	return H5_SUCCESS;
}

static h5_err_t
init_struct_elems_ldta (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;
	h5_id_t idx = 0;
	h5_id_t num_elems = t->num_elems[t->num_levels-1];
	h5_id_t level_id = 0;

	for (local_eid=0;
	     local_eid < num_elems;
	     local_eid++, elp+=h5tpriv_sizeof_elem[t->mesh_type], el_ldta++) {
		el = (h5_elem_t*)elp;
		TRY( h5t_map_global_vertex_indices2local (
			     f,
			     el->global_vertex_indices,
			     num_vertices,
			     el_ldta->local_vertex_indices) );
		if (el->global_parent_idx >= 0)
			TRY( el_ldta->local_parent_idx =
			     h5t_map_global_elem_idx2local (
				     f, el->global_parent_idx) );
		
		if (el->global_child_idx >= 0)
			TRY( el_ldta->local_child_idx =
			     h5t_map_global_elem_idx2local (
				     f, el->global_child_idx) );
		
		if (local_eid >= t->num_elems[level_id]) {
			level_id++;
		}
		el_ldta->level_id = level_id;
	}
	return H5_SUCCESS;
}


struct h5t_read_methods {
	init_loc_elems_struct;
} h5t_read_trim_methods;
