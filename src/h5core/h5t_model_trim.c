#include "h5core/h5_core.h"
#include "h5_core_private.h"

static struct h5t_methods tri_funcs = {
	&h5tpriv_read_trim_methods,
	&h5tpriv_trim_store_methods,
	&h5tpriv_trim_retrieve_methods,
	&h5tpriv_access_trim_methods,
	&h5tpriv_trim_adjacency_methods
};


static inline h5_err_t
open_trimeshes_group (
	h5_file_t* const f
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;

	if (t->topo_gid < 0) {
		TRY (open_topo_group (f));
	}
	TRY (t->meshes_gid = h5priv_open_group (
		      f,
		      t->topo_gid,
		      TRIANGLE_MESHES_GRPNAME));
	t->mesh_type = H5_OID_TRIANGLE;
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
open_trimesh_group (
	h5_file_t* const f,
	const h5_id_t id
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;

	if (t->topo_gid == 0 || t->topo_gid == -1) {
		TRY (t->topo_gid = h5priv_open_group (
			     f, f->root_gid, H5T_CONTAINER_GRPNAME));
	}
	snprintf (t->mesh_name, sizeof (t->mesh_name), "%lld", (long long)id);

	TRY (t->mesh_gid = h5priv_open_group (
		      f,
		      t->meshes_gid,
		      t->mesh_name));
	t->cur_mesh = id;
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5t_open_triangle_mesh (
	h5_file_t* const f,
	h5_id_t id
	) {
	H5_CORE_API_ENTER2 (h5_err_t, "f=0x%p, id=%lld", f, (long long)id);
	h5t_fdata_t* t = f->t;

	TRY (h5t_close_mesh (f));

	if (t->num_meshes < 0) {
		h5_size_t result = h5t_get_num_trimeshes (f);
		t->num_meshes = (result > 0 ? result : 0);
	}
	if ((id < -1) || (id >= t->num_meshes)) {
		H5_CORE_API_LEAVE (HANDLE_H5_OUT_OF_RANGE_ERR ("mesh", id));
	}
	t->dsinfo_elems.type_id = t->dtypes.h5_triangle_t;
	t->methods = tri_funcs;
	t->ref_elem = &h5t_tri_ref_elem;
	TRY (open_trimesh_group (f, id));

	if (id == -1) {			// append new
		id = t->num_meshes;
		t->num_meshes++;
		t->mesh_changed = id;
		t->num_leaf_levels = 0;
	} else {			// read existing
		TRY (h5tpriv_read_mesh (f));
	} 
	H5_CORE_API_RETURN (H5_SUCCESS);
}


/*!
  Add new mesh

  \return mesh id
*/
h5_id_t
h5t_add_triangle_mesh (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER1 (h5_id_t, "f=0x%p", f);
	h5_id_t mesh_id = 0;
	TRY (mesh_id = h5t_open_triangle_mesh (f, -1)); 
	TRY (h5t_add_level (f));
	f->t->mesh_changed = 1;
	H5_CORE_API_RETURN (mesh_id);
}
