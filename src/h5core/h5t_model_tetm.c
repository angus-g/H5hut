#include <string.h>
#include "h5core/h5_core.h"
#include "h5_core_private.h"

static struct h5t_methods tet_funcs = {
	&h5tpriv_read_tetm_methods,
	&h5tpriv_tetm_store_methods,
	&h5tpriv_tetm_retrieve_methods,
	&h5tpriv_access_tetm_methods,
	&h5tpriv_tetm_adjacency_methods
};

/*
  open tetrahedral mesh
*/
h5_err_t
h5t_open_tetrahedral_mesh_by_idx (
	h5_file_t* const f,
	const h5_id_t idx,
	h5t_mesh_t** mesh
	) {
	H5_CORE_API_ENTER (h5_err_t, "f=%p, idx=%lld, mesh=%p", f, (long long)idx, mesh);
	hid_t ctn_hid;
	char name[1024];

	TRY (ctn_hid = h5priv_open_group (
		     0,
		     f->root_gid,
		     H5T_CONTAINER_GRPNAME,
		     TETRAHEDRAL_MESHES_GRPNAME));
	TRY (hdf5_get_name_of_group_by_idx (ctn_hid, idx, name, sizeof (name)));
	TRY (hdf5_close_group (ctn_hid));

	H5_CORE_API_RETURN (h5t_open_tetrahedral_mesh (f, name, mesh));
}

h5_err_t
h5t_open_tetrahedral_mesh (
	h5_file_t* const f,
	const char* name,
	h5t_mesh_t** mesh
	) {
	H5_CORE_API_ENTER (h5_err_t, "f=%p, name=%s, mesh=%p", f, name, mesh);
	hid_t mesh_hid;

	TRY (mesh_hid = h5priv_open_group (
		     0,		// do not create intermediate groups
		     f->root_gid,
		     H5T_CONTAINER_GRPNAME, 
		     TETRAHEDRAL_MESHES_GRPNAME,
		     name));

	TRY (*mesh = h5_alloc (NULL, sizeof(**mesh)));
	h5t_mesh_t* m = *mesh;
	TRY (h5tpriv_init_mesh (
		     m,
		     f,
		     name,
		     mesh_hid));

	m->dsinfo_elems.type_id = m->dtypes.h5_tet_t;
	m->methods = tet_funcs;
	m->ref_elem = &h5t_tet_ref_elem;
	m->leaf_level = 0;
	TRY (h5tpriv_read_mesh (m));

	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*!
  Add new mesh

  \return mesh id
*/
h5_err_t
h5t_add_tetrahedral_mesh (
	h5_file_t* const f,
	const char* name,
	h5t_mesh_t** mesh
	) {
	H5_CORE_API_ENTER (h5_err_t, "f=%p, name=%s, mesh=%p", f, name, mesh);
	CHECK_WRITABLE_MODE (f);
	h5_err_t exists;
	TRY (exists = h5priv_link_exists (
		     f->root_gid, 
		     H5T_CONTAINER_GRPNAME,
		     TETRAHEDRAL_MESHES_GRPNAME,
		     name));
	if (exists) {
		H5_CORE_API_LEAVE (
			h5_error (
				H5_ERR,
				"Tetrahedral mesh '%s' already exists!",
				name));
	}
	hid_t mesh_hid;
	TRY (mesh_hid = h5priv_open_group (
		     1,		// create intermediate groups in path
		     f->root_gid,
		     H5T_CONTAINER_GRPNAME,
		     TETRAHEDRAL_MESHES_GRPNAME,
		     name));

	TRY (*mesh = h5_alloc (NULL, sizeof(**mesh)));
	h5t_mesh_t* m = *mesh;
	TRY (h5tpriv_init_mesh (
		     m,
		     f,
		     name,
		     mesh_hid));

	m->dsinfo_elems.type_id = m->dtypes.h5_tet_t;
	m->methods = tet_funcs;
	m->ref_elem = &h5t_tet_ref_elem;
	m->leaf_level = 0;
	m->num_leaf_levels = 0;
	TRY (h5t_add_level (m));
	m->mesh_changed = 1;

	H5_CORE_API_RETURN (H5_SUCCESS);
}
