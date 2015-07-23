/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "h5t_types_private.h"

#include "h5_hdf5_private.h"
#include "h5_va_macros.h"


#include "h5t_model_private.h"
#include "h5t_access_private.h"
#include "h5t_adjacencies_private.h"
#include "h5t_readwrite_private.h"
#include "h5t_retrieve_private.h"
#include "h5t_store_private.h"

#include "h5_init_private.h"
#include "h5t_core_private.h"

static struct h5t_methods tet_funcs = {
	&h5tpriv_read_tetm_methods,
	&h5tpriv_tetm_store_methods,
	&h5tpriv_tetm_retrieve_methods,
	&h5tpriv_access_tetm_methods,
	&h5tpriv_tetm_adjacency_methods,
	&h5tpriv_tetm_core_methods
};

/*
   open tetrahedral mesh
 */
h5_err_t
h5t_open_tetrahedral_mesh_by_idx (
	const h5_file_t fh,
	const h5_id_t idx,
	h5t_mesh_t** mesh
	) {
        h5_file_p f = (h5_file_p)fh;
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

	H5_CORE_API_RETURN (h5t_open_tetrahedral_mesh ((h5_file_t)f, name, mesh));
}

h5_err_t
h5t_open_tetrahedral_mesh (
	const h5_file_t fh,
	const char* name,
	h5t_mesh_t** mesh
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t, "f=%p, name=%s, mesh=%p", f, name, mesh);
	hid_t mesh_hid;

	TRY (mesh_hid = h5priv_open_group (
	             0,         // do not create intermediate groups
	             f->root_gid,
	             H5T_CONTAINER_GRPNAME,
	             TETRAHEDRAL_MESHES_GRPNAME,
	             name));

	TRY (*mesh = h5_calloc (1, sizeof(**mesh)));
	h5t_mesh_t* m = *mesh;
	TRY (h5tpriv_init_mesh (
	             m,
	             f,
	             name,
	             mesh_hid,
	             h5_dta_types.h5_tet_t,
	             &h5t_tet_ref_elem,
	             &tet_funcs,
	             0));
#ifdef PARALLEL_IO  // reason: even if we have a chunked mesh, if h5hut is not parallel
	// it does not support reading chunked meshes
	TRY (m->is_chunked ? h5tpriv_read_chunked_mesh (m) :h5tpriv_read_mesh (m));
#else
	TRY (h5tpriv_read_mesh (m));
#endif
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5t_open_tetrahedral_mesh_part (
        const h5_file_p f,
        const char* name,
        h5t_mesh_t** mesh,
        h5_glb_idx_t* elem_indices,
        h5_glb_idx_t dim
        ) {
	H5_CORE_API_ENTER (h5_err_t, "f=%p, name=%s, mesh=%p", f, name, mesh);
	hid_t mesh_hid;

	TRY (mesh_hid = h5priv_open_group (
	             0,
	             f->root_gid,
	             H5T_CONTAINER_GRPNAME,
	             TRIANGLE_MESHES_GRPNAME,
	             name));

	TRY (*mesh = h5_calloc (1, sizeof(**mesh)));
	h5t_mesh_t* m = *mesh;
	TRY (h5tpriv_init_mesh (
	             m,
	             f,
	             name,
	             mesh_hid,
	             h5_dta_types.h5_tet_t,
	             &h5t_tet_ref_elem,
	             &tet_funcs,
	             0));
	TRY (h5tpriv_read_mesh_part (m, elem_indices, dim));

	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*!
   Add new mesh

   \return mesh id
 */
h5_err_t
h5t_add_tetrahedral_mesh (
	const h5_file_t fh,
	const char* name,
        const h5_weight_t num_weights,
	h5t_mesh_t** mesh
	) {
        h5_file_p f = (h5_file_p)fh;
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
	             1,         // create intermediate groups in path
	             f->root_gid,
	             H5T_CONTAINER_GRPNAME,
	             TETRAHEDRAL_MESHES_GRPNAME,
	             name));

	TRY (*mesh = h5_calloc (1, sizeof(**mesh)));
	h5t_mesh_t* m = *mesh;
	TRY (h5tpriv_init_mesh (
	             m,
	             f,
	             name,
	             mesh_hid,
	             h5_dta_types.h5_tet_t,
	             &h5t_tet_ref_elem,
	             &tet_funcs,
	             1));

	m->leaf_level = 0;
	m->num_leaf_levels = 0;
        m->num_weights = num_weights;
	TRY (h5tpriv_add_level (m));
	m->mesh_changed = 1;

	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*!
   Add new mesh

   \return mesh id
 */
h5_err_t
h5t_add_chunked_tetrahedral_mesh (
	const h5_file_t fh,
        const char* name,
        const h5_weight_t num_weights,
        h5t_mesh_t** mesh
        ) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t, "f=%p, name=%s, mesh=%p", f, name, mesh);
#ifdef PARALLEL_IO
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
	             1,         // create intermediate groups in path
	             f->root_gid,
	             H5T_CONTAINER_GRPNAME,
	             TETRAHEDRAL_MESHES_GRPNAME,
	             name));

	TRY (*mesh = h5_calloc (1, sizeof(**mesh)));
	h5t_mesh_t* m = *mesh;
	TRY (h5tpriv_init_mesh (
	             m,
	             f,
	             name,
	             mesh_hid,
	             h5_dta_types.h5_tet_t,
	             &h5t_tet_ref_elem,
	             &tet_funcs,
	             1));
	m->is_chunked = 1;
	TRY (H5t_init_octree(&m->octree, sizeof (h5t_oct_userdata_t), NULL, -1, m->f->props->comm));
	m->leaf_level = 0;
	m->num_leaf_levels = 0;
	TRY (h5tpriv_add_level (m));
	m->mesh_changed = 1;
#endif
	H5_CORE_API_RETURN (H5_SUCCESS);
}
