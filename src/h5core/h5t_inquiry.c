#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*!
  Get number of meshes of given type.

  \param[in]	f	File handle
  \param[in]	type_id	Type of mesh we want the number of.

  \return	Number of meshes of type \c type_id or error code.
 */
h5_ssize_t
h5t_get_num_meshes (
	h5_file_t* const f,
	const h5_oid_t type_id
	) {
	H5_CORE_API_ENTER (h5_ssize_t);
	hid_t topo_gid = -1;
	hid_t meshes_gid = -1;

	h5_err_t exists;
	TRY (exists = hdf5_link_exists (f->root_gid, H5T_CONTAINER_GRPNAME));
	if (!exists) return 0;

	TRY (topo_gid = hdf5_open_group (f->root_gid, H5T_CONTAINER_GRPNAME));

	TRY (exists = hdf5_link_exists (topo_gid, h5tpriv_meshes_grpnames[type_id]));
	if (!exists) return 0;

	TRY (meshes_gid = hdf5_open_group (topo_gid, h5tpriv_meshes_grpnames[type_id]));
	h5_ssize_t num_meshes;
	TRY (num_meshes = h5_get_num_hdf5_groups (meshes_gid));
	TRY (hdf5_close_group (meshes_gid) );
	TRY (hdf5_close_group (topo_gid) );

	H5_CORE_API_RETURN (num_meshes);
}

/*!
  Get the number of hierarchical mesh levels for the current mesh.

  \param[in]	f	File handle

  \return	Number of hierarchical mesh levels or error code.
 */
h5_ssize_t
h5t_get_num_leaf_levels (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER (h5_ssize_t);
	if (f->t->cur_mesh < 0) {
		H5_CORE_API_LEAVE (h5tpriv_error_undef_mesh ());
	}
	H5_CORE_API_RETURN (f->t->num_leaf_levels);
}

/*!
  Get current level.

  \param[in]	f	File handle.

  \return	Current level ID.
*/
h5t_lvl_idx_t
h5t_get_level (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER (h5t_lvl_idx_t);
	H5_CORE_API_RETURN (f->t->leaf_level);
}

/*!
  Return number of elems on compute node \c cnode_id on
  current level. If \cnode_id is equal \c -1 return the 
  number of elements in the entire mesh.

  \remark
  Refined elems are *not* counted.

  \param[in]	f	File handle.
  \param[in]	cnode	Compute node

  \return	Number of elements or error code.
 */
h5_ssize_t
h5t_get_num_elems (
	h5_file_t* const f,
	const h5_id_t cnode
	) {
	H5_CORE_API_ENTER (h5_ssize_t);
	UNUSED_ARGUMENT (cnode);

	if (f->t->cur_mesh < 0) {
		H5_CORE_API_LEAVE (h5tpriv_error_undef_mesh ());
	}
	if (f->t->leaf_level < 0) {
		H5_CORE_API_LEAVE (h5tpriv_error_undef_level ());
	}
	H5_CORE_API_RETURN (f->t->num_elems_on_leaf_level[f->t->leaf_level]);
}

/*!
  Return number of vertices on compute node \c cnode_id
  on current level.  If \cnode_id is equal \c -1 return the 
  number of vertices in the entire mesh.

  \remark
  There is nothing like "refined vertices".

  \param[in]	f	File handle.
  \param[in]	cnode	Compute node

  \return	Number of vertices or error code.
 */
h5_ssize_t
h5t_get_num_vertices (
	h5_file_t* const f,
	h5_id_t cnode
	) {
	H5_CORE_API_ENTER (h5_ssize_t);
	UNUSED_ARGUMENT (cnode);

	if (f->t->cur_mesh < 0) {
		H5_CORE_API_LEAVE (h5tpriv_error_undef_mesh ());
	}
	if (f->t->leaf_level < 0) {
		H5_CORE_API_LEAVE (h5tpriv_error_undef_level ());
	}
	H5_CORE_API_RETURN (f->t->num_vertices[f->t->leaf_level]);
}

