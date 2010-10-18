#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*!
  Get number of meshes of given type.

  \param[in]	f	File handle
  \param[in]	type_id	Type of mesh we want the number of.

  \return	Number of meshes of type \c type_id or error code.
 */
h5_size_t
h5t_get_num_meshes (
	h5_file_t* const f,
	const h5_oid_t type_id
	) {
	hid_t topo_gid = -1;
	hid_t meshes_gid = -1;

	h5_err_t exists;
	TRY( exists = h5priv_hdf5_link_exists (f, f->root_gid, H5T_CONTAINER_GRPNAME) );
	if (!exists) return 0;

	TRY( topo_gid = h5priv_open_hdf5_group (f, f->root_gid, H5T_CONTAINER_GRPNAME) );

	TRY( exists = h5priv_hdf5_link_exists (f, topo_gid, h5tpriv_meshes_grpnames[type_id]) );
	if (!exists) return 0;

	TRY( meshes_gid = h5priv_open_hdf5_group (f, topo_gid, h5tpriv_meshes_grpnames[type_id]) );

	h5_size_t num_meshes = h5_get_num_hdf5_groups (f, meshes_gid);
	TRY( h5priv_close_hdf5_group (f, meshes_gid) );
	TRY( h5priv_close_hdf5_group (f, topo_gid) );
	return num_meshes;
}

/*!
  Get the number of hierarchical mesh levels for the current mesh.

  \param[in]	f	File handle

  \return	Number of hierarchical mesh levels or error code.
 */
h5_size_t
h5t_get_num_levels (
	h5_file_t* const f
	) {
	h5t_fdata_t* t = f->t;

	if (t->cur_mesh < 0) {
		return h5tpriv_error_undef_mesh (f);
	}
	return t->num_levels;
}

/*!
  Get current level.

  \param[in]	f	File handle.

  \return	Current level ID.
*/
h5_id_t
h5t_get_level (
	h5_file_t* const f
	) {
	return f->t->cur_level;
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
h5_size_t
h5t_get_num_elems (
	h5_file_t* const f,
	const h5_id_t cnode
	) {
#pragma unused cnode
	h5t_fdata_t* t = f->t;

	if (t->cur_mesh < 0) {
		return h5tpriv_error_undef_mesh (f);
	}
	if (t->cur_level < 0) {
		return h5tpriv_error_undef_level (f);
	}
	return t->num_elems_on_level[t->cur_level];
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
h5_size_t
h5t_get_num_vertices (
	h5_file_t* const f,
	h5_id_t cnode
	) {
#pragma unused cnode
	h5t_fdata_t* t = f->t;

	if (t->cur_mesh < 0) {
		return h5tpriv_error_undef_mesh (f);
	}
	if (t->cur_level < 0) {
		return h5tpriv_error_undef_level (f);
	}
	return t->num_vertices[t->cur_level];
}

