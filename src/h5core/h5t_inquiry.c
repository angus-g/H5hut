#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*!
  Get number of meshes of given type.

  \param[in]	f	File handle
  \param[in]	type_id	Type of mesh we want the number of.

  \return	Number of meshes of type \c type_id or error code.
 */
static inline h5_ssize_t
get_num_meshes (
	h5_file_t* const f,
	const char* grpname
	) {
	H5_PRIV_FUNC_ENTER (h5_ssize_t, "f=%p, grpname=%s", f, grpname);
	hid_t topo_gid = -1;
	hid_t meshes_gid = -1;

	h5_err_t exists;
	TRY (exists = hdf5_link_exists (f->root_gid, H5T_CONTAINER_GRPNAME));
	if (!exists) H5_CORE_API_LEAVE (0);

	TRY (topo_gid = hdf5_open_group (f->root_gid, H5T_CONTAINER_GRPNAME));

	TRY (exists = hdf5_link_exists (topo_gid, grpname));
	if (!exists) H5_CORE_API_LEAVE (0);

	TRY (meshes_gid = hdf5_open_group (topo_gid, grpname));
	h5_ssize_t num_meshes;
	TRY (num_meshes = hdf5_get_num_groups (meshes_gid));
	TRY (hdf5_close_group (meshes_gid) );
	TRY (hdf5_close_group (topo_gid) );

	H5_CORE_API_RETURN (num_meshes);
}

h5_ssize_t
h5t_get_num_tetmeshes (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER (h5_ssize_t, "f=%p", f);
	H5_CORE_API_RETURN (get_num_meshes (f, TETRAHEDRAL_MESHES_GRPNAME));
}

h5_ssize_t
h5t_get_num_trimeshes (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER (h5_ssize_t, "f=%p", f);
	H5_CORE_API_RETURN (get_num_meshes (f, TRIANGLE_MESHES_GRPNAME));
}

/*!
  Get the number of hierarchical mesh levels for the current mesh.

  \param[in]	f	File handle

  \return	Number of hierarchical mesh levels or error code.
 */
h5_ssize_t
h5t_get_num_leaf_levels (
	h5t_mesh_t* const m
	) {
	H5_CORE_API_ENTER (h5_ssize_t, "m=%p", m);
	H5_CORE_API_RETURN (m->num_leaf_levels);
}

/*!
  Get current level.

  \param[in]	f	File handle.

  \return	Current level ID.
*/
h5t_lvl_idx_t
h5t_get_level (
	h5t_mesh_t* const m
	) {
	H5_CORE_API_ENTER (h5t_lvl_idx_t, "m=%p", m);
	H5_CORE_API_RETURN (m->leaf_level);
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
	h5t_mesh_t* const m,
	const h5_id_t cnode
	) {
	H5_CORE_API_ENTER (h5_ssize_t, "m=%p, cnode=%llu",
			   m, (long long unsigned)cnode);
	UNUSED_ARGUMENT (cnode);

	if (m->leaf_level < 0) {
		H5_CORE_API_LEAVE (h5tpriv_error_undef_level ());
	}
	H5_CORE_API_RETURN (m->num_leaf_elems[m->leaf_level]);
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
	h5t_mesh_t* const m,
	h5_id_t cnode
	) {
	H5_CORE_API_ENTER (h5_ssize_t,
			   "m=%p, cnode=%llu",
			   m, (long long unsigned)cnode);
	UNUSED_ARGUMENT (cnode);

	if (m->leaf_level < 0) {
		H5_CORE_API_LEAVE (h5tpriv_error_undef_level ());
	}
	H5_CORE_API_RETURN (m->num_vertices[m->leaf_level]);
}

