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
	const enum h5_oid type_id
	) {
	h5t_fdata_t* t = f->t;

	if (t->num_meshes >= 0) {
		return t->num_meshes;
	}
	if (t->topo_gid < 0) {
		TRY( h5tpriv_open_topo_group (f) );
	}
	TRY( t->num_meshes = h5_get_num_hdf5_groups (f, t->meshes_gid) );

	return t->num_meshes;
}

/*!
  Get the number of hierarchical mesh levels.

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
	h5t_fdata_t* t = f->t;

	if (t->cur_mesh < 0) {
		return h5tpriv_error_undef_mesh (f);
	}
	if (t->cur_level < 0) {
		return h5tpriv_error_undef_level (f);
	}
	return t->num_vertices[t->cur_level];
}

