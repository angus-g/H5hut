#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

h5_size_t
h5t_get_num_meshes (
	h5_file_t * const f,
	const enum h5_oid type
	) {
	struct h5t_fdata *t = f->t;

	if ( t->num_meshes >= 0 ) {
		return t->num_meshes;
	}
	if ( t->topo_gid < 0 ) {
		TRY( _h5t_open_topo_group ( f ) );
	}
	TRY( t->num_meshes = (h5_size_t)hdf5_get_num_objects (
			t->topo_gid,
			_h5t_meshes_grpnames[type],
			H5G_GROUP ) );

	return t->num_meshes;
}

/*
 */
h5_size_t
h5t_get_num_levels (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;

	if ( t->cur_mesh < 0 ) {
		return _h5t_error_undef_mesh ( f );
	}
	return t->num_levels;
}


/*
  query number of topological elems:

  * on this cnode on current level
    H5FedGetNumXXX(), h5t_get_num_elems()

  * on given cnode on current level
    H5FedGetNumXXXCnode(), h5t_get_num_elems_cnode()

  * on this cnode on current level total (internal)
    _h5t_get_num_elems_total()

  * on given cnode on current level total (internal)
  * on all cnodes on current level
    H5FedGetNumXXXCnodeTotal()
  * on all cnodes on current level total (internal)

  * on this cnode on given level
    H5FedGetNumXXXOnLevel()
  * on this cnode on given level total (internal)
  * on given cnode on given level
    H5FedGetNumXXXCnodeOnLevel()
  * on given cnode on given level total (internal)
  * on all cnodes on given level
    H5FedGetNumXXXTotalOnLevel()
  * on all cnodes on given level total (internal)


  * on this cnode on last level (internal)
  * on given cnode on last level (internal)
  * on this cnode on last level total (internal)
  * on given cnode on last level total (internal)
  * on all cnodes on last level (internal)
  * on all cnodes on last level total (internal)
  
*/

/*!
  Return number of elems on compute node \c cnode_id
  and level \c level_id.  If \cnode_id is equal \c -1 return the 
  number of elems in the entire mesh. If \level_id is equal \c -1
  return the number of elems in the last level.

  \remark
  Refined elems are *not* counted.
 */
h5_size_t
h5t_get_num_elems (
	h5_file_t * f,
	h5_id_t cnode_id,
	h5_id_t level_id
	) {
	h5t_fdata_t *t = f->t;

	if ( t->cur_mesh < 0 ) {
		return _h5t_error_undef_mesh ( f );
	}
	if ( level_id < 0 || level_id >= t->num_levels ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR ( f, "Level", level_id );
	}
	return t->num_elems_on_level[level_id];
}

/*!
  Return number of all elems on compute node \c cnode_id
  and level \c level_id including refined elems.   If
  \cnode_id is equal \c -1 return the number of elems
  in the entire mesh. If \level_id is equal \c -1
  return the number of elems in the last level.
 */
h5_size_t
h5t_get_num_elems_total (
	h5_file_t * f,
	h5_id_t cnode_id,
	h5_id_t level_id
	) {
	h5t_fdata_t *t = f->t;
	if ( t->cur_mesh < 0 ) {
		return _h5t_error_undef_mesh ( f );
	}
	if ( level_id < 0 || level_id >= t->num_levels ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR ( f, "Level", level_id );
	}
	return t->num_elems[level_id];
}

/*!
  Return number of vertices on compute node \c cnode_id
  and level \c level_id.  If \cnode_id is equal \c -1 return the 
  number of vertices in the entire mesh. If \level_id is equal \c -1
  return the number of vertices in the last level.

  \remark
  There is nothing like "refined vertices".
 */
h5_size_t
h5t_get_num_vertices (
	h5_file_t * f,
	h5_id_t cnode_id,
	h5_id_t level_id
	) {
	h5t_fdata_t *t = f->t;

	if ( t->cur_mesh < 0 ) {
		return _h5t_error_undef_mesh ( f );
	}
	if ( level_id < 0 || level_id >= t->num_levels ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR ( f, "Level", level_id );
	}
	return t->num_vertices[level_id];
}

h5_id_t
h5t_get_level (
	h5_file_t * f
	) {
	return f->t->cur_level;
}
