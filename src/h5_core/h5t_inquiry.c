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
	h5_file_t * f,
	const enum h5_oid type
	) {
	struct h5t_fdata *t = f->t;

	if ( t->num_meshes != -1 ) {
		return t->num_meshes;
	}
	if ( t->topo_gid < 0 ) {
		TRY( _h5t_open_topo_group ( f ) );
	}
	TRY( t->num_meshes = (h5_size_t)hdf5_get_num_objects (
			t->topo_gid,
			_h5t_meshes_grpnames[t->mesh_type],
			H5G_GROUP ) );

	return t->num_meshes;
}

/*
 */
h5_size_t
h5t_get_num_levels (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;
	h5_err_t h5err;

	if ( t->num_levels >= 0 ) return t->num_levels;
	if ( t->cur_mesh < 0 ) {
		return _h5t_error_undef_mesh ( f );
	}
	if ( t->mesh_gid < 0 ) {
		h5err = _h5t_open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	hid_t dataset_id = H5Dopen ( t->mesh_gid, "NumVertices", H5P_DEFAULT );
	if ( dataset_id < 0 )
		return HANDLE_H5D_OPEN_ERR ( f, "NumVertices" );
	hid_t diskspace_id = H5Dget_space( dataset_id );
	if ( diskspace_id < 0 )
		return HANDLE_H5D_GET_SPACE_ERR ( f );
	hssize_t size = H5Sget_simple_extent_npoints ( diskspace_id );
	if ( size < 0 )
		return HANDLE_H5S_GET_SIMPLE_EXTENT_NPOINTS_ERR ( f );

	herr_t herr = H5Sclose ( diskspace_id );
	if ( herr < 0 )
		return HANDLE_H5S_CLOSE_ERR ( f );
	t->num_levels = size;
	return size;
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
	hid_t cnode_id,
	hid_t level_id
	) {
	struct h5t_fdata *t = f->t;

	if ( cnode_id < 0 ) {
		cnode_id = f->nprocs;
	}
	if ( level_id < 0 ) {
		level_id = f->t->num_levels-1;
	}
	if ( t->cur_mesh < 0 ) {
		return _h5t_error_undef_mesh ( f );
	}
	if ( t->cur_level < 0 ) {
		return _h5t_error_undef_level( f );
	}
	if ( t->num_elems_on_level == NULL ) {
		TRY( _h5t_read_num_elems ( f ) );
	}
	return t->num_elems_on_level[t->cur_level];
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
	hid_t cnode_id,
	hid_t level_id
	) {
	struct h5t_fdata *t = f->t;

	if ( cnode_id < 0 ) {
		cnode_id = f->nprocs;
	}
	if ( level_id < 0 ) {
		level_id = f->t->num_levels-1;
	}
	if ( t->cur_mesh < 0 ) {
		return _h5t_error_undef_mesh ( f );
	}
	if ( t->num_elems == NULL ) {
		TRY( _h5t_read_num_elems ( f ) );
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
	hid_t cnode_id,
	hid_t level_id
	) {
	struct h5t_fdata *t = f->t;

	if ( cnode_id < 0 ) {
		cnode_id = f->nprocs;
	}
	if ( level_id < 0 ) {
		level_id = f->t->num_levels-1;
	}
	if ( t->cur_mesh < 0 ) {
		return _h5t_error_undef_mesh ( f );
	}
	if ( t->cur_level < 0 ) {
		return _h5t_error_undef_level( f );
	}
	if ( t->num_vertices == NULL ) {
		TRY( _h5t_read_num_vertices ( f ) );
	}
	return t->num_vertices[level_id];
}

h5_id_t
h5t_get_level (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;
	return t->cur_level;
}
