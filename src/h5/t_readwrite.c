#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "H5PartTypes.h"
#include "H5BlockTypes.h"
#include "H5Part.h"
#include "H5Block.h"
#include "H5PartPrivate.h"
#include "H5PartErrors.h"
#include "H5BlockErrors.h"
#include "H5.h"

/*
  Initialize required data structures only!
*/
h5_err_t
_h5t_init_step (
	h5_file * f
	) {
	memset ( &f->t, 0, sizeof(f->t) );

	f->t.topo_gid = -1;
	f->t.num_levels = f->is_new_step ? 0 : -1;
	f->t.cur_level = -1;

	return H5_SUCCESS;
}

static hid_t
_open_group (
	h5_file * f,
	hid_t parent_gid,
	const char * const grpname
	) {
	hid_t gid;
	herr_t herr = H5Gget_objinfo(
		parent_gid, grpname, 1, NULL );
	if ( herr >= 0 ) {
		gid = H5Gopen ( parent_gid, grpname );
	} else {
		gid = H5Gcreate ( parent_gid, grpname, 0 );
	}
	if ( gid < 0 ) 
		return HANDLE_H5G_OPEN_ERR ( H5T_CONTAINER_GRPNAME );

	return H5_SUCCESS;
}

static h5_err_t
_open_topo_group (
	h5_file * f
	) {
	return _open_group ( f, f->step_gid, H5T_CONTAINER_GRPNAME );
}

static h5_err_t
_open_coord_group (
	h5_file * f
	) {
	if ( f->t.topo_gid < 0 ) {
		h5_err_t h5err = _open_topo_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	return _open_group ( f, f->t.topo_gid, H5T_COORD_GRPNAME );
}

static h5_err_t
_open_vmesh_group (
	h5_file * f
	) {
	if ( f->t.topo_gid < 0 ) {
		h5_err_t h5err = _open_topo_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	return _open_group ( f, f->t.topo_gid, H5T_VMESH_GRPNAME );
}

static h5_err_t
_write_obj (
	const hid_t	gid,
	const hsize_t  current_dims,
	const hsize_t  max_dims,
	const hid_t    tid,
	const void * const object,
	const char * const dsname
	) {
	hsize_t dims[1] = { current_dims };
	hsize_t maxdims[1] = { max_dims };

	hid_t sid = H5Screate_simple (
		1,
		dims,
		maxdims
		);
	if ( sid < 0 ) return -1;

	hid_t did = H5Dcreate (
		gid,
		dsname,
		tid,
		sid,
		H5P_DEFAULT);
	if ( did < 0 ) return -1;

	herr_t herr = H5Dwrite (
		did,
		tid,
		H5S_ALL, H5S_ALL, H5P_DEFAULT,
		object );
	if ( herr < 0 ) return -1;
	
	herr = H5Dclose ( did );
	if ( herr < 0 ) return -1;

	herr = H5Sclose ( sid );
	if ( herr < 0 ) return -1;

	return H5_SUCCESS;
}

/*
  store vertices -> Topo.VERTICES_COORD3D
  levels -> TOPO.VERTICES_LEVELS3D
 */
static h5_err_t
_write_vertices (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;
	h5_err_t h5err;

	if ( t->coord_gid < 0 ) {
		h5err = _open_coord_group ( f );
		if ( h5err < 0 ) return h5err;
	}

	hsize_t maxdim = H5S_UNLIMITED;
	h5err = _write_obj (
		t->coord_gid,
		t->vertex_tid,
		t->num_vertices[t->num_levels-1],
		maxdim,
		(void*)t->vertices,
		H5T_COORD3D_DSNAME
		);

	return H5_SUCCESS;
}

static h5_err_t
_write_tets (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;
	h5_err_t h5err;

	if ( t->vmesh_gid < 0 ) {
		h5err = _open_vmesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}

	hsize_t maxdim = H5S_UNLIMITED;
	herr_t herr = _write_obj (
		t->vmesh_gid,
		t->tet_tid,
		t->num_tets[t->num_levels-1],
		maxdim,
		(void*)t->tets,
		H5T_TETMESH_DSNAME
		);

	return H5_SUCCESS;
}

/*
  determine new levels

  foreach Object
  - create HDF5 groups
  - write data
 */
static h5_err_t
_write_data (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;
	h5_err_t h5err;

	if ( t->num_levels == 0 ) return 0;
	if ( t->num_levels < 0 ) return -1; 

	if ( t->topo_gid < 0 ) {
		h5err = _open_topo_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	h5err = _write_vertices( f );
	if ( h5err < 0 ) return h5err;
	h5err = _write_tets( f );
	if ( h5err < 0 ) return h5err;

	return -1;
}

static h5_err_t
_close_hdf5_objs (
	h5_file * f
	) {

	
	return -1;
}

static h5_err_t
_release_memory (
	h5_file * f
	) {

	return -1;
}


/*
 - write data
 - close HDF5 objects we cannot reuse
 - free memory
*/
h5_err_t
_h5t_close_step (
	h5_file * f
	) {
	h5_err_t h5err = _write_data ( f );
	if ( h5err < 0 ) return h5err;

	h5err = _close_hdf5_objs ( f );
	if ( h5err < 0 ) return h5err;

	h5err = _release_memory ( f );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}
