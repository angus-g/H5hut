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
#include "H5BlockPrivate.h"
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
	memset ( f->t, 0, sizeof(f->t) );

	f->t.mesh_gid = -1;
	f->t.num_levels = f->is_new_step ? 0 : -1;
	f->t.cur_level = -1;

	return H5_SUCCESS;
}

static h5_err_t
_open_topo_group (
	h5_file * f
	) {
	herr_t herr = H5Gget_objinfo( f->step_gid, "Topo", 1, NULL );
	if ( herr => 0 ) {
		f->t.mesh_gid = H5Gopen ( f->step_gid, "Topo" );
	} else {
		f->t.mesh_gid = H5Gcreate ( f->step_gid, "Topo", 0 );
	}
	if ( f->t.mesh_gid < 0 ) 
		return HANDLE_H5G_OPEN_ERR ( "Topo" );
}

/*
  store vertices -> Topo.VERTICES_COORD3D
  levels -> TOPO.VERTICES_LEVELS3D
 */
static h5_err_t
_write_vertices (
	h5_file * f
	) {
	h5t_fdata t &f->t;
	herr_t herr;

	if ( t->coord3d_gid < 0 ) {
		h5err = _open_coord3d_group ( f );
		if ( h5err < 0 ) return h5err;
	}

	if ( t->num_levels == 0 ) return 0;
	if ( t->num_levels < 0 ) return -1; 

	hsize_t current_dims[1];
	current_dims[0] = t->levels[t->num_levels-1].num_vertices;

	t->coord3d_sid = H5Screate_simple (
		1,
		current_dims,
		 H5S_UNLIMITED
		);
	if ( t->coord3d_sid < 0 ) return -1;

	t->coord3d_did = H5Dcreate (
		coord3d_gid,
		"COORD3D",
		t->vertex_tid,
		t->coord3d_sid,
		H5P_DEFAULT);
	if ( t->coord3d_did < 0 ) return -1;

	herr = H5Dwrite (
		coord3d,
		t->vertex_tid,
		H5S_ALL, H5S_ALL, H5P_DEFAULT,
		t->vertices );
	if ( herr < 0 ) return -1;

	return H5_SUCCESS;
}

static h5_err_t
_write_tets (
	h5_file * f
	) {
	return -1;
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

	h5_err_t h5err = _close_hdf5_objs ( f );
	if ( h5err < 0 ) return h5err;

	h5_err_t h5err = _release_memory ( f );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}
