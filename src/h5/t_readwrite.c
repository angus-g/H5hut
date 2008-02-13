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

	return gid;
}

static h5_err_t
_open_topo_group (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

	t->topo_gid = _open_group ( f, f->root_gid, H5T_CONTAINER_GRPNAME );
	return t->topo_gid;
}

static h5_err_t
_open_mesh_group (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->topo_gid < 0 ) {
		h5_err_t h5err = _open_topo_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	t->mesh_gid = _open_group ( f, t->topo_gid, t->mesh_name );
	return (h5_err_t)t->mesh_gid;
}

static h5_err_t
_open_coord_group (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->mesh_gid < 0 ) {
		h5_err_t h5err = _open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	t->coord_gid = _open_group ( f, t->mesh_gid, H5T_COORD_GRPNAME );
	return (h5_err_t) t->coord_gid;
}

static h5_err_t
_open_vmesh_group (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->mesh_gid < 0 ) {
		h5_err_t h5err = _open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	t->vmesh_gid = _open_group ( f, t->mesh_gid, H5T_VMESH_GRPNAME );
	return (h5_err_t) t->vmesh_gid;
}

static h5_err_t
_write_obj (
	h5_file * f,
	const hid_t	gid,
	const hsize_t  current_dims,
	const hsize_t  max_dims,
	const hid_t    tid,
	const void * const object,
	const char * const dsname
	) {
	hsize_t dims[1] = { current_dims };
	hsize_t maxdims[1] = { current_dims };
	/*hsize_t maxdims[1] = { max_dims };*/

	hid_t sid = H5Screate_simple (
		1,
		dims,
		maxdims
		);
	if ( sid < 0 ) return -1;

	h5_err_t h5err = (h5_err_t)H5_write_data (
		f,
		dsname,
		object,
		tid,
		gid,
		sid,
		H5S_ALL,
		H5S_ALL );
	if ( h5err < 0 ) return h5err;
/*
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
*/
	herr_t herr = H5Sclose ( sid );
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

	if ( t->num_vertices <= 0 ) return H5_SUCCESS;

	if ( t->coord_gid < 0 ) {
		h5err = _open_coord_group ( f );
		if ( h5err < 0 ) return h5err;
	}

	hsize_t maxdim = H5S_UNLIMITED;
	h5err = _write_obj (
		f,
		t->coord_gid,
		t->num_vertices[t->num_levels-1],
		maxdim,
		t->vertex_tid,
		(void*)t->vertices,
		H5T_COORD3D_DSNAME
		);
	if ( h5err < 0 ) return h5err;
	return _write_obj (
		f,
		t->coord_gid,
		t->num_levels,
		maxdim,
		H5T_NATIVE_INT32,
		(void*)t->num_vertices,
		H5T_COORD3D_NUM_ELEMS_DSNAME
		);
}

static h5_err_t
_write_tets (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;
	h5_err_t h5err;
	
	if ( t->num_tets <= 0 ) return H5_SUCCESS;

	if ( t->vmesh_gid < 0 ) {
		h5err = _open_vmesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}

	hsize_t maxdim = H5S_UNLIMITED;
	h5err = _write_obj (
		f,
		t->vmesh_gid,
		t->num_tets[t->num_levels-1],
		maxdim,
		t->tet_tid,
		(void*)t->tets,
		H5T_TETMESH_DSNAME
		);
	if ( h5err < 0 ) return h5err;

	return _write_obj (
		f,
		t->vmesh_gid,
		t->num_levels,
		maxdim,
		H5T_NATIVE_INT32,
		(void*)t->num_tets,
		H5T_TETMESH_NUM_ELEMS_DSNAME
		);

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

	if ( t->num_levels <= 0 ) return 0;

	if ( t->topo_gid < 0 ) {
		h5err = _open_topo_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	h5err = _write_vertices( f );
	if ( h5err < 0 ) return h5err;
	h5err = _write_tets( f );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}

static h5_err_t
_close_hdf5_objs (
	h5_file * f
	) {

	
	return H5_SUCCESS;
}

static h5_err_t
_release_memory (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->num_vertices ) {
		free ( t->num_vertices );
	}
	t->num_vertices = NULL;

	if ( t->num_tets ) {
		free ( t->num_tets );
	}
	t->num_tets = NULL;

	if ( t->map_tets_g2l ) {
		free ( t->map_tets_g2l );
	}
	t->map_tets_g2l = NULL;

	if ( t->vertices ) {
		free ( t->vertices );
	}
	t->vertices = NULL;

	if ( t->tets ) {
		free ( t->tets );
	}
	t->tets = NULL;

	return H5_SUCCESS;
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

	return H5_SUCCESS;
}

h5_err_t
_h5t_create_mesh (
	h5_file *f		/*!< file handle */
	) {

	struct h5t_fdata *t = &f->t;
	h5_err_t h5err = H5_SUCCESS;

	h5err = _open_mesh_group ( f );
	if ( h5err < 0 ) return h5err;

	t->num_levels = 0;

	return h5err;
}

h5_err_t
_h5t_close_mesh (
	h5_file *f		/*!< file handle */
	) {

	h5_err_t h5err = H5_SUCCESS;
	
	h5err = _write_data ( f );
	if ( h5err < 0 ) return h5err;

	h5err = _close_hdf5_objs ( f );
	if ( h5err < 0 ) return h5err;

	h5err = _release_memory ( f );
	if ( h5err < 0 ) return h5err;

	if (( h5err = _h5t_init_fdata ( f )) < 0 ) return h5err;

	return h5err;
}


h5_id_t
H5t_add_mesh (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;
	h5_err_t h5err = H5_SUCCESS;

	/*
	  - close current mesh
	  - count number of objects in /TOPO - this is the number of stored
	  meshes.
	  - create new group
	*/
	h5err = _h5t_close_mesh ( f );
	if ( h5err < 0 ) return h5err;

	h5err = _open_topo_group ( f );
	if ( h5err < 0 ) return h5err;

	t->cur_mesh = (h5_id_t)H5_get_num_objects (
		f->root_gid,
		H5T_CONTAINER_GRPNAME,
		H5G_GROUP );
	if ( t->cur_mesh < 0 ) return t->cur_mesh;
	if ( t->new_mesh < 0 )
		t->new_mesh = t->cur_mesh;

	snprintf ( t->mesh_name, sizeof ( t->mesh_name ), "Mesh#%d", t->cur_mesh );

	h5err = _h5t_create_mesh ( f );
	if ( h5err < 0 ) return h5err;

	return t->cur_mesh;
}
