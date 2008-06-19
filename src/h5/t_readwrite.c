#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core.h"
#include "h5_private.h"
#include "H5Part.h"
#include "H5Block.h"

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
		h5_info (
			"Opening group %s/%s.",
			h5_get_objname(parent_gid),
			grpname );
		gid = H5Gopen ( parent_gid, grpname );
	} else {
		h5_info (
			"Creating group %s/%s.",
			h5_get_objname(parent_gid),
			grpname );
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
	if ( sid < 0 ) return HANDLE_H5S_CREATE_SIMPLE_ERR ( 1 );

	h5_err_t h5err = (h5_err_t)h5_write_data (
		f,
		dsname,
		object,
		tid,
		gid,
		sid,
		H5S_ALL,
		H5S_ALL );
	if ( h5err < 0 ) return h5err;

	herr_t herr = H5Sclose ( sid );
	if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;

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

	if ( t->num_vertices <= 0 ) return H5_SUCCESS;  /* ???? */

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

	h5err = _write_obj (
		f,
		t->vmesh_gid,
		t->num_levels,
		maxdim,
		H5T_NATIVE_INT32,
		(void*)t->num_tets,
		H5T_TETMESH_NUM_ELEMS_DSNAME
		);
	if ( h5err < 0 ) return h5err;

	return _write_obj (
		f,
		t->vmesh_gid,
		t->num_levels,
		maxdim,
		H5T_NATIVE_INT32,
		(void*)t->num_tets_on_level,
		H5T_TETMESH_NUM_ELEMS_ON_LEVEL_DSNAME
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
	if ( t->new_mesh < 0 ) return 0;

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

	if ( t->num_tets_on_level ) {
		free ( t->num_tets_on_level );
	}
	t->num_tets_on_level = NULL;

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

h5_size_t
H5t_get_num_meshes (
	h5_file * f
	) {
	return (h5_size_t)h5_get_num_objects (
		f->root_gid,
		H5T_CONTAINER_GRPNAME,
		H5G_GROUP );
}

/*
  If the value of parameter \c id is \c -1, a new mesh will be appended.
  After calling this function, the number of levels is stored in the
  file structure.
*/
h5_err_t
H5t_open_mesh (
	h5_file * f,
	h5_id_t id
	) {
	struct h5t_fdata *t = &f->t;

	h5_err_t h5err = _h5t_close_mesh ( f );
	if ( h5err < 0 ) return h5err;

	if ( t->num_meshes < 0 ) {
		h5_size_t result = H5t_get_num_meshes ( f );
		t->num_meshes = ( result > 0 ? result : 0 );
	}
	if ( (id < -1) || (id >= t->num_meshes) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( "mesh", id );
	}
	if ( id == -1 ) {  /* append new mesh */
		id = t->num_meshes;
	}
	snprintf ( t->mesh_name, sizeof (t->mesh_name), "Mesh#%d", id );

	h5err = _open_mesh_group ( f );
	if ( h5err < 0 ) return h5err;

	t->cur_mesh = id;

	if ( id != t->num_meshes ) {	/* open existing */
		t->num_levels = H5t_get_num_levels ( f );
		if ( t->num_levels < 0 ) return t->num_levels;
	} else {			/* append new */
		t->num_meshes++;
		if ( t->new_mesh < 0 )
			t->new_mesh = id;
		t->num_levels = 0;
	} 

	return H5_SUCCESS;
}

/*
  Number of levels: Number of elements in dataset H5T_COORD3D_NUM_ELEMS_DSNAME
 */
h5_size_t
H5t_get_num_levels (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;
	h5_err_t h5err;

	if ( t->num_levels >= 0 ) return t->num_levels;
	if ( t->cur_mesh < 0 ) {
		return HANDLE_H5_UNDEF_MESH_ERR;
	}
	if ( t->coord_gid < 0 ) {
		h5err = _open_coord_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	hid_t dset_id = H5Dopen ( t->coord_gid, H5T_COORD3D_NUM_ELEMS_DSNAME );
	if ( dset_id < 0 )
		return HANDLE_H5D_OPEN_ERR ( H5T_COORD3D_NUM_ELEMS_DSNAME );
	hid_t space_id = H5Dget_space( dset_id );
	if ( space_id < 0 )
		return HANDLE_H5D_GET_SPACE_ERR;
	hssize_t size = H5Sget_simple_extent_npoints ( space_id );
	if ( size < 0 )
		return HANDLE_H5S_GET_SIMPLE_EXTENT_NPOINTS_ERR;

	herr_t herr = H5Sclose ( space_id );
	if ( herr < 0 )
		return HANDLE_H5S_CLOSE_ERR;
	t->num_levels = size;
	return size;
}

h5_err_t
H5t_open_level (
	h5_file * f,
	const h5_id_t id
	) {
	struct h5t_fdata *t = &f->t;

	if ( (id < 0) || (id >= t->num_levels) )
		return HANDLE_H5_OUT_OF_RANGE_ERR ( "Level", id );
	t->cur_level = id;
	t->last_retrieved_vertex_id = -1;
	t->last_retrieved_tet_id = -1;

	return H5_SUCCESS;
}

h5_id_t
H5t_add_level (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

	if ( f->mode == H5_O_RDONLY ) {
		return H5_ERR_INVAL;
	}

	/* t->num_levels will be set to zero on file creation(!) */
	if ( t->num_levels == -1 ) {	/* unknown number of levels	*/
		/* determine number of levels */
		return -1;		/* not implemented		*/
	}
	t->cur_level = t->num_levels++;

	ssize_t num_bytes = t->num_levels*sizeof ( h5_size_t );
	t->num_vertices = realloc ( t->num_vertices, num_bytes );
	t->num_vertices[t->cur_level] = -1;

	t->num_tets = realloc ( t->num_tets, num_bytes );
	t->num_tets[t->cur_level] = -1;
	t->num_tets_on_level = realloc ( t->num_tets_on_level, num_bytes );
	t->num_tets_on_level[t->cur_level] = -1;

	t->new_level = t->cur_level;
	if ( t->cur_level == 0 ) {
		/* nothing stored yet */
		t->last_stored_vertex_id = -1;
		t->last_stored_tet_id = -1;
	}
	return t->cur_level;
}

h5_size_t
H5t_add_num_vertices (
	h5_file * f,
	const h5_size_t num
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->cur_level < 0 ) {
		return HANDLE_H5_UNDEF_LEVEL_ERR;
	}
	ssize_t num_elems = (t->cur_level > 0 ?
			     t->num_vertices[t->cur_level-1] + num : num);
	t->num_vertices[t->cur_level] = num_elems;
	ssize_t num_bytes = num_elems*sizeof ( t->vertices[0] );
	h5_debug ( "Allocating %ld bytes.", num_bytes ); 
	t->vertices = realloc (	t->vertices, num_bytes );
	if ( t->vertices == NULL ) {
		return HANDLE_H5_NOMEM_ERR;
	}

	return num;
}

/*
  read everything with this function !?
*/
static h5_err_t
_read_dataset (
	h5_file * f,
	hid_t group_id,
	const char dataset_name[],
	hid_t type_id,
	hid_t (*open_mem_space)(h5_file*,hid_t),
	hid_t (*open_file_space)(h5_file*,hid_t),
	void * const data ) {

	hid_t dataset_id = H5Dopen ( group_id, dataset_name );
	if ( dataset_id < 0 ) return HANDLE_H5D_OPEN_ERR ( dataset_name );

	hid_t mem_space_id = (*open_mem_space)( f, dataset_id );
	if ( mem_space_id < 0 ) return mem_space_id;
	hid_t file_space_id = (*open_file_space)( f, dataset_id );
	if ( file_space_id < 0 ) return file_space_id;

	herr_t herr = H5Dread (
		dataset_id,
		type_id,
		mem_space_id,
		file_space_id,
		f->xfer_prop,
		data );
	if ( herr < 0 )
		return HANDLE_H5D_READ_ERR ( h5_get_objname ( dataset_id ) );

	if ( file_space_id != H5S_ALL ) {
		herr = H5Sclose ( file_space_id );
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;
	}

	if ( mem_space_id != H5S_ALL )
		herr = H5Sclose ( mem_space_id );
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;

	herr = H5Dclose ( dataset_id );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR;

	return H5_SUCCESS;
}

static hid_t
_open_mem_space_vertices (
	h5_file * f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

static hid_t
_open_file_space_vertices (
	h5_file * f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

static hid_t
_open_space_all (
	h5_file * f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

static h5_err_t
_read_num_vertices (
	h5_file * f
	) {
	h5_err_t h5err;
	struct h5t_fdata *t = &f->t;

	if ( t->cur_level < 0 ) 
		return HANDLE_H5_UNDEF_LEVEL_ERR;

 	if ( t->coord_gid < 0 ) {
		h5err = _open_coord_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	ssize_t num_bytes = t->num_levels*sizeof ( t->num_vertices[0] );
	h5_debug ( "Allocating %ld bytes.", num_bytes ); 
	t->num_vertices = realloc ( t->num_vertices, num_bytes );
	if ( t->num_vertices == NULL )
		return HANDLE_H5_NOMEM_ERR;
	h5err = _read_dataset (
		f,
		t->coord_gid,
		H5T_COORD3D_NUM_ELEMS_DSNAME,
		H5T_NATIVE_INT32,
		_open_space_all,
		_open_space_all,
		t->num_vertices );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}

static h5_err_t
_read_vertices (
	h5_file * f
	) {
	h5_err_t h5err;
	struct h5t_fdata *t = &f->t;

	if ( t->cur_level < 0 ) 
		return HANDLE_H5_UNDEF_LEVEL_ERR;

 	if ( t->coord_gid < 0 ) {
		h5err = _open_coord_group ( f );
		if ( h5err < 0 ) return h5err;
	}

	if ( t->num_vertices == NULL ) {
		h5err = _read_num_vertices ( f );
		if ( h5err < 0 ) return h5err;
	}

	ssize_t num_elems = t->num_vertices[t->num_levels-1];
	ssize_t num_bytes = num_elems*sizeof ( t->vertices[0] );
	h5_debug ( "Allocating %ld bytes.", num_bytes ); 
	t->vertices = realloc (	t->vertices, num_bytes );
	if ( t->vertices == NULL )
		return HANDLE_H5_NOMEM_ERR;
	h5err = _read_dataset (
		f,
		t->coord_gid,
		H5T_COORD3D_DSNAME,
		t->vertex_tid,
		_open_mem_space_vertices,
		_open_file_space_vertices,
		t->vertices );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}

h5_size_t
H5t_get_num_vertices (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->cur_mesh < 0 ) {
		return HANDLE_H5_UNDEF_MESH_ERR;
	}
	if ( t->cur_level < 0 ) {
		return HANDLE_H5_UNDEF_LEVEL_ERR;
	}
	if ( t->num_vertices == NULL ) {
		h5_err_t h5err = _read_num_vertices ( f );
		if ( h5err < 0 ) return h5err;
	}
	return t->num_vertices[t->cur_level];
}

h5_id_t
H5t_store_vertex (
	h5_file * f,			/*!< file handle		*/
	const h5_id_t id,		/*!< global vertex id or -1	*/
	const h5_float64_t P[3]		/*!< coordinates		*/
	) {
	struct h5t_fdata *t = &f->t;

	/*
	  more than allocated
	*/
	if ( t->last_stored_vertex_id+1 >= t->num_vertices[t->cur_level] ) 
		return HANDLE_H5_OVERFLOW_ERR( "vertex",
					       t->num_vertices[t->cur_level] );

	/*
	  missing call to add the first level
	 */
	if ( t->cur_level < 0 )
		return HANDLE_H5_UNDEF_LEVEL_ERR;

	/*
	  check id
	*/
	if ( (t->cur_level == 0) && (
		     (id < 0) || (id >= t->num_vertices[0]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( "vertex", id );
	}
	if ( (t->cur_level > 0) && (
		     (id <  t->num_vertices[t->cur_level-1]) ||
		     (id >= t->num_vertices[t->cur_level]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( "vertex", id );
	}

	h5_vertex *vertex = &t->vertices[++t->last_stored_vertex_id];
	vertex->id = id;
	memcpy ( &vertex->P, P, sizeof ( vertex->P ) );
	
	return t->last_stored_vertex_id;
}


h5_size_t
H5t_get_vertex_ids (
	h5_file * f,
	h5_id_t * const ids[]
	) {
	return -1;
}

h5_id_t
H5t_get_vertex (
	h5_file * f,			/*!< file handle		*/
	h5_id_t * const id,		/*!< global vertex		*/
	h5_float64_t P[3]		/*!< coordinates		*/
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->vertices == NULL ) {
		h5_err_t h5err = _read_vertices ( f );
		if ( h5err < 0 ) return h5err;
	}
	if ( t->last_retrieved_vertex_id+1 >= t->num_vertices[t->cur_level] ) {
		h5_warn ( "Trying to read more tets than available!" );
		return -1;
	}
	h5_vertex *vertex = &t->vertices[++t->last_retrieved_vertex_id];
	*id = vertex->id;
	memcpy ( P, &vertex->P, sizeof ( vertex->P ) );

	return t->last_retrieved_vertex_id;
}

h5_size_t
H5t_add_num_tets (
	h5_file * f,
	const h5_size_t num
	) {
	struct h5t_fdata *t = &f->t;

	ssize_t num_tets = t->cur_level > 0 ?
			    num + t->num_tets[t->cur_level-1] : num;
	t->num_tets[t->cur_level] = num_tets;

	t->num_tets_on_level[t->cur_level] = t->cur_level > 0 ?
			    num + t->num_tets_on_level[t->cur_level-1] : num;

	ssize_t num_bytes = num_tets*sizeof ( t->tets[0] );
	h5_debug ( "Allocating %ld bytes.", num_bytes ); 
	t->tets = realloc ( t->tets, num_bytes );
	if ( t->tets == NULL ) {
		return H5_ERR_NOMEM;
	}

	t->map_tets_g2l = realloc (
		t->map_tets_g2l,
		num_tets*sizeof ( t->map_tets_g2l[0] ) );

	return num;
}


h5_id_t
H5t_store_tet (
	h5_file * f,
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	const h5_id_t parent_id,	/*!< global parent id
					     if level \c >0 else \c -1	*/
	const h5_id_t vertex_ids[4]	/*!< tuple with vertex id's	*/
	) {

	struct h5t_fdata *t = &f->t;

	/*
	  more than allocated
	*/
	if ( t->last_stored_tet_id+1 >= t->num_tets[t->cur_level] ) 
		return HANDLE_H5_OVERFLOW_ERR(  "tet", t->num_tets[t->cur_level] );

	/*
	  missing call to add the first level
	 */
	if ( t->cur_level < 0 )
		return HANDLE_H5_UNDEF_LEVEL_ERR;

	/*
	  check parent id
	*/
	if ( (t->cur_level == 0) && (parent_id != -1) ) {
		return HANDLE_H5_PARENT_ID_ERR ( "tet", tet_id, parent_id );
	} 
	if ( (t->cur_level >  0) && (parent_id < 0) ) {
		return HANDLE_H5_PARENT_ID_ERR ( "tet", tet_id, parent_id );
	}
	if ( (t->cur_level >  0) && (parent_id >= t->num_tets[t->cur_level-1]) ) {
		return HANDLE_H5_PARENT_ID_ERR ( "tet", tet_id, parent_id );
	}
	/*
	  check tet_id
	*/
	if ( (t->cur_level == 0) && (
		     (tet_id < 0) || (tet_id >= t->num_tets[0]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( "tet", tet_id );
	}
	if ( (t->cur_level > 0) && (
		     (tet_id <  t->num_tets[t->cur_level-1]) ||
		     (tet_id >= t->num_tets[t->cur_level]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( "tet", tet_id );
	}
	

	h5_tetrahedron *tet = &t->tets[++t->last_stored_tet_id];
	tet->id = tet_id;
	tet->parent_id = parent_id;
	tet->refined_on_level = -1;
	tet->unused = 0;
	memcpy ( &tet->vertex_ids, vertex_ids, sizeof ( tet->vertex_ids ) );

	t->map_tets_g2l[tet_id] = t->last_stored_tet_id;
	if ( parent_id >= 0 ) {
		h5_id_t local_parent_id = t->map_tets_g2l[parent_id];
		if ( t->tets[local_parent_id].refined_on_level < 0 ) {
			t->tets[local_parent_id].refined_on_level = t->cur_level;
			t->num_tets_on_level[t->cur_level]--;
		}
	}
	return t->last_stored_vertex_id;
}

static h5_err_t
_read_num_tets (
	h5_file * f
	) {


	h5_err_t h5err;
	struct h5t_fdata *t = &f->t;

	if ( t->cur_level < 0 ) 
		return HANDLE_H5_UNDEF_LEVEL_ERR;

 	if ( t->vmesh_gid < 0 ) {
		h5err = _open_vmesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	ssize_t num_bytes = t->num_levels*sizeof ( t->num_tets[0] );
	h5_debug ( "Allocating %ld bytes.", num_bytes ); 
	t->num_tets = realloc ( t->num_tets, num_bytes );
	if ( t->num_tets == NULL )
		return HANDLE_H5_NOMEM_ERR;
	h5err = _read_dataset (
		f,
		t->vmesh_gid,
		H5T_TETMESH_NUM_ELEMS_DSNAME,
		H5T_NATIVE_INT32,
		_open_space_all,
		_open_space_all,
		t->num_tets );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}

static h5_err_t
_read_num_tets_on_level (
	h5_file * f
	) {

	h5_err_t h5err;
	struct h5t_fdata *t = &f->t;

	if ( t->cur_level < 0 ) 
		return HANDLE_H5_UNDEF_LEVEL_ERR;

 	if ( t->vmesh_gid < 0 ) {
		h5err = _open_vmesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	ssize_t num_bytes = t->num_levels*sizeof ( t->num_tets_on_level[0] );
	h5_debug ( "Allocating %ld bytes.", num_bytes ); 
	t->num_tets_on_level = realloc ( t->num_tets_on_level, num_bytes );
	if ( t->num_tets_on_level == NULL )
		return HANDLE_H5_NOMEM_ERR;
	h5err = _read_dataset (
		f,
		t->vmesh_gid,
		H5T_TETMESH_NUM_ELEMS_ON_LEVEL_DSNAME,
		H5T_NATIVE_INT32,
		_open_space_all,
		_open_space_all,
		t->num_tets_on_level );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}

static hid_t
_open_mem_space_tets (
	h5_file * f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

static hid_t
_open_file_space_tets (
	h5_file * f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

static h5_err_t
_read_tets (
	h5_file * f
	) {
	h5_err_t h5err;
	struct h5t_fdata *t = &f->t;

	if ( t->cur_level < 0 ) 
		return HANDLE_H5_UNDEF_LEVEL_ERR;

 	if ( t->vmesh_gid < 0 ) {
		h5err = _open_vmesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}

	if ( t->num_tets == NULL ) {
		h5err = _read_num_tets ( f );
		if ( h5err < 0 ) return h5err;
	}

	ssize_t num_elems = t->num_tets[t->num_levels-1];
	ssize_t num_bytes = num_elems*sizeof ( t->tets[0] );
	h5_debug ( "Allocating %ld bytes.", num_bytes ); 
	t->tets = realloc (	t->tets, num_bytes );
	if ( t->tets == NULL )
		return HANDLE_H5_NOMEM_ERR;
	h5err = _read_dataset (
		f,
		t->vmesh_gid,
		H5T_TETMESH_DSNAME,
		t->tet_tid,
		_open_mem_space_tets,
		_open_file_space_tets,
		t->tets );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}

h5_size_t
H5t_get_num_tets (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->cur_mesh < 0 ) {
		return HANDLE_H5_UNDEF_MESH_ERR;
	}
	if ( t->cur_level < 0 ) {
		return HANDLE_H5_UNDEF_LEVEL_ERR;
	}
	if ( t->num_tets_on_level == NULL ) {
		h5_err_t h5err = _read_num_tets_on_level ( f );
		if ( h5err < 0 ) return h5err;
	}
	return t->num_tets_on_level[t->cur_level];
}

h5_id_t
H5t_get_tet (
	h5_file * f,
	h5_id_t * const id,		/*!< global tetrahedron id	*/
	h5_id_t * const parent_id,	/*!< global parent id
					     if level \c >0 else \c -1	*/
	h5_id_t ids[4]			/*!< tuple with vertex id's	*/
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->tets == NULL ) {
		h5_err_t h5err = _read_tets ( f );
		if ( h5err < 0 ) return h5err;
	}
	if ( t->last_retrieved_tet_id+1 >= t->num_tets[t->cur_level] ) {
		h5_warn ( "Trying to read more tets than available!" );
		return -1;
	}
	h5_tetrahedron *tet = &t->tets[++t->last_retrieved_tet_id];

	while ( (tet->refined_on_level != -1) &&
		(tet->refined_on_level <= t->cur_level) ){
		tet++;
		t->last_retrieved_tet_id++;
		if ( t->last_retrieved_tet_id >= t->num_tets[t->cur_level] ) {
			return HANDLE_H5_INTERNAL_ERR;
		}
	}

	*id = tet->id;
	*parent_id = tet->parent_id;
	memcpy ( ids, &tet->vertex_ids, sizeof ( tet->vertex_ids ) );

	return t->last_retrieved_tet_id;
}
