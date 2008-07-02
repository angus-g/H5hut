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
_open_meshes_group (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->topo_gid < 0 ) {
		h5_err_t h5err = _open_topo_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	switch ( t->mesh_type) {
	case TETRAHEDRAL_MESH:
		t->meshes_gid = _open_group ( f, t->topo_gid, "TetMeshes" );
		break;
	case TRIANGLE_MESH:
		t->meshes_gid = _open_group ( f, t->topo_gid, "TriangleMeshes" );
		break;
	default:
		t->meshes_gid = -1;
	}
	return (h5_err_t)t->meshes_gid;
}

static h5_err_t
_open_mesh_group (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->meshes_gid < 0 ) {
		h5_err_t h5err = _open_meshes_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	t->mesh_gid = _open_group ( f, t->meshes_gid, t->mesh_name );
	return (h5_err_t)t->mesh_gid;
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

 	if ( t->mesh_gid < 0 ) {
		h5err = _open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}

	hsize_t maxdim = H5S_UNLIMITED;
	h5err = _write_obj (
		f,
		t->mesh_gid,
		t->num_vertices[t->num_levels-1],
		maxdim,
		t->vertex_tid,
		(void*)t->vertices,
		"Vertices"
		);
	if ( h5err < 0 ) return h5err;
	return _write_obj (
		f,
		t->mesh_gid,
		t->num_levels,
		maxdim,
		H5T_NATIVE_INT32,
		(void*)t->num_vertices,
		"NumVertices"
		);
}

static h5_err_t
_write_entities (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;
	h5_err_t h5err;
	
	if ( t->num_entities <= 0 ) return H5_SUCCESS;

	if ( t->mesh_gid < 0 ) {
		h5err = _open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}

	hsize_t maxdim = H5S_UNLIMITED;
	h5err = _write_obj (
		f,
		t->mesh_gid,
		t->num_entities[t->num_levels-1],
		maxdim,
		t->entity_tid,
		(void*)t->entities.data,
		"Entities"
		);
	if ( h5err < 0 ) return h5err;

	h5err = _write_obj (
		f,
		t->mesh_gid,
		t->num_levels,
		maxdim,
		H5T_NATIVE_INT32,
		(void*)t->num_entities,
		"NumEntities"
		);
	if ( h5err < 0 ) return h5err;

	return _write_obj (
		f,
		t->mesh_gid,
		t->num_levels,
		maxdim,
		H5T_NATIVE_INT32,
		(void*)t->num_entities_on_level,
		"NumEntitiesOnLevel"
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

	h5err = _write_vertices( f );
	if ( h5err < 0 ) return h5err;
	h5err = _write_entities( f );
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

	if ( t->num_entities ) {
		free ( t->num_entities );
	}
	t->num_entities = NULL;

	if ( t->num_entities_on_level ) {
		free ( t->num_entities_on_level );
	}
	t->num_entities_on_level = NULL;

	if ( t->map_entity_g2l ) {
		free ( t->map_entity_g2l );
	}
	t->map_entity_g2l = NULL;

	if ( t->vertices ) {
		free ( t->vertices );
	}
	t->vertices = NULL;

	if ( t->entities.data ) {
		free ( t->entities.data );
	}
	t->entities.data = NULL;

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
h5t_get_num_meshes (
	h5_file * f,
	const enum h5_mesh_types type
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->topo_gid < 0 ) {
		h5_err_t h5err = _open_topo_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	switch ( type ) {
	case TETRAHEDRAL_MESH:
		return (h5_size_t)h5_get_num_objects (
			f->t.topo_gid,
			"TetMeshes",
			H5G_GROUP );
	case TRIANGLE_MESH:
		return (h5_size_t)h5_get_num_objects (
			f->t.topo_gid,
			"TriangleMeshes",
			H5G_GROUP );
	default:
		return -1;
	}
}

/*
  If the value of parameter \c id is \c -1, a new mesh will be appended.
  After calling this function, the number of levels is stored in the
  file structure.
*/
h5_err_t
h5t_open_mesh (
	h5_file * f,
	h5_id_t id,
	const enum h5_mesh_types type
	) {
	struct h5t_fdata *t = &f->t;

	h5_err_t h5err = _h5t_close_mesh ( f );
	if ( h5err < 0 ) return h5err;

	if ( t->num_meshes < 0 ) {
		h5_size_t result = h5t_get_num_meshes ( f, type );
		t->num_meshes = ( result > 0 ? result : 0 );
	}
	if ( (id < -1) || (id >= t->num_meshes) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( "mesh", id );
	}
	if ( id == -1 ) {  /* append new mesh */
		id = t->num_meshes;
	}
	t->mesh_type = type;
	snprintf ( t->mesh_name, sizeof (t->mesh_name), "%d", id );

	switch( type ) {
	case TETRAHEDRAL_MESH:
		t->entity_tid = t->tet_tid;
		break;
	case TRIANGLE_MESH:
		t->entity_tid = t->triangle_tid;
		break;
	default:
		return -1;
	}

	h5err = _open_mesh_group ( f );
	if ( h5err < 0 ) return h5err;

	t->cur_mesh = id;

	if ( id != t->num_meshes ) {	/* open existing */
		t->num_levels = h5t_get_num_levels ( f );
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
h5t_get_num_levels (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;
	h5_err_t h5err;

	if ( t->num_levels >= 0 ) return t->num_levels;
	if ( t->cur_mesh < 0 ) {
		return HANDLE_H5_UNDEF_MESH_ERR;
	}
	if ( t->mesh_gid < 0 ) {
		h5err = _open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	hid_t dset_id = H5Dopen ( t->mesh_gid, "NumVertices" );
	if ( dset_id < 0 )
		return HANDLE_H5D_OPEN_ERR ( "NumVertices" );
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
h5t_open_level (
	h5_file * f,
	const h5_id_t id
	) {
	struct h5t_fdata *t = &f->t;

	if ( (id < 0) || (id >= t->num_levels) )
		return HANDLE_H5_OUT_OF_RANGE_ERR ( "Level", id );
	t->cur_level = id;
	t->last_retrieved_vertex_id = -1;
	t->last_retrieved_entity_id = -1;

	return H5_SUCCESS;
}

h5_id_t
h5t_get_level (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;
	return t->cur_level;
}

h5_id_t
h5t_add_level (
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

	t->num_entities = realloc ( t->num_entities, num_bytes );
	t->num_entities[t->cur_level] = -1;
	t->num_entities_on_level = realloc ( t->num_entities_on_level, num_bytes );
	t->num_entities_on_level[t->cur_level] = -1;

	t->new_level = t->cur_level;
	if ( t->cur_level == 0 ) {
		/* nothing stored yet */
		t->last_stored_vertex_id = -1;
		t->last_stored_entity_id = -1;
	}
	return t->cur_level;
}

h5_err_t
h5t_add_num_vertices (
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

	return H5_SUCCESS;
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

 	if ( t->mesh_gid < 0 ) {
		h5err = _open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	ssize_t num_bytes = t->num_levels*sizeof ( t->num_vertices[0] );
	h5_debug ( "Allocating %ld bytes.", num_bytes ); 
	t->num_vertices = realloc ( t->num_vertices, num_bytes );
	if ( t->num_vertices == NULL )
		return HANDLE_H5_NOMEM_ERR;
	h5err = _read_dataset (
		f,
		t->mesh_gid,
		"NumVertices",
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

 	if ( t->mesh_gid < 0 ) {
		h5err = _open_mesh_group ( f );
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
		t->mesh_gid,
		"Vertices",
		t->vertex_tid,
		_open_mem_space_vertices,
		_open_file_space_vertices,
		t->vertices );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}

h5_size_t
h5t_get_num_vertices (
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
h5t_store_vertex (
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

h5_err_t
h5t_start_traverse_vertices (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

	t->last_retrieved_vertex_id = -1;
	return H5_SUCCESS;
}

h5_id_t
h5t_traverse_vertices (
	h5_file * f,			/*!< file handle		*/
	h5_id_t * const id,		/*!< OUT: global vertex id	*/
	h5_float64_t P[3]		/*!< OUT: coordinates		*/
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->vertices == NULL ) {
		h5_err_t h5err = _read_vertices ( f );
		if ( h5err < 0 ) return h5err;
	}
	if ( t->last_retrieved_vertex_id+1 >= t->num_vertices[t->cur_level] ) {
		h5_debug ( "Traversing done!" );
		return 0;
	}
	h5_vertex *vertex = &t->vertices[++t->last_retrieved_vertex_id];
	*id = vertex->id;
	memcpy ( P, &vertex->P, sizeof ( vertex->P ) );

	return t->last_retrieved_vertex_id;
}

h5_err_t
h5t_add_num_entities (
	h5_file * f,
	const h5_size_t num
	) {
	struct h5t_fdata *t = &f->t;
	ssize_t num_bytes = 0;
	ssize_t num_entities = t->cur_level > 0 ?
			    num + t->num_entities[t->cur_level-1] : num;
	t->num_entities[t->cur_level] = num_entities;

	t->num_entities_on_level[t->cur_level] = t->cur_level > 0 ?
			    num + t->num_entities_on_level[t->cur_level-1] : num;

	switch ( t->mesh_type ) {
	case TETRAHEDRAL_MESH:
		num_bytes = num_entities*sizeof ( t->entities.tets[0] );
		break;
	case TRIANGLE_MESH:
		num_bytes = num_entities*sizeof ( t->entities.tris[0] );
		break;
	default:
		return -1;
	}
	h5_debug ( "Allocating %ld bytes.", num_bytes ); 
	t->entities.data = realloc ( t->entities.data, num_bytes );
	if ( t->entities.data == NULL ) {
		return H5_ERR_NOMEM;
	}

	t->map_entity_g2l = realloc (
		t->map_entity_g2l,
		num_entities*sizeof ( t->map_entity_g2l[0] ) );

	return H5_SUCCESS;
}


h5_id_t
h5t_store_tet (
	h5_file * f,
	const h5_id_t id,		/*!< global tetrahedron id	*/
	const h5_id_t parent_id,	/*!< global parent id
					     if level \c >0 else \c -1	*/
	const h5_id_t vertex_ids[4]	/*!< tuple with vertex id's	*/
	) {

	struct h5t_fdata *t = &f->t;

	/*
	  more than allocated
	*/
	if ( t->last_stored_entity_id+1 >= t->num_entities[t->cur_level] ) 
		return HANDLE_H5_OVERFLOW_ERR(  "tet", t->num_entities[t->cur_level] );

	/*
	  missing call to add the first level
	 */
	if ( t->cur_level < 0 )
		return HANDLE_H5_UNDEF_LEVEL_ERR;

	/*
	  check parent id
	*/
	if ( (t->cur_level == 0) && (parent_id != -1) ) {
		return HANDLE_H5_PARENT_ID_ERR ( "tet", id, parent_id );
	} 
	if ( (t->cur_level >  0) && (parent_id < 0) ) {
		return HANDLE_H5_PARENT_ID_ERR ( "tet", id, parent_id );
	}
	if ( (t->cur_level >  0) && (parent_id >= t->num_entities[t->cur_level-1]) ) {
		return HANDLE_H5_PARENT_ID_ERR ( "tet", id, parent_id );
	}
	/*
	  check id
	*/
	if ( (t->cur_level == 0) && (
		     (id < 0) || (id >= t->num_entities[0]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( "tet", id );
	}
	if ( (t->cur_level > 0) && (
		     (id <  t->num_entities[t->cur_level-1]) ||
		     (id >= t->num_entities[t->cur_level]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( "tet", id );
	}
	

	h5_tetrahedron *tet = &t->entities.tets[++t->last_stored_entity_id];
	tet->id = id;
	tet->parent_id = parent_id;
	tet->refined_on_level = -1;
	tet->unused = 0;
	memcpy ( &tet->vertex_ids, vertex_ids, sizeof ( tet->vertex_ids ) );

	t->map_entity_g2l[id] = t->last_stored_entity_id;
	if ( parent_id >= 0 ) {
		h5_id_t local_parent_id = t->map_entity_g2l[parent_id];
		if ( t->entities.tets[local_parent_id].refined_on_level < 0 ) {
			t->entities.tets[local_parent_id].refined_on_level = t->cur_level;
			t->num_entities_on_level[t->cur_level]--;
		}
	}
	return t->last_stored_entity_id;
}


h5_id_t
h5t_store_triangle (
	h5_file * f,
	const h5_id_t id,		/*!< global triangle id		*/
	const h5_id_t parent_id,	/*!< global parent id
					     if level \c >0 else \c -1	*/
	const h5_id_t vertex_ids[3]	/*!< tuple with vertex id's	*/
	) {

	struct h5t_fdata *t = &f->t;

	/*
	  more than allocated
	*/
	if ( t->last_stored_entity_id+1 >= t->num_entities[t->cur_level] ) 
		return HANDLE_H5_OVERFLOW_ERR(  "triangle", t->num_entities[t->cur_level] );

	/*
	  missing call to add the first level
	 */
	if ( t->cur_level < 0 )
		return HANDLE_H5_UNDEF_LEVEL_ERR;

	/*
	  check parent id
	*/
	if ( (t->cur_level == 0) && (parent_id != -1) ) {
		return HANDLE_H5_PARENT_ID_ERR ( "triangle", id, parent_id );
	} 
	if ( (t->cur_level >  0) && (parent_id < 0) ) {
		return HANDLE_H5_PARENT_ID_ERR ( "triangle", id, parent_id );
	}
	if ( (t->cur_level >  0) && (parent_id >= t->num_entities[t->cur_level-1]) ) {
		return HANDLE_H5_PARENT_ID_ERR ( "triangle", id, parent_id );
	}
	/*
	  check id
	*/
	if ( (t->cur_level == 0) && (
		     (id < 0) || (id >= t->num_entities[0]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( "triangle", id );
	}
	if ( (t->cur_level > 0) && (
		     (id <  t->num_entities[t->cur_level-1]) ||
		     (id >= t->num_entities[t->cur_level]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( "triangle", id );
	}
	

	h5_triangle *tri = &t->entities.tris[++t->last_stored_entity_id];
	tri->id = id;
	tri->parent_id = parent_id;
	tri->refined_on_level = -1;
	memcpy ( &tri->vertex_ids, vertex_ids, sizeof ( tri->vertex_ids ) );

	t->map_entity_g2l[id] = t->last_stored_entity_id;
	if ( parent_id >= 0 ) {
		h5_id_t local_parent_id = t->map_entity_g2l[parent_id];
		if ( t->entities.tris[local_parent_id].refined_on_level < 0 ) {
			t->entities.tris[local_parent_id].refined_on_level = t->cur_level;
			t->num_entities_on_level[t->cur_level]--;
		}
	}
	return t->last_stored_entity_id;
}

static h5_err_t
_read_num_entities (
	h5_file * f
	) {


	h5_err_t h5err;
	struct h5t_fdata *t = &f->t;

	if ( t->cur_level < 0 ) 
		return HANDLE_H5_UNDEF_LEVEL_ERR;

 	if ( t->mesh_gid < 0 ) {
		h5err = _open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	ssize_t num_bytes = t->num_levels*sizeof ( t->num_entities[0] );
	h5_debug ( "Allocating %ld bytes.", num_bytes ); 
	t->num_entities = realloc ( t->num_entities, num_bytes );
	if ( t->num_entities == NULL )
		return HANDLE_H5_NOMEM_ERR;
	h5err = _read_dataset (
		f,
		t->mesh_gid,
		"NumEntities",
		H5T_NATIVE_INT32,
		_open_space_all,
		_open_space_all,
		t->num_entities );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}

static h5_err_t
_read_num_entities_on_level (
	h5_file * f
	) {

	h5_err_t h5err;
	struct h5t_fdata *t = &f->t;

	if ( t->cur_level < 0 ) 
		return HANDLE_H5_UNDEF_LEVEL_ERR;

 	if ( t->mesh_gid < 0 ) {
		h5err = _open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	ssize_t num_bytes = t->num_levels*sizeof ( t->num_entities_on_level[0] );
	h5_debug ( "Allocating %ld bytes.", num_bytes ); 
	t->num_entities_on_level = realloc ( t->num_entities_on_level, num_bytes );
	if ( t->num_entities_on_level == NULL )
		return HANDLE_H5_NOMEM_ERR;
	h5err = _read_dataset (
		f,
		t->mesh_gid,
		"NumEntitiesOnLevel",
		H5T_NATIVE_INT32,
		_open_space_all,
		_open_space_all,
		t->num_entities_on_level );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}

static hid_t
_open_mem_space_entities (
	h5_file * f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

static hid_t
_open_file_space_entities (
	h5_file * f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

static h5_err_t
_read_entities (
	h5_file * f
	) {
	h5_err_t h5err;
	ssize_t num_bytes;
	struct h5t_fdata *t = &f->t;

	if ( t->cur_level < 0 ) 
		return HANDLE_H5_UNDEF_LEVEL_ERR;

 	if ( t->mesh_gid < 0 ) {
		h5err = _open_mesh_group ( f );
		if ( h5err < 0 ) return h5err;
	}

	if ( t->num_entities == NULL ) {
		h5err = _read_num_entities ( f );
		if ( h5err < 0 ) return h5err;
	}

	ssize_t num_elems = t->num_entities[t->num_levels-1];

	switch ( t->mesh_type ) {
	case TETRAHEDRAL_MESH: {
		num_bytes = num_elems*sizeof ( t->entities.tets[0] );
		break;
	}
	case TRIANGLE_MESH: {
		num_bytes = num_elems*sizeof ( t->entities.tris[0] );
		break;
	}
	default:
		return -1;
	}
	h5_debug ( "Allocating %ld bytes.", num_bytes ); 
	t->entities.data = realloc ( t->entities.data, num_bytes );
	if ( t->entities.data == NULL )
		return HANDLE_H5_NOMEM_ERR;
	h5err = _read_dataset (
		f,
		t->mesh_gid,
		"Entities",
		t->entity_tid,
		_open_mem_space_entities,
		_open_file_space_entities,
		t->entities.data );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}

h5_size_t
h5t_get_num_entities (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->cur_mesh < 0 ) {
		return HANDLE_H5_UNDEF_MESH_ERR;
	}
	if ( t->cur_level < 0 ) {
		return HANDLE_H5_UNDEF_LEVEL_ERR;
	}
	if ( t->num_entities_on_level == NULL ) {
		h5_err_t h5err = _read_num_entities_on_level ( f );
		if ( h5err < 0 ) return h5err;
	}
	return t->num_entities_on_level[t->cur_level];
}

h5_err_t
h5t_start_traverse_tets (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

	switch ( t->mesh_type ) {
	case TRIANGLE_MESH: {
		return H5_ERR_INVAL;
	}
	case TETRAHEDRAL_MESH: {
		t->last_retrieved_entity_id = -1;
		return H5_SUCCESS;
	}
	default:
		return H5_ERR_INTERNAL;
	}
}

h5_id_t
_traverse_tets (
	h5_file * f,
	h5_id_t * const id,		/*!< OUT: global tetrahedron id	*/
	h5_id_t * const parent_id,	/*!< OUT: global parent id
					     if level \c >0 else \c -1	*/
	h5_id_t ids[4]			/*!< OUT: tuple with vertex id's */
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->entities.data == NULL ) {
		h5_err_t h5err = _read_entities ( f );
		if ( h5err < 0 ) return h5err;
	}
	if ( t->last_retrieved_entity_id+1 >= t->num_entities[t->cur_level] ) {
		h5_debug ( "Traversing done!" );
		return 0;
	}
	h5_tetrahedron *tet = &t->entities.tets[++t->last_retrieved_entity_id];

	while ( (tet->refined_on_level != -1) &&
		(tet->refined_on_level <= t->cur_level) ){
		tet++;
		t->last_retrieved_entity_id++;
		if ( t->last_retrieved_entity_id >= t->num_entities[t->cur_level] ) {
			return HANDLE_H5_INTERNAL_ERR;
		}
	}

	*id = tet->id;
	*parent_id = tet->parent_id;
	memcpy ( ids, &tet->vertex_ids, sizeof ( tet->vertex_ids ) );

	return t->last_retrieved_entity_id;
}

h5_id_t
h5t_traverse_tets (
	h5_file * f,
	h5_id_t * const id,		/*!< OUT: global tetrahedron id	*/
	h5_id_t * const parent_id,	/*!< OUT: global parent id
					     if level \c >0 else \c -1	*/
	h5_id_t ids[4]			/*!< OUT: tuple with vertex id's */
	) {
	struct h5t_fdata *t = &f->t;

	switch ( t->mesh_type ) {
	case TRIANGLE_MESH: {
		return H5_ERR_INVAL;
	}
	case TETRAHEDRAL_MESH: {
		return _traverse_tets ( f, id, parent_id, ids );
	}
	default:
		return H5_ERR_INTERNAL;
	}
}


h5_err_t
h5t_start_traverse_triangles (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

	switch ( t->mesh_type ) {
	case TRIANGLE_MESH: {
		t->last_retrieved_entity_id = -1;
		return H5_SUCCESS;
	}
	case TETRAHEDRAL_MESH: {
		return H5_ERR_NOT_IMPLEMENTED;
	}
	default:
		return H5_ERR_INTERNAL;
	}
}


static h5_id_t
_traverse_triangles (
	h5_file * f,
	h5_id_t * const id,		/*!< OUT: global triangle id	*/
	h5_id_t * const parent_id,	/*!< OUT: global parent id
					     if level \c >0 else \c -1	*/
	h5_id_t ids[3]			/*!< OUT: tuple with vertex id's */
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->entities.data == NULL ) {
		h5_err_t h5err = _read_entities ( f );
		if ( h5err < 0 ) return h5err;
	}
	if ( t->last_retrieved_entity_id+1 >= t->num_entities[t->cur_level] ) {
		h5_debug ( "Traversing done!" );
		return 0;
	}
	h5_triangle *tri = &t->entities.tris[++t->last_retrieved_entity_id];

	while ( (tri->refined_on_level != -1) &&
		(tri->refined_on_level <= t->cur_level) ){
		tri++;
		t->last_retrieved_entity_id++;
		if ( t->last_retrieved_entity_id >= t->num_entities[t->cur_level] ) {
			return HANDLE_H5_INTERNAL_ERR;
		}
	}

	*id = tri->id;
	*parent_id = tri->parent_id;
	memcpy ( ids, &tri->vertex_ids, sizeof ( tri->vertex_ids ) );

	return t->last_retrieved_entity_id;
}

h5_id_t
h5t_traverse_triangles (
	h5_file * f,
	h5_id_t * const id,		/*!< OUT: global triangle id	*/
	h5_id_t * const parent_id,	/*!< OUT: global parent id
					     if level \c >0 else \c -1	*/
	h5_id_t ids[3]			/*!< OUT: tuple with vertex id's */
	) {

	struct h5t_fdata *t = &f->t;

	switch ( t->mesh_type ) {
	case TRIANGLE_MESH: {
		return _traverse_triangles ( f, id, parent_id, ids );
	}
	case TETRAHEDRAL_MESH: {
		return H5_ERR_NOT_IMPLEMENTED;
	}
	default:
		return H5_ERR_INTERNAL;
	}
}
