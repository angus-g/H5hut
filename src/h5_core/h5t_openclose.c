#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

/*
  create several HDF5 types
*/
static h5_err_t
_create_array_types (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;

	hsize_t dims[1] = { 3 };
	hid_t hid = H5Tarray_create ( H5T_NATIVE_DOUBLE, 1, dims );
	if ( hid < 0 ) {
		return HANDLE_H5T_ARRAY_CREATE_ERR ( "H5T_NATIVE_DOUBLE", 1 );
	}
	t->float64_3tuple_tid = hid;

	dims[0] = 2;
	hid = H5Tarray_create ( H5T_NATIVE_INT32, 1, dims );
	if ( hid < 0 ) {
		return HANDLE_H5T_ARRAY_CREATE_ERR ( "H5T_NATIVE_INT32", 1 );
	}
	t->int32_2tuple_tid = hid;

	dims[0] = 3;
	hid = H5Tarray_create ( H5T_NATIVE_INT32, 1, dims );
	if ( hid < 0 ) {
		return HANDLE_H5T_ARRAY_CREATE_ERR ( "H5T_NATIVE_INT32", 1 );
	}
	t->int32_3tuple_tid = hid;

	dims[0] = 4;
	hid = H5Tarray_create ( H5T_NATIVE_INT32, 1, dims );
	if ( hid < 0 ) {
		return HANDLE_H5T_ARRAY_CREATE_ERR ( "H5T_NATIVE_INT32", 1 );
	}
	t->int32_4tuple_tid = hid;

	return H5_SUCCESS;
}

static h5_err_t
_create_vertex_type (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;

	hid_t hid = H5Tcreate ( H5T_COMPOUND, sizeof(struct h5_vertex) );
	if ( hid < 0 ) {
		return HANDLE_H5T_CREATE_ERR ( "H5T_COMPOUND", "verticies" );
	}
	t->vertex_tid = hid;

	herr_t herr = H5Tinsert (
		t->vertex_tid,
		"id",
		HOFFSET(struct h5_vertex, id),
		H5T_NATIVE_INT32 );
	if ( herr < 0 ) {
		return HANDLE_H5T_INSERT_ERR ( "id", "verticies" );
	}

	herr = H5Tinsert (
		t->vertex_tid,
		"unused",
		HOFFSET(struct h5_vertex, unused),
		H5T_NATIVE_INT32 );
	if ( herr < 0 ) {
		return HANDLE_H5T_INSERT_ERR ( "unused", "verticies" );
	}

	herr = H5Tinsert (
		t->vertex_tid,
		"P",
		HOFFSET(struct h5_vertex, P),
		t->float64_3tuple_tid );
	if ( herr < 0 ) {
		return HANDLE_H5T_INSERT_ERR ( "P", "verticies" );
	}

	return H5_SUCCESS;
}

static h5_err_t
_create_triangle_type (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;

	hid_t hid = H5Tcreate ( H5T_COMPOUND, sizeof(struct h5_triangle) );
	if ( hid < 0 ) {
		return HANDLE_H5T_CREATE_ERR ( "H5T_COMPOUND", "triangle" );
	}
	t->triangle_tid = hid;

	herr_t herr = H5Tinsert (
		t->triangle_tid,
		"id",
		HOFFSET(struct h5_triangle, id),
		H5T_NATIVE_INT32 );
	if ( herr < 0 ) {
		return HANDLE_H5T_INSERT_ERR ( "id", "triangle" );
	}

	herr = H5Tinsert (
		t->triangle_tid,
		"parent_id",
		HOFFSET(struct h5_triangle, parent_id),
		H5T_NATIVE_INT32 );
	if ( herr < 0 ) {
		return HANDLE_H5T_INSERT_ERR ( "parent_id", "triangle" );
	}

	herr = H5Tinsert (
		t->triangle_tid,
		"refined_on_level",
		HOFFSET(struct h5_triangle, refined_on_level),
		H5T_NATIVE_INT32 );
	if ( herr < 0 ) {
		return HANDLE_H5T_INSERT_ERR ( "refined_on_level", "triangle" );
	}

	herr = H5Tinsert (
		t->triangle_tid,
		"vertex_ids",
		HOFFSET(struct h5_triangle, vertex_ids),
		t->int32_3tuple_tid );
	if ( herr < 0 ) {
		return HANDLE_H5T_INSERT_ERR ( "vertex_ids", "triangle" );
	}

	return H5_SUCCESS;
}

static h5_err_t
_create_tet_type (
	h5_file_t * f
	) {
	struct h5t_fdata *t = f->t;

	hid_t hid = H5Tcreate ( H5T_COMPOUND, sizeof(struct h5_tetrahedron) );
	if ( hid < 0 ) {
		return HANDLE_H5T_CREATE_ERR ( "H5T_COMPOUND", "tetrahedra" );
	}
	t->tet_tid = hid;

	herr_t herr = H5Tinsert (
		t->tet_tid,
		"id",
		HOFFSET(struct h5_tetrahedron, id),
		H5T_NATIVE_INT32 );
	if ( herr < 0 ) {
		return HANDLE_H5T_INSERT_ERR ( "id", "tetrahedra" );
	}

	herr = H5Tinsert (
		t->tet_tid,
		"parent_id",
		HOFFSET(struct h5_tetrahedron, parent_id),
		H5T_NATIVE_INT32 );
	if ( herr < 0 ) {
		return HANDLE_H5T_INSERT_ERR ( "parent_id", "tetrahedra" );
	}

	herr = H5Tinsert (
		t->tet_tid,
		"refined_on_level",
		HOFFSET(struct h5_tetrahedron, refined_on_level),
		H5T_NATIVE_INT32 );
	if ( herr < 0 ) {
		return HANDLE_H5T_INSERT_ERR ( "refined_on_level", "tetrahedra" );
	}

	herr = H5Tinsert (
		t->tet_tid,
		"unused",
		HOFFSET(struct h5_tetrahedron, unused),
		H5T_NATIVE_INT32 );
	if ( herr < 0 ) {
		return HANDLE_H5T_INSERT_ERR ( "unused", "tetrahedra" );
	}

	herr = H5Tinsert (
		t->tet_tid,
		"vertex_ids",
		HOFFSET(struct h5_tetrahedron, vertex_ids),
		t->int32_4tuple_tid );
	if ( herr < 0 ) {
		return HANDLE_H5T_INSERT_ERR ( "vertex_ids", "tetrahedra" );
	}

	return H5_SUCCESS;
}

static h5_err_t
_init_fdata (
	h5_file_t * f
	) {
	struct h5t_fdata * t = f->t;

	memset ( t->mesh_name, 0, sizeof ( t->mesh_name ) );
	t->num_meshes = -1;
	t->cur_mesh = -1;
	t->num_levels = -1;
	t->new_level = -1;
	t->cur_level = -1;
	t->last_stored_vertex_id = -1;
	t->last_stored_entity_id = -1;
	t->topo_gid = -1;
	t->meshes_gid = -1;
	t->mesh_gid = -1;

	t->num_boundaries = -1;
	
	return H5_SUCCESS;
}

/*!
  \ingroup h5_private

  \internal

  Initialize topo internal structure. The structure has already be initialized
  with zero's.

  \return	H5_SUCCESS or error code
*/
h5_err_t
_h5t_open_file (
	h5_file_t * f			/*!< IN: file handle */
	) {
	TRY( f->t = _h5_alloc ( NULL, sizeof(*f->t) ) );
	TRY( _init_fdata ( f ) );
	TRY( _create_array_types ( f ) );
	TRY( _create_vertex_type ( f ) );
	TRY( _create_triangle_type ( f ) );
	TRY( _create_tet_type ( f ) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5_private

  \internal

  De-initialize topological internal structure.  Open HDF5 objects are 
  closed and allocated memory freed.

  \return	H5_SUCCESS or error code
*/
h5_err_t
_h5t_close_file (
	h5_file_t *f		/*!< IN: file handle */
	) {

	h5_err_t h5err = H5_SUCCESS;

	h5err = _h5t_close_mesh ( f );

	return h5err;
}

h5_err_t
_h5t_init_step (
	h5_file_t * const f
	) {

	return H5_SUCCESS;
}

/*
 - write data
 - close HDF5 objects we cannot reuse
 - free memory
*/
h5_err_t
_h5t_close_step (
	h5_file_t * f
	) {

	return H5_SUCCESS;
}


h5_err_t
_h5t_open_topo_group (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;

	t->topo_gid = _h5_open_group ( f, f->root_gid, H5T_CONTAINER_GRPNAME );
	return t->topo_gid;
}

h5_err_t
_h5t_open_meshes_group (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;

	if ( t->topo_gid < 0 ) {
		h5_err_t h5err = _h5t_open_topo_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	switch ( t->mesh_type) {
	case H5_OID_TETRAHEDRON:
		t->meshes_gid = _h5_open_group ( f, t->topo_gid, "TetMeshes" );
		break;
	case H5_OID_TRIANGLE:
		t->meshes_gid = _h5_open_group ( f, t->topo_gid, "TriangleMeshes" );
		break;
	default:
		t->meshes_gid = -1;
	}
	return (h5_err_t)t->meshes_gid;
}

h5_err_t
_h5t_open_mesh_group (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;

	if ( t->meshes_gid < 0 ) {
		h5_err_t h5err = _h5t_open_meshes_group ( f );
		if ( h5err < 0 ) return h5err;
	}
	t->mesh_gid = _h5_open_group ( f, t->meshes_gid, t->mesh_name );
	return (h5_err_t)t->mesh_gid;
}

/*
  If the value of parameter \c id is \c -1, a new mesh will be appended.
*/
h5_err_t
h5t_open_mesh (
	h5_file_t * const f,
	h5_id_t id,
	const h5_oid_t type
	) {
	struct h5t_fdata *t = f->t;

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
	case H5_OID_TETRAHEDRON:
		t->entity_tid = t->tet_tid;
		break;
	case H5_OID_TRIANGLE:
		t->entity_tid = t->triangle_tid;
		break;
	default:
		return -1;
	}

	h5err = _h5t_open_mesh_group ( f );
	if ( h5err < 0 ) return h5err;

	t->cur_mesh = id;

	if ( id != t->num_meshes ) {	/* open existing */
		t->num_levels = h5t_get_num_levels ( f );
		if ( t->num_levels < 0 ) return t->num_levels;
	} else {			/* append new */
		t->num_meshes++;
		t->mesh_changed = id;
		t->num_levels = 0;
	} 

	return H5_SUCCESS;
}

static h5_err_t
_close_hdf5_objs (
	h5_file_t * const f
	) {

	
	return H5_SUCCESS;
}

static h5_err_t
_release_memory (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;

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

	if ( t->map_vertex_g2l.items ) {
		free ( t->map_vertex_g2l.items );
	}
	t->map_vertex_g2l.items = NULL;

	if ( t->map_entity_g2l.items ) {
		free ( t->map_entity_g2l.items );
	}
	t->map_entity_g2l.items = NULL;

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

h5_err_t
_h5t_close_mesh (
	h5_file_t * const f		/*!< file handle */
	) {

	h5_err_t h5err = H5_SUCCESS;
	
	h5err = _h5t_write_mesh ( f );
	if ( h5err < 0 ) return h5err;

	h5err = _close_hdf5_objs ( f );
	if ( h5err < 0 ) return h5err;

	h5err = _release_memory ( f );
	if ( h5err < 0 ) return h5err;

	h5err = h5t_close_boundary ( f );
	if ( h5err < 0 ) return h5err;

	if (( h5err = _init_fdata ( f )) < 0 ) return h5err;

	return h5err;
}

h5_err_t
h5t_open_level (
	h5_file_t * const f,
	const h5_id_t id
	) {
	struct h5t_fdata *t = f->t;

	if ( (id < 0) || (id >= t->num_levels) )
		return HANDLE_H5_OUT_OF_RANGE_ERR ( "Level", id );
	t->cur_level = id;
	t->last_retrieved_vertex_id = -1;
	t->last_retrieved_entity_id = -1;

	return H5_SUCCESS;
}
