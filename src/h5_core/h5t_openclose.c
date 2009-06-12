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
	h5t_fdata_t *t = f->t;
	h5_dtypes_t *dtypes = &(t->dtypes);

	hsize_t dims[1] = { 3 };
	TRY(
		dtypes->h5_coord3d_t = _h5_create_array_type (
			f,
			H5_FLOAT64_T,
			1,
			dims )
		);
	TRY( 
		dtypes->h5_3id_t = _h5_create_array_type (
			f,
			H5_ID_T,
			1,
			dims )
		);
	dims[0] = 4;
	TRY(
		dtypes->h5_4id_t = _h5_create_array_type (
			f,
			H5_ID_T,
			1,
			dims )
		);

	return H5_SUCCESS;
}

static h5_err_t
_create_vertex_type (
	h5_file_t * f
	) {
	h5_dtypes_t *dtypes = &f->t->dtypes;

	TRY(
		dtypes->h5_vertex_t = _h5_create_type (
			f,
			H5_COMPOUND_T,
			sizeof(struct h5_vertex) ) );
	TRY(
		_h5_insert_type (
			f,
			dtypes->h5_vertex_t,
			"global_vid",
			HOFFSET(struct h5_vertex, global_vid),
			H5_ID_T ) );
	TRY(
		_h5_insert_type (
			f,
			dtypes->h5_vertex_t,
			"P",
			HOFFSET(struct h5_vertex, P),
			dtypes->h5_coord3d_t ) );

	return H5_SUCCESS;
}

static h5_err_t
_create_triangle_type (
	h5_file_t * f
	) {
	h5_dtypes_t *dtypes = &f->t->dtypes;

	TRY(
		dtypes->h5_triangle_t = _h5_create_type (
			f,
			H5_COMPOUND_T,
			sizeof(struct h5_triangle) ) );
	TRY(
		_h5_insert_type (
			f,
			dtypes->h5_triangle_t,
			"global_eid",
			HOFFSET(struct h5_triangle, global_eid),
			H5_ID_T ) );
	TRY(
		_h5_insert_type (
			f,
			dtypes->h5_triangle_t,
			"global_parent_eid",
			HOFFSET(struct h5_triangle, global_parent_eid),
			H5_ID_T ) );
	TRY(
		_h5_insert_type (
			f,
			dtypes->h5_triangle_t,
			"global_child_eid",
			HOFFSET(struct h5_triangle, global_child_eid),
			H5_ID_T ) );
	TRY(
		_h5_insert_type (
			f,
			dtypes->h5_triangle_t,
			"global_vids",
			HOFFSET(struct h5_triangle, global_vids),
			dtypes->h5_3id_t ) );

	return H5_SUCCESS;
}

static h5_err_t
_create_tet_type (
	h5_file_t * f
	) {
	h5_dtypes_t *dtypes = &f->t->dtypes;

	TRY(
		dtypes->h5_tet_t = _h5_create_type (
			f,
			H5_COMPOUND_T,
			sizeof(struct h5_tetrahedron) ) );
	TRY(
		_h5_insert_type (
			f,
			dtypes->h5_tet_t,
			"global_eid",
			HOFFSET(struct h5_tetrahedron, global_eid),
			H5_ID_T ) );
	TRY(
		_h5_insert_type (
			f,
			dtypes->h5_tet_t,
			"global_parent_eid",
			HOFFSET(struct h5_tetrahedron, global_parent_eid),
			H5_ID_T ) );
	TRY(
		_h5_insert_type (
			f,
			dtypes->h5_tet_t,
			"global_child_eid",
			HOFFSET(struct h5_tetrahedron, global_child_eid),
			H5T_NATIVE_INT32 ) );
	TRY(
		_h5_insert_type (
			f,
			dtypes->h5_tet_t,
			"global_vids",
			HOFFSET(struct h5_tetrahedron, global_vids),
			dtypes->h5_4id_t ) );

	return H5_SUCCESS;
}

#if 0
h5_err_t
_h5_set_dataset_properties (
	h5_dataset_info_t *dsinfo,
	const char *name,
	const hid_t type,
	const int rank,
	const hsize_t *dims,
	const hsize_t *maxdims,
	const hsize_t chunk_size
	) {

	return H5_SUCCESS;
}
#endif

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
	t->last_stored_vid = -1;
	t->last_stored_eid = -1;
	t->topo_gid = -1;
	t->meshes_gid = -1;
	t->mesh_gid = -1;
	t->num_boundaries = -1;

	/* vertices */
	strcpy( t->dsinfo_vertices.name, "Vertices" );
	t->dsinfo_vertices.rank = 1;
	t->dsinfo_vertices.dims[0] = 0;
	t->dsinfo_vertices.maxdims[0] = H5S_UNLIMITED;
	t->dsinfo_vertices.chunk_size[0] = 4096;
	t->dsinfo_vertices.type_id = &t->dtypes.h5_vertex_t;
	TRY( t->dsinfo_vertices.create_prop = _h5_create_property (
		     f,
		     H5P_DATASET_CREATE ) );
	TRY( _h5_set_chunk_property (
		     f,
		     t->dsinfo_vertices.create_prop,
		     t->dsinfo_vertices.rank,
		     t->dsinfo_vertices.chunk_size ) );
	t->dsinfo_vertices.access_prop = H5P_DEFAULT;

	/* NumVertices */
	strcpy( t->dsinfo_num_vertices.name, "NumVertices" );
	t->dsinfo_num_vertices.rank = 1;
	t->dsinfo_num_vertices.dims[0] = 0;
	t->dsinfo_num_vertices.maxdims[0] = H5S_UNLIMITED;
	t->dsinfo_num_vertices.chunk_size[0] = 4096;
	t->dsinfo_num_vertices.type_id = &t->dtypes.h5_id_t;
	TRY( t->dsinfo_num_vertices.create_prop = _h5_create_property (
		     f,
		     H5P_DATASET_CREATE ) );
	TRY( _h5_set_chunk_property (
		     f,
		     t->dsinfo_num_vertices.create_prop,
		     t->dsinfo_num_vertices.rank,
		     t->dsinfo_num_vertices.chunk_size ) );
	t->dsinfo_num_vertices.access_prop = H5P_DEFAULT;

	/* Elems */
	strcpy( t->dsinfo_elems.name, "Elems" );
	t->dsinfo_elems.rank = 1;
	t->dsinfo_elems.dims[0] = 0;
	t->dsinfo_elems.maxdims[0] = H5S_UNLIMITED;
	t->dsinfo_elems.chunk_size[0] = 4096;
	t->dsinfo_elems.type_id = &t->elem_tid;
	TRY( t->dsinfo_elems.create_prop = _h5_create_property (
		     f,
		     H5P_DATASET_CREATE ) );
	TRY( _h5_set_chunk_property (
		     f,
		     t->dsinfo_elems.create_prop,
		     t->dsinfo_elems.rank,
		     t->dsinfo_elems.chunk_size ) );
	t->dsinfo_elems.access_prop = H5P_DEFAULT;

	/* NumElems */
	strcpy( t->dsinfo_num_elems.name, "NumElems" );
	t->dsinfo_num_elems.rank = 1;
	t->dsinfo_num_elems.dims[0] = 0;
	t->dsinfo_num_elems.maxdims[0] = H5S_UNLIMITED;
	t->dsinfo_num_elems.chunk_size[0] = 4096;
	t->dsinfo_num_elems.type_id = &t->dtypes.h5_id_t;
	TRY( t->dsinfo_num_elems.create_prop = _h5_create_property (
		     f,
		     H5P_DATASET_CREATE ) );
	TRY( _h5_set_chunk_property (
		     f,
		     t->dsinfo_num_elems.create_prop,
		     t->dsinfo_num_elems.rank,
		     t->dsinfo_num_elems.chunk_size ) );
	t->dsinfo_num_elems.access_prop = H5P_DEFAULT;

	/* NumElemsOnLevel */
	strcpy( t->dsinfo_num_elems_on_level.name, "NumElemsOnLevel" );
	t->dsinfo_num_elems_on_level.rank = 1;
	t->dsinfo_num_elems_on_level.dims[0] = 0;
	t->dsinfo_num_elems_on_level.maxdims[0] = H5S_UNLIMITED;
	t->dsinfo_num_elems_on_level.chunk_size[0] = 4096;
	t->dsinfo_num_elems_on_level.type_id = &t->dtypes.h5_id_t;
	TRY( t->dsinfo_num_elems_on_level.create_prop = _h5_create_property (
		     f,
		     H5P_DATASET_CREATE ) );
	TRY( _h5_set_chunk_property (
		     f,
		     t->dsinfo_num_elems_on_level.create_prop,
		     t->dsinfo_num_elems_on_level.rank,
		     t->dsinfo_num_elems_on_level.chunk_size ) );
	t->dsinfo_num_elems_on_level.access_prop = H5P_DEFAULT;

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

	TRY( (f->t = _h5_alloc ( f, NULL, sizeof(*f->t) ) ) );
	h5t_fdata_t *t = f->t;

	t->dtypes.h5_id_t = H5_INT64_T;
	t->dtypes.h5_float64_t = H5_FLOAT64_T;

	TRY( _create_array_types ( f ) );
	TRY( _create_vertex_type ( f ) );
	TRY( _create_triangle_type ( f ) );
	TRY( _create_tet_type ( f ) );
	TRY( _init_fdata ( f ) );

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

	TRY( _h5t_close_mesh ( f ) );

	if ( t->num_meshes < 0 ) {
		h5_size_t result = h5t_get_num_meshes ( f, type );
		t->num_meshes = ( result > 0 ? result : 0 );
	}
	if ( (id < -1) || (id >= t->num_meshes) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( f, "mesh", id );
	}
	if ( id == -1 ) {  /* append new mesh */
		id = t->num_meshes;
	}
	t->mesh_type = type;
	snprintf ( t->mesh_name, sizeof (t->mesh_name), "%lld", id );

	switch( type ) {
	case H5_OID_TETRAHEDRON:
		t->elem_tid = t->dtypes.h5_tet_t;
		break;
	case H5_OID_TRIANGLE:
		t->elem_tid = t->dtypes.h5_triangle_t;
		break;
	default:
		return h5_error_internal (
			f, __FILE__, __func__, __LINE__ );
	}

	TRY( _h5t_open_mesh_group ( f ) );

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

	_h5_free ( f, t->vertices );
	t->vertices = NULL;
	_h5_free ( f, t->vertices_data );
	t->vertices_data = NULL;
	_h5_free ( f, t->num_vertices );
	t->num_vertices = NULL;
	_h5_free ( f, t->map_vertex_g2l.items );

	_h5_free ( f, t->elems.data );
	t->elems.data = NULL;
	_h5_free ( f, t->num_elems );
	_h5_free ( f, t->elems_data.data );
	t->elems_data.data = NULL;
	_h5_free ( f, t->num_elems_on_level );
	t->num_elems_on_level = NULL;
	_h5_free ( f, t->map_elem_g2l.items );
	t->map_elem_g2l.items = NULL;

	return H5_SUCCESS;
}

h5_err_t
_h5t_close_mesh (
	h5_file_t * const f		/*!< file handle */
	) {
	TRY( _h5t_write_mesh ( f ) );
	TRY( _close_hdf5_objs ( f ) );
	TRY( _release_memory ( f ) );
	TRY( h5t_close_boundary ( f ) );
	TRY( _init_fdata ( f ) );

	return H5_SUCCESS;
}

h5_err_t
h5t_open_level (
	h5_file_t * const f,
	const h5_id_t id
	) {
	struct h5t_fdata *t = f->t;

	if ( (id < 0) || (id >= t->num_levels) )
		return HANDLE_H5_OUT_OF_RANGE_ERR ( f, "Level", id );
	t->cur_level = id;

	return H5_SUCCESS;
}

herr_t
_h5_set_chunk_property (
	h5_file_t * const f,
	hid_t plist,
	int ndims,
	const hsize_t * dim
	);
