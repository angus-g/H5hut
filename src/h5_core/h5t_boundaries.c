#include <stdlib.h>
#include <string.h>

#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

static h5_err_t
_open_boundaries_group (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;

	if ( t->cur_mesh < 0 )
		return _h5t_error_undef_mesh ( f );
	TRY( t->boundaries_gid = _h5_open_group (
		      f, t->mesh_gid, H5T_BOUNDARYMESH_GRPNAME ) );
	return H5_SUCCESS;
}

static h5_err_t
_open_boundary_group (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;
	boundary_t *boundary = &t->boundary;

	if ( t->cur_mesh < 0 )
		return _h5t_error_undef_mesh ( f );

	if ( t->boundaries_gid < 0 ) {
		TRY( _open_boundaries_group ( f ) );
	}
	boundary->gid = _h5_open_group (
		f, t->boundaries_gid, boundary->name );
	return H5_SUCCESS;
}

h5_id_t
h5t_get_num_boundaries (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;

	if ( t->cur_mesh < 0 )
		return _h5t_error_undef_mesh ( f );
	if ( t->num_boundaries < 0 ) {
		t->num_boundaries = hdf5_get_num_objects (
			t->mesh_gid,
			H5T_BOUNDARYMESH_GRPNAME, 
			H5G_GROUP );
	}
	if ( t->num_boundaries < 0 ) t->num_boundaries = 0;
	return t->num_boundaries;
}

h5_err_t
h5t_set_boundary_label (
	h5_file_t * const f,
	const char * const bname
	) {
	return h5_error_not_implemented ( f, __FILE__, __func__, __LINE__ );
}

h5_err_t
h5t_get_boundary_label (
	h5_file_t * const f,
	char * const boundary_name,
	const size_t size
	) {
	return h5_error_not_implemented ( f, __FILE__, __func__, __LINE__ );
}

h5_err_t
h5t_open_boundary (
	h5_file_t * const f,
	const h5_id_t boundary_id
	) {
	struct h5t_fdata *t = f->t;
	boundary_t *boundary = &t->boundary;

	TRY( h5t_close_boundary ( f ) );

	if ( t->num_boundaries < 0 ) {
		TRY( h5t_get_num_boundaries ( f ) );
	}
	if ( (boundary_id < -1) || (boundary_id >= t->num_boundaries) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( f, "boundary", boundary_id );
	}
	if ( boundary_id == -1 ) {  /* append new boundary */
		boundary->id = t->num_boundaries++;
		boundary->changed = boundary_id;
	} else {
		boundary->id = boundary_id;
	}
	snprintf ( boundary->name, sizeof (boundary->name),
		   "%lld", boundary->id );
	TRY( _open_boundary_group ( f ) );

	return H5_SUCCESS;
}

h5_err_t
h5t_open_boundary_with_label (
	h5_file_t * const f,
	const char * const boundary_label
	) {
	return h5_error_not_implemented ( f, __FILE__, __func__, __LINE__ );
}

h5_err_t
_h5t_read_boundaryfaces (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;
	boundary_t *boundary = &t->boundary;
	hid_t diskspace_id;

	const char * const dataset_name = "Faces";
	hid_t dataset_id;
	h5_size_t num_faces;
	TRY ( dataset_id = _h5_open_dataset ( f, boundary->gid, dataset_name ) );
	TRY ( diskspace_id = _h5_get_dataset_space ( f, dataset_id ) );
	TRY ( num_faces = _h5_get_npoints_of_space ( f, diskspace_id ) );
	TRY ( _h5_close_dataspace( f, diskspace_id ) );
	TRY ( _h5_close_dataset( f, dataset_id ) );
	TRY ( h5t_add_num_boundaryfaces ( f, num_faces ) );

	TRY ( _h5_read_dataset (
		f,
		dataset_id,
		H5T_NATIVE_INT32,
		H5S_ALL,
		H5S_ALL,
		f->xfer_prop,
		boundary->faces ) );
	TRY ( _h5_close_dataset( f, dataset_id ) );

	return H5_SUCCESS;
}

static hid_t
_open_space_all (
	h5_file_t * const f,
	hid_t dataset_id
	) {
	return H5S_ALL;
}

h5_err_t
_h5t_write_boundary (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;
	boundary_t *boundary = &t->boundary;

	return _h5_write (
		f,
		boundary->gid,
		&boundary->dsinfo,
		_open_space_all,
		_open_space_all,
		(void*)boundary->faces
		);
}

h5_err_t
h5t_close_boundary (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;
	boundary_t *boundary = &t->boundary;

	if ( boundary->changed ) {
		h5_err_t h5err = _h5t_write_boundary ( f );
		if ( h5err < 0 ) return h5err;
	}
	if ( boundary->faces ) free ( boundary->faces );
	if ( boundary->lfaces ) free ( boundary->faces );
	if ( boundary->num_faces ) free ( boundary->num_faces );
	if ( boundary->num_faces_on_level ) free ( boundary->num_faces_on_level );

	bzero ( boundary, sizeof(*boundary) );

	TRY( _h5_close_group( f, boundary->gid ) );

	boundary->id = -1;
	boundary->gid = -1;
	boundary->last_accessed_face = -1;

	return H5_SUCCESS;
}

h5_err_t
h5t_add_num_boundaryfaces (
	h5_file_t * const f,
	const h5_id_t num_faces
	) {
	struct h5t_fdata *t = f->t;
	boundary_t *b = &t->boundary;

	size_t size = t->num_levels * sizeof(b->num_faces[0]);
	TRY ( b->num_faces = _h5_alloc ( f, b->num_faces, size ) );
	memset ( b->num_faces, 0, size );

	size = t->num_levels * sizeof(b->num_faces_on_level[0]);
	TRY ( b->num_faces_on_level = _h5_alloc ( f, b->num_faces_on_level, size ) );
	memset ( b->num_faces_on_level, 0, size );

	size = num_faces * sizeof(b->faces[0]);
	TRY ( b->faces = _h5_alloc ( f, b->faces, size ) );
	memset ( b->faces, -1, size );

	size = num_faces*sizeof(b->lfaces[0]);
	TRY ( b->lfaces = _h5_alloc ( f, b->lfaces, size ) );
	memset ( b->lfaces, 0, size );

	b->num_faces[0] = num_faces;
	b->last_accessed_face = -1;

	return H5_SUCCESS;
}

/*!
  Number of boundary faces in current level
*/
h5_id_t
h5t_get_num_boundaryfaces (
	h5_file_t * const f
	) {
	return h5_error_not_implemented ( f, __FILE__, __func__, __LINE__ );
}

h5_id_t
h5t_store_boundaryface (
	h5_file_t * const f,
	h5_id_t * const global_vids
	) {
	struct h5t_fdata *t = f->t;

	if ( t->vertices == NULL ) {
		TRY ( _h5t_read_mesh ( f ) );
	}

	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON: {
		h5_id_t local_vids[3];
		h5_err_t h5err = h5t_map_global_vids2local (
			f, global_vids, 3, local_vids );
		if ( h5err < 0 ) return h5err;
		h5_id_t local_tid = h5t_get_local_triangle_id ( f, local_vids );
		if ( local_tid < 0 )
			return _h5t_error_local_triangle_id_nexist( f, local_vids );
		return h5t_store_boundaryface_local_id ( f, local_tid );
	}
	default:
		return h5_error_not_implemented (
			f, __FILE__, __func__, __LINE__ );
	}
}

h5_id_t
h5t_store_boundaryface_global_id (
	h5_file_t * const f,
	const h5_id_t global_fid
	) {
	struct h5t_fdata *t = f->t;

	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON: {
		h5_id_t local_tid = h5t_map_global_triangle_id2local (
			f, global_fid );
		if ( local_tid < 0 ) return local_tid;
		return h5t_store_boundaryface_local_id ( f, local_tid );
	}
	default:
		return h5_error_not_implemented (
			f, __FILE__, __func__, __LINE__ );
	}
}

h5_id_t
h5t_store_boundaryface_local_id (
	h5_file_t * const f,
	const h5_id_t local_fid
	) {
	struct h5t_fdata *t = f->t;
	boundary_t *boundary = &t->boundary;

	if ( boundary->num_faces == NULL )
		return HANDLE_H5_OVERFLOW_ERR (
			f,
			"boundary faces", 
			(h5_ssize_t)-1 );
	if ( ++(boundary->last_accessed_face) >= boundary->num_faces[0] )
		return HANDLE_H5_OVERFLOW_ERR (
			f,
			"boundary faces", 
			boundary->num_faces[0] );

	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON: {
		h5_id_t local_tet_id = _h5t_get_elem_id ( local_fid );
		if ( t->elems.tets[local_tet_id].global_parent_eid != -1 ) {
			return _h5t_error_store_boundaryface_local_id (
				f,
				local_fid );
		}
		h5_id_t global_tid = h5t_map_local_triangle_id2global(
			f, local_fid );
		boundary->faces[boundary->last_accessed_face] = global_tid;
		return H5_SUCCESS;
	}
	default:
		return h5_error_not_implemented (
			f, __FILE__, __func__, __LINE__ );
	}
}

h5_err_t
h5t_start_traverse_boundary_faces (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;
	boundary_t *boundary = &t->boundary;

	if ( boundary->faces == NULL ) {
		TRY( _h5t_read_boundaryfaces ( f ) );
	}
	boundary->last_accessed_face = -1;
	return H5_SUCCESS;
}

h5_id_t
h5t_traverse_boundary_faces (
	h5_file_t * const f,
	h5_id_t * const global_id,
	h5_id_t * const parent_id,
	h5_id_t vids[]
	) {
	return h5_error_not_implemented ( f, __FILE__, __func__, __LINE__ );
}
