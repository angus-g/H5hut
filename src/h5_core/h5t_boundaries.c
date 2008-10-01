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
		t->num_boundaries = h5_get_num_objects (
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
	return -1;
}
h5_err_t
h5t_get_boundary_label (
	h5_file_t * const f,
	char * const boundary_name,
	const size_t size
	) {
	return -1;
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
		return HANDLE_H5_OUT_OF_RANGE_ERR( "boundary", boundary_id );
	}
	if ( boundary_id == -1 ) {  /* append new boundary */
		boundary->id = t->num_boundaries++;
		boundary->changed = boundary_id;
	} else {
		boundary->id = boundary_id;
	}
	snprintf ( boundary->name, sizeof (boundary->name),
		   "%d", boundary->id );
	TRY( _open_boundary_group ( f ) );

	return H5_SUCCESS;
}

h5_err_t
h5t_open_boundary_with_label (
	h5_file_t * const f,
	const char * const boundary_label
	) {
	return -1;
}

h5_err_t
_h5t_read_boundaryfaces (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;
	boundary_t *boundary = &t->boundary;

	const char * const dataset_name = "Faces";
	hid_t dataset_id = H5Dopen ( boundary->gid, dataset_name, H5P_DEFAULT );
	if ( dataset_id < 0 ) 
		return HANDLE_H5D_OPEN_ERR ( dataset_name );

	hid_t diskspace_id = H5Dget_space(dataset_id);
	if ( diskspace_id < 0 ) return (hid_t)HANDLE_H5D_GET_SPACE_ERR;

	h5_id_t num_faces = H5Sget_simple_extent_npoints ( diskspace_id );
	if ( num_faces < 0 )
		return HANDLE_H5S_GET_SIMPLE_EXTENT_NPOINTS_ERR;

	herr_t herr = H5Sclose ( diskspace_id );
	if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;

	herr = H5Dclose ( dataset_id );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR;

	h5t_add_num_boundaryfaces ( f, num_faces );

	h5_err_t h5err = _h5_read_dataset (
		f,
		dataset_id,
		H5T_NATIVE_INT32,
		H5S_ALL,
		H5S_ALL,
		boundary->faces );
	if ( h5err < 0 ) return h5err;

	herr = H5Dclose ( dataset_id );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR;

	return H5_SUCCESS;
}

h5_err_t
_h5t_write_boundary (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;
	boundary_t *boundary = &t->boundary;

	hsize_t maxdim = H5S_UNLIMITED;

	return _h5t_write_obj (
		f,
		boundary->gid,
		boundary->num_faces[0],
		maxdim,
		H5T_NATIVE_INT32,
		(void*)boundary->faces,
		"Faces"
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

	TRY( _h5_close_group( boundary->gid ) );

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
	boundary_t *boundary = &t->boundary;
	
	boundary->num_faces = realloc (
		boundary->num_faces,
		t->num_levels*sizeof(boundary->num_faces[0]) );
	boundary->num_faces_on_level = realloc (
		boundary->num_faces_on_level,
		t->num_levels*sizeof(boundary->num_faces_on_level[0]) );
	boundary->faces = realloc (
		boundary->faces,
		num_faces*sizeof(boundary->faces[0]) );
	boundary->lfaces = realloc (
		boundary->lfaces,
		num_faces*sizeof(boundary->lfaces[0]) );
	if ( boundary->num_faces == NULL ||
	     boundary->num_faces_on_level == NULL ||
	     boundary->faces == NULL ||
	     boundary->lfaces == NULL ) {
		return HANDLE_H5_NOMEM_ERR;
	}
	memset ( boundary->num_faces, 
		 0, t->num_levels*sizeof(boundary->num_faces[0]) );
	memset ( boundary->num_faces_on_level,
		 0, t->num_levels*sizeof(boundary->num_faces_on_level[0]) );
	memset ( boundary->faces,
		 -1, num_faces*sizeof(boundary->faces[0]) );
	memset (
		boundary->lfaces,
		0, num_faces*sizeof(boundary->lfaces[0]) );
	boundary->num_faces[0] = num_faces;
	boundary->last_accessed_face = -1;

	return H5_SUCCESS;
}

/*!
  Number of boundary faces in current level
*/
h5_id_t
h5t_get_num_boundaryfaces (
	h5_file_t * const f
	) {
	return -1;
}

h5_id_t
h5t_store_boundaryface (
	h5_file_t * const f,
	h5_id_t * const global_vids
	) {
	struct h5t_fdata *t = f->t;

	if ( t->vertices == NULL ) {
		h5_err_t h5err = _h5t_read_mesh ( f );
		if ( h5err < 0 ) return h5err;
	}

	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON: {
		h5_id_t local_vids[3];
		h5_err_t h5err = h5t_map_global_vertex_ids2local (
			f, global_vids, 3, local_vids );
		if ( h5err < 0 ) return h5err;
		h5_id_t local_tid = h5t_get_local_triangle_id ( f, local_vids );
		if ( local_tid < 0 )
			return _h5t_error_local_triangle_id_nexist( local_vids );
		return h5t_store_boundaryface_local_id ( f, local_tid );
	}
	default:
		return -1;
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
		return -1;
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
			"boundary faces", 
			-1 );
	if ( ++(boundary->last_accessed_face) >= boundary->num_faces[0] )
		return HANDLE_H5_OVERFLOW_ERR (
			"boundary faces", 
			boundary->num_faces[0] );

	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON: {
		h5_id_t local_tet_id = local_fid & H5_TET_MASK;
		if ( t->entities.tets[local_tet_id].parent_id != -1 ) {
			return _h5t_error_store_boundaryface_local_id (
				local_fid );
		}
		h5_id_t global_tid = h5t_map_local_triangle_id2global(
			f, local_fid );
		boundary->faces[boundary->last_accessed_face] = global_tid;
		return H5_SUCCESS;
	}
	default:
		return -1;
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
	h5_id_t vertex_ids[]
	) {
	return -1;
}
