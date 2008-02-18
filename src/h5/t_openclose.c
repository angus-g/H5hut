#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "H5PartTypes.h"
#include "H5BlockTypes.h"
#include "H5Part.h"
#include "H5PartPrivate.h"
#include "h5/h5.h"
#include "h5/h5_private.h"

extern h5part_error_handler	_err_handler;
extern h5part_int64_t		_h5part_errno;
extern unsigned			_debug;


static h5_err_t
_create_array_types (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

	hsize_t dims[1] = { 3 };
	hid_t hid = H5Tarray_create ( H5T_NATIVE_DOUBLE, 1, dims, NULL);
	if ( hid < 0 ) {
		return HANDLE_H5T_ARRAY_CREATE_ERR ( "H5T_NATIVE_DOUBLE", 1 );
	}
	t->float64_3tuple_tid = hid;

	dims[0] = 2;
	hid = H5Tarray_create ( H5T_NATIVE_INT32, 1, dims, NULL );
	if ( hid < 0 ) {
		return HANDLE_H5T_ARRAY_CREATE_ERR ( "H5T_NATIVE_INT32", 1 );
	}
	t->int32_2tuple_tid = hid;

	dims[0] = 3;
	hid = H5Tarray_create ( H5T_NATIVE_INT32, 1, dims, NULL );
	if ( hid < 0 ) {
		return HANDLE_H5T_ARRAY_CREATE_ERR ( "H5T_NATIVE_INT32", 1 );
	}
	t->int32_3tuple_tid = hid;

	dims[0] = 4;
	hid = H5Tarray_create ( H5T_NATIVE_INT32, 1, dims, NULL );
	if ( hid < 0 ) {
		return HANDLE_H5T_ARRAY_CREATE_ERR ( "H5T_NATIVE_INT32", 1 );
	}
	t->int32_4tuple_tid = hid;

	return H5_SUCCESS;
}

static h5_err_t
_create_vertex_type (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

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
_create_tet_type (
	h5_file * f
	) {
	struct h5t_fdata *t = &f->t;

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

h5_err_t
_h5t_init_fdata (
	h5_file * f
	) {
	struct h5t_fdata * t = &f->t;

	memset ( t->mesh_name, 0, sizeof ( t->mesh_name ) );
	t->cur_mesh = -1;
	t->num_levels = -1;
	t->new_level = -1;
	t->cur_level = -1;
	t->last_stored_vertex_id = -1;
	t->last_stored_tet_id = -1;
	t->topo_gid = -1;
	t->mesh_gid = -1;
	t->coord_gid = -1;
	t->vmesh_gid = -1;

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
	h5_file * f			/*!< IN: file handle */
	) {
	h5_err_t h5err = H5_SUCCESS;;

	f->t.new_mesh = -1;
	if (( h5err = _h5t_init_fdata ( f )) < 0 ) return h5err;

	if (( h5err = _create_array_types ( f )) < 0 ) return h5err;
	if (( h5err = _create_vertex_type ( f )) < 0 ) return h5err;
	if (( h5err = _create_tet_type ( f )) < 0 ) return h5err;

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
	h5_file *f		/*!< IN: file handle */
	) {

	h5_err_t h5err = H5_SUCCESS;

	h5err = _h5t_close_mesh ( f );

	return h5err;
}
