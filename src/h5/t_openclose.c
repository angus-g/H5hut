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
#include "H5Block.h"
#include "H5PartPrivate.h"
#include "H5BlockPrivate.h"
#include "H5PartErrors.h"
#include "H5BlockErrors.h"
#include "H5.h"

extern h5part_error_handler	_err_handler;
extern h5part_int64_t		_h5part_errno;
extern unsigned			_debug;


static h5_err_t
_create_array_types (
	h5_file * f
	) {
	hsize_t dims[1] = { 3 };
	t->float64_3tuple_tid = H5Tarray_create ( H5T_NATIVE_DOUBLE, 1, dims,NULL);
	dims[0] = 2;
	t->int32_2tuple_tid = H5Tarray_create ( H5T_NATIVE_INT32, 1, dims, NULL );
	dims[0] = 3;
	t->int32_3tuple_tid = H5Tarray_create ( H5T_NATIVE_INT32, 1, dims, NULL );
	dims[0] = 4;
	t->int32_4tuple_tid = H5Tarray_create ( H5T_NATIVE_INT32, 1, dims, NULL );

	return H5_SUCCESS;
}

static h5_err_t
_create_vertex_type (
	h5_file * f
	) {
	t->vertex_tid = H5Tcreate ( H5T_COMPOUND, sizeof(struct h5_vertex) );
	H5Tinsert (
		t->vertex_tid,
		"id",
		HOFFSET(struct h5_vertex, id),
		H5T_NATIVE_INT32 );
	H5Tinsert (
		t->vertex_tid,
		"unused",
		HOFFSET(struct h5_vertex, unused),
		H5T_NATIVE_INT32 );
	H5Tinsert (
		t->vertex_tid,
		"P",
		HOFFSET(struct h5_vertex, P),
		t->float64_3tuple_tid );
	return H5_SUCCESS;
}

static h5_err_t
_create_tet_type (
	h5_file * f
	) {
	t->tet_tid = H5Tcreate ( H5T_COMPOUND, sizeof(struct h5_tetrahedron) );
	H5Tinsert (
		t->tet_tid,
		"id",
		HOFFSET(struct h5_tet, id),
		H5T_NATIVE_INT32 );
	H5Tinsert (
		t->tet_tid,
		"parent_id",
		HOFFSET(struct h5_tet, parent_id),
		H5T_NATIVE_INT32 );
	H5Tinsert (
		t->tet_tid,
		"vertex_ids",
		HOFFSET(struct h5_tet, vertex_ids),
		t->int32_4tuple_tid );

	return H5_SUCCESS;

/*!
  \ingroup h5_private

  \internal

  Initialize H5Block internal structure.

  \return	H5_SUCCESS or error code
*/
h5_err_t
_h5t_open_file (
	h5_file * f			/*!< IN: file handle */
	) {
	h5_err_t h5err = H5_SUCCESS;;
	struct h5t_fdata * t = &fh->t; 

	t->num_levels = -1;
	levels = NULL;

	if (h5err = _create_array_types ( f )) < 0 ) return herr;
	if (h5err = _create_vertex_type ( f )) < 0 ) return herr;
	if (h5err = _create_tet_type ( f )) < 0 ) return herr;


	return H5_SUCCESS;
}
