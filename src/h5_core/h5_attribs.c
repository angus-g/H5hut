#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core.h"
#include "h5_core_private.h"
#include "H5Part.h"
#include "H5Block.h"

h5_int64_t
h5_read_attrib (
	hid_t id,
	const char *attrib_name,
	void *attrib_value
	) {

	herr_t herr;
	hid_t attrib_id;
	hid_t space_id;
	hid_t type_id;
	hid_t mytype;
	hsize_t nelem;

	attrib_id = H5Aopen_name ( id, attrib_name );
	if ( attrib_id <= 0 ) return HANDLE_H5A_OPEN_NAME_ERR( attrib_name );

	mytype = H5Aget_type ( attrib_id );
	if ( mytype < 0 ) return HANDLE_H5A_GET_TYPE_ERR;

	space_id = H5Aget_space ( attrib_id );
	if ( space_id < 0 ) return HANDLE_H5A_GET_SPACE_ERR;

	nelem = H5Sget_simple_extent_npoints ( space_id );
	if ( nelem < 0 ) return HANDLE_H5S_GET_SIMPLE_EXTENT_NPOINTS_ERR;

	type_id = h5_normalize_h5_type ( mytype );

	herr = H5Aread (attrib_id, type_id, attrib_value );
	if ( herr < 0 ) return HANDLE_H5A_READ_ERR;

	herr = H5Sclose ( space_id );
	if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;

	herr = H5Tclose ( mytype );
	if ( herr < 0 ) return HANDLE_H5T_CLOSE_ERR;

	herr = H5Aclose ( attrib_id );
	if ( herr < 0 ) return HANDLE_H5A_CLOSE_ERR;

	return H5_SUCCESS;
}

h5_int64_t
h5_write_attrib (
	hid_t id,
	const char *attrib_name,
	const hid_t attrib_type,
	const void *attrib_value,
	const hsize_t attrib_nelem
	) {

	herr_t herr;
	hid_t space_id;
	hid_t attrib_id;

	space_id = H5Screate_simple (1, &attrib_nelem, NULL);
	if ( space_id < 0 )
		return HANDLE_H5S_CREATE_SIMPLE_ERR ( 1 );

	attrib_id = H5Acreate ( 
		id,
		attrib_name,
		attrib_type,
		space_id,
		H5P_DEFAULT, H5P_DEFAULT );
	if ( attrib_id < 0 ) return HANDLE_H5A_CREATE_ERR ( attrib_name );

	herr = H5Awrite ( attrib_id, attrib_type, attrib_value);
	if ( herr < 0 ) return HANDLE_H5A_WRITE_ERR ( attrib_name );

	herr = H5Aclose ( attrib_id );
	if ( herr < 0 ) return HANDLE_H5A_CLOSE_ERR;

	herr = H5Sclose ( space_id );
	if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;

	return H5_SUCCESS;
}

h5_int64_t
h5_get_attrib_info (
	hid_t id,
	const h5_int64_t attrib_idx,
	char *attrib_name,
	const h5_int64_t len_attrib_name,
	h5_int64_t *attrib_type,
	h5_int64_t *attrib_nelem
	) {

	herr_t herr;
	hid_t attrib_id;
	hid_t mytype;
	hid_t space_id;

	attrib_id = H5Aopen_idx ( id, (unsigned int)attrib_idx );
	if ( attrib_id < 0 ) return HANDLE_H5A_OPEN_IDX_ERR ( attrib_idx );

	if ( attrib_nelem ) {
		space_id =  H5Aget_space ( attrib_id );
		if ( space_id < 0 ) return HANDLE_H5A_GET_SPACE_ERR;

		*attrib_nelem = H5Sget_simple_extent_npoints ( space_id );
		if ( *attrib_nelem < 0 )
			return HANDLE_H5S_GET_SIMPLE_EXTENT_NPOINTS_ERR;

		herr = H5Sclose ( space_id );
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;
	}
	if ( attrib_name ) {
		herr = H5Aget_name (
			attrib_id,
			(size_t)len_attrib_name,
			attrib_name );
		if ( herr < 0 ) return HANDLE_H5A_GET_NAME_ERR;
	}
	if ( attrib_type ) {
		mytype = H5Aget_type ( attrib_id );
		if ( mytype < 0 ) return HANDLE_H5A_GET_TYPE_ERR;

		*attrib_type = h5_normalize_h5_type ( mytype );

		herr = H5Tclose ( mytype );
		if ( herr < 0 ) return HANDLE_H5T_CLOSE_ERR;
	}
	herr = H5Aclose ( attrib_id);
	if ( herr < 0 ) return HANDLE_H5A_CLOSE_ERR;

	return H5_SUCCESS;
}

h5_int64_t
h5_get_num_attribs (
	h5_file_t *f,
	hid_t id
	) {

	CHECK_FILEHANDLE ( f );

	int nattribs = H5Aget_num_attrs ( id );
	if ( nattribs < 0 ) HANDLE_H5A_GET_NUM_ATTRS_ERR;

	return (h5_int64_t) nattribs;
}
