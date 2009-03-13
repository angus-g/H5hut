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

/*!
  \ingroup h5_core
  \defgroup h5_core_attrib	Attribute handling

*/
/*!
  \ingroup h5_core_attrib

  Read attribute of HDF5 object.

  \return \c H5_SUCCESS or error code.

  \sa h5_write_attrib(), h5_get_num_attribs(), h5_get_attrib_info()
 */
h5_err_t
h5_read_attrib (
	h5_file_t * const f,		/*!< handle to open file */
	const hid_t id,			/*!< id of HDF5 object */
	const char *attrib_name,	/*!< name of HDF5 attribute to read */
	void * const attrib_value	/*!< OUT: attribute value */
	) {
	hid_t attrib_id;
	hid_t space_id;
	hid_t type_id;
	hid_t mytype;
	hsize_t nelem;

	TRY ( attrib_id = _h5_open_attribute_by_name ( f, id, attrib_name ) );
	TRY ( mytype = _h5_get_attribute_type ( f, attrib_id ) );
	TRY ( space_id = _h5_get_attribute_space ( f, attrib_id ) );

	nelem = H5Sget_simple_extent_npoints ( space_id );
	if ( nelem < 0 ) return HANDLE_H5S_GET_SIMPLE_EXTENT_NPOINTS_ERR( f );

	type_id = h5_normalize_h5_type ( f, mytype );

	TRY ( _h5_read_attribute ( f, attrib_id, type_id, attrib_value ) );
	TRY ( _h5_close_dataspace( f, space_id ) );
	TRY ( _h5_close_type( f, mytype ) );
	TRY ( _h5_close_attribute ( f, attrib_id ) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_attrib

  Write attribute to HDF5 object.

  \return \c H5_SUCCESS or error code.
*/
h5_err_t
h5_write_attrib (
	h5_file_t * const f,		/*!< handle to open file */
	const hid_t id,			/*!< id of HDF5 object */
	const char *attrib_name,	/*!< name of HDF5 attribute to write */
	const hid_t attrib_type,	/*!< HDF5 type of attribute */
	const void *attrib_value,	/*!< value of attribute */
	const hsize_t attrib_nelem	/*!< number of elements (dimension) */
	) {
	hid_t space_id;
	hid_t attrib_id;

	TRY ( space_id = _h5_create_dataset_space ( f, 1, &attrib_nelem, NULL) );
	TRY ( attrib_id = _h5_create_attribute ( 
		      f,
		      id,
		      attrib_name,
		      attrib_type,
		      space_id,
		      H5P_DEFAULT, H5P_DEFAULT ) );

	TRY ( _h5_write_attribute ( f, attrib_id, attrib_type, attrib_value ) );
	TRY ( _h5_close_attribute ( f, attrib_id ) );
	TRY ( _h5_close_dataspace ( f, space_id ) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_attrib

  Get information about an attribute of a HDF5 object.

  \return \c H5_SUCCESS or error code.
*/
h5_err_t
h5_get_attrib_info (
	h5_file_t * const f,		/*!< handle to open file */
	const hid_t id,			/*!< id of HDF5 object */
	const h5_int64_t attrib_idx,	/*!< index of attribute */
	char *attrib_name,		/*!< OUT: name of attribute */
	const h5_int64_t len_attrib_name, /*!< buffer length */
	h5_int64_t *attrib_type,	/*!< OUT: H5 type of attribute */
	h5_int64_t *attrib_nelem	/*!< OUT: number of elements (dimension) */
	) {
	hid_t attrib_id;
	hid_t mytype;
	hid_t space_id;

	attrib_id = H5Aopen_idx ( id, (unsigned int)attrib_idx );
	if ( attrib_id < 0 ) return HANDLE_H5A_OPEN_IDX_ERR ( f, attrib_idx );

	if ( attrib_nelem ) {
		TRY ( space_id = _h5_get_attribute_space ( f, attrib_id ) );

		*attrib_nelem = H5Sget_simple_extent_npoints ( space_id );
		if ( *attrib_nelem < 0 )
			return HANDLE_H5S_GET_SIMPLE_EXTENT_NPOINTS_ERR( f );

		TRY( _h5_close_dataspace( f, space_id ) );
	}
	if ( attrib_name ) {
		TRY ( _h5_get_attribute_name (
			      f,
			      attrib_id,
			      (size_t)len_attrib_name,
			      attrib_name ) );
	}
	if ( attrib_type ) {
		TRY ( mytype = _h5_get_attribute_type ( f, attrib_id ) );
		*attrib_type = h5_normalize_h5_type ( f, mytype );
		TRY ( _h5_close_type( f, mytype ) );
	}
	TRY ( _h5_close_attribute ( f, attrib_id ) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_attrib

  Get number of attributes of a HDF5 object.

  \return number of attributes or error code.
*/
h5_size_t
h5_get_num_attribs (
	h5_file_t * const f,		/*!< handle to open file */
	const hid_t id
	) {

	CHECK_FILEHANDLE ( f );

	int nattribs = H5Aget_num_attrs ( id );
	if ( nattribs < 0 ) HANDLE_H5A_GET_NUM_ATTRS_ERR( f );

	return (h5_size_t) nattribs;
}
