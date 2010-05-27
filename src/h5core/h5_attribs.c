#include "h5_core.h"
#include "h5_core_private.h"

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
	h5_file_t* const f,		/*!< handle to open file */
	const hid_t id,			/*!< id of HDF5 object */
	const char* attrib_name,	/*!< name of HDF5 attribute to read */
	void* const attrib_value	/*!< OUT: attribute value */
	) {
	hid_t attrib_id;
	hid_t space_id;
	hid_t type_id;
	hid_t mytype;
	hsize_t nelem;

	TRY( attrib_id = h5priv_open_hdf5_attribute (f, id, attrib_name) );
	TRY( mytype = h5priv_get_hdf5_attribute_type (f, attrib_id) );
	TRY( space_id = h5priv_get_hdf5_attribute_dataspace (f, attrib_id) );
	TRY( nelem = h5priv_get_npoints_of_hdf5_dataspace (f, space_id) );
	TRY( type_id = h5_normalize_h5_type (f, mytype) );
	TRY( h5priv_read_hdf5_attribute (f, attrib_id, type_id, attrib_value) );
	TRY( h5priv_close_hdf5_dataspace(f, space_id) );
	TRY( h5priv_close_hdf5_type(f, mytype) );
	TRY( h5priv_close_hdf5_attribute (f, attrib_id) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_attrib

  Write attribute to HDF5 object.

  \return \c H5_SUCCESS or error code.
*/
h5_err_t
h5_write_attrib (
	h5_file_t* const f,		/*!< handle to open file */
	const hid_t id,			/*!< id of HDF5 object */
	const char* attrib_name,	/*!< name of HDF5 attribute to write */
	const hid_t attrib_type,	/*!< HDF5 type of attribute */
	const void* attrib_value,	/*!< value of attribute */
	const hsize_t attrib_nelem	/*!< number of elements (dimension) */
	) {
	hid_t space_id;
	hid_t attrib_id;

	TRY( space_id = h5priv_create_hdf5_dataspace (f, 1, &attrib_nelem, NULL) );
	TRY( attrib_id = h5priv_create_hdf5_attribute ( 
		      f,
		      id,
		      attrib_name,
		      attrib_type,
		      space_id,
		      H5P_DEFAULT, H5P_DEFAULT) );

	TRY( h5priv_write_hdf5_attribute (f, attrib_id, attrib_type, attrib_value) );
	TRY( h5priv_close_hdf5_attribute (f, attrib_id) );
	TRY( h5priv_close_hdf5_dataspace (f, space_id) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_attrib

  Get information about an attribute of a HDF5 object.

  \return \c H5_SUCCESS or error code.
*/
h5_err_t
h5_get_attrib_info (
	h5_file_t* const f,		/*!< handle to open file */
	const hid_t id,			/*!< id of HDF5 object */
	const h5_int64_t attrib_idx,	/*!< index of attribute */
	char* attrib_name,		/*!< OUT: name of attribute */
	const h5_int64_t len_attrib_name, /*!< buffer length */
	h5_int64_t* attrib_type,	/*!< OUT: H5 type of attribute */
	h5_int64_t* attrib_nelem	/*!< OUT: number of elements (dimension) */
	) {
	hid_t attrib_id;
	hid_t mytype;
	hid_t space_id;

	TRY( attrib_id = h5priv_open_hdf5_attribute_idx (
		      f,
		      id,
		      (unsigned int)attrib_idx) );

	if (attrib_nelem) {
		TRY( space_id = h5priv_get_hdf5_attribute_dataspace (f, attrib_id) );
		TRY( *attrib_nelem = h5priv_get_npoints_of_hdf5_dataspace (
			      f, space_id) );
		TRY( h5priv_close_hdf5_dataspace (f, space_id) );
	}
	if (attrib_name) {
		TRY( h5priv_get_hdf5_attribute_name (
			      f,
			      attrib_id,
			      (size_t)len_attrib_name,
			      attrib_name) );
	}
	if (attrib_type) {
		TRY( mytype = h5priv_get_hdf5_attribute_type (f, attrib_id) );
		TRY( *attrib_type = h5_normalize_h5_type (f, mytype) );
		TRY( h5priv_close_hdf5_type(f, mytype) );
	}
	TRY( h5priv_close_hdf5_attribute (f, attrib_id) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_attrib

  Get number of attributes of a HDF5 object.

  \return number of attributes or error code.
*/
h5_ssize_t
h5_get_num_attribs (
	h5_file_t* const f,		/*!< handle to open file */
	const hid_t id
	) {
	CHECK_FILEHANDLE (f);
	return h5priv_get_num_hdf5_attribute (f, id);
}
