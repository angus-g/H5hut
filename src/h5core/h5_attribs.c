#include "h5core/h5_core.h"
#include "h5_core_private.h"

static h5_err_t
get_hdf5_obj_id(
	h5_file_t *const f,
	const char mode,
	hid_t *id
	) {
	if (mode == H5_ATTRIB_FILE) *id = f->root_gid;
	else if (mode == H5_ATTRIB_STEP) *id = f->step_gid;
	else if (mode == H5_ATTRIB_FIELD) *id = f->b->field_gid;
	else h5_error(f, H5_ERR_INVAL, "Attibute flag not recognized");
	return H5_SUCCESS;
}

h5_err_t
h5priv_read_attrib (
	h5_file_t* const f,		/*!< handle to open file */
	const hid_t id,			/*!< HDF5 object ID */
	const char* attrib_name,	/*!< name of HDF5 attribute to read */
	const hid_t attrib_type,	/*!< HDF5 type of attribute */
	void* const attrib_value	/*!< OUT: attribute value */
	) {
	hid_t attrib_id;
	hid_t type_id;
	hid_t space_id;
	TRY( attrib_id = h5priv_open_hdf5_attribute (f, id, attrib_name) );
	TRY( type_id = h5priv_get_hdf5_attribute_type (f, attrib_id) );

        hid_t h5type_id;
        TRY( h5type_id = h5_normalize_h5_type(f, type_id) );
	if ( h5type_id != attrib_type )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Attribute '%s' has type '%s' but was requested as '%s'.",
			attrib_name,
			h5priv_get_base_type_name(f, h5type_id),
			h5priv_get_base_type_name(f, attrib_type) );

	TRY( space_id = h5priv_get_hdf5_attribute_dataspace (f, attrib_id) );
	TRY( h5priv_read_hdf5_attribute (f, attrib_id, type_id, attrib_value) );
	TRY( h5priv_close_hdf5_dataspace(f, space_id) );
	TRY( h5priv_close_hdf5_type(f, type_id) );
	TRY( h5priv_close_hdf5_attribute (f, attrib_id) );
	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_attrib

  Read attribute of HDF5 object.

  \return \c H5_SUCCESS or error code.

  \sa h5_write_attrib(), h5_get_num_attribs(), h5_get_attrib_info()
 */
h5_err_t
h5_read_attrib (
	h5_file_t* const f,		/*!< handle to open file */
	const char mode,		/*!< FILE or STEP flag */
	const char* attrib_name,	/*!< name of HDF5 attribute to read */
	const hid_t attrib_type,	/*!< HDF5 type of attribute */
	void* const attrib_value	/*!< OUT: attribute value */
	) {
	if (mode != H5_ATTRIB_FILE) CHECK_TIMEGROUP( f );

	hid_t id;
	TRY( get_hdf5_obj_id(f, mode, &id) );
	TRY( h5priv_read_attrib (f, id, attrib_name, attrib_type, attrib_value) );

	return H5_SUCCESS;
}

h5_err_t
h5priv_write_attrib (
	h5_file_t* const f,		/*!< handle to open file */
	const hid_t id,			/*!< HDF5 object ID */
	const char* attrib_name,	/*!< name of HDF5 attribute to write */
	const hid_t attrib_type,	/*!< HDF5 type of attribute */
	const void* attrib_value,	/*!< value of attribute */
	const hsize_t attrib_nelem	/*!< number of elements (dimension) */
	) {

	hid_t space_id;
	hid_t attrib_id;
        hid_t type_id;
	if ( attrib_type == H5T_NATIVE_CHAR ) {
		TRY( type_id = h5priv_create_hdf5_string_type(f,
			    				attrib_nelem) );
		TRY( space_id = h5priv_create_hdf5_dataspace_scalar(f) );
	} else {
		type_id = attrib_type;
		TRY( space_id = h5priv_create_hdf5_dataspace (f,
			    			1, &attrib_nelem, NULL) );
	}

	TRY( attrib_id = h5priv_create_hdf5_attribute ( 
		      f,
		      id,
		      attrib_name,
		      type_id,
		      space_id,
		      H5P_DEFAULT, H5P_DEFAULT) );

	TRY( h5priv_write_hdf5_attribute (f,
			    attrib_id, type_id, attrib_value) );
	TRY( h5priv_close_hdf5_attribute (f, attrib_id) );
	TRY( h5priv_close_hdf5_dataspace (f, space_id) );

	if ( attrib_type == H5T_NATIVE_CHAR )
		TRY( h5priv_close_hdf5_type(f, type_id) );

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
	const char mode,		/*!< FILE or STEP flag */
	const char* attrib_name,	/*!< name of HDF5 attribute to write */
	const hid_t attrib_type,	/*!< HDF5 type of attribute */
	const void* attrib_value,	/*!< value of attribute */
	const hsize_t attrib_nelem	/*!< number of elements (dimension) */
	) {
	if (mode != H5_ATTRIB_FILE) CHECK_TIMEGROUP( f );
	CHECK_WRITABLE_MODE( f );

	hid_t id;
	TRY( get_hdf5_obj_id(f, mode, &id) );
	TRY( h5priv_write_attrib (f, id, attrib_name, attrib_type,
				  attrib_value, attrib_nelem) );
	return H5_SUCCESS;
}

h5_err_t
h5priv_get_attrib_info (
	h5_file_t* const f,		/*!< handle to open file */
	const hid_t id,			/*!< HDF5 object ID */
	const h5_size_t attrib_idx,	/*!< index of attribute */
	char* attrib_name,		/*!< OUT: name of attribute */
	const h5_size_t len_attrib_name,/*!< buffer length */
	h5_int64_t* attrib_type,	/*!< OUT: H5 type of attribute */
	h5_size_t* attrib_nelem		/*!< OUT: number of elements */
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

  Get information about an attribute of a HDF5 object.

  \return \c H5_SUCCESS or error code.
*/
h5_err_t
h5_get_attrib_info (
	h5_file_t* const f,			/*!< handle to open file */
	const char mode,			/*!< FILE or STEP flag */
	const h5_size_t attrib_idx,		/*!< index of attribute */
	char* attrib_name,			/*!< OUT: name of attribute */
	const h5_size_t len_attrib_name,	/*!< buffer length */
	h5_int64_t* attrib_type,		/*!< OUT: H5 type of attribute */
	h5_size_t* attrib_nelem			/*!< OUT: number of elements */
	) {
	if (mode != H5_ATTRIB_FILE) CHECK_TIMEGROUP( f );

	hid_t id;
	TRY( get_hdf5_obj_id(f, mode, &id) );
	TRY( h5priv_get_attrib_info (f, id, attrib_idx, attrib_name, len_attrib_name,
				     attrib_type, attrib_nelem) );
	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_attrib

  Get number of attributes of a HDF5 object.

  \return number of attributes or error code.
*/
h5_ssize_t
h5_get_num_attribs (
	h5_file_t *const f,	/*!< handle to open file */
	const char mode		/*!< FILE or STEP flag */
	) {
	if (mode != H5_ATTRIB_FILE) CHECK_TIMEGROUP( f );
	hid_t id;
	TRY( get_hdf5_obj_id(f, mode, &id) );
	return h5priv_get_num_hdf5_attribute (f, id);
}

