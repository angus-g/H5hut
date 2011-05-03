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
	else h5_error(H5_ERR_INVAL, "Attibute flag not recognized");
	return H5_SUCCESS;
}

h5_err_t
h5priv_read_attrib (
	const hid_t id,			/*!< HDF5 object ID */
	const char* attrib_name,	/*!< name of HDF5 attribute to read */
	const hid_t attrib_type,	/*!< HDF5 type of attribute */
	void* const attrib_value	/*!< OUT: attribute value */
	) {
	H5_PRIV_API_ENTER4 (h5_err_t,
			    "id=%d, attrib_name=\"%s\", attrib_type=%d, attrib_value=%p",
			    id,
			    attrib_name,
			    attrib_type,
			    attrib_value);
	hid_t attrib_id;
	hid_t type_id;
	hid_t space_id;
	TRY (attrib_id = hdf5_open_attribute (id, attrib_name));
	TRY (type_id = hdf5_get_attribute_type (attrib_id));

        hid_t h5type_id;
        TRY (h5type_id = h5_normalize_h5_type (type_id));
	if (h5type_id != attrib_type)
		H5_PRIV_API_LEAVE (
			h5_error (
				H5_ERR_HDF5,
				"Attribute '%s' has type '%s' but was requested as '%s'.",
				attrib_name,
				hdf5_get_type_name (h5type_id),
				hdf5_get_type_name (attrib_type)));

	TRY (space_id = hdf5_get_attribute_dataspace (attrib_id));
	TRY (hdf5_read_attribute (attrib_id, type_id, attrib_value));
	TRY (hdf5_close_dataspace(space_id));
	TRY (hdf5_close_type (type_id));
	TRY (hdf5_close_attribute (attrib_id));
	H5_PRIV_API_RETURN (H5_SUCCESS);
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
	H5_CORE_API_ENTER5 (h5_err_t,
			    "f=%p mode=%d, "
			    "attrib_name=\"%s\", attrib_type=%d, "
			    "attrib_value=%p",
			    f,
			    mode,
			    attrib_name,
			    attrib_type,
			    attrib_value);

	if (mode != H5_ATTRIB_FILE) CHECK_TIMEGROUP( f );

	hid_t id;
	TRY (get_hdf5_obj_id(f, mode, &id));
	TRY (h5priv_read_attrib (id, attrib_name, attrib_type, attrib_value));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5priv_write_attrib (
	const hid_t id,			/*!< HDF5 object ID */
	const char* attrib_name,	/*!< name of HDF5 attribute to write */
	const hid_t attrib_type,	/*!< HDF5 type of attribute */
	const void* attrib_value,	/*!< value of attribute */
	const hsize_t attrib_nelem	/*!< number of elements (dimension) */
	) {
	H5_PRIV_API_ENTER5 (h5_err_t,
			    "id=%d, attrib_name=\"%s\", attrib_type=%d, "
			    "attrib_value=%p, attrib_nelem=%llu",
			    id,
			    attrib_name,
			    attrib_type,
			    attrib_value,
			    attrib_nelem);
	hid_t space_id;
	hid_t attrib_id;
        hid_t type_id;
	if ( attrib_type == H5T_NATIVE_CHAR ) {
		TRY (type_id = hdf5_create_string_type (attrib_nelem));
		TRY (space_id = hdf5_create_dataspace_scalar ());
	} else {
		type_id = attrib_type;
		TRY (space_id = hdf5_create_dataspace (1, &attrib_nelem, NULL));
	}

	TRY (attrib_id = hdf5_create_attribute (
		      id,
		      attrib_name,
		      type_id,
		      space_id,
		      H5P_DEFAULT, H5P_DEFAULT));

	TRY (hdf5_write_attribute (attrib_id, type_id, attrib_value));
	TRY (hdf5_close_attribute (attrib_id));
	TRY (hdf5_close_dataspace (space_id));

	if (attrib_type == H5T_NATIVE_CHAR)
		TRY (hdf5_close_type (type_id));

	H5_PRIV_API_RETURN (H5_SUCCESS);
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
	H5_CORE_API_ENTER6 (h5_err_t,
			    "f=%p mode=%d, "
			    "attrib_name=\"%s\", attrib_type=%d, "
			    "attrib_value=%p, attrib_nelem=%llu",
			    f,
			    mode,
			    attrib_name,
			    attrib_type,
			    attrib_value,
			    attrib_nelem);

	if (mode != H5_ATTRIB_FILE) CHECK_TIMEGROUP( f );
	CHECK_WRITABLE_MODE( f );

	hid_t id;
	TRY (get_hdf5_obj_id(f, mode, &id));
	TRY (h5priv_write_attrib (id, attrib_name, attrib_type,
				  attrib_value, attrib_nelem));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5priv_get_attrib_info (
	const hid_t id,			/*!< HDF5 object ID */
	const h5_size_t attrib_idx,	/*!< index of attribute */
	char* attrib_name,		/*!< OUT: name of attribute */
	const h5_size_t len_attrib_name,/*!< buffer length */
	h5_int64_t* attrib_type,	/*!< OUT: H5 type of attribute */
	h5_size_t* attrib_nelem		/*!< OUT: number of elements */
	) {
	H5_PRIV_API_ENTER6 (h5_err_t,
			    "id=%d, "
			    "attrib_idx=%llu, "
			    "attrib_name=%p, len_attrib_name=%llu, "
			    "attrib_type=%p, attrib_nelem=%p",
			    id,
			    attrib_idx,
			    attrib_name,
			    len_attrib_name,
			    attrib_type,
			    attrib_nelem);
	hid_t attrib_id;
	hid_t mytype;
	hid_t space_id;
	TRY (attrib_id = hdf5_open_attribute_idx (
		      id,
		      (unsigned int)attrib_idx));

	if (attrib_nelem) {
		TRY (space_id = hdf5_get_attribute_dataspace (attrib_id));
		TRY (*attrib_nelem = hdf5_get_npoints_of_dataspace (space_id));
		TRY (hdf5_close_dataspace (space_id));
	}
	if (attrib_name) {
		TRY (hdf5_get_attribute_name (
			     attrib_id,
			     (size_t)len_attrib_name,
			     attrib_name));
	}
	if (attrib_type) {
		TRY (mytype = hdf5_get_attribute_type (attrib_id));
		TRY (*attrib_type = h5_normalize_h5_type (mytype));
		TRY (hdf5_close_type (mytype));
	}
	TRY (hdf5_close_attribute (attrib_id));
	H5_PRIV_API_RETURN (H5_SUCCESS);
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
	H5_PRIV_API_ENTER7 (h5_err_t,
			    "f=0x%p, mode=%d, "
			    "attrib_idx=%llu, attrib_name=%p, len_attrib_name=%llu, "
			    "attrib_type=%p, attrib_nelem=%p",
			    f, mode,
			    attrib_idx,
			    attrib_name,
			    len_attrib_name,
			    attrib_type,
			    attrib_nelem);

	if (mode != H5_ATTRIB_FILE) CHECK_TIMEGROUP( f );

	hid_t id;
	TRY (get_hdf5_obj_id(f, mode, &id));
	TRY (h5priv_get_attrib_info (id, attrib_idx, attrib_name, len_attrib_name,
				     attrib_type, attrib_nelem));
	H5_CORE_API_RETURN (H5_SUCCESS);
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
	H5_CORE_API_ENTER2 (h5_ssize_t,
			    "f=%p, mode=%d", f, mode);
	if (mode != H5_ATTRIB_FILE) CHECK_TIMEGROUP( f );
	hid_t id;
	TRY (get_hdf5_obj_id(f, mode, &id));
	H5_CORE_API_RETURN (hdf5_get_num_attribute (id));
}

