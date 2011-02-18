#include "h5core/h5_core.h"
#include "h5_core_private.h"

h5_err_t
h5_write_field_attrib (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const hid_t attrib_type,		/*!< IN: attribute type */
	const void *attrib_value,		/*!< IN: attribute value */
	const h5_int64_t attrib_nelem		/*!< IN: number of elements */
	) {
	H5_CORE_API_ENTER (h5_err_t);
	TRY( h5bpriv_create_field_group(f, field_name) );

	TRY( h5_write_attrib (
		f,
		H5_ATTRIB_FIELD,
		attrib_name,
		attrib_type,
		attrib_value,
		attrib_nelem) );

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5_read_field_attrib (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const h5_int64_t attrib_type,		/*!< IN: attribute type */
	void *buffer		                /*!< OUT: attribute value */
	) {
	H5_CORE_API_ENTER (h5_err_t);
	TRY( h5bpriv_open_field_group(f, field_name) );

	TRY( h5_read_attrib (
		f,
		H5_ATTRIB_FIELD,
		attrib_name,
		attrib_type,
		buffer) );

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_ssize_t
h5b_get_num_field_attribs (
	h5_file_t *const f,			/*<! IN: file handle */
	const char *field_name			/*<! IN: field name */
	) {
	H5_CORE_API_ENTER (h5_ssize_t);

	TRY (h5bpriv_open_field_group(f, field_name));

	H5_CORE_API_RETURN (hdf5_get_num_attribute (f->b->field_gid));
}

h5_err_t
h5b_get_field_attrib_info (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const h5_size_t attrib_idx,		/*!< IN: attribute index */
	char *attrib_name,			/*!< OUT: attribute name */
	const h5_size_t len_attrib_name,	/*!< IN: buffer size */
	h5_int64_t *attrib_type,		/*!< OUT: attribute type */
	h5_size_t *attrib_nelem			/*!< OUT: number of elements */
	) {
	H5_CORE_API_ENTER (h5_err_t);
	TRY (h5bpriv_open_field_group(f, field_name));

	H5_CORE_API_RETURN (
		h5_get_attrib_info (
			f,
			H5_ATTRIB_FIELD,
			attrib_idx,
			attrib_name,
			len_attrib_name,
			attrib_type,
			attrib_nelem));
}

