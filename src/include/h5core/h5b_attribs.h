#ifndef __H5B_ATTRIBS_H
#define __H5B_ATTRIBS_H

h5_err_t
h5_write_field_attrib (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const hid_t attrib_type,		/*!< IN: attribute type */
	const void *attrib_value,		/*!< IN: attribute value */
	const h5_int64_t attrib_nelem		/*!< IN: number of elements */
	);

h5_err_t
h5_read_field_attrib (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const h5_int64_t attrib_type,		/*!< IN: attribute type */
	void *buffer		                /*!< OUT: attribute value */
	);

h5_ssize_t
h5b_get_num_field_attribs (
	h5_file_t *const f,			/*<! IN: file handle */
	const char *field_name			/*<! IN: field name */
	);

h5_err_t
h5b_get_field_attrib_info (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const h5_size_t attrib_idx,		/*!< IN: attribute index */
	char *attrib_name,			/*!< OUT: attribute name */
	const h5_size_t len_attrib_name,	/*!< IN: buffer size */
	h5_int64_t *attrib_type,		/*!< OUT: attribute type */
	h5_size_t *attrib_nelem			/*!< OUT: number of elements */
	);

#endif
