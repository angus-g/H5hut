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

	TRY( h5bpriv_open_field_group(f, field_name) );

	TRY( h5_write_attrib (
		f,
		H5_ATTRIB_FIELD,
		attrib_name,
		attrib_type,
		attrib_value,
		attrib_nelem) );

	return h5bpriv_close_field_group(f);
}

h5_ssize_t
h5b_get_num_field_attribs (
	h5_file_t *const f,			/*<! IN: file handle */
	const char *field_name			/*<! IN: field name */
	) {

	h5_ssize_t n;

	TRY( h5bpriv_open_field_group(f, field_name) );
	TRY( n = h5priv_get_num_hdf5_attribute(f, f->b->field_gid ) );
	TRY( h5bpriv_close_field_group(f) );

	return n;
}

/*!
  \ingroup h5block_attrib

  Query information about a attribute given by index \c attrib_idx and
  field name \c field_name. The function returns the name of the attribute,
  the type of the attribute and the number of elements of this type.

  \return \c H5_SUCCESS or error code
*/
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

	TRY( h5bpriv_open_field_group(f, field_name) );

	return h5_get_attrib_info (
		f,
		H5_ATTRIB_FIELD,
		attrib_idx,
		attrib_name,
		len_attrib_name,
		attrib_type,
		attrib_nelem );
}

