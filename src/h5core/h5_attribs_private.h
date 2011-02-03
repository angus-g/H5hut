#ifndef __H5_ATTRIBS_PRIVATE_H
#define __H5_ATTRIBS_PRIVATE_H

h5_err_t
h5priv_read_attrib (
	h5_file_t* const f,
	const hid_t id,
	const char* attrib_name,
	const hid_t attrib_type,
	void* const attrib_value
	);

h5_err_t
h5priv_write_attrib (
	h5_file_t* const f,
	const hid_t id,
	const char* attrib_name,
	const hid_t attrib_type,
	const void* attrib_value,
	const hsize_t attrib_nelem
	);

h5_err_t
h5priv_get_attrib_info (
	h5_file_t* const f,
	const hid_t id,
	const h5_size_t attrib_idx,
	char* attrib_name,
	const h5_size_t len_attrib_name,
	h5_int64_t* attrib_type,
	h5_size_t* attrib_nelem
	);
#endif
