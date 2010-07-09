#ifndef __H5_ATTRIBS_H
#define __H5_ATTRIBS_H

#define H5_ATTRIB_FILE 0
#define H5_ATTRIB_STEP 1
#define H5_ATTRIB_FIELD 2

h5_err_t
h5_read_attrib (
	h5_file_t * const f,
	const char mode,
	const char *attrib_name,
	const hid_t attrib_type,
	void *attrib_value
	);

h5_err_t
h5_write_attrib (
	h5_file_t * const f,
	const char mode,
	const char *attrib_name,
	const hid_t attrib_type,
	const void *attrib_value,
	const hsize_t attrib_nelem
	);

h5_err_t
h5_get_attrib_info (
	h5_file_t *const f,
	const char mode,
	const h5_size_t attrib_idx,
	char *attrib_name,
	const h5_size_t len_attrib_name,
	h5_int64_t *attrib_type,
	h5_size_t *attrib_nelem
	);

h5_ssize_t
h5_get_num_attribs (
	h5_file_t *const f,
	const char mode
	);

#endif
