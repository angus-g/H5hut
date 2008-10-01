#ifndef __ATTRIBS_H
#define __ATTRIBS_H

h5_int64_t
h5_read_attrib (
	hid_t id,
	const char *attrib_name,
	void *attrib_value
	);

h5_int64_t
h5_write_attrib (
	hid_t id,
	const char *attrib_name,
	const hid_t attrib_type,
	const void *attrib_value,
	const hsize_t attrib_nelem
	);

h5_int64_t
h5_get_attrib_info (
	hid_t id,
	const h5_int64_t attrib_idx,
	char *attrib_name,
	const h5_int64_t len_attrib_name,
	h5_int64_t *attrib_type,
	h5_int64_t *attrib_nelem
	);

h5_int64_t
h5_get_num_attribs (
	h5_file_t *f,
	hid_t id
	);

#endif
