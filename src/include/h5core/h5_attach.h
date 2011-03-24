#ifndef __H5_ATTACH_H
#define __H5_ATTACH_H

h5_err_t
h5_write_attachment (
	h5_file_t* const f,
	const char* const name,
	const hid_t type,
	const char* const content,
	const h5_size_t size
	);

h5_ssize_t
h5_get_num_attachments (
	h5_file_t* const f
	);

h5_err_t
h5_get_attachment_info_by_idx (
	h5_file_t* const f,
	const h5_size_t idx,		// IN
	char* const name,		// OUT
	h5_size_t len_name,		// IN
	h5_int64_t* const type,		// OUT
	h5_size_t* const npoints	// OUT
	);

h5_err_t
h5_get_attachment_info_by_name (
	h5_file_t* const f,
	const char* const name,		// IN
	h5_int64_t* const type,		// OUT
	h5_size_t* const npoints	// OUT
	);

h5_err_t
h5_read_attachment (
	h5_file_t* const f,
	const char* const name,
	const hid_t type,
	void* const content
	);

h5_err_t
h5_delete_attachment (
	h5_file_t* const f,
	const char* const name
	);

#endif
