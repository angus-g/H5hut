#ifndef __H5_ATTACH_H
#define __H5_ATTACH_H

#ifdef __cplusplus
extern "C" {
#endif

h5_err_t
h5_add_attachment (
	h5_file_t* const f,
	const char* const name
	);

h5_ssize_t
h5_get_num_attachments (
	h5_file_t* const f
	);

h5_err_t
h5_get_attachment_info_by_idx (
	h5_file_t* const f,
	const h5_size_t idx,		// IN
	char* const fname,		// OUT
	h5_size_t len_fname,		// IN
	h5_size_t* const fsize		// OUT
	);

h5_err_t
h5_get_attachment_info_by_name (
	h5_file_t* const f,
	const char* const fname,	// IN
	h5_size_t* const fsize		// OUT
	);

h5_err_t
h5_get_attachment (
	h5_file_t* const f,
	const char* const fname
	);

h5_err_t
h5_delete_attachment (
	h5_file_t* const f,
	const char* const name
	);
#ifdef __cplusplus
}
#endif

#endif
