#ifndef __H5_ATTACHMENTS_H
#define __H5_ATTACHMENTS_H

#ifdef __cplusplus
extern "C" {
#endif

h5_ssize_t
H5GetNumAttachments (
	h5_file_t* const f
	);

h5_err_t
H5GetAttachmentInfoByIdx (
	h5_file_t* const f,
	const h5_size_t idx,		// IN
	char* const fname,		// OUT
	h5_size_t len_fname,		// IN
	h5_size_t* const fsize		// OUT
	);

h5_err_t
H5GetAttachmentInfoByName (
	h5_file_t* const f,
	char* const fname,
	h5_size_t* const fsize
	);

h5_err_t
H5AddAttachment (
	h5_file_t* const f,
	const char* fname
	);

h5_err_t
H5GetAttachment (
	h5_file_t* const f,
	const char* name
	);

h5_err_t
H5DeleteAttachment (
	h5_file_t* const f,
	const char* const name
	);

#ifdef __cplusplus
}
#endif

#endif
