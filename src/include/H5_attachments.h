#ifndef __H5_ATTACHMENTS_H
#define __H5_ATTACHMENTS_H

h5_ssize_t
H5GetNumAttachments (
	h5_file_t* const f
	);

h5_err_t
H5GetAttachmentInfoByIdx (
	h5_file_t* const f,
	const h5_size_t idx,
	char* const name,
	h5_size_t len_name,
	h5_int64_t* const type,
	h5_size_t* const npoints
	);

h5_err_t
H5GetAttachmentInfoByName (
	h5_file_t* const f,
	char* const name,
	h5_int64_t* const type,
	h5_size_t* const npoints
	);

h5_err_t
H5WriteAttachmentBitstream (
	h5_file_t* const f,
	const char* name,
	const void* const content,
	const h5_size_t size
	);

h5_err_t
H5WriteAttachmentFloat32 (
	h5_file_t* const f,
	const char* name,
	const void* const content,
	const h5_size_t size
	);

h5_err_t
H5WriteAttachmentFloat64 (
	h5_file_t* const f,
	const char* name,
	const void* const content,
	const h5_size_t size
	);

h5_err_t
H5WriteAttachmentInt32 (
	h5_file_t* const f,
	const char* name,
	const void* const content,
	const h5_size_t size
	);

h5_err_t
H5WriteAttachmentInt64 (
	h5_file_t* const f,
	const char* name,
	const void* const content,
	const h5_size_t size
	);

h5_err_t
H5ReadAttachmentBitstream (
	h5_file_t* const f,
	const char* name,
	void* const content
	);

h5_err_t
H5ReadAttachmentFloat32 (
	h5_file_t* const f,
	const char* name,
	void* const content
	);

h5_err_t
H5ReadAttachmentFloat64 (
	h5_file_t* const f,
	const char* name,
	void* const content
	);

h5_err_t
H5ReadAttachmentInt32 (
	h5_file_t* const f,
	const char* name,
	void* const content
	);

h5_err_t
H5ReadAttachmentInt64 (
	h5_file_t* const f,
	const char* name,
	void* const content
	);

h5_err_t
H5DeleteAttachment (
	h5_file_t* const f,
	const char* const name
	);

#endif
