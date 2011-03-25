#include "h5core/h5_core.h"
#include "H5hut.h"

h5_ssize_t
H5GetNumAttachments (
	h5_file_t* const f	/*!< [in] Handle to open file */
	) {
	H5_API_ENTER (h5_ssize_t);
	H5_API_RETURN (h5_get_num_attachments (f));
}

h5_err_t
H5GetAttachmentInfoByIdx (
	h5_file_t* const f,
	const h5_size_t idx,		// IN
	char* const name,		// OUT
	h5_size_t len_name,		// IN
	h5_int64_t* const type,		// OUT
	h5_size_t* const npoints	// OUT
	) {
	H5_API_ENTER5 (h5_err_t,
		       "idx=%llu, name=0x%p, len_name=%llu, type=0x%p, npoints=0x%p",
		       (long long unsigned)idx,
		       name, (long long unsigned)len_name,
		       type, npoints);
	H5_API_RETURN (h5_get_attachment_info_by_idx (
			       f, idx, name, len_name, type, npoints));
}

h5_err_t
H5GetAttachmentInfoByName (
	h5_file_t* const f,
	char* const name,		// OUT
	h5_int64_t* const type,		// OUT
	h5_size_t* const npoints	// OUT
	) {
	H5_API_ENTER3 (h5_err_t, "name=\"%s\", type=0x%p, npoints=0x%p",
		       name, type, npoints);
	H5_API_RETURN (h5_get_attachment_info_by_name (
			       f, name, type, npoints));
}

h5_err_t
H5WriteAttachmentBitstream (
	h5_file_t* const f,		/*!< [in] Handle to open file */
	const char* name,		/*!< [in] Name of attribute to create */
	const void* const content,	/*!< [in] Value of attribute */ 
	const h5_size_t size
	) {
	H5_API_ENTER3 (h5_err_t, "name=\"%s\", content=0x%p, size=%llu",
		       name, content, (unsigned long long)size);
	H5_API_RETURN (h5_write_attachment (f, name, H5T_NATIVE_CHAR, content, size));
}

h5_err_t
H5WriteAttachmentFloat32 (
	h5_file_t* const f,		/*!< [in] Handle to open file */
	const char* name,		/*!< [in] Name of attribute to create */
	const void* const content,	/*!< [in] Value of attribute */ 
	const h5_size_t size
	) {
	H5_API_ENTER3 (h5_err_t, "name=\"%s\", content=0x%p, size=%llu",
		       name, content, (unsigned long long)size);
	H5_API_RETURN (h5_write_attachment (f, name, H5T_NATIVE_FLOAT, content, size));
}

h5_err_t
H5WriteAttachmentFloat64 (
	h5_file_t* const f,		/*!< [in] Handle to open file */
	const char* name,		/*!< [in] Name of attribute to create */
	const void* const content,	/*!< [in] Value of attribute */ 
	const h5_size_t size
	) {
	H5_API_ENTER3 (h5_err_t, "name=\"%s\", content=0x%p, size=%llu",
		       name, content, (unsigned long long)size);
	H5_API_RETURN (h5_write_attachment (f, name, H5T_NATIVE_DOUBLE, content, size));
}

h5_err_t
H5WriteAttachmentInt32 (
	h5_file_t* const f,		/*!< [in] Handle to open file */
	const char* name,		/*!< [in] Name of attribute to create */
	const void* const content,	/*!< [in] Value of attribute */ 
	const h5_size_t size
	) {
	H5_API_ENTER3 (h5_err_t, "name=\"%s\", content=0x%p, size=%llu",
		       name, content, (unsigned long long)size);
	H5_API_RETURN (h5_write_attachment (f, name, H5T_NATIVE_INT32, content, size));
}

h5_err_t
H5WriteAttachmentInt64 (
	h5_file_t* const f,		/*!< [in] Handle to open file */
	const char* name,		/*!< [in] Name of attribute to create */
	const void* const content,	/*!< [in] Value of attribute */ 
	const h5_size_t size
	) {
	H5_API_ENTER3 (h5_err_t, "name=\"%s\", content=0x%p, size=%llu",
		       name, content, (unsigned long long)size);
	H5_API_RETURN (h5_write_attachment (f, name, H5T_NATIVE_INT64, content, size));
}

h5_err_t
H5ReadAttachmentBitstream (
	h5_file_t* const f,	/*!< [in] Handle to open file */
	const char* name,	/*!< [in] Name of attribute to create */
	void* const content	/*!< [in] Value of attribute */ 
	) {
	H5_API_ENTER2 (h5_err_t, "name=\"%s\", content=0x%p", name, content);
	H5_API_RETURN (h5_read_attachment (f, name, H5T_NATIVE_CHAR, content));
}

h5_err_t
H5ReadAttachmentFloat32 (
	h5_file_t* const f,	/*!< [in] Handle to open file */
	const char* name,	/*!< [in] Name of attribute to create */
	void* const content	/*!< [in] Attachment */ 
	) {
	H5_API_ENTER2 (h5_err_t, "name=\"%s\", content=0x%p", name, content);
	H5_API_RETURN (h5_read_attachment (f, name, H5T_NATIVE_FLOAT, content));
}

h5_err_t
H5ReadAttachmentFloat64 (
	h5_file_t* const f,	/*!< [in] Handle to open file */
	const char* name,	/*!< [in] Name of attribute to create */
	void* const content	/*!< [out] Attachment */ 
	) {
	H5_API_ENTER2 (h5_err_t, "name=\"%s\", content=0x%p", name, content);
	H5_API_RETURN (h5_read_attachment (f, name, H5T_NATIVE_DOUBLE, content));
}

h5_err_t
H5ReadAttachmentInt32 (
	h5_file_t* const f,	/*!< [in] Handle to open file */
	const char* name,	/*!< [in] Name of attribute to create */
	void* const content	/*!< [out] Attachment */ 
	) {
	H5_API_ENTER2 (h5_err_t, "name=\"%s\", content=0x%p", name, content);
	H5_API_RETURN (h5_read_attachment (f, name, H5T_NATIVE_INT32, content));
}

h5_err_t
H5ReadAttachmentInt64 (
	h5_file_t* const f,	/*!< [in] Handle to open file */
	const char* name,	/*!< [in] Name of attribute to create */
	void* const content	/*!< [out] Attachment */ 
	) {
	H5_API_ENTER2 (h5_err_t, "name=\"%s\", content=0x%p", name, content);
	H5_API_RETURN (h5_read_attachment (f, name, H5T_NATIVE_INT64, content));
}

h5_err_t
H5DeleteAttachment (
	h5_file_t* const f,
	const char* const name
	) {
	H5_API_ENTER1 (h5_err_t, "name=\"%s\"", name);
	H5_API_RETURN (h5_delete_attachment (f, name));
}
