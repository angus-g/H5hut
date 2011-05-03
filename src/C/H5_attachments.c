#include "h5core/h5_core.h"
#include "H5hut.h"

h5_ssize_t
H5GetNumAttachments (
	h5_file_t* const f	/*!< [in] Handle to open file */
	) {
	H5_API_ENTER1 (h5_ssize_t, "f=0x%p", f);
	H5_API_RETURN (h5_get_num_attachments (f));
}

h5_err_t
H5GetAttachmentInfoByIdx (
	h5_file_t* const f,
	const h5_size_t idx,		// IN
	char* const fname,		// OUT
	h5_size_t len_fname,		// IN
	h5_size_t* const fsize		// OUT
	) {
	H5_API_ENTER4 (h5_err_t,
		       "idx=%llu, fname=0x%p, len_fname=%llu, fsize=0x%p",
		       (long long unsigned)idx,
		       fname, (long long unsigned)len_fname,
		       fsize);
	H5_API_RETURN (h5_get_attachment_info_by_idx (
			       f, idx, fname, len_fname, fsize));
}

h5_err_t
H5GetAttachmentInfoByName (
	h5_file_t* const f,
	char* const fname,		// OUT
	h5_size_t* const fsize		// OUT
	) {
	H5_API_ENTER2 (h5_err_t, "fname=\"%s\", fsize=0x%p", fname, fsize);
	H5_API_RETURN (h5_get_attachment_info_by_name (
			       f, fname, fsize));
}

h5_err_t
H5AddAttachment (
	h5_file_t* const f,		/*!< [in] Handle to open file */
	const char* fname		/*!< [in] Name of file to attach */
	) {
	H5_API_ENTER1 (h5_err_t, "fname=\"%s\"", fname);
	H5_API_RETURN (h5_add_attachment (f, fname));
}

h5_err_t
H5GetAttachment (
	h5_file_t* const f,	/*!< [in] Handle to open file */
	const char* fname	/*!< [in] Name of attachment */
	) {
	H5_API_ENTER1 (h5_err_t, "fname=\"%s\"", fname);
	H5_API_RETURN (h5_get_attachment (f, fname));
}

h5_err_t
H5DeleteAttachment (
	h5_file_t* const f,
	const char* const fname
	) {
	H5_API_ENTER1 (h5_err_t, "fname=\"%s\"", fname);
	H5_API_RETURN (h5_delete_attachment (f, fname));
}
