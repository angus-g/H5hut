#include "h5core/h5_core.h"
#include "h5_core_private.h"

static hid_t
open_space_all (
	h5_file_t* const f,
	const hid_t dataset_id
	) {
	UNUSED_ARGUMENT (f);
	UNUSED_ARGUMENT (dataset_id);
	return H5S_ALL;
}

h5_err_t
h5_add_attachment (
	h5_file_t* const f,
	const char* const name,
	h5_int64_t* const type,
	const char* const content,
	const h5_size_t size
	) {
	H5_CORE_API_ENTER2 (h5_ssize_t,
			    "name=\"%s\, type=%d, content=0x%p",
			    name, type, content);
	// write only on proc 0
	if (f->myproc != 0) {
		H5_CORE_API_LEAVE (H5_SUCCESS);
	}
	TRY (group_id = h5priv_open_group (f, f->file, H5_ATTACHMENT));
        TRY (exists = hdf5_link_exists (group_id, name));
	if (exists > 0) {
		// warn user on overwrite
		h5_warn ("Dataset %s/%s already exists",
			 hdf5_get_objname(group_id), name);

	}
	h5_dsinfo_t dsinfo;
	strncpy (dsinfo.name, name, sizeof (dsinfo.name) - 1);
	dsinfo.rank = 1;
	dsinfo.dims[0] = size;
	dsinfo.max_dims[0] = size;
	dsinfo.type_id = type;
	dsinfo.create_prop = f->create_prop;
	dsinfo.access_prop = f->access_prop;
	TRY (h5priv_write_dataset_by_name (
		f,
		group_id,
		&dsinfo,
		open_space_all,
		open_space_all,
		content));
	TRY (hdf5_close_group (group_id));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

static inline hid_t
open_attachments (
	h5_file_t* const f
	) {
	h5_err_t exists;
        TRY (exists = hdf5_link_exists(f->file, H5_ATTACHMENT));
	if (exists > 0) {
		return hdf5_open_group (f->file, H5_ATTACHMENT);
	} else {
		return h5_warn ("No attachment group in file");
	}
}

h5_ssize_t
h5_get_num_attachments (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER0 (h5_ssize_t);
	h5_ssize_t num = 0;
	hid_t group_id;
        TRY (group_id = open_attachments (f));
	if (group_id < 0) {
		H5_CORE_API_LEAVE (0);
	}
	TRY (num = hdf5_get_num_datasets (f->file));
	TRY (hdf5_close_group (group_id));
	H5_CORE_API_RETURN (num);
}

h5_err_t
h5_get_attachment_info_by_idx (
	h5_file_t* const f,
	const size_t idx,		// IN
	char* const name,		// OUT
	size_t l_name,			// IN
	h5_int64_t* const type,		// OUT
	h5_size_t* const npoints	// OUT
	) {
	H5_CORE_API_ENTER0 (h5_err_t);
        TRY (group_id = open_attachments (f));
	if (group_id < 0) {
		H5_CORE_API_LEAVE (0);
	}
	TRY (hdf5_get_dataset_info_by_idx (group_id, idx, name, l_name, type, npoints));
	TRY (hdf5_close_group (group_id));
	H5_CORE_API_RETURN (num);
}

h5_err_t
h5_get_attachment_info_by_name (
	h5_file_t* const f,
	const char* const name,	// IN
	h5_int64_t* const type,	// OUT
	h5_size_t* const nelem	// OUT
	) {
	H5_CORE_API_ENTER0 (h5_err_t);
	hid_t group_id;
        TRY (group_id = open_attachments (f));
	if (group_id < 0) {
		H5_CORE_API_LEAVE (H5_NOK);
	}
	TRY (hdf5_get_dataset_info_by_name (group_id, idx, name, type, npoints));
	TRY (hdf5_close_group (group_id));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5_get_attachment (
	h5_file_t* const f,
	const char* const name,
	const hid_t type,
	void* const content,
	) {
	H5_CORE_API_ENTER0 (h5_err_t);
	H5_CORE_API_RETURN (H5_SUCCESS);
}


h5_err_t
h5_remove_attachment (
	h5_file_t* const f,
	const char* const name
	) {
	H5_CORE_API_ENTER0 (h5_err_t);
	hid_t group_id;
        TRY (group_id = open_attachments (f));
	if (group_id < 0) {
		H5_CORE_API_LEAVE (H5_NOK);
	}
	TRY (hdf5_delete_link (group_id, name, H5P_DEFAULT));
	TRY (hdf5_close_group (group_id));
	H5_CORE_API_RETURN (H5_SUCCESS);
}
