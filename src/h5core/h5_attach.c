#include "h5core/h5_core.h"
#include "h5_core_private.h"
#include <string.h>

static hid_t
open_space_all (
	h5_file_t* const f,
	const hid_t dataset_id
	) {
	UNUSED_ARGUMENT (f);
	UNUSED_ARGUMENT (dataset_id);
	return H5S_ALL;
}

/*
  experimental code, will replace  h5priv_write_dataset_by_name() somewhen
*/
write_dataset_by_name (
 	h5_file_t* const f,
	const hid_t loc_id,
	h5_dsetobject_t* dset
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	h5_info ("Writing dataset %s/%s.",
		 hdf5_get_objname (loc_id), dset->name);

	h5_err_t exists;
	TRY (exists = hdf5_link_exists (loc_id, dsinfo->name));
	if ((exists > 0) && ((f->mode==H5_O_WRONLY) || (f->mode==H5_O_APPEND))) {
		h5_warn ("Dataset %s/%s already exist.",
			 hdf5_get_objname (loc_id), dsinfo->name);
		H5_PRIV_API_LEAVE (h5priv_handle_file_mode_error(f->mode));
	}

	/*
	  open/create dataset
	*/
	hid_t dset_id;
	hid_t dataspace_id;
	hid_t diskspace_id;
	hid_t memspace_id;

	if (exists) {
		/* overwrite dataset */
		TRY (dset_id = hdf5_open_dataset (loc_id, dsinfo->name));
		TRY (dataspace_id = hdf5_get_dataset_space (dset_id));
		TRY (hdf5_set_dataset_extent (dset_id, dsinfo->dims));
		/* exten dataset? */
	} else {
		/* create dataset */
		TRY (dataspace_id = hdf5_create_dataspace (
			     dsinfo->rank,
			     dsinfo->dims,
			     dsinfo->max_dims));
		TRY (dset_id = hdf5_create_dataset (
			     loc_id,
			     dsinfo->name,
			     dsinfo->type_id,
			     dataspace_id,
			     dsinfo->create_prop));
	}
	TRY (memspace_id = (*set_memspace)(f, 0));
	TRY (diskspace_id = (*set_diskspace)(f, dataspace_id));
#ifdef PARALLEL_IO
	TRY (h5_start_throttle (f));
#endif
	TRY (hdf5_write_dataset (
		     dset_id,
		     dsinfo->type_id,
		     memspace_id,
		     diskspace_id,
		     f->xfer_prop,
		     data));
#ifdef PARALLEL_IO
	TRY (h5_end_throttle (f));
#endif
	TRY (hdf5_close_dataspace (diskspace_id));
	TRY (hdf5_close_dataspace (memspace_id));
	TRY (hdf5_close_dataset (dset_id));
	f->empty = 0;

	H5_PRIV_API_RETURN (H5_SUCCESS);
}

/*
  - on all procs create dataset with given size of content
  - create diskspace
  - set hyperslap on diskspace:
     - select all on proc 0
     - select 0 byte on proc > 0
  - create memspace with size of selection
  - write 
  - close objects
*/
h5_err_t
h5_write_attachment (
	h5_file_t* const f,
	const char* const name,
	const hid_t type,
	const char* const content,
	const h5_size_t size
	) {
	H5_CORE_API_ENTER4 (h5_ssize_t,
			    "name=\"%s\", type=%ld, content=0x%p, size=%llu",
			    name, (long)type, content, (unsigned long long)size);
	h5_dsinfo_t dsinfo;
	strncpy (dsinfo.name, name, sizeof (dsinfo.name) - 1);
	dsinfo.rank = 1;
	dsinfo.type_id = type;
	dsinfo.create_prop = f->create_prop;
	dsinfo.access_prop = f->access_prop;

	hid_t group_id;
	TRY (group_id = h5priv_open_group (f, f->file, H5_ATTACHMENT));

	h5_err_t exists;
        TRY (exists = hdf5_link_exists (group_id, name));
	// write only on proc 0
	if (f->myproc != 0) {
		// write on proc 0 only
		dsinfo.dims[0] = size;
		dsinfo.max_dims[0] = size;
		if (exists > 0) {
			h5_warn ("Dataset %s/%s already exists",
				 hdf5_get_objname(group_id), name);
		}
	} else {
		// write 0 bytes on all other procs
		dsinfo.dims[0] = 0;
		dsinfo.max_dims[0] = 0;
	}
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
	h5_err_t exists = hdf5_link_exists (f->file, H5_ATTACHMENT);
	if (exists > 0) {
		return hdf5_open_group (f->file, H5_ATTACHMENT);
	} else if (exists == 0) {
		return h5_warn ("No attachment group in file");
	}
	return exists;
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
	const h5_size_t idx,		// IN
	char* const name,		// OUT
	h5_size_t len_name,		// IN
	h5_int64_t* const type,		// OUT
	h5_size_t* const npoints	// OUT
	) {
	H5_CORE_API_ENTER0 (h5_err_t);
	hid_t loc_id;
        TRY (loc_id = open_attachments (f));
	if (loc_id < 0) { // no attachment group in file
		H5_CORE_API_LEAVE (0);
	}
	TRY (hdf5_get_name_of_dataset_by_idx (
		     loc_id,
		     idx,
		     name, len_name));

	if (npoints) {
		// get number of elements, do not change value on error
		h5_ssize_t np;
		TRY (np = hdf5_get_npoints_of_dataset_by_name (loc_id, name));
		*npoints = np;
	}
	if (type) {
		// get normalized data type, do not change value on error
		h5_int64_t t;
		TRY (t = h5_get_dataset_type (loc_id, name));
		*type = t;
	}
	TRY (hdf5_close_group (loc_id));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5_get_attachment_info_by_name (
	h5_file_t* const f,
	const char* const name,		// IN
	h5_int64_t* const type,		// OUT
	h5_size_t* const npoints	// OUT
	) {
	H5_CORE_API_ENTER3 (h5_err_t,
			    "name=\"%s\", type=0x%p, npoints=0x%p",
			    name, type, npoints);

	hid_t loc_id;
        TRY (loc_id = open_attachments (f));
	if (loc_id < 0) {
		H5_CORE_API_LEAVE (H5_NOK);
	}
	if (npoints) {
		h5_ssize_t np;
		TRY (np = hdf5_get_npoints_of_dataset_by_name (loc_id, name));
		*npoints = np;
	}

	if (type) {
		h5_int64_t t;
		TRY (t = h5_get_dataset_type (loc_id, name));
		*type = t;
	}
	TRY (hdf5_close_group (loc_id));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5_read_attachment (
	h5_file_t* const f,
	const char* const name,
	const hid_t type,
	void* const content
	) {
	H5_CORE_API_ENTER0 (h5_err_t);
	hid_t group_id;
        TRY (group_id = open_attachments (f));
	h5_dsinfo_t dsinfo;
	strncpy (dsinfo.name, name, sizeof (dsinfo.name) - 1);
	dsinfo.type_id = type;
	TRY (h5priv_read_dataset_by_name (
		f,
		group_id,
		&dsinfo,
		open_space_all,
		open_space_all,
		content));
	TRY (hdf5_close_group (group_id));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5_delete_attachment (
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
