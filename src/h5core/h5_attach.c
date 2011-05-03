#include "h5core/h5_core.h"
#include "h5_core_private.h"
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>

h5_err_t
h5_add_attachment (
	h5_file_t* const f,
	const char* const fname
	) {
	H5_CORE_API_ENTER1 (h5_err_t, "fname=\"%s\"", fname);
	// allowed file modes: O_RDWR, O_WRONLY; O_APPEND
	if (f->mode == H5_O_RDONLY) {
		H5_PRIV_FUNC_LEAVE (
			h5priv_handle_file_mode_error (f->mode));
	}

	struct stat st;
        if (stat (fname, &st) < 0) {
		H5_CORE_API_LEAVE (
			h5_error (
				H5_ERR_HDF5,
				"Cannot stat file \"%s\"",
				fname));
	}
	hsize_t fsize = st.st_size;
	hsize_t write_length;
	char* buf = NULL;
	if (f->myproc == 0) {
		buf = malloc (fsize);
		write_length = fsize;
		int fd;
		if ((fd = open (fname, O_RDONLY)) < 0) {
			H5_CORE_API_LEAVE (
				h5_error (
					H5_ERR_HDF5,
					"Cannot open file \"%s\" for reading",
					fname));
		}
	again:
		if (read (fd, buf, fsize) < 0) {
			if (errno == EINTR) {
				goto again;
			} else {
				H5_CORE_API_LEAVE (
					h5_error (
						H5_ERR_HDF5,
						"Cannot read file \"%s\"",
						fname));
			}
		}
		if (close (fd) < 0) {
			H5_CORE_API_LEAVE (
				h5_error (
					H5_ERR_HDF5,
					"Cannot close file \"%s\"",
					fname));
		}

	} else {
		buf = malloc (1);
		write_length = 0;
	}

	hid_t loc_id;
	TRY (loc_id = h5priv_open_group (f, f->file, H5_ATTACHMENT));
	h5_err_t exists;
	TRY (exists = hdf5_link_exists (loc_id, fname));
	if (exists && (f->mode == H5_O_RDWR || f->mode == H5_O_WRONLY)) {
		// remove
	} else if (exists && f->mode == H5_O_APPEND) {
		H5_PRIV_FUNC_LEAVE (
			h5priv_handle_file_mode_error (f->mode));
	}		
	hid_t diskspace_id;
	TRY (diskspace_id = hdf5_create_dataspace (1, &fsize, &fsize));
	hid_t dataset_id;
	TRY (dataset_id = hdf5_create_dataset (loc_id,
					       fname, 
					       H5T_NATIVE_CHAR,
					       diskspace_id,
					       H5P_DEFAULT));
	hsize_t start = 0;
	TRY (hdf5_select_hyperslab_of_dataspace (
		     diskspace_id,
		     H5S_SELECT_SET,
		     &start,
		     NULL,
		     &write_length,
		     NULL));

	hid_t memspace_id;
	hsize_t max = H5S_UNLIMITED;
	TRY (memspace_id = hdf5_create_dataspace (1, &write_length, &max));
	TRY (hdf5_write_dataset (dataset_id, 
				 H5T_NATIVE_CHAR,
				 memspace_id,
				 diskspace_id,
				 f->xfer_prop,
				 buf));

	TRY (hdf5_close_dataspace (diskspace_id));
	TRY (hdf5_close_dataspace (memspace_id));
	TRY (hdf5_close_dataset (dataset_id));
	TRY (hdf5_close_group (loc_id));

	TRY (h5_free (buf));

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
	TRY (num = hdf5_get_num_datasets (group_id));
	TRY (hdf5_close_group (group_id));
	H5_CORE_API_RETURN (num);
}

h5_err_t
h5_get_attachment_info_by_idx (
	h5_file_t* const f,
	const h5_size_t idx,		// IN
	char* const fname,		// OUT
	h5_size_t len_fname,		// IN
	h5_size_t* const fsize		// OUT
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
		     fname, len_fname));

	if (fsize) {
		// get number of elements, do not change value on error
		h5_ssize_t ssize;
		TRY (ssize = hdf5_get_npoints_of_dataset_by_name (loc_id, fname));
		*fsize = ssize;
	}
	TRY (hdf5_close_group (loc_id));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5_get_attachment_info_by_name (
	h5_file_t* const f,
	const char* const fname,	// IN
	h5_size_t* const fsize		// OUT
	) {
	H5_CORE_API_ENTER2 (h5_err_t, "fname=\"%s\", fsize=0x%p", fname, fsize);

	hid_t loc_id;
        TRY (loc_id = open_attachments (f));
	if (loc_id < 0) {
		H5_CORE_API_LEAVE (H5_NOK);
	}
	if (fsize) {
		// get number of elements, do not change value on error
		h5_ssize_t ssize;
		TRY (ssize = hdf5_get_npoints_of_dataset_by_name (loc_id, fname));
		*fsize = ssize;
	}
	TRY (hdf5_close_group (loc_id));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5_get_attachment (
	h5_file_t* const f,
	const char* const fname
	) {
	H5_CORE_API_ENTER1 (h5_err_t, "fname=\"%s\"", fname);
	// allowed modes: O_RDWR, O_RDONLY; O_APPEND
	// forbidden modes: O_WRONLY
	if (f->mode == H5_O_WRONLY) {
		H5_PRIV_FUNC_LEAVE (
			h5priv_handle_file_mode_error (f->mode));
	}

	hid_t loc_id;
	TRY (loc_id = h5priv_open_group (f, f->file, H5_ATTACHMENT));
	h5_err_t exists;
	TRY (exists = hdf5_link_exists (loc_id, fname));
	if (f->mode == H5_O_WRONLY) {
		H5_PRIV_FUNC_LEAVE (
			h5priv_handle_file_mode_error (f->mode));
	} else if (!exists) {
		H5_PRIV_FUNC_LEAVE (
			h5_error (
				H5_ERR_H5,
				"Attachment \"%s\" doesn't exist", fname));
	}

	// read dataset
	hid_t dataset_id, diskspace_id;
	h5_ssize_t fsize;
	TRY (dataset_id = hdf5_open_dataset (loc_id, fname));
	TRY (diskspace_id = hdf5_get_dataset_space (dataset_id));
	TRY (fsize = hdf5_get_npoints_of_dataspace (diskspace_id));

	hsize_t read_length;
	char* buf = NULL;
	if (f->myproc == 0) {
		buf = malloc (fsize);
		read_length = fsize;

	} else {
		buf = malloc (1);
		read_length = 0;
	}

	hsize_t start = 0;
	TRY (hdf5_select_hyperslab_of_dataspace (
		     diskspace_id,
		     H5S_SELECT_SET,
		     &start,
		     NULL,
		     &read_length,
		     NULL));

	hid_t memspace_id;
	hsize_t max = H5S_UNLIMITED;
	TRY (memspace_id = hdf5_create_dataspace (1, &read_length, &max));
	TRY (hdf5_read_dataset (dataset_id, 
				 H5T_NATIVE_CHAR,
				 memspace_id,
				 diskspace_id,
				 f->xfer_prop,
				 buf));

	TRY (hdf5_close_dataspace (diskspace_id));
	TRY (hdf5_close_dataspace (memspace_id));
	TRY (hdf5_close_dataset (dataset_id));
	TRY (hdf5_close_group (loc_id));

	// write file
	if (f->myproc == 0) {
		int fd;
		if ((fd = open (fname, O_WRONLY|O_CREAT|O_TRUNC)) < 0) {
			H5_CORE_API_LEAVE (
				h5_error (
					H5_ERR_H5,
					"Error opening file \"%s\": %s",
					fname, strerror(errno)));
		}
		if (write (fd, buf, fsize) != fsize) {
			H5_CORE_API_LEAVE (
				h5_error (
					H5_ERR_H5,
					"Error writing to file \"%s\": %s",
					fname, strerror(errno)));
		}
		if (close (fd) < 0) {
			H5_CORE_API_LEAVE (
				h5_error (
					H5_ERR_H5,
					"Error closing file \"%s\": %s",
					fname, strerror(errno)));
		}
	}
	TRY (h5_free (buf));

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5_delete_attachment (
	h5_file_t* const f,
	const char* const fname
	) {
	H5_CORE_API_ENTER1 (h5_err_t, "fname=\"%s\"", fname);
	hid_t group_id;
        TRY (group_id = open_attachments (f));
	if (group_id < 0) {
		H5_CORE_API_LEAVE (H5_NOK);
	}
	TRY (hdf5_delete_link (group_id, fname, H5P_DEFAULT));
	TRY (hdf5_close_group (group_id));
	H5_CORE_API_RETURN (H5_SUCCESS);
}
