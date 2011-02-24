#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"


/*!
  Write data to dataset.

  - Open/Create dataset
  - set hyperslabs for disk and memory via callback functions
  - Write data
  - Close dataset  
 */
h5_err_t
h5priv_write_dataset_by_name (
 	h5_file_t* const f,
	const hid_t loc_id,
	h5_dsinfo_t* dsinfo,
	hid_t (*set_memspace)(h5_file_t*,hid_t),
	hid_t (*set_diskspace)(h5_file_t*,hid_t),
	const void* const data
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	h5_info ("Writing dataset %s/%s.",
		 h5_get_objname (loc_id), dsinfo->name);

	h5_err_t exists;
	TRY (exists = hdf5_link_exists (loc_id, dsinfo->name));
	if ((exists > 0) && ((f->mode==H5_O_WRONLY) || (f->mode==H5_O_APPEND))) {
		h5_warn ("Dataset %s/%s already exist.",
			 h5_get_objname (loc_id), dsinfo->name);
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

h5_err_t
h5priv_read_dataset (
	h5_file_t* const f,
	hid_t dset_id,
	h5_dsinfo_t* dsinfo,
	hid_t (*set_mspace)(h5_file_t*,hid_t),
	hid_t (*set_dspace)(h5_file_t*,hid_t),
	void* const data
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	hid_t mspace_id;
	hid_t dspace_id;

	TRY (mspace_id = (*set_mspace)(f, dset_id));
	TRY (dspace_id = (*set_dspace)(f, dset_id));
#ifdef PARALLEL_IO
	TRY (h5_start_throttle (f));
#endif
	TRY (hdf5_read_dataset (
		     dset_id,
		     dsinfo->type_id,
		     mspace_id,
		     dspace_id,
		     f->xfer_prop,
		     data));
#ifdef PARALLEL_IO
	TRY (h5_end_throttle (f));
#endif
	TRY (hdf5_close_dataspace (dspace_id));
	TRY (hdf5_close_dataspace (mspace_id));

	H5_PRIV_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5priv_read_dataset_by_name (
	h5_file_t* const f,
	hid_t loc_id,
	h5_dsinfo_t* dsinfo,
	hid_t (*set_mspace)(h5_file_t*,hid_t),
	hid_t (*set_dspace)(h5_file_t*,hid_t),
	void* const data
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	hid_t dset_id;
	TRY (dset_id = hdf5_open_dataset (loc_id, dsinfo->name));
	TRY (h5priv_read_dataset (f, dset_id, dsinfo, set_mspace, set_dspace, data));
	TRY (hdf5_close_dataset (dset_id));

	H5_PRIV_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5priv_close_step (
	h5_file_t* const f
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	if (f->step_gid <= 0)
		H5_PRIV_API_LEAVE (H5_SUCCESS);
	TRY (h5tpriv_close_step (f));
	TRY (hdf5_close_group (f->step_gid));

	f->step_gid = -1;

	H5_PRIV_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5_set_step (
	h5_file_t* const f,		/*!< [in]  Handle to open file */
	const h5_id_t step_idx		/*!< [in]  Step to set. */
	) {
	H5_CORE_API_ENTER (h5_err_t);
	TRY (h5priv_close_step (f));
	f->step_idx = step_idx;

	sprintf (
		f->step_name,
		"%s#%0*lld",
		f->prefix_step_name, f->width_step_idx, (long long) f->step_idx);
	h5_info (
		"Proc[%d]: Open step #%lld for file %lld",
		f->myproc,
		(long long)f->step_idx,
		(long long)(size_t) f);
	TRY (f->step_gid = h5priv_open_group (f, f->file, f->step_name));
	TRY (h5tpriv_init_step (f));

	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*!
   Normalize HDF5 type
*/
h5_int64_t
h5_normalize_h5_type (
	hid_t type
	) {
	H5_CORE_API_ENTER (h5_int64_t);
	H5T_class_t tclass;
	int size;
	TRY (tclass = H5Tget_class (type));
	TRY (size = H5Tget_size (type));

	switch (tclass){
	case H5T_INTEGER:
		if (size==8) {
			H5_CORE_API_LEAVE (H5_INT64_T);
		}
		else if (size==4) {
		        H5_CORE_API_LEAVE (H5_INT32_T);
		}
		break;
	case H5T_FLOAT:
		if ( size==8 ) {
			H5_CORE_API_LEAVE (H5_FLOAT64_T);
		}
		else if ( size==4 ) {
			H5_CORE_API_LEAVE (H5_FLOAT32_T);
		}
		break;
	case H5T_STRING:
		H5_CORE_API_LEAVE (H5_STRING_T);
	default:
		; /* NOP */
	}
	H5_CORE_API_RETURN (h5_warn ("Unknown type %d", (int)type));
}

h5_int64_t
h5_get_dataset_type(
	h5_file_t* const f,
	hid_t group_id,
	const char* dset_name
	) {
	UNUSED_ARGUMENT (f);
	H5_CORE_API_ENTER (h5_int64_t);
	hid_t dset_id;
	hid_t hdf5_type;
	h5_int64_t type;
	TRY (dset_id = hdf5_open_dataset (group_id, dset_name));
	TRY (hdf5_type = hdf5_get_dataset_type (dset_id));
	TRY (type = h5_normalize_h5_type (hdf5_type));
	TRY (hdf5_close_type (hdf5_type));
	TRY (hdf5_close_dataset (dset_id));

	H5_CORE_API_RETURN (type);
}

h5_err_t
h5_has_step (
	h5_file_t* const f,		/*!< [in]  Handle to open file */
	const h5_id_t step		/*!< [in]  Step number to query */
	) {
	H5_CORE_API_ENTER (h5_err_t);
	char name[2*H5_STEPNAME_LEN];
	sprintf (name,
		"%s#%0*lld",
		f->prefix_step_name, f->width_step_idx, (long long)step);
	H5_CORE_API_RETURN (hdf5_link_exists(f->file, name));
}

h5_err_t
h5_normalize_dataset_name (
	const char *name,
	char *name2
	) {
	H5_CORE_API_ENTER (h5_err_t);
	if ( strlen(name) > H5_DATANAME_LEN ) {
		strncpy ( name2, name, H5_DATANAME_LEN-1 );
		name2[H5_DATANAME_LEN-1] = '\0';
		h5_warn ("Truncated name '%s' to '%s'.", name, name2);
	} else {
		strcpy ( name2, name );
	}

	if ( strcmp( name2, H5_BLOCKNAME ) == 0 ) {
		H5_CORE_API_LEAVE (
			h5_error (
				H5_ERR_INVAL,
				"Can't create dataset or field with name '%s'"
				" because it is reserved by H5Block.",
				H5_BLOCKNAME));
	}
	H5_CORE_API_RETURN (H5_SUCCESS);
}

#ifdef PARALLEL_IO
h5_err_t
h5_set_throttle (
	h5_file_t* const f,
	const int factor
	) {
	H5_CORE_API_ENTER (h5_err_t);
	if ( (f->mode & H5_VFD_INDEPENDENT) || (f->mode & H5_VFD_MPIPOSIX) ) {
		f->throttle = factor;
		h5_info ("Throttling enabled with factor = %d", f->throttle );
	} else {
		h5_warn ("Throttling is only permitted with the MPI-POSIX "
			"or MPI-IO Independent VFD." );
	}

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5_start_throttle (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER (h5_err_t);
	if (f->throttle > 0) {
		int token = 1;
		h5_info ("Throttling with factor = %d",	f->throttle);
		if (f->myproc / f->throttle > 0) {
			h5_debug ("[%d] throttle: waiting on token from %d",
				  f->myproc, f->myproc - f->throttle);
			// wait to receive token before continuing with read
            TRY( h5priv_mpi_recv(
				&token, 1, MPI_INT,
				f->myproc - f->throttle, // receive from previous proc
				f->myproc, // use this proc id as message tag
				f->comm
				) );
		}
		h5_debug ("[%d] throttle: received token", f->myproc);
	}
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5_end_throttle (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER (h5_err_t);
	if (f->throttle > 0) {
		int token;
		if (f->myproc + f->throttle < f->nprocs) {
			// pass token to next proc 
			h5_debug ("[%d] throttle: passing token to %d",
				  f->myproc, f->myproc + f->throttle);
			TRY (h5priv_mpi_send(
				     &token, 1, MPI_INT,
				     f->myproc + f->throttle, // send to next proc
				     f->myproc + f->throttle, // use the id of the target as tag
				     f->comm
				     ));
		}
	}
	H5_CORE_API_RETURN (H5_SUCCESS);
}
#endif // PARALLEL_IO

