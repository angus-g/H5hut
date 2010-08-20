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
	h5_info (f, "Writing dataset %s/%s.",
		 h5_get_objname (loc_id), dsinfo->name);

	H5O_info_t obj_info;
	herr_t herr = H5Oget_info_by_name(
		loc_id,
		dsinfo->name,
		&obj_info,
		H5P_DEFAULT);

	if ((herr >= 0) && ((f->mode==H5_O_WRONLY) || (f->mode==H5_O_APPEND))) {
		h5_warn (f,
			 "Dataset %s/%s already exist.",
			 h5_get_objname (loc_id), dsinfo->name);
		return h5priv_handle_file_mode_error(f, f->mode);
	}

	/*
	  open/create dataset
	*/
	hid_t dset_id;
	hid_t dataspace_id;
	hid_t diskspace_id;
	hid_t memspace_id;

	if (herr >= 0) {
		/* overwrite dataset */
		TRY( dset_id = h5priv_open_hdf5_dataset (
			     f, 
			     loc_id,
			     dsinfo->name) );
		TRY( dataspace_id = h5priv_get_hdf5_dataset_space (
			     f,
			     dset_id) );
		TRY( h5priv_set_hdf5_dataset_extent (
			     f,
			     dset_id,
			     dsinfo->dims) );
		/* exten dataset? */
	} else {
		/* create dataset */
		TRY( dataspace_id = h5priv_create_hdf5_dataspace (
			     f,
			     dsinfo->rank,
			     dsinfo->dims,
			     dsinfo->max_dims) );
		TRY ( dset_id = h5priv_create_hdf5_dataset (
			      f,
			      loc_id,
			      dsinfo->name,
			      dsinfo->type_id,
			      dataspace_id,
			      dsinfo->create_prop) );
	}
	TRY( memspace_id = (*set_memspace)(f, 0) );
	TRY( diskspace_id = (*set_diskspace)(f, dataspace_id) );
	TRY( h5priv_write_hdf5_dataset (
		     f,
		     dset_id,
		     dsinfo->type_id,
		     memspace_id,
		     diskspace_id,
		     f->xfer_prop,
		     data) );
	TRY( h5priv_close_hdf5_dataspace (f, diskspace_id) );
	TRY( h5priv_close_hdf5_dataspace (f, memspace_id) );
	TRY( h5priv_close_hdf5_dataset (f, dset_id) );
	f->empty = 0;

	return H5_SUCCESS;
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

	hid_t mspace_id;
	hid_t dspace_id;

	TRY( mspace_id = (*set_mspace)(f, dset_id) );
	TRY( dspace_id = (*set_dspace)(f, dset_id) );
	TRY( h5priv_read_hdf5_dataset (
		     f,
		     dset_id,
		     dsinfo->type_id,
		     mspace_id,
		     dspace_id,
		     f->xfer_prop,
		     data) );
	TRY( h5priv_close_hdf5_dataspace (f, dspace_id) );
	TRY( h5priv_close_hdf5_dataspace (f, mspace_id) );

	return H5_SUCCESS;
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

	hid_t dset_id;
	TRY( dset_id = h5priv_open_hdf5_dataset (
		      f,
		      loc_id,
		      dsinfo->name) );
	TRY( h5priv_read_dataset (f, dset_id, dsinfo, set_mspace, set_dspace,
				  data) );
	TRY( h5priv_close_hdf5_dataset (f, dset_id) );

	return H5_SUCCESS;
}


static h5_err_t
_init_step (
	h5_file_t* const f
	) {
	TRY( h5tpriv_init_step (f) );

	return H5_SUCCESS;
}	

h5_err_t
h5priv_close_step (
	h5_file_t* const f
	) {

	if (f->step_gid < 0) return H5_SUCCESS;
	TRY( h5tpriv_close_step (f) );
	TRY( h5priv_close_hdf5_group (f, f->step_gid) );

	f->step_gid = -1;

	return H5_SUCCESS;
}

static h5_err_t
_set_step (
	h5_file_t* const f,
	const h5_id_t step_idx	/*!< [in]  Step to set. */
	) {
	f->step_idx = step_idx;

	sprintf (
		f->step_name,
		"%s#%0*lld",
		f->prefix_step_name, f->width_step_idx, (long long) f->step_idx);
	h5_info (
		f,
		"Proc[%d]: Open step #%lld for file %lld",
		f->myproc,
		(long long)f->step_idx,
		(long long)(size_t) f);
	TRY ( f->step_gid = h5priv_open_group (f, f->file, f->step_name) );

	return H5_SUCCESS;
}

h5_err_t
h5_set_step (
	h5_file_t* const f,		/*!< [in]  Handle to open file */
	const h5_id_t step_idx		/*!< [in]  Step to set. */
	) {

	TRY( h5priv_close_step (f) );
	TRY( _set_step (f, step_idx) );
	TRY( _init_step (f) );

	return H5_SUCCESS;
}

/*!
   Normalize HDF5 type
*/
h5_int64_t
h5_normalize_h5_type (
	h5_file_t* const f,
	hid_t type
	) {
	H5T_class_t tclass = H5Tget_class (type);
	int size = H5Tget_size (type);

	switch (tclass){
	case H5T_INTEGER:
		if (size==8) {
			return H5_INT64_T;
		}
		else if (size==4) {
			return H5_INT32_T;
		}
		break;
	case H5T_FLOAT:
		if ( size==8 ) {
			return H5_FLOAT64_T;
		}
		else if ( size==4 ) {
			return H5_FLOAT32_T;
		}
		break;
	case H5T_STRING:
		return H5_STRING_T;
	default:
		; /* NOP */
	}
	h5_warn (f, "Unknown type %d", (int)type);

	return -1;
}


h5_int64_t
h5_get_dataset_type(
	h5_file_t* const f,
	hid_t group_id,
	const char* dset_name
	) {
	hid_t dset_id;
	hid_t hdf5_type;

	TRY( dset_id = h5priv_open_hdf5_dataset (f, group_id, dset_name) );
	TRY( hdf5_type = h5priv_get_hdf5_dataset_type (f, dset_id) );
	h5_int64_t type = h5_normalize_h5_type (f, hdf5_type);
	TRY( h5priv_close_hdf5_type (f, hdf5_type) );
	TRY( h5priv_close_hdf5_dataset (f, dset_id) );

	return type;
}

h5_err_t
h5_has_step (
	h5_file_t* const f,		/*!< [in]  Handle to open file */
	const h5_id_t step		/*!< [in]  Step number to query */
	) {
	char name[2*H5_STEPNAME_LEN];
	sprintf (name,
		"%s#%0*lld",
		f->prefix_step_name, f->width_step_idx, (long long)step);
	return h5priv_hdf5_link_exists(f, f->file, name);
}

h5_err_t
h5_normalize_dataset_name (
	h5_file_t *const f,
	const char *name,
	char *name2
	) {

	if ( strlen(name) > H5_DATANAME_LEN ) {
		strncpy ( name2, name, H5_DATANAME_LEN-1 );
		name2[H5_DATANAME_LEN-1] = '\0';
		h5_warn (f, "Truncated name '%s' to '%s'.", name, name2);
	} else {
		strcpy ( name2, name );
	}

	if ( strcmp( name2, H5_BLOCKNAME ) == 0 ) {
		h5_error (f,
			H5_ERR_INVAL,
			"Can't create dataset or field with name '%s' because "
			"it is reserved by H5Block.",
			H5_BLOCKNAME);
	}

	return H5_SUCCESS;
}

#ifdef PARALLEL_IO
h5_err_t
h5_set_throttle (
	h5_file_t* const f,
	const int factor
	) {
	if ( (f->mode & H5_VFD_INDEPENDENT) || (f->mode & H5_VFD_MPIPOSIX) ) {
		f->throttle = factor;
		h5_info (f,
			"Throttling enabled with factor = %d", f->throttle );
	} else {
		h5_warn (f,
			"Throttling is only permitted with the MPI-POSIX "
			"or MPI-IO Independent VFD." );
	}

	return H5_SUCCESS;
}

h5_err_t
h5_start_throttle (
	h5_file_t* const f
	) {

	if (f->throttle > 0) {
		int token = 1;
		h5_info (f,
			"Throttling with factor = %d",
			f->throttle);
		if (f->myproc / f->throttle > 0) {
			h5_debug (f,
				"[%d] throttle: waiting on token from %d",
				f->myproc, f->myproc - f->throttle);
			// wait to receive token before continuing with read
            TRY( h5priv_mpi_recv(f,
				&token, 1, MPI_INT,
				f->myproc - f->throttle, // receive from previous proc
				f->myproc, // use this proc id as message tag
				f->comm
				) );
		}
		h5_debug (f,
			"[%d] throttle: received token",
			f->myproc);
	}
	return H5_SUCCESS;
}

h5_err_t
h5_end_throttle (
	h5_file_t* const f
	) {

	if (f->throttle > 0) {
		int token;
		if (f->myproc + f->throttle < f->nprocs) {
			// pass token to next proc 
			h5_debug (f,
				"[%d] throttle: passing token to %d",
				f->myproc, f->myproc + f->throttle);
			TRY( h5priv_mpi_send(f,
				&token, 1, MPI_INT,
				f->myproc + f->throttle, // send to next proc
				f->myproc + f->throttle, // use the id of the target as tag
				f->comm
				) );
		}
	}
	return H5_SUCCESS;
}
#endif // PARALLEL_IO

