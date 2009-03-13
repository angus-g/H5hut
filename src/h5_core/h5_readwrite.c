#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core.h"
#include "h5_core_private.h"

/*
  Obsolete function - still in old H5Part code !
*/

h5_err_t
h5_write_data (
	h5_file_t * const f,	/*!< IN: Handle to open file */
	const char *name,	/*!< IN: Name to associate array with */
	const void *array,	/*!< IN: Array to commit to disk */
	const hid_t type_id,	/*!< IN: Type of data */
	const hid_t group_id,
	const hid_t memspace_id,
	const hid_t diskspace_id
	) {
	hid_t dataset_id;

	h5_info ( f, "Writing dataset %s/%s.", h5_get_objname(group_id), name );
	TRY ( dataset_id = _h5_create_dataset (
		      f,
		      group_id,
		      name,
		      type_id,
		      diskspace_id,
		      H5P_DEFAULT ) );
	TRY ( _h5_write_dataset (
		      f,
		      dataset_id,
		      type_id,
		      memspace_id,
		      diskspace_id,
		      f->xfer_prop,
		      array ) );
	TRY ( _h5_close_dataset( f, dataset_id ) );

	f->empty = 0;

	return H5_SUCCESS;
}

/*!
  Write data to dataset.

  - Open/Create dataset
  - set hyperslabs for disk and memory via callback functions
  - Write data
  - Close dataset  
 */
h5_err_t
_h5_write (
 	h5_file_t * const f,
	const hid_t loc_id,
	h5_dataset_info_t *ds_info,
	hid_t (*set_memspace)(h5_file_t*,hid_t),
	hid_t (*set_diskspace)(h5_file_t*,hid_t),
	const void * const data
	) {
	h5_info ( f, "Writing dataset %s/%s.",
		  h5_get_objname ( loc_id ), ds_info->name );

	H5O_info_t obj_info;
	herr_t herr = H5Oget_info_by_name(
		loc_id,
		ds_info->name,
		&obj_info,
		H5P_DEFAULT  );

	if ( (herr >= 0) && ( (f->mode==H5_O_WRONLY) || (f->mode==H5_O_APPEND) ) ) {
		h5_warn ( f,
			  "Dataset %s/%s already exist.",
			  h5_get_objname ( loc_id ), ds_info->name );
		return _h5_handle_file_mode_error( f, f->mode );
	}

	/*
	  open/create dataset
	*/
	hid_t dataset_id;
	hid_t dataspace_id;
	hid_t diskspace_id;
	hid_t memspace_id;

	if ( herr >= 0 ) {
		TRY( (dataset_id = _h5_open_dataset (
			      f, 
			      loc_id,
			      ds_info->name ) ) );
		TRY( (dataspace_id = _h5_get_dataset_space (
			      f,
			      dataset_id ) ) );
		/* 
		   extend dataset?
		*/
	} else {
		TRY( (dataspace_id = _h5_create_dataset_space (
			      f,
			      ds_info->rank,
			      ds_info->dims,
			      ds_info->maxdims ) ) );
		TRY( (dataset_id = _h5_create_dataset (
			      f,
			      loc_id,
			      ds_info->name,
			      *ds_info->type_id,
			      dataspace_id,
			      ds_info->create_prop ) ) );
	}
	TRY( (memspace_id = (*set_memspace)( f, 0 ) ) );
	TRY( (diskspace_id = (*set_diskspace)( f, dataspace_id ) ) );

	TRY( _h5_write_dataset (
		     f,
		     dataset_id,
		     *ds_info->type_id,
		     memspace_id,
		     diskspace_id,
		     f->xfer_prop,
		     data ) );

	TRY( _h5_close_dataset( f, dataset_id ) );

	f->empty = 0;

	return H5_SUCCESS;
}

h5_err_t
_h5_read (
	h5_file_t * const f,
	hid_t loc_id,
	h5_dataset_info_t *ds_info,
	hid_t (*set_memspace)(h5_file_t*,hid_t),
	hid_t (*set_diskspace)(h5_file_t*,hid_t),
	void * const data
	) {

	hid_t dataset_id;
	hid_t memspace_id;
	hid_t diskspace_id;

	TRY ( (dataset_id = _h5_open_dataset (
		      f,
		      loc_id,
		      ds_info->name ) ) );
	TRY ( (memspace_id = (*set_memspace)( f, dataset_id ) ) );
	TRY ( (diskspace_id = (*set_diskspace)( f, dataset_id ) ) );
	TRY ( _h5_read_dataset (
		f,
		dataset_id,
		*ds_info->type_id,
		memspace_id,
		diskspace_id,
		f->xfer_prop,
		data ) );
	TRY ( _h5_close_dataspace ( f, diskspace_id ) );
	TRY ( _h5_close_dataspace ( f, memspace_id ) );
	TRY ( _h5_close_dataset ( f, dataset_id ) );

	return H5_SUCCESS;
}

static h5_err_t
_init_step (
	h5_file_t * f
	) {
	TRY ( _h5t_init_step ( f ) );

	return H5_SUCCESS;
}	

h5_err_t
_h5_close_step (
	h5_file_t * f
	) {

	if ( f->step_gid < 0 ) return H5_SUCCESS;
	TRY ( _h5t_close_step ( f ) );
	TRY ( _h5_close_group ( f, f->step_gid ) );

	f->step_gid = -1;

	return H5_SUCCESS;
}

h5_err_t
_set_step (
	h5_file_t * f,
	const h5_int64_t step_idx	/*!< [in]  Step to set. */
	) {
	f->step_idx = step_idx;

	sprintf (
		f->step_name,
		"%s#%0*lld",
		f->prefix_step_name, f->width_step_idx, (long long) f->step_idx );
	h5_info (
		f,
		"Proc[%d]: Open step #%lld for file %lld",
		f->myproc,
		(long long)f->step_idx,
		(long long)(size_t) f );
	TRY ( f->step_gid = _h5_open_group ( f, f->file, f->step_name ) );

	return H5_SUCCESS;
}

h5_int64_t
h5_set_step (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	const h5_int64_t step_idx	/*!< [in]  Step to set. */
	) {

	TRY ( _h5_close_step ( f ) );
	TRY ( _set_step ( f, step_idx ) );
	TRY ( _init_step ( f ) );

	return H5_SUCCESS;
}

/*!
   Normalize HDF5 type
*/
hid_t
h5_normalize_h5_type (
	h5_file_t * const f,
	hid_t type
	) {
	H5T_class_t tclass = H5Tget_class ( type );
	int size = H5Tget_size ( type );

	switch ( tclass ){
	case H5T_INTEGER:
		if ( size==8 ) {
			return H5T_NATIVE_INT64;
		}
		else if ( size==1 ) {
			return H5T_NATIVE_CHAR;
		}
		break;
	case H5T_FLOAT:
		return H5T_NATIVE_DOUBLE;
	default:
		; /* NOP */
	}
	h5_warn ( f, "Unknown type %d", (int)type );

	return -1;
}


h5_int64_t
h5_get_dataset_type(
	h5_file_t * const f,
	hid_t group_id,
	const char *dataset_name
	) {
	hid_t dataset_id;
	hid_t hdf5_type;

	TRY( dataset_id = _h5_open_dataset ( f, group_id, dataset_name ) );
	TRY ( hdf5_type = _h5_get_dataset_type ( f, dataset_id ) );
	h5_int64_t type = (h5_int64_t) h5_normalize_h5_type ( f, hdf5_type );
	TRY( _h5_close_type( f, hdf5_type ) );
	TRY( _h5_close_dataset( f, dataset_id ) );

	return type;
}

h5_int64_t
h5_has_index (
	h5_file_t *f,		/*!< [in]  Handle to open file */
	h5_int64_t step	/*!< [in]  Step number to query */
	) {
	char name[128];
	sprintf ( name,
		  "%s#%0*lld",
		  f->prefix_step_name, f->width_step_idx, (long long) step );
	return ( H5Gget_objinfo( f->file, name, 1, NULL ) >= 0 );
}
