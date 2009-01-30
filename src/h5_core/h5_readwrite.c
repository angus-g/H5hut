#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core.h"
#include "h5_core_private.h"

h5_int64_t
h5_write_data (
	h5_file_t *f,		/*!< IN: Handle to open file */
	const char *name,	/*!< IN: Name to associate array with */
	const void *array,	/*!< IN: Array to commit to disk */
	const hid_t type_id,	/*!< IN: Type of data */
	const hid_t group_id,
	const hid_t memspace_id,
	const hid_t diskspace_id
	) {
	herr_t herr;
	hid_t dataset_id;

	h5_info ( f, "Writing dataset %s/%s.", h5_get_objname(group_id), name );

	dataset_id = H5Dcreate ( 
		group_id,
		name,
		type_id,
		diskspace_id,
		H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT );
	if ( dataset_id < 0 )
		return HANDLE_H5D_CREATE_ERR ( f, name );

	herr = H5Dwrite (
		dataset_id,
		type_id,
		memspace_id,
		diskspace_id,
		f->xfer_prop,
		array );

	if ( herr < 0 ) return HANDLE_H5D_WRITE_ERR ( f, name );

	herr = H5Dclose ( dataset_id );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR( f );

	f->empty = 0;

	return H5_SUCCESS;
}

h5_err_t
h5_write_dataset (
	h5_file_t * const f,		/*!< IN: Handle to open file */
	const hid_t group_id,
	const char dataset_name[],/*!< IN: Name to associate data with */
	const hid_t type_id,	/*!< IN: Type of data */
	const hid_t memspace_id,
	const hid_t diskspace_id,
	const void * const data	/*!< IN: Data to commit to disk */
	) {
	h5_info ( f, "Writing dataset %s/%s.",
		  h5_get_objname(group_id), dataset_name );

	if ( f->mode == O_RDONLY ) 
		return _h5_handle_file_mode_error( f, f->mode );

	/*
	  file modes:
	  H5_O_RDONLY: only reading allowed
	  H5_O_WRONLY: create new file, dataset must not exist
	  H5_O_APPEND: allows appendings of new datasets to an existing file
	  H5_O_RDWR:   dataset may exist
	*/
	H5O_info_t dataset_info;
	herr_t herr = H5Oget_info_by_name(
		group_id,
		dataset_name,
		&dataset_info,
		H5P_DEFAULT  );

	if ( (herr >= 0) && ( (f->mode==H5_O_WRONLY) || (f->mode==H5_O_APPEND) ) ) {
		h5_warn ( f,
			  "Dataset %s/%s already exist.",
			  h5_get_objname(group_id), dataset_name );
		return _h5_handle_file_mode_error( f, f->mode );
	}
	hid_t dataset_id;
	if ( herr >= 0 ) {
		dataset_id = H5Dopen ( 
			group_id,
			dataset_name,
			H5P_DEFAULT );
		if ( dataset_id < 0 )
			return HANDLE_H5D_OPEN_ERR ( f, dataset_name );
	} else {
		dataset_id = H5Dcreate ( 
			group_id,
			dataset_name,
			type_id,
			diskspace_id,
			H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT );
		if ( dataset_id < 0 )
			return HANDLE_H5D_CREATE_ERR ( f, dataset_name );
	}
	herr = H5Dwrite (
		dataset_id,
		type_id,
		memspace_id,
		diskspace_id,
		f->xfer_prop,
		data );

	if ( herr < 0 ) return HANDLE_H5D_WRITE_ERR ( f, dataset_name );

	herr = H5Dclose ( dataset_id );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR( f );

	f->empty = 0;

	return H5_SUCCESS;
}

h5_err_t
h5_read_dataset2 (
	h5_file_t * const f,
	hid_t group_id,
	const char dataset_name[],
	hid_t type_id,
	hid_t (*set_memspace)(h5_file_t*,hid_t),
	hid_t (*set_diskspace)(h5_file_t*,hid_t),
	void * const data ) {

	hid_t dataset_id = H5Dopen ( group_id, dataset_name, H5P_DEFAULT );
	if ( dataset_id < 0 ) return HANDLE_H5D_OPEN_ERR ( f, dataset_name );

	hid_t memspace_id = (*set_memspace)( f, dataset_id );
	if ( memspace_id < 0 ) return memspace_id;
	hid_t diskspace_id = (*set_diskspace)( f, dataset_id );
	if ( diskspace_id < 0 ) return diskspace_id;

	
	h5_err_t h5err = _h5_read_dataset (
		f,
		dataset_id,
		type_id,
		memspace_id,
		diskspace_id,
		data );
	if ( h5err < 0 ) return h5err;

	herr_t herr = H5Sclose ( diskspace_id );
	if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR( f );
	herr = H5Sclose ( memspace_id );
	if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR( f );

	herr = H5Dclose ( dataset_id );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR( f );

	return H5_SUCCESS;
}

h5_err_t
h5_read_dataset (
	h5_file_t *f,
	hid_t group_id,
	const char dataset_name[],
	hid_t type_id,
	hid_t memspace_id,
	hid_t diskspace_id,
	void * const data ) {

	hid_t dataset_id = H5Dopen ( group_id, dataset_name, H5P_DEFAULT );
	if ( dataset_id < 0 ) return HANDLE_H5D_OPEN_ERR ( f, dataset_name );

	h5_err_t h5err = _h5_read_dataset (
		f,
		dataset_id,
		type_id,
		memspace_id,
		diskspace_id,
		data );
	if ( h5err < 0 ) return h5err;

	herr_t herr = H5Dclose ( dataset_id );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR( f );

	return H5_SUCCESS;
}

h5_err_t
_h5_read_dataset (
	h5_file_t * const f,
	hid_t dataset_id,
	hid_t type_id,
	hid_t memspace_id,
	hid_t diskspace_id,
	void * const data ) {

	herr_t herr = H5Dread (
		dataset_id,
		type_id,
		memspace_id,
		diskspace_id,
		f->xfer_prop,
		data );
	if ( herr < 0 )
		return HANDLE_H5D_READ_ERR ( f, h5_get_objname ( dataset_id ) );

	return H5_SUCCESS;
}





h5_int64_t
_create_step (
	h5_file_t * f
	) {
	h5_debug (
		f,
		"Proc[%d]: Create step #%lld for file %lld", 
		f->myproc,
		(long long)f->step_idx,
		(long long)(size_t) f );
	f->is_new_step = 1;
	f->step_gid = H5Gcreate ( f->file, f->step_name, 0,
				  H5P_DEFAULT, H5P_DEFAULT );
	if ( f->step_gid < 0 )
		return HANDLE_H5G_CREATE_ERR ( f, f->step_name );

	return H5_SUCCESS;
}

h5_int64_t
_open_step (
	h5_file_t * f
	) {
	h5_info (
		f,
		"Proc[%d]: Open step #%lld for file %lld",
		f->myproc,
		(long long)f->step_idx,
		(long long)(size_t) f );
	f->is_new_step = 0;
	f->step_gid = H5Gopen ( f->file, f->step_name, H5P_DEFAULT ); 
	if ( f->step_gid < 0 )
		return HANDLE_H5G_OPEN_ERR( f, "", f->step_name );

	return H5_SUCCESS;
}

static h5_err_t
_init_step (
	h5_file_t * f
	) {
	h5_err_t h5err = _h5t_init_step ( f );
	if ( h5err < 0 ) return h5err;

	return H5_SUCCESS;
}	

h5_err_t
_h5_close_step (
	h5_file_t * f
	) {

	if ( f->step_gid < 0 ) return H5_SUCCESS;

	h5_err_t h5err = _h5t_close_step ( f );
	if ( h5err < 0 ) return h5err;

	herr_t herr = H5Gclose ( f->step_gid );
	if ( herr < 0 ) return HANDLE_H5G_CLOSE_ERR( f );

	f->step_gid = -1;

	return H5_SUCCESS;
}

h5_int64_t
_set_step (
	h5_file_t * f,
	const h5_int64_t step_idx	/*!< [in]  Step to set. */
	) {
	f->step_idx = step_idx;

	sprintf (
		f->step_name,
		"%s#%0*lld",
		f->prefix_step_name, f->width_step_idx, (long long) f->step_idx );

	herr_t herr = H5Gget_objinfo( f->file, f->step_name, 1, NULL );

	if( f->mode == H5_O_RDONLY ) {
		if ( herr < 0 ) return HANDLE_H5G_OPEN_ERR( f, "", f->step_name );
		herr = _open_step ( f );
		if ( herr < 0 ) return herr;
	} else if ( (f->mode == H5_O_WRONLY)  || (f->mode == H5_O_APPEND) ) {
		if ( herr > 0 ) return HANDLE_H5_STEP_EXISTS_ERR ( f, step_idx );
 		herr = _create_step ( f );
		if ( herr < 0 ) return herr;
	} else if ( (f->mode == H5_O_RDWR) && (herr < 0) ) {
		herr = _create_step ( f );
		if ( herr < 0 ) return herr;
	} else if ( (f->mode == H5_O_RDWR) && (herr >= 0) ) {
		herr = _open_step ( f );
		if ( herr < 0 ) return herr;
	}
	return H5_SUCCESS;
}

h5_int64_t
h5_set_step (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	const h5_int64_t step_idx	/*!< [in]  Step to set. */
	) {

	h5_err_t h5err = _h5_close_step ( f );
	if ( h5err < 0 ) return h5err;

	h5err = _set_step ( f, step_idx );
	if ( h5err < 0 ) return h5err;

	h5err = _init_step ( f );
	if ( h5err < 0 ) return h5err;

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
	hid_t dataset_id = H5Dopen ( group_id, dataset_name, H5P_DEFAULT );
	if ( dataset_id < 0 ) HANDLE_H5D_OPEN_ERR ( f, dataset_name );

	hid_t hdf5_type = H5Dget_type ( dataset_id );
	if ( hdf5_type < 0 ) HANDLE_H5D_GET_TYPE_ERR( f );

	h5_int64_t type = (h5_int64_t) h5_normalize_h5_type ( f, hdf5_type );

	herr_t herr = H5Tclose(hdf5_type);
	if ( herr < 0 ) HANDLE_H5T_CLOSE_ERR( f );

	herr = H5Dclose(dataset_id);
	if ( herr < 0 ) HANDLE_H5D_CLOSE_ERR( f );

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
