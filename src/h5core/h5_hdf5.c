#include <stdlib.h>
#include <string.h>
#include <hdf5.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

/****** G r o u p ************************************************************/

hid_t
h5priv_open_hdf5_group (
	h5_file_t* const f,
	const hid_t loc_id,
	const char* const group_name
	) {
	h5_debug (f, "%s (loc_id=%lld, group_name=\"%s/%s\")", 
		  __func__, (long long)loc_id,
		  h5_get_objname (loc_id), group_name);
	hid_t group_id = H5Gopen (loc_id, group_name, H5P_DEFAULT);
	if (group_id < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot open group \"%s/%s\".",
			h5_get_objname (loc_id),
			group_name);
	h5_debug (f, "%s (): return group id: %lld", __func__, (long long)group_id);
	return group_id;
}

hid_t
h5priv_create_hdf5_group (
	h5_file_t* const f,
	const hid_t loc_id,
	const char* const group_name
	) {
	h5_debug (f, "%s (loc_id=%lld, group_name=\"%s/%s\")", 
		  __func__, (long long)loc_id,
		  h5_get_objname (loc_id), group_name);
	hid_t group_id = H5Gcreate (
		loc_id, group_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	if (group_id < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot create group \"%s/%s\".",
			h5_get_objname (loc_id),
			group_name);
	h5_debug (f, "%s (): return group id: %lld", __func__, (long long)group_id);
	return group_id;
}

/*!
  Open HDF5 group. If group doesn't exist create it.

  \param[in]	f		file handle
  \param[in]	loc_id		location id
  \param[in]	group_name	name of group to open
*/
hid_t
h5priv_open_group (
	h5_file_t* const f,
	const hid_t loc_id,
	const char* const group_name
	) {
	hid_t group_id;

	/*
	  check access modes:
			Open	Create
	  H5_O_RDWR	x	x
	  H5_O_RDONLY	x	-
	  H5_O_WRONLY	x	x  (overwrite/append data to existing dataset)
	  H5_O_APPEND	x	x  (append datasets to an existing group)
	*/

        h5_err_t exists;
        TRY( exists = h5priv_hdf5_link_exists(f, loc_id, group_name) );
	if (exists > 0) {
		TRY( group_id = h5priv_open_hdf5_group (f, loc_id, group_name) );
	} else {
		CHECK_WRITABLE_MODE (f);
		TRY( group_id = h5priv_create_hdf5_group (f, loc_id, group_name) );
	}

	return group_id;
}

/*!
  Close group.

  \param[in]	f		file handle
  \param[in]	group_id        id of group to close
*/
h5_err_t
h5priv_close_hdf5_group (
	h5_file_t* const f,
	const hid_t group_id
	) {
	h5_debug (f, "%s (group_id=%lld, group_name=\"%s\")", 
		  __func__,
		  (long long)group_id,
		  h5_get_objname (group_id));

	h5_debug (f, "%s (group_id=%lld)", __func__, (long long)group_id);
	if (group_id <= 0) return H5_SUCCESS; 
	if (H5Gclose (group_id) < 0 ) {
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot terminate access to group \"%s\".",
			h5_get_objname (group_id));
	}
	return H5_SUCCESS;
}

h5_ssize_t
h5priv_get_num_objs_in_hdf5_group (
	h5_file_t* const f,
	const hid_t group_id
	) {
	H5G_info_t group_info;
	if (H5Gget_info (group_id, &group_info) < 0) {
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get number of objects in group %s.",
			h5_get_objname(group_id));
	}
	return (h5_ssize_t)group_info.nlinks;
}


/*
  Get name of object given by index \c idx in group \c loc_id. If name is \c NULL,
  return size of name.
*/
h5_ssize_t
h5priv_get_hdf5_objname_by_idx (
	h5_file_t* const f,
	hid_t loc_id,
	hsize_t idx,
	char *name,
	size_t size
	) {
	ssize_t len;
	if (name == NULL) {
		len = H5Lget_name_by_idx (loc_id, ".",
					    H5_INDEX_NAME, H5_ITER_INC,
					    idx,
					    NULL, 0,
					    H5P_DEFAULT);
		if (len < 0) goto error;
	} else {
		len = H5Lget_name_by_idx (loc_id, ".",
					    H5_INDEX_NAME, H5_ITER_INC,
					    idx,
					    name, size,
					    H5P_DEFAULT);
		if (len < 0) goto error;
	}
	return len;
error:
	return h5_error (
		f,
		H5_ERR_HDF5,
		"Cannot get name of object %ld in group %s.",
		(long)idx,
		h5_get_objname (loc_id));
}

/****** D a t a s e t ********************************************************/
/*!
  Open dataset. H5Dopen wrapper.

  \param[in]	f		file handle
  \param[in]	loc_id		location id
  \param[in]	dataset_name	name of dataset to open
 */
hid_t
h5priv_open_hdf5_dataset (
	h5_file_t* const f,
	const hid_t loc_id,
	const char* const dataset_name
	) {
	h5_debug (f, "%s (loc_id=%lld, dataset_name=\"%s/%s\")", 
		  __func__, (long long)loc_id,
		  h5_get_objname (loc_id), dataset_name);

	hid_t dataset_id;

	dataset_id = H5Dopen ( 
		loc_id,
		dataset_name,
		H5P_DEFAULT);
	if (dataset_id < 0)
		return 	h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot open dataset \"%s\".", dataset_name);
	h5_debug (f, "%s (): return dataset id: %lld", __func__, (long long)dataset_id);
	return dataset_id;
}

/*!
  Create new dataset

  \param[in]	f		file handle
  \param[in]	loc_id		id of group or file
  \param[in]	dataset_name	name of dataset
  \param[in]	type_id		type used in dataset
  \param[in]	dataspace_id	dataspace of dataset
  \param[in]	create_prop	property list for dataset creation

 */
hid_t
h5priv_create_hdf5_dataset (
	h5_file_t* const f,
	hid_t loc_id,
	const char* dataset_name,
	const hid_t type_id,
	const hid_t dataspace_id,
	const hid_t create_proplist
	) {
	h5_debug (f, "%s (loc_id=%lld, dataset_name=\"%s/%s\")", 
		  __func__, (long long)loc_id,
		  h5_get_objname (loc_id), dataset_name);
	
	hid_t dataset_id = H5Dcreate ( 
		loc_id,
		dataset_name,
		type_id,
		dataspace_id,
		H5P_DEFAULT,
		create_proplist,
		H5P_DEFAULT);
	if (dataset_id < 0)
		return h5_error(
			f,
			H5_ERR_HDF5,
			"Cannot create dataset %s/%s",
			h5_get_objname (loc_id),
			dataset_name);
	h5_debug (f, "%s (): return dataset id: %lld", __func__, (long long)dataset_id);
	return dataset_id;
}

/*!
  Close dataset.

  \param[in]	f		file handle
  \param[in]	dataset_id	id of dataset to close
*/
h5_err_t
h5priv_close_hdf5_dataset (
	h5_file_t* const f,
	const hid_t dset_id
	) {
	if (dset_id == 0 || dset_id == -1) return H5_SUCCESS; 
	h5_debug (f, "%s (dataset_id=%lld, dataset_name=\"%s\")", 
		  __func__,
		  (long long)dset_id,
		  h5_get_objname (dset_id));

	if (H5Dclose (dset_id) < 0) {
		return 	h5_error(
			f,
			H5_ERR_HDF5,
			"Close of dataset \"%s\" failed.",
			h5_get_objname (dset_id) );
	}
	return H5_SUCCESS;
}

/*!
  Get dataspace of existing dataset

  \param[in]	f		file handle
  \param[in]	dataset_id	id of dataset

 */
hid_t
h5priv_get_hdf5_dataset_space (
	h5_file_t* const f,
	const hid_t dataset_id
	) {
	hid_t dataspace_id = H5Dget_space (dataset_id);
	if (dataspace_id < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get dataspace for dataset \"%s\".",
			h5_get_objname (dataset_id) );
	return dataspace_id;
}

/*!
  Wrapper for H5Dwrite.
  

  \param[in]	f		file handle
  \param[in]	dataset_id	id of dataset
  \param[in]	type_id		type used in dataset
  \param[in]	memspace_id	id of memory space 
  \param[in]	diskspace_id	id of disk space
  \param[in]	xfer_prop	transfer property list
  \param[in]	buf		buffer with date to write

 */
h5_err_t
h5priv_write_hdf5_dataset (
	h5_file_t* const f,
	const hid_t dataset_id,
	const hid_t type_id,
	const hid_t memspace_id,
	const hid_t diskspace_id,
	const hid_t xfer_prop,
	const void* buf
	) {
#ifdef PARALLEL_IO
	TRY ( h5_start_throttle ( f ) );
#endif
	herr_t herr = H5Dwrite (
		dataset_id,
		type_id,
		memspace_id,
		diskspace_id,
		xfer_prop,
		buf);
	if (herr < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Write to dataset \"%s\" failed.",	\
			h5_get_objname (dataset_id));

#ifdef PARALLEL_IO
	TRY ( h5_end_throttle ( f ) );
#endif
	return H5_SUCCESS;
}

/*
  Wrapper for H5Dread
*/
h5_err_t
h5priv_read_hdf5_dataset (
	h5_file_t* const f,
	const hid_t dataset_id,
	const hid_t type_id,
	const hid_t memspace_id,
	const hid_t diskspace_id,
	const hid_t xfer_prop,
	void* const buf ) {

#ifdef PARALLEL_IO
	TRY ( h5_start_throttle ( f ) );
#endif
	herr_t herr = H5Dread (
		dataset_id,
		type_id,
		memspace_id,
		diskspace_id,
		xfer_prop,
		buf);
	if (herr < 0)
		return h5_error(
			f,
			H5_ERR_HDF5,
			"Error reading dataset \"%s\".",
			h5_get_objname (dataset_id) );
	
#ifdef PARALLEL_IO
	TRY ( h5_end_throttle ( f ) );
#endif
	return H5_SUCCESS;
}

hid_t
h5priv_get_hdf5_dataset_type ( 
	h5_file_t* const f,
	const hid_t dataset_id
	) {
	hid_t datatype_id = H5Dget_type (dataset_id);
	if (datatype_id < 0)
		return h5_error(
			f,
			H5_ERR_HDF5,
			"Cannot determine dataset type.");

	return datatype_id;
}


h5_err_t
h5priv_set_hdf5_dataset_extent (
	h5_file_t* const f,
	hid_t dset_id,
	const hsize_t* size
	) {
	if (H5Dset_extent(dset_id, size) < 0) {
		return 	h5_error(
			f,
			H5_ERR_HDF5,
			"Changing size of dataset \"%s\" dimensions failed.",
			h5_get_objname (dset_id));
	}
	return H5_SUCCESS;
}

h5_ssize_t
h5priv_get_npoints_of_hdf5_dataset (
	h5_file_t* const f,
	hid_t dset_id
	) {
	hid_t dspace_id;
	hsize_t size;
	TRY( dspace_id = h5priv_get_hdf5_dataset_space (f, dset_id) );
	TRY( size = h5priv_get_npoints_of_hdf5_dataspace (f, dspace_id) );
	TRY( h5priv_close_hdf5_dataspace (f, dspace_id) );
	return size;
}

h5_ssize_t
h5priv_get_npoints_of_hdf5_dataset_by_name (
	h5_file_t* const f,
	hid_t loc_id,
	char* name
	) {
	hid_t dset_id;
	hsize_t size;
	TRY( dset_id = h5priv_open_hdf5_dataset (f, loc_id, name) );
	TRY( size = h5priv_get_npoints_of_hdf5_dataset (f, dset_id) );
	TRY( h5priv_close_hdf5_dataset (f, dset_id) );
	return size;
}


/****** D a t a s p a c e ****************************************************/
/*!
  Create dataspace for dataset. H5Screate_simple wrapper.

  \param[in]	f		file handle
  \param[in]	rank		rank of dataspace
  \param[in]	dims		dimensions of dataspace
  \param[in]	maxdims		maximum dimensions of dataspace

 */
hid_t 
h5priv_create_hdf5_dataspace (
	h5_file_t* const f,
	const int rank,
	const hsize_t* dims,
	const hsize_t* maxdims 
	) {
	hid_t dataspace_id = H5Screate_simple (rank, dims, maxdims);
	if (dataspace_id < 0)
		h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot create dataspace with rank %d.",
			rank);
	return dataspace_id;
}

hid_t 
h5priv_create_hdf5_dataspace_scalar (
	h5_file_t* const f
	) {
	hid_t dataspace_id = H5Screate (H5S_SCALAR);
	if (dataspace_id < 0)
		h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot create scalar dataspace.");
	return dataspace_id;
}

h5_err_t
h5priv_select_hyperslab_of_hdf5_dataspace (
	h5_file_t* const f,
	hid_t space_id,
	H5S_seloper_t op,
	const hsize_t* start,
	const hsize_t* stride,
	const hsize_t* count,
	const hsize_t* block
	) {
	herr_t herr = H5Sselect_hyperslab (
		space_id,
		op,
		start,
		stride,
		count,
		block);
	if (herr < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot set select hyperslap region or add the "
			"specified region");
	return H5_SUCCESS;
}

h5_err_t
h5priv_select_elements_of_hdf5_dataspace (
	h5_file_t* const f,
	hid_t space_id,
	H5S_seloper_t op,
	hsize_t nelems,
	const hsize_t* indices
	) {
	herr_t herr;
	if ( nelems > 0 ) {
		herr = H5Sselect_elements (
			space_id,
			op,
			nelems,
			indices);
	} else {
		herr = H5Sselect_none ( space_id );
	}
	if (herr < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot set select hyperslap region or add the "
			"specified region");
	return H5_SUCCESS;
}

h5_ssize_t
h5priv_get_selected_npoints_of_hdf5_dataspace (
	h5_file_t* const f,
	hid_t space_id
	) {
	hssize_t size = H5Sget_select_npoints (space_id);
	if (size < 0)
		h5_error(
			f,
			H5_ERR_HDF5,
			"Cannot determine number of selected elements in dataspace.");
	return size;
}

h5_ssize_t
h5priv_get_npoints_of_hdf5_dataspace (
	h5_file_t* const f,
	hid_t space_id
	) {
	hssize_t size = H5Sget_simple_extent_npoints (space_id);
	if (size < 0)
		h5_error(
			f,
			H5_ERR_HDF5,
			"Cannot determine number of elements in dataspace.");
	return size;
}

int
h5priv_get_dims_of_hdf5_dataspace (
	h5_file_t* const f,
	hid_t space_id,
	hsize_t* dims,
	hsize_t* maxdims 
	) {
	int rank = H5Sget_simple_extent_dims (space_id, dims, maxdims);
	if (rank < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot determine rank of dataspace.");
	return rank;
}


/*!
  Close space.

  \param[in]	f		file handle
  \param[in]	dataspace_id	id of space to close
*/
h5_err_t
h5priv_close_hdf5_dataspace (
	h5_file_t* const f,
	const hid_t dataspace_id
	) {
	if (dataspace_id <= 0 || dataspace_id == H5S_ALL)
		return H5_SUCCESS; 

	herr_t herr = H5Sclose (dataspace_id);
	if (herr < 0)
		return h5_error(
			f,
			H5_ERR_HDF5,
			"Cannot terminate access to dataspace!");

	return H5_SUCCESS;
}

/****** D a t a t y p e ******************************************************/

const char*
h5priv_get_base_type_name (
	h5_file_t* const f,
	hid_t base_type_id
	) {
	if (base_type_id == H5_INT32_T)		return "H5_INT32_T";
	if (base_type_id == H5_INT64_T)		return "H5_INT64_T";
	if (base_type_id == H5_FLOAT32_T)	return "H5_FLOAT32_T";
	if (base_type_id == H5_FLOAT64_T)	return "H5_FLOAT64_T";
	if (base_type_id == H5_STRING_T)	return "H5_STRING_T";

	h5_warn (f, "Unknown base type id %lu", (unsigned long)base_type_id);
	return "[unknown]";
}

static const char*
get_class_type_name (
	h5_file_t* const f,
	hid_t base_type_id
	) {
#pragma unused f
	if (base_type_id == H5_COMPOUND_T)	return "H5_COMPOUND_T";

	return "[unknown]";
}

/*!
  Create array type. Wrapper for "H5Tarray_create".

  \param[in]	f		file handle
  \param[in]	base_type_id	base type
  \param[in]	rank		rank of array
  \param[in]	dims		dimensions
*/
hid_t
h5priv_create_hdf5_array_type (
	h5_file_t* const f,
	hid_t base_type_id,
	int rank,
	const hsize_t* dims
	) {
	hid_t type_id = H5Tarray_create (base_type_id, rank, dims);
	if (type_id < 0) {
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Can't create array datatype object with base "
			"type %s and rank %d",
			h5priv_get_base_type_name (f, base_type_id),
			rank);
	}
	return type_id;
}

hid_t
h5priv_create_hdf5_type (
	h5_file_t* const f,
	H5T_class_t class,
	const size_t size
	) {
	hid_t type_id = H5Tcreate (class, size);
	if (type_id < 0) {
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Can't create datatype object of class %s.",
			get_class_type_name (f, class)
			);
	}
	return type_id;
}

hid_t
h5priv_create_hdf5_string_type(
	h5_file_t *const f,
	const hsize_t len
	) {
	hid_t type_id = H5Tcopy ( H5T_C_S1 );
	if (type_id < 0)
		return h5_error(
			f,
			H5_ERR_HDF5,
			"Can't duplicate C string type.");
	
	herr_t herr = H5Tset_size ( type_id, len );
	if (herr < 0)
		return h5_error(
			f,
			H5_ERR_HDF5,
			"Can't set length of C string type.");
	return type_id;
}

h5_err_t
h5priv_insert_hdf5_type (
	h5_file_t* const f,
	hid_t dtype_id,
	const char* name,
	size_t offset,
	hid_t field_id
	) {
	herr_t herr = H5Tinsert (dtype_id, name, offset, field_id);
	if (herr < 0)
		return h5_error(
			f,
			H5_ERR_HDF5,
			"Can't insert field %s to compound datatype.",
			name);
	return H5_SUCCESS;
}

h5_err_t
h5priv_close_hdf5_type (
	h5_file_t* const f,
	hid_t dtype_id
	) {
	herr_t herr = H5Tclose (dtype_id);
	if (herr < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot release datatype.");
	return H5_SUCCESS;
}

/****** P r o p e r t y ******************************************************/

hid_t
h5priv_create_hdf5_property (
	h5_file_t* const f,
	hid_t cls_id
	) {
	hid_t prop_id = H5Pcreate (cls_id);
	if (prop_id < 0) 
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot create property list.");
	return prop_id;
}

/*!
  Get create properties of existing dataset

  \param[in]	f		file handle
  \param[in]	dataset_id	id of dataset

 */
hid_t
h5priv_get_hdf5_dataset_create_plist (
	h5_file_t* const f,
	const hid_t dataset_id
	) {
	hid_t plist_id = H5Dget_create_plist (dataset_id);
	if (plist_id < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get create properties for dataset \"%s\".",
			h5_get_objname (dataset_id) );
	return plist_id;
}

h5_err_t
h5priv_set_hdf5_chunk_property (
	h5_file_t* const f,
	hid_t plist,
	int rank,
	hsize_t* dims
	) {
	if (H5Pset_chunk (plist, rank, dims) < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot add chunking property to list.");

	return H5_SUCCESS;
}

h5_err_t
h5priv_get_hdf5_chunk_property (
	h5_file_t* const f,
	hid_t plist,
	int rank,
	hsize_t* dims
	) {
	if (H5Pget_chunk (plist, rank, dims) < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get chunking property from list.");

	return H5_SUCCESS;
}

h5_err_t
h5priv_set_hdf5_layout_property (
	h5_file_t* const f,
	hid_t plist,
	H5D_layout_t layout
	) {
	if (H5Pset_layout (plist, layout) < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot add layout property to list.");

	return H5_SUCCESS;
}

#ifdef PARALLEL_IO
h5_err_t
h5priv_set_hdf5_fapl_mpio_property (
	h5_file_t* const f,
	hid_t fapl_id,
	MPI_Comm comm,
	MPI_Info info
	) {
	herr_t herr = H5Pset_fapl_mpio (fapl_id, comm, info);
	if (herr < 0)
		h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot store IO communicator information to the "
			"file access property list.");
	return H5_SUCCESS;
}

h5_err_t
h5priv_set_hdf5_fapl_mpiposix_property (
	h5_file_t* const f,
	hid_t fapl_id,
	MPI_Comm comm,
	hbool_t	use_gpfs
	) {
	herr_t herr = H5Pset_fapl_mpiposix (fapl_id, comm, use_gpfs);
	if (herr < 0)
		h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot store IO communicator information to the "
			"file access property list.");
	return H5_SUCCESS;
}

h5_err_t
h5priv_set_hdf5_dxpl_mpio_property (
	h5_file_t* const f,
	hid_t dxpl_id,
	H5FD_mpio_xfer_t mode
	) {
	herr_t herr = H5Pset_dxpl_mpio (dxpl_id, mode);
	if (herr < 0)
		h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot store IO communicator information to the "
			"dataset transfer property list.");
	return H5_SUCCESS;
}
#endif

h5_err_t
h5priv_set_hdf5_mdc_property (
	h5_file_t* const f,
	hid_t fapl_id,
	H5AC_cache_config_t *config
	) {
	herr_t herr = H5Pset_mdc_config (fapl_id, config);
	if (herr < 0)
		h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot store metadata cache configuration in the "
			"file access property list.");
	return H5_SUCCESS;
}

h5_err_t
h5priv_get_hdf5_mdc_property (
	h5_file_t* const f,
	hid_t fapl_id,
	H5AC_cache_config_t *config
	) {
	herr_t herr = H5Pget_mdc_config (fapl_id, config);
	if (herr < 0)
		h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get metadata cache configuration in the "
			"file access property list.");
	return H5_SUCCESS;
}

h5_err_t
h5priv_set_hdf5_alignment_property (
	h5_file_t* const f,
	const hid_t fapl_id,
	const hsize_t threshold,
	const hsize_t alignment
	) {
	herr_t herr = H5Pset_alignment (fapl_id, threshold, alignment);
	if (herr < 0)
		h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot set alignment in the "
			"file access property list.");
	return H5_SUCCESS;
}

h5_err_t
h5priv_set_hdf5_btree_ik_property (
	h5_file_t* const f,
	const hid_t fcpl_id,
	const hsize_t btree_ik
	) {
	herr_t herr = H5Pset_istore_k (fcpl_id, btree_ik);
	if (herr < 0)
		h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot set btree size in the "
			"file access property list.");
	return H5_SUCCESS;
}

h5_err_t
h5priv_close_hdf5_property (
	h5_file_t* const f,
	hid_t prop
	) {
	herr_t herr = H5Pclose (prop);
	if (herr < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot close property.");
	return H5_SUCCESS;
}

/****** F i l e **************************************************************/

h5_err_t
h5priv_close_hdf5_file (
	h5_file_t* const f,
	hid_t fileid
	) {
	herr_t herr = H5Fclose (fileid);
	if (herr < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot close file.");
	return H5_SUCCESS;
}

/****** E r r o r h a n d l i n g ********************************************/

h5_err_t
h5priv_set_hdf5_errorhandler (
	h5_file_t* const f,
	hid_t estack_id,
	H5E_auto_t func,
	void* client_data
	) {
	herr_t herr = H5Eset_auto (estack_id, func, client_data);
	if (herr < 0)
		return h5_error (
			f,
			H5_ERR_INIT,
			"Cannot initialize H5.");
	return H5_SUCCESS;
}

/****** A t t r i b u t e ****************************************************/
hid_t
h5priv_open_hdf5_attribute (
	h5_file_t* const f,
	hid_t loc_id,
	const char* attr_name
	) {
	hid_t attr_id = H5Aopen (loc_id, attr_name, H5P_DEFAULT);
	if (attr_id < 0) 
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot open attribute \"%s\" of \"%s\".",
			attr_name,
			h5_get_objname (loc_id));
	return attr_id;
}

hid_t
h5priv_open_hdf5_attribute_idx (
	h5_file_t* const f,
	hid_t loc_id,
	unsigned int idx
	) {
	hid_t attr_id = H5Aopen_idx (loc_id, idx);
	if (attr_id < 0) 
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot open attribute \"%u\" of \"%s\".",
			idx,
			h5_get_objname (loc_id));
	return attr_id;
}	

hid_t
h5priv_open_hdf5_attribute_by_name (
	h5_file_t* const f,
	hid_t loc_id,
	const char* obj_name,
	const char* attr_name
	) {
	hid_t attr_id = H5Aopen_by_name (
		loc_id,
		obj_name,
		attr_name,
		H5P_DEFAULT,
		H5P_DEFAULT);
	if (attr_id < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot open attribute \"%s\" of \"%s\".",
			attr_name,
			obj_name);
	return attr_id;
}

hid_t
h5priv_create_hdf5_attribute (
	h5_file_t* const f,
	hid_t loc_id,
	const char* attr_name,
	hid_t type_id,
	hid_t space_id,
	hid_t acpl_id,
	hid_t aapl_id
	) {
	hid_t attr_id = H5Acreate ( 
		loc_id,
		attr_name,
		type_id,
		space_id,
		acpl_id,
		aapl_id);
	if (attr_id < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot create attribute \"%s\" for \"%s\".",
			attr_name,
			h5_get_objname (loc_id));
	return attr_id;
}

h5_err_t
h5priv_read_hdf5_attribute (
	h5_file_t* const f,
	hid_t attr_id,
	hid_t mem_type_id,
	void* buf
	) {
	herr_t herr = H5Aread (attr_id, mem_type_id, buf);
	if (herr < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot read attribute \"%s\".",
			h5_get_objname (attr_id));

	return H5_SUCCESS;
}

/*
  Wrapper for H5Awrite.
 */
h5_err_t
h5priv_write_hdf5_attribute (
	h5_file_t* const f,
	hid_t attr_id,
	hid_t mem_type_id,
	const void* buf
	) {
	herr_t herr = H5Awrite (attr_id, mem_type_id, buf);
	if (herr < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot write attribute \"%s\".",
			h5_get_objname (attr_id));

	return H5_SUCCESS;
}

h5_ssize_t
h5priv_get_hdf5_attribute_name (
	h5_file_t * const f,
	hid_t attr_id,
	size_t buf_size,
	char *buf
	) {
	ssize_t size = H5Aget_name ( attr_id, buf_size, buf );
	if ( size < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get attribute name." );
	return (h5_size_t)size;
}

hid_t
h5priv_get_hdf5_attribute_type (
	h5_file_t* const f,
	hid_t attr_id
	) {
	hid_t datatype_id = H5Aget_type (attr_id);
	if (datatype_id < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get type of attribute \"%s\".",
			h5_get_objname (attr_id));
	return datatype_id;
}

hid_t
h5priv_get_hdf5_attribute_dataspace (
	h5_file_t* const f,
	hid_t attr_id
	) {
	hid_t space_id = H5Aget_space (attr_id);
	if (space_id < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get dataspace of attribute \"%s\".",
			h5_get_objname (attr_id));
	return space_id;
}

int
h5priv_get_num_hdf5_attribute (
	h5_file_t* const f,
	hid_t loc_id
	) {
	int num = H5Aget_num_attrs (loc_id);
	if (num < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get number of attributes of \"%s\".",
			h5_get_objname (loc_id));
	return num;
}


h5_err_t
h5priv_close_hdf5_attribute (
	h5_file_t* const f,
	hid_t attr_id
	) {
	herr_t herr = H5Aclose (attr_id);
	if (herr < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot close attribute \"%s\".",
			h5_get_objname (attr_id));

	return H5_SUCCESS;
}

/****** L i n k **************************************************************/
h5_err_t
h5priv_hdf5_link_exists (
	h5_file_t* const f,
	const hid_t loc_id,
	const char* name
	) {
	htri_t exists = H5Lexists ( loc_id, name, H5P_DEFAULT );
	if (exists < 0 )
		return h5_error (f,
			H5_ERR_HDF5,
			"Cannot query link %s/%s.",
			h5_get_objname (loc_id), name);
	return exists;
}

h5_err_t
h5priv_delete_hdf5_link (
	h5_file_t* const f,
	hid_t loc_id,
	const char* name,
	hid_t lapl_id
	) {
	herr_t herr = H5Ldelete (loc_id, name, lapl_id);
	if (herr < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot remove link %s/%s.",
			h5_get_objname (loc_id), name);

	return H5_SUCCESS;
} 

typedef struct op_data {
	int queried_idx;
	int cnt;
	H5O_type_t type;
	char *name;
	size_t len;
	char *prefix;
	h5_file_t *f;
} op_data_t;

static H5O_type_t
_iter_op_get_obj_type (
	h5_file_t *const f,
	const hid_t g_id,
	const char* name,
	const H5L_info_t* info
	) {
	herr_t herr;
	H5O_info_t objinfo;

	if ( info->type == H5L_TYPE_EXTERNAL ) {
		char *buf;
		TRY( buf = h5priv_alloc(f, NULL, info->u.val_size) );

		herr = H5Lget_val(g_id, name, buf,
					info->u.val_size, H5P_DEFAULT);
		if ( herr < 0 )
			return (H5O_type_t)h5_error (f,
				H5_ERR_HDF5,
				"Can't get external link for object '%s'!",
				name);

		const char *filename;
		const char *objname;
		herr = H5Lunpack_elink_val(buf, info->u.val_size, 0,
							&filename, &objname);
		if ( herr < 0 )
			return (H5O_type_t)h5_error(f,
				H5_ERR_HDF5,
				"Can't unpack external link for object '%s'!",
				name);

		h5_debug(f,
			"Followed external link to file '%s' / object '%s'.",
			filename, objname);

		free(buf);

		hid_t obj_id = H5Oopen(g_id, name, H5P_DEFAULT);
		if ( obj_id < 0 )
			return (H5O_type_t)h5_error(f,
				H5_ERR_HDF5,
				"Can't open external link for object '%s'!",
				name);
		herr = H5Oget_info(obj_id, &objinfo);	
	}
	else { // H5L_TYPE_HARD
		herr = H5Oget_info_by_name(g_id, name, &objinfo, H5P_DEFAULT);
	}
	
	if ( herr < 0 )
		return (H5O_type_t)h5_error(f,
			H5_ERR_HDF5,
			"Can't query object with name '%s'!", name);

	return objinfo.type;
}

static herr_t
iter_op_count (
	hid_t g_id,
	const char* name,
	const H5L_info_t* info,
	void* _op_data
	) {
	op_data_t* op_data = (op_data_t*)_op_data;
	H5O_type_t type;
	TRY( type = _iter_op_get_obj_type(op_data->f, g_id, name, info) );
	if ( type != op_data->type ) return 0;
	op_data->cnt++;
	return 0;
}

static herr_t
iter_op_idx (
	hid_t g_id,
	const char* name,
	const H5L_info_t* info,
	void* _op_data
	) {
	op_data_t* op_data = (op_data_t*)_op_data;
	H5O_type_t type;
	TRY( type = _iter_op_get_obj_type(op_data->f, g_id, name, info) );
	if ( type != op_data->type ) return 0;
	op_data->cnt++;
	/* stop iterating if index is equal cnt */
	if (op_data->queried_idx == op_data->cnt) {
		memset (op_data->name, 0, op_data->len);
		strncpy (op_data->name, name, op_data->len-1);
		return 1;
	}
	return 0;
}

static herr_t
iter_op_count_match (
	hid_t g_id,
	const char* name,
	const H5L_info_t* info,
	void* _op_data
	) {
	op_data_t* op_data = (op_data_t*)_op_data;
	H5O_type_t type;
	TRY( type = _iter_op_get_obj_type(op_data->f, g_id, name, info) );
	if ( type != op_data->type ) return 0;
	/* count if prefix matches */
	if (strncmp (name, op_data->prefix, strlen(op_data->prefix)) == 0) {
		op_data->cnt++;
	}
	return 0;
}

ssize_t
h5_get_num_hdf5_groups (
	h5_file_t* const f,
	const hid_t loc_id
	) {
	op_data_t op_data;
	memset (&op_data, 0, sizeof (op_data));
	op_data.type = H5O_TYPE_GROUP;
	hsize_t start_idx = 0;
	herr_t herr = H5Literate (loc_id, H5_INDEX_NAME, H5_ITER_INC,
				  &start_idx,
				  iter_op_count, &op_data);
	if (herr < 0) {
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get number of groups in \"%s\".",
			h5_get_objname (loc_id));
	}
	return op_data.cnt;
}

ssize_t
h5_get_num_hdf5_groups_matching_prefix (
	h5_file_t* const f,
	const hid_t loc_id,
	char* prefix
	) {
	op_data_t op_data;
	memset (&op_data, 0, sizeof (op_data));
	op_data.f = f;
	op_data.type = H5O_TYPE_GROUP;
	op_data.prefix = prefix;
	hsize_t start_idx = 0;
	herr_t herr = H5Literate (loc_id, H5_INDEX_NAME, H5_ITER_INC,
				  &start_idx,
				  iter_op_count_match, &op_data);
	if (herr < 0) {
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get number of groups with prefix \"%s\" in \"%s\".",
			prefix, h5_get_objname (loc_id));
	}
	return op_data.cnt;
}

h5_err_t
h5_get_hdf5_groupname_by_idx (
	h5_file_t* const f,
	hid_t loc_id,
	hsize_t idx,
	char *name,
	size_t len
	) {
	op_data_t op_data;
	memset (&op_data, 0, sizeof (op_data));
	op_data.f = f;
	op_data.type = H5O_TYPE_GROUP;
	op_data.cnt = -1;
	op_data.queried_idx = idx;
        op_data.name = name;
        op_data.len = len;
	hsize_t start_idx = 0;
	herr_t herr = H5Literate (loc_id, H5_INDEX_NAME, H5_ITER_INC,
				  &start_idx,
				  iter_op_idx, &op_data);
	if (herr < 0) {
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get name of group with index \"%lu\" in \"%s\".",
			(long unsigned int)idx, h5_get_objname (loc_id));
	}
	return H5_SUCCESS;
}

ssize_t
h5_get_num_hdf5_datasets (
	h5_file_t* const f,
	const hid_t loc_id
	) {
	op_data_t op_data;
	memset (&op_data, 0, sizeof (op_data));
	op_data.f = f;
	op_data.type = H5O_TYPE_DATASET;
	hsize_t start_idx = 0;
	herr_t herr = H5Literate (loc_id, H5_INDEX_NAME, H5_ITER_INC,
				  &start_idx,
				  iter_op_count, &op_data);
	if (herr < 0) {
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get number of datasets in \"%s\".",
			h5_get_objname (loc_id));
	}
	return op_data.cnt;
}

/*
  Get name of the \c idx th dataset in group \c loc_id.
 */
h5_err_t
h5_get_hdf5_datasetname_by_idx (
	h5_file_t* const f,
	hid_t loc_id,
	hsize_t idx,
	char *name,
	size_t len
	) {
	op_data_t op_data;
	memset (&op_data, 0, sizeof (op_data));
	op_data.f = f;
	op_data.type = H5O_TYPE_DATASET;
	op_data.cnt = -1;
	op_data.queried_idx = idx;
        op_data.name = name;
        op_data.len = len;
	hsize_t start_idx = 0;
	herr_t herr = H5Literate (loc_id, H5_INDEX_NAME, H5_ITER_INC,
				  &start_idx,
				  iter_op_idx, &op_data);
	if (herr < 0) {
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get name of dataset with index \"%lu\" in \"%s\".",
			(long unsigned int)idx, h5_get_objname (loc_id));
	}
	return H5_SUCCESS;
}

/****** I d e n t i f i e r **************************************************/

const char *
h5_get_objname (
	hid_t id
	) {
	static char objname[256];

	memset ( objname, 0, sizeof(objname) );
	ssize_t size = H5Iget_name ( id, objname, sizeof(objname) );
	if ( size < 0 ) {
		strcpy ( objname, "[error getting object name]" );
	} else if ( size == 0 ) {
		strcpy ( objname, "[no name associated with identifier]" );
	}

	return objname;
}
