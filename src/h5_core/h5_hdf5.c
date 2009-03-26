#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

/****** G r o u p ************************************************************/
/*!
  Open HDF5 group. If group doesn't exist create it.

  \param[in]	f		file handle
  \param[in]	loc_id		location id
  \param[in]	group_name	name of group to open
*/
hid_t
_h5_open_group (
	h5_file_t * f,
	const hid_t loc_id,
	const char * const group_name
	) {
	hid_t group_id;
	herr_t herr = H5Gget_objinfo(
		loc_id, group_name, 1, NULL );

	/*
	  check access modes:
			Open	Create
	  H5_O_RDWR	x	x
	  H5_O_RDONLY	x	-
	  H5_O_WRONLY	x	x  (overwrite/append data to existing dataset)
	  H5_O_APPEND	x	x  (append datasets to an existing group)
	*/

	if ( herr >= 0 ) {
		h5_info (
			f,
			"Opening group %s/%s.",
			h5_get_objname ( loc_id ),
			group_name );
		group_id = H5Gopen ( loc_id, group_name, H5P_DEFAULT );
	} else {
		CHECK_WRITABLE_MODE( f );
		h5_info (
			f,
			"Creating group %s/%s.",
			h5_get_objname ( loc_id ),
			group_name );
		group_id = H5Gcreate ( loc_id, group_name, 0,
				  H5P_DEFAULT, H5P_DEFAULT );
	}
	if ( group_id < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot open group \"%s/%s\".",
			h5_get_objname ( loc_id ),
			group_name );

	return group_id;
}

/*!
  Close group.

  \param[in]	f		file handle
  \param[in]	group_id        id of group to close
*/
h5_err_t
_h5_close_group (
	h5_file_t * const f,
	const hid_t group_id
	) {
	if ( group_id <= 0 ) return H5_SUCCESS; 
	const char *group_name = h5_get_objname( group_id );
	if ( H5Gclose ( group_id ) < 0 ) {
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot terminate access to group \"%s\".",
			group_name );
	}
	return H5_SUCCESS;
}


/****** D a t a s e t ********************************************************/
/*!
  Open dataset. H5Dopen wrapper.

  \param[in]	f		file handle
  \param[in]	loc_id		location id
  \param[in]	dataset_name	name of dataset to open
 */
hid_t
_h5_open_dataset (
	h5_file_t * const f,
	const hid_t loc_id,
	const char * const dataset_name
	) {
	hid_t dataset_id;

	dataset_id = H5Dopen ( 
		loc_id,
		dataset_name,
		H5P_DEFAULT );
	if ( dataset_id < 0 )
		return 	h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot open dataset \"%s\".", dataset_name );

	return dataset_id;
}

/*!
  Get dataspace of existing dataset

  \param[in]	f		file handle
  \param[in]	dataset_id	id of dataset

 */
hid_t
_h5_get_dataset_space (
	h5_file_t * const f,
	const hid_t dataset_id
	) {
	hid_t dataspace_id = H5Dget_space ( dataset_id );
	if ( dataspace_id < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get dataspace for dataset \"%s\".",
			h5_get_objname ( dataset_id ) );
	return dataspace_id;
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
_h5_create_dataset (
	h5_file_t * const f,
	hid_t loc_id,
	const char * dataset_name,
	const hid_t type_id,
	const hid_t dataspace_id,
	const hid_t create_proplist
	) {
	hid_t dataset_id = H5Dcreate ( 
		loc_id,
		dataset_name,
		type_id,
		dataspace_id,
		H5P_DEFAULT,
		create_proplist,
		H5P_DEFAULT );
	if ( dataset_id < 0 )
		return h5_error(
			f,
			H5_ERR_HDF5,
			"Cannot create dataset name \"%s\"",
			dataset_name );
	return dataset_id;
}

/*!
  Write dataset.

  \param[in]	f		file handle
  \param[in]	dataset_id	id of dataset
  \param[in]	type_id		type used in dataset
  \param[in]	memspace_id	id of memory space 
  \param[in]	diskspace_id	id of disk space
  \param[in]	xfer_prop	transfer property list
  \param[in]	buf		buffer with date to write

 */
herr_t
_h5_write_dataset (
	h5_file_t * const f,
	const hid_t dataset_id,
	const hid_t type_id,
	const hid_t memspace_id,
	const hid_t diskspace_id,
	const hid_t xfer_prop,
	const void * buf
	) {
	herr_t herr = H5Dwrite (
		dataset_id,
		type_id,
		memspace_id,
		diskspace_id,
		xfer_prop,
		buf );
	if ( herr < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Write to dataset \"%s\" failed.",	\
			h5_get_objname(dataset_id) );

	return H5_SUCCESS;
}

h5_err_t
_h5_read_dataset (
	h5_file_t * const f,
	const hid_t dataset_id,
	const hid_t type_id,
	const hid_t memspace_id,
	const hid_t diskspace_id,
	const hid_t xfer_prop,
	void * const buf ) {

	herr_t herr = H5Dread (
		dataset_id,
		type_id,
		memspace_id,
		diskspace_id,
		xfer_prop,
		buf );
	if ( herr < 0 )
		return h5_error(
			f,
			H5_ERR_HDF5,
			"Read from dataset \"%s\".",	\
			h5_get_objname ( dataset_id ) );
	
	return H5_SUCCESS;
}

hid_t
_h5_get_dataset_type ( 
	h5_file_t * const f,
	const hid_t dataset_id
	) {
	hid_t datatype_id = H5Dget_type ( dataset_id );
	if ( datatype_id < 0 )
		h5_error(
			f,
			H5_ERR_HDF5,
			"Cannot determine dataset type.");

	return H5_SUCCESS;
}

/*!
  Close dataset.

  \param[in]	f		file handle
  \param[in]	dataset_id	id of dataset to close
*/
herr_t
_h5_close_dataset (
	h5_file_t * const f,
	const hid_t dataset_id
	) {
	if ( dataset_id == 0 || dataset_id == -1 ) return H5_SUCCESS; 
	const char *dataset_name = h5_get_objname( dataset_id );

	if ( H5Dclose ( dataset_id ) < 0 ) {
		return 	h5_error(
			f,
			H5_ERR_HDF5,
			"Close of dataset \"%s\" failed.", dataset_name );
	}
	return H5_SUCCESS;
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
_h5_create_space (
	h5_file_t * const f,
	const int rank,
	const hsize_t * dims,
	const hsize_t * maxdims 
	) {
	hid_t dataspace_id = H5Screate_simple ( rank, dims, maxdims );
	if ( dataspace_id < 0 )
		h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot create dataspace with rank %d.",
			rank );
	return dataspace_id;
}

herr_t
_h5_select_hyperslab_of_space (
	h5_file_t * const f,
	hid_t space_id,
	H5S_seloper_t op,
	const hsize_t *start,
	const hsize_t *stride,
	const hsize_t *count,
	const hsize_t *block
	) {
	herr_t herr = H5Sselect_hyperslab (
		space_id,
		op,
		start,
		stride,
		count,
		block );
	if ( herr < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot set select hyperslap region or add the "
			"specified region" );
	return H5_SUCCESS;
}

hssize_t
_h5_get_selected_npoints_of_space (
	h5_file_t * const f,
	hid_t space_id
	) {
	hssize_t size = H5Sget_select_npoints ( space_id );
	if ( size < 0 )
		h5_error(
			f,
			H5_ERR_HDF5,
			"Cannot determine number of selected elements in dataspace." );
	return size;
}

hssize_t
_h5_get_npoints_of_space (
	h5_file_t * const f,
	hid_t space_id
	) {
	hssize_t size = H5Sget_simple_extent_npoints ( space_id );
	if ( size < 0 )
		h5_error(
			f,
			H5_ERR_HDF5,
			"Cannot determine number of elements in dataspace." );
	return size;
}

int
_h5_get_dims_of_space (
	h5_file_t * const f,
	hid_t space_id,
	hsize_t *dims,
	hsize_t *maxdims 
	) {
	int rank = H5Sget_simple_extent_dims ( space_id, dims, maxdims );
	if ( rank < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot determine rank of dataspace." );
	return rank;
}


/*!
  Close space.

  \param[in]	f		file handle
  \param[in]	dataspace_id	id of space to close
*/
herr_t
_h5_close_dataspace (
	h5_file_t * const f,
	const hid_t dataspace_id
	) {
	if ( dataspace_id == 0 || dataspace_id == -1 || dataspace_id == H5S_ALL )
		return H5_SUCCESS; 

	herr_t herr = H5Sclose ( dataspace_id );
	if ( herr < 0 )
		return h5_error(
			f,
			H5_ERR_HDF5,
			"Cannot terminate access to dataspace \"%s\".",
			h5_get_objname( dataspace_id ) ); 

	return H5_SUCCESS;
}

/****** D a t a t y p e ******************************************************/

const char*
_h5_get_base_type_name (
	h5_file_t * const f,
	hid_t base_type_id
	) {
	if ( base_type_id == H5_INT32_T )	return "H5_INT32_T";
	if ( base_type_id == H5_INT64_T )	return "H5_INT64_T";
	if ( base_type_id == H5_FLOAT32_T )	return "H5_FLOAT32_T";
	if ( base_type_id == H5_FLOAT64_T )	return "H5_FLOAT64_T";

	return "[unknown]";
}

const char*
_h5_get_class_type_name (
	h5_file_t * const f,
	hid_t base_type_id
	) {
	if ( base_type_id == H5_COMPOUND_T )	return "H5_COMPOUND_T";

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
_h5_create_array_type (
	h5_file_t * const f,
	hid_t base_type_id,
	int rank,
	const hsize_t *dims
	) {
	hid_t type_id = H5Tarray_create( base_type_id, rank, dims );
	if ( type_id < 0 ) {
		return h5_error(
			f,
			H5_ERR_HDF5,
			"Can't create array datatype object with base "
			"type %s and rank %d",
			_h5_get_base_type_name ( f, base_type_id ),
			rank );
	}
	return type_id;
}

hid_t
_h5_create_type (
	h5_file_t * const f,
	H5T_class_t class,
	const size_t size
	) {
	hid_t type_id = H5Tcreate( class, size );
	if ( type_id < 0 ) {
		return h5_error(
			f,
			H5_ERR_HDF5,
			"Can't create datatype object of class %s.",
			_h5_get_class_type_name ( f, class )
			);
	}
	return type_id;
}

herr_t
_h5_insert_type (
	h5_file_t * const f,
	hid_t dtype_id,
	const char * name,
	size_t offset,
	hid_t field_id
	) {
	herr_t herr = H5Tinsert ( dtype_id, name, offset, field_id );
	if ( herr < 0 )
		return h5_error(
			f,
			H5_ERR_HDF5,
			"Can't insert field %s to compound datatype.",
			name );
	return H5_SUCCESS;
}

herr_t
_h5_close_type (
	h5_file_t * const f,
	hid_t dtype_id
	) {
	herr_t herr = H5Tclose ( dtype_id );
	if ( herr < 0 )
		return h5_error(
			f,
			H5_ERR_HDF5,
			"Cannot release datatype." );
	return H5_SUCCESS;
}

/****** P r o p e r t y ******************************************************/

hid_t
_h5_create_property (
	h5_file_t * const f,
	hid_t cls_id
	) {
	hid_t prop_id = H5Pcreate( cls_id );
	if ( prop_id < 0 ) 
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot create property list." );
	return prop_id;
}

herr_t
_h5_set_chunk_property (
	h5_file_t * const f,
	hid_t plist,
	int ndims,
	const hsize_t * dim
	) {
	herr_t herr = H5Pset_chunk ( plist, ndims, dim );
	if ( herr < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot add chunking property to list." );

	return H5_SUCCESS;
}

#ifdef PARALLEL_IO
h5_err_t
_h5_set_fapl_mpio_property (
	h5_file_t * const f,
	hid_t fapl_id,
	MPI_Comm comm,
	MPI_Info info
	) {
	herr_t herr = H5Pset_fapl_mpio ( fapl_id, comm, info );
	if ( herr < 0 )
		h5_error(
			f,
			H5_ERR_HDF5,
			"Cannot store IO communicator information to the "
			"file access property list.");
	return H5_SUCCESS;
}
#endif

h5_err_t
_h5_close_property (
	h5_file_t * const f,
	hid_t prop
	) {
	herr_t herr = H5Pclose ( prop );
	if ( herr < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot close property." );
	return H5_SUCCESS;
}

/****** F i l e **************************************************************/

herr_t
_h5_close_file (
	h5_file_t * const f,
	hid_t fileid
	) {
	herr_t herr = H5Fclose ( fileid );
	if ( herr < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot close file." );
	return H5_SUCCESS;
}

/****** E r r o r h a n d l i n g ********************************************/

herr_t
_h5_set_errorhandler (
	h5_file_t * const f,
	hid_t estack_id,
	H5E_auto_t func,
	void *client_data
	) {
	herr_t herr = H5Eset_auto ( estack_id, func, client_data );
	if ( herr < 0 )
		return h5_error(
			f,
			H5_ERR_INIT,
			"Cannot initialize H5." );
	return H5_SUCCESS;
}

/****** A t t r i b u t e ****************************************************/
hid_t
_h5_open_attribute (
	h5_file_t * const f,
	hid_t loc_id,
	const char *attr_name
	) {
	hid_t attr_id = H5Aopen ( loc_id, attr_name, H5P_DEFAULT );
	if ( attr_id < 0 ) 
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot open attribute \"%s\" of \"%s\".",
			attr_name,
			h5_get_objname( loc_id ) );
	return attr_id;
}

hid_t
_h5_open_attribute_idx (
	h5_file_t * const f,
	hid_t loc_id,
	unsigned int idx
	) {
	hid_t attr_id = H5Aopen_idx ( loc_id, idx );
	if ( attr_id < 0 ) 
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot open attribute \"%u\" of \"%s\".",
			idx,
			h5_get_objname( loc_id ) );
	return attr_id;
}	

hid_t
_h5_open_attribute_by_name (
	h5_file_t * const f,
	hid_t loc_id,
	const char *obj_name,
	const char *attr_name
	) {
	hid_t attr_id = H5Aopen_by_name (
		loc_id,
		obj_name,
		attr_name,
		H5P_DEFAULT,
		H5P_DEFAULT );
	if ( attr_id < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot open attribute \"%s\" of \"%s\".",
			attr_name,
			obj_name );
	return attr_id;
}

hid_t
_h5_create_attribute (
	h5_file_t * const f,
	hid_t loc_id,
	const char *attr_name,
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
		aapl_id );
	if ( attr_id < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot create attribute \"%s\" for \"%s\".",
			attr_name,
			h5_get_objname( loc_id ) );
	return attr_id;
}

herr_t
_h5_read_attribute (
	h5_file_t * const f,
	hid_t attr_id,
	hid_t mem_type_id,
	void *buf
	) {
	herr_t herr = H5Aread ( attr_id, mem_type_id, buf );
	if ( herr < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot read attribute \"%s\".",
			h5_get_objname( attr_id ) );

	return H5_SUCCESS;
}

herr_t
_h5_write_attribute (
	h5_file_t * const f,
	hid_t attr_id,
	hid_t mem_type_id,
	const void *buf
	) {
	herr_t herr = H5Awrite ( attr_id, mem_type_id, buf );
	if ( herr < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot write attribute \"%s\".",
			h5_get_objname( attr_id ) );

	return H5_SUCCESS;
}

ssize_t
_h5_get_attribute_name (
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
	return size;
}

hid_t
_h5_get_attribute_type (
	h5_file_t * const f,
	hid_t attr_id
	) {
	hid_t datatype_id = H5Aget_type ( attr_id );
	if ( datatype_id < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get type of attribute \"%s\".",
			h5_get_objname( attr_id ) );
	return datatype_id;
}

hid_t
_h5_get_attribute_space (
	h5_file_t * const f,
	hid_t attr_id
	) {
	hid_t space_id = H5Aget_space ( attr_id );
	if ( space_id < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get dataspace of attribute \"%s\".",
			h5_get_objname( attr_id ) );
	return space_id;
}

int
_h5_get_num_attributes (
	h5_file_t * const f,
	hid_t loc_id
	) {
	int num = H5Aget_num_attrs ( loc_id );
	if ( num < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot get number of attributes of \"%s\".",
			h5_get_objname( loc_id ) );
	return num;
}


herr_t
_h5_close_attribute (
	h5_file_t * const f,
	hid_t attr_id
	) {
	herr_t herr = H5Aclose ( attr_id );
	if ( herr < 0 )
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot close attribute \"%s\".",
			h5_get_objname( attr_id ) );

	return H5_SUCCESS;
}
