#ifndef __H5_HDF5_H
#define __H5_HDF5_H

/*** group ***/
hid_t
_hdf_open_group (
	h5_file_t *f,
	const hid_t loc_id,
	const char * const group_name
	);

hid_t
_hdf_create_group (
	h5_file_t * f,
	const hid_t loc_id,
	const char * const group_name
	);

hid_t
_h5_open_group (
	h5_file_t *f,
	const hid_t loc_id,
	const char * const group_name
	);

h5_err_t
_hdf_close_group (
	h5_file_t * const f,
	const hid_t group_id
	);

hsize_t
_hdf_get_num_objs_in_group (
	h5_file_t * const f,
	const hid_t group_id
	);

h5_err_t
_hdf_get_objname_by_idx_in_group (
	h5_file_t * const f,
	hid_t loc_id,
	hsize_t idx,
	char **name );

/*** dataset ***/
hid_t
_hdf_open_dataset (
	h5_file_t * const f,
	const hid_t gid,
	const char * const dataset_name
	);

hid_t
_hdf_create_dataset (
	h5_file_t * const f,
	hid_t loc_id,
	const char * dataset_name,
	const hid_t type_id,
	const hid_t dataspace_id,
	const hid_t create_proplist
	);

herr_t
_hdf_close_dataset (
	h5_file_t * const f,
	const hid_t dataset_id
	);


herr_t
_hdf_write_dataset (
	h5_file_t * const f,
	const hid_t dataset_id,
	const hid_t type_id,
	const hid_t memspace_id,
	const hid_t diskspace_id,
	const hid_t xfer_prop,
	const void * buf
	);

h5_err_t
_hdf_read_dataset (
	h5_file_t * const f,
	const hid_t dataset_id,
	const hid_t type_id,
	const hid_t memspace_id,
	const hid_t diskspace_id,
	const hid_t xfer_prop,
	void * const buf );

hid_t
_hdf_get_dataset_type ( 
	h5_file_t * const f,
	const hid_t dataset_id
	);

herr_t
_hdf_set_dataset_extent (
	h5_file_t * const f,
	hid_t dset_id,
	const hsize_t *size
	);

hssize_t
_hdf_get_npoints_of_dataset (
	h5_file_t * const f,
	hid_t dset_id
	);

hssize_t
_hdf_get_npoints_of_dataset_by_name (
	h5_file_t * const f,
	hid_t loc_id,
	char * name
	);

/*** dataspace ***/
hid_t 
_hdf_create_dataspace (
	h5_file_t * const f,
	const int rank,
	const hsize_t * dims,
	const hsize_t * maxdims 
	);

hid_t
_hdf_get_dataset_space (
	h5_file_t * const f,
	const hid_t dataset_id
	);

herr_t
_hdf_select_hyperslab_of_dataspace (
	h5_file_t * const f,
	hid_t space_id,
	H5S_seloper_t op,
	const hsize_t *start,
	const hsize_t *stride,
	const hsize_t *count,
	const hsize_t *block
	);

hssize_t
_hdf_get_selected_npoints_of_dataspace (
	h5_file_t * const f,
	hid_t space_id
	);

hssize_t
_hdf_get_npoints_of_dataspace (
	h5_file_t * const f,
	hid_t space_id
	);

herr_t
_hdf_close_dataspace (
	h5_file_t * const f,
	const hid_t dataspace_id
	);

int
_hdf_get_dims_of_dataspace (
	h5_file_t * const f,
	hid_t space_id,
	hsize_t *dims,
	hsize_t *maxdims 
	);

/*** type ***/
hid_t
_hdf_create_array_type (
	h5_file_t * const f,
	hid_t base_type_id,
	int rank,
	const hsize_t *dims
	);

hid_t
_hdf_create_type (
	h5_file_t * const f,
	H5T_class_t _class,
	const size_t size
	);

herr_t
_hdf_insert_type (
	h5_file_t * const f,
	hid_t dtype_id,
	const char * name,
	size_t offset,
	hid_t field_id
	);

herr_t
_hdf_close_type (
	h5_file_t * const f,
	hid_t dtype_id
	);

/*** property ***/
hid_t
_hdf_create_property (
	h5_file_t * const f,
	hid_t cls_id
	);

herr_t
_hdf_set_chunk_property (
	h5_file_t * const f,
	hid_t plist,
	int ndims,
	const hsize_t * dim
	);

#ifdef PARALLEL_IO
h5_err_t
_hdf_set_fapl_mpio_property (
	h5_file_t * const f,
	hid_t fapl_id,
	MPI_Comm comm,
	MPI_Info info
	);
#endif

h5_err_t
_hdf_close_property (
	h5_file_t * const f,
	hid_t prop
	);

/*** file ***/
herr_t
_hdf_close_file (
	h5_file_t * const f,
	hid_t fileid
	);

/*** error handling ***/
herr_t
_hdf_set_errorhandler (
	h5_file_t * const f,
	hid_t estack_id,
	H5E_auto_t func,
	void *client_data
	);

/*** attributes ***/
hid_t
_hdf_open_attribute (
	h5_file_t * const f,
	hid_t loc_id,
	const char *attr_name
	);

hid_t
_hdf_open_attribute_idx (
	h5_file_t * const f,
	hid_t loc_id,
	unsigned int idx
	);

hid_t
_hdf_open_attribute_by_name (
	h5_file_t * const f,
	hid_t loc_id,
	const char *obj_name,
	const char *attr_name
	);

hid_t
_hdf_open_attribute_by_idx (
	h5_file_t * const f,	
	hid_t loc_id,
	const char *obj_name,
	H5_index_t idx_type,
	H5_iter_order_t order,
	hsize_t n,
	hid_t aapl_id,
	hid_t lapl_id
	);

hid_t
_hdf_create_attribute (
	h5_file_t * const f,
	hid_t loc_id,
	const char *attr_name,
	hid_t type_id,
	hid_t space_id,
	hid_t acpl_id,
	hid_t aapl_id
	);

herr_t
_hdf_read_attribute (
	h5_file_t * const f,
	hid_t attr_id,
	hid_t mem_type_id,
	void *buf
	);

herr_t
_hdf_write_attribute (
	h5_file_t * const f,
	hid_t attr_id,
	hid_t mem_type_id,
	const void *buf
	);

ssize_t
_hdf_get_attribute_name (
	h5_file_t * const f,
	hid_t attr_id,
	size_t buf_size,
	char *buf
	);

hid_t
_hdf_get_attribute_type (
	h5_file_t * const f,
	hid_t attr_id
	);

hid_t
_hdf_get_attribute_dataspace (
	h5_file_t * const f,
	hid_t attr_id
	);

int
_hdf_get_num_attributes (
	h5_file_t * const f,
	hid_t loc_id
	);

herr_t
_hdf_close_attribute (
	h5_file_t * const f,
	hid_t attr_id
	);
#endif
