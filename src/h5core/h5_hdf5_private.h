#ifndef __H5_HDF5_PRIVATE_H
#define __H5_HDF5_PRIVATE_H

/*** group ***/
hid_t
h5priv_open_hdf5_group (
	h5_file_t* const f,
	const hid_t loc_id,
	const char* const group_name
	);

hid_t
h5priv_create_hdf5_group (
	h5_file_t* const f,
	const hid_t loc_id,
	const char* const group_name
	);

hid_t
h5priv_open_group (
	h5_file_t* const f,
	const hid_t loc_id,
	const char* const group_name
	);

h5_err_t
h5priv_close_hdf5_group (
	h5_file_t* const f,
	const hid_t group_id
	);

hsize_t
h5priv_get_num_objs_in_hdf5_group (
	h5_file_t* const f,
	const hid_t group_id
	);

ssize_t
h5priv_get_hdf5_objname_by_idx (
	h5_file_t* const f,
	hid_t loc_id,
	hsize_t idx,
	char* name,
	size_t size
	);

/*** dataset ***/
hid_t
h5priv_open_hdf5_dataset (
	h5_file_t* const f,
	const hid_t gid,
	const char* const dataset_name
	);

hid_t
h5priv_create_hdf5_dataset (
	h5_file_t* const f,
	hid_t loc_id,
	const char* dataset_name,
	const hid_t type_id,
	const hid_t dataspace_id,
	const hid_t create_proplist
	);

herr_t
h5priv_close_hdf5_dataset (
	h5_file_t* const f,
	const hid_t dataset_id
	);


herr_t
h5priv_write_hdf5_dataset (
	h5_file_t* const f,
	const hid_t dataset_id,
	const hid_t type_id,
	const hid_t memspace_id,
	const hid_t diskspace_id,
	const hid_t xfer_prop,
	const void* buf
	);

h5_err_t
h5priv_read_hdf5_dataset (
	h5_file_t* const f,
	const hid_t dataset_id,
	const hid_t type_id,
	const hid_t memspace_id,
	const hid_t diskspace_id,
	const hid_t xfer_prop,
	void* const buf );

hid_t
h5priv_get_hdf5_dataset_type ( 
	h5_file_t* const f,
	const hid_t dataset_id
	);

herr_t
h5priv_set_hdf5_dataset_extent (
	h5_file_t* const f,
	hid_t dset_id,
	const hsize_t* size
	);

hssize_t
h5priv_get_npoints_of_hdf5_dataset (
	h5_file_t* const f,
	hid_t dset_id
	);

hssize_t
h5priv_get_npoints_of_hdf5_dataset_by_name (
	h5_file_t* const f,
	hid_t loc_id,
	char* name
	);

/*** dataspace ***/
hid_t 
h5priv_create_hdf5_dataspace (
	h5_file_t* const f,
	const int rank,
	const hsize_t* dims,
	const hsize_t* maxdims 
	);

hid_t
h5priv_get_hdf5_dataset_space (
	h5_file_t* const f,
	const hid_t dataset_id
	);

herr_t
h5priv_select_hyperslab_of_hdf5_dataspace (
	h5_file_t* const f,
	hid_t space_id,
	H5S_seloper_t op,
	const hsize_t* start,
	const hsize_t* stride,
	const hsize_t* count,
	const hsize_t* block
	);

herr_t
h5priv_select_elements_of_hdf5_dataspace (
	h5_file_t* const f,
	hid_t space_id,
	H5S_seloper_t op,
	hsize_t nelems,
	const hsize_t* indices
	);

hssize_t
h5priv_get_selected_npoints_of_hdf5_dataspace (
	h5_file_t* const f,
	hid_t space_id
	);

hssize_t
h5priv_get_npoints_of_hdf5_dataspace (
	h5_file_t* const f,
	hid_t space_id
	);

herr_t
h5priv_close_hdf5_dataspace (
	h5_file_t* const f,
	const hid_t dataspace_id
	);

int
h5priv_get_dims_of_hdf5_dataspace (
	h5_file_t* const f,
	hid_t space_id,
	hsize_t* dims,
	hsize_t* maxdims 
	);

/*** type ***/
hid_t
h5priv_create_hdf5_array_type (
	h5_file_t* const f,
	hid_t base_type_id,
	int rank,
	const hsize_t* dims
	);

hid_t
h5priv_create_hdf5_type (
	h5_file_t* const f,
	H5T_class_t _class,
	const size_t size
	);

herr_t
h5priv_insert_hdf5_type (
	h5_file_t* const f,
	hid_t dtype_id,
	const char* name,
	size_t offset,
	hid_t field_id
	);

herr_t
h5priv_close_hdf5_type (
	h5_file_t* const f,
	hid_t dtype_id
	);

/*** property ***/
hid_t
h5priv_create_hdf5_property (
	h5_file_t* const f,
	hid_t cls_id
	);

herr_t
h5priv_set_hdf5_chunk_property (
	h5_file_t* const f,
	hid_t plist,
	int ndims,
	hsize_t* dim
	);

herr_t
h5priv_set_hdf5_layout_property (
	h5_file_t* const f,
	hid_t plist,
	H5D_layout_t layout
	);

#ifdef PARALLEL_IO
h5_err_t
h5priv_set_hdf5_fapl_mpio_property (
	h5_file_t* const f,
	hid_t fapl_id,
	MPI_Comm comm,
	MPI_Info info
	);

h5_err_t
h5priv_set_hdf5_fapl_mpiposix_property (
	h5_file_t* const f,
	hid_t fapl_id,
	MPI_Comm comm,
	hbool_t use_gpfs
	);

h5_err_t
h5priv_set_hdf5_dxpl_mpio_property (
	h5_file_t* const f,
	hid_t dxpl_id,
	H5FD_mpio_xfer_t mode
	);
#endif

h5_err_t
h5priv_set_hdf5_mdc_property (
	h5_file_t* const f,
	hid_t fapl_id,
	H5AC_cache_config_t *config
	);

h5_err_t
h5priv_get_hdf5_mdc_property (
	h5_file_t* const f,
	hid_t fapl_id,
	H5AC_cache_config_t *config
	);

h5_err_t
h5priv_set_hdf5_alignment_property (
	h5_file_t* const f,
	const hid_t fapl_id,
	const hsize_t threshold,
	const hsize_t alignment
	);

h5_err_t
h5priv_set_hdf5_btree_ik_property (
	h5_file_t* const f,
	const hid_t fcpl_id,
	const hsize_t btree_ik
	);

h5_err_t
h5priv_close_hdf5_property (
	h5_file_t* const f,
	hid_t prop
	);

/*** file ***/
herr_t
h5priv_close_hdf5_file (
	h5_file_t* const f,
	hid_t fileid
	);

/*** error handling ***/
herr_t
h5priv_set_hdf5_errorhandler (
	h5_file_t* const f,
	hid_t estack_id,
	H5E_auto_t func,
	void* client_data
	);

/*** attributes ***/
hid_t
h5priv_open_hdf5_attribute (
	h5_file_t* const f,
	hid_t loc_id,
	const char *attr_name
	);

hid_t
h5priv_open_hdf5_attribute_idx (
	h5_file_t* const f,
	hid_t loc_id,
	unsigned int idx
	);

hid_t
h5priv_open_hdf5_attribute_by_name (
	h5_file_t* const f,
	hid_t loc_id,
	const char* obj_name,
	const char* attr_name
	);

hid_t
h5priv_create_hdf5_attribute (
	h5_file_t* const f,
	hid_t loc_id,
	const char* attr_name,
	hid_t type_id,
	hid_t space_id,
	hid_t acpl_id,
	hid_t aapl_id
	);

herr_t
h5priv_read_hdf5_attribute (
	h5_file_t* const f,
	hid_t attr_id,
	hid_t mem_type_id,
	void* buf
	);

herr_t
h5priv_write_hdf5_attribute (
	h5_file_t* const f,
	hid_t attr_id,
	hid_t mem_type_id,
	const void* buf
	);

ssize_t
h5priv_get_hdf5_attribute_name (
	h5_file_t* const f,
	hid_t attr_id,
	size_t buf_size,
	char* buf
	);

hid_t
h5priv_get_hdf5_attribute_type (
	h5_file_t* const f,
	hid_t attr_id
	);

hid_t
h5priv_get_hdf5_attribute_dataspace (
	h5_file_t* const f,
	hid_t attr_id
	);

int
h5priv_get_num_hdf5_attribute (
	h5_file_t* const f,
	hid_t loc_id
	);

herr_t
h5priv_close_hdf5_attribute (
	h5_file_t* const f,
	hid_t attr_id
	);

/*** link ***/
herr_t
h5priv_delete_hdf5_link (
	h5_file_t* const f,
	hid_t loc_id,
	const char* name,
	hid_t lapl_id
	);

#endif
