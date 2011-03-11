#ifndef __H5_HDF5_PRIVATE_H
#define __H5_HDF5_PRIVATE_H

/*** group ***/
hid_t
hdf5_open_group (
	const hid_t loc_id,
	const char* const group_name
	);

hid_t
hdf5_create_group (
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
hdf5_close_group (
	const hid_t group_id
	);

h5_ssize_t
hdf5_get_num_objs_in_group (
	const hid_t group_id
	);

h5_ssize_t
hdf5_get_objname_by_idx (
	hid_t loc_id,
	hsize_t idx,
	char* name,
	size_t size
	);

/*** dataset ***/
hid_t
hdf5_open_dataset (
	const hid_t gid,
	const char* const dataset_name
	);

hid_t
hdf5_create_dataset (
	hid_t loc_id,
	const char* dataset_name,
	const hid_t type_id,
	const hid_t dataspace_id,
	const hid_t create_proplist
	);

h5_err_t
hdf5_close_dataset (
	const hid_t dataset_id
	);

h5_err_t
hdf5_write_dataset (
	const hid_t dataset_id,
	const hid_t type_id,
	const hid_t memspace_id,
	const hid_t diskspace_id,
	const hid_t xfer_prop,
	const void* buf
	);

h5_err_t
hdf5_read_dataset (
	const hid_t dataset_id,
	const hid_t type_id,
	const hid_t memspace_id,
	const hid_t diskspace_id,
	const hid_t xfer_prop,
	void* const buf );

hid_t
hdf5_get_dataset_type ( 
	const hid_t dataset_id
	);

h5_err_t
hdf5_set_dataset_extent (
	hid_t dset_id,
	const hsize_t* size
	);

h5_ssize_t
hdf5_get_npoints_of_dataset (
	hid_t dset_id
	);

h5_ssize_t
hdf5_get_npoints_of_dataset_by_name (
	const hid_t loc_id,
	const char* const name
	);

/*** dataspace ***/
hid_t 
hdf5_create_dataspace (
	const int rank,
	const hsize_t* dims,
	const hsize_t* maxdims 
	);

hid_t 
hdf5_create_dataspace_scalar (
	);

hid_t
hdf5_get_dataset_space (
	const hid_t dataset_id
	);

h5_err_t
hdf5_select_hyperslab_of_dataspace (
	hid_t space_id,
	H5S_seloper_t op,
	const hsize_t* start,
	const hsize_t* stride,
	const hsize_t* count,
	const hsize_t* block
	);

h5_err_t
hdf5_select_elements_of_dataspace (
	hid_t space_id,
	H5S_seloper_t op,
	hsize_t nelems,
	const hsize_t* indices
	);

h5_ssize_t
hdf5_get_selected_npoints_of_dataspace (
	hid_t space_id
	);

h5_ssize_t
hdf5_get_npoints_of_dataspace (
	hid_t space_id
	);

h5_err_t
hdf5_close_dataspace (
	const hid_t dataspace_id
	);

int
hdf5_get_dims_of_dataspace (
	hid_t space_id,
	hsize_t* dims,
	hsize_t* maxdims 
	);

/*** type ***/
hid_t
hdf5_create_array_type (
	hid_t base_type_id,
	int rank,
	const hsize_t* dims
	);

hid_t
hdf5_create_type (
	H5T_class_t _class,
	const size_t size
	);

hid_t
hdf5_create_string_type(
	const hsize_t len
	);

h5_err_t
hdf5_insert_type (
	hid_t dtype_id,
	const char* name,
	size_t offset,
	hid_t field_id
	);

h5_err_t
hdf5_close_type (
	hid_t dtype_id
	);

/*** property ***/
hid_t
hdf5_create_property (
	hid_t cls_id
	);

hid_t
hdf5_get_dataset_create_plist (
	const hid_t dataset_id
	);

h5_err_t
hdf5_set_chunk_property (
	hid_t plist,
	int rank,
	hsize_t* dims
	);

h5_err_t
hdf5_get_chunk_property (
	hid_t plist,
	int rank,
	hsize_t* dims
	);

h5_err_t
hdf5_set_layout_property (
	hid_t plist,
	H5D_layout_t layout
	);

#ifdef PARALLEL_IO
h5_err_t
hdf5_set_fapl_mpio_property (
	hid_t fapl_id,
	MPI_Comm comm,
	MPI_Info info
	);

h5_err_t
hdf5_set_fapl_mpiposix_property (
	hid_t fapl_id,
	MPI_Comm comm,
	hbool_t use_gpfs
	);

h5_err_t
hdf5_set_dxpl_mpio_property (
	hid_t dxpl_id,
	H5FD_mpio_xfer_t mode
	);
#endif

h5_err_t
hdf5_set_mdc_property (
	hid_t fapl_id,
	H5AC_cache_config_t *config
	);

h5_err_t
hdf5_get_mdc_property (
	hid_t fapl_id,
	H5AC_cache_config_t *config
	);

h5_err_t
hdf5_set_alignment_property (
	const hid_t fapl_id,
	const hsize_t threshold,
	const hsize_t alignment
	);

h5_err_t
hdf5_set_btree_ik_property (
	const hid_t fcpl_id,
	const hsize_t btree_ik
	);

h5_err_t
hdf5_close_property (
	hid_t prop
	);

/*** file ***/
h5_err_t
hdf5_close_file (
	hid_t fileid
	);

/*** error handling ***/
h5_err_t
hdf5_set_errorhandler (
	hid_t estack_id,
	H5E_auto_t func,
	void* client_data
	);

/*** attributes ***/
hid_t
hdf5_open_attribute (
	hid_t loc_id,
	const char *attr_name
	);

hid_t
hdf5_open_attribute_idx (
	hid_t loc_id,
	unsigned int idx
	);

hid_t
hdf5_open_attribute_by_name (
	hid_t loc_id,
	const char* obj_name,
	const char* attr_name
	);

hid_t
hdf5_create_attribute (
	hid_t loc_id,
	const char* attr_name,
	hid_t type_id,
	hid_t space_id,
	hid_t acpl_id,
	hid_t aapl_id
	);

h5_err_t
hdf5_read_attribute (
	hid_t attr_id,
	hid_t mem_type_id,
	void* buf
	);

h5_err_t
hdf5_write_attribute (
	hid_t attr_id,
	hid_t mem_type_id,
	const void* buf
	);

h5_ssize_t
hdf5_get_attribute_name (
	hid_t attr_id,
	size_t buf_size,
	char* buf
	);

hid_t
hdf5_get_attribute_type (
	hid_t attr_id
	);

hid_t
hdf5_get_attribute_dataspace (
	hid_t attr_id
	);

int
hdf5_get_num_attribute (
	hid_t loc_id
	);

h5_err_t
hdf5_close_attribute (
	hid_t attr_id
	);

char_p
hdf5_get_type_name (
	hid_t base_type_id
	);

/*** link ***/
h5_err_t
hdf5_link_exists (
	const hid_t loc_id,
	const char* name
	);

h5_err_t
hdf5_delete_link (
	hid_t loc_id,
	const char* name,
	hid_t lapl_id
	);

/*** other ***/
h5_err_t
hdf5_get_dataset_info_by_name (
	const hid_t loc_id,
	const hsize_t idx,
	const char* name,
	h5_int64_t* const type,
	hsize_t* const npoints
	);
#endif
