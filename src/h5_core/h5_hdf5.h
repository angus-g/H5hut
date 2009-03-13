#ifndef __H5_HDF5_H
#define __H5_HDF5_H

hid_t
_h5_open_group (
	h5_file_t *f,
	const hid_t parent_group_id,
	const char * const group_name
	);

h5_err_t
_h5_close_group (
	h5_file_t * const f,
	const hid_t group_id
	);

hid_t
_h5_open_dataset (
	h5_file_t * const f,
	const hid_t gid,
	const char * const dataset_name
	);

hid_t
_h5_open_dataset (
	h5_file_t * const f,
	const hid_t loc_id,
	const char * const dataset_name
	);

hid_t 
_h5_create_dataset_space (
	h5_file_t * const f,
	const int rank,
	const hsize_t * dims,
	const hsize_t * maxdims 
	);

hid_t
_h5_get_dataset_space (
	h5_file_t * const f,
	const hid_t dataset_id
	);

herr_t
_h5_close_dataspace (
	h5_file_t * const f,
	const hid_t dataspace_id
	);

hid_t
_h5_create_dataset (
	h5_file_t * const f,
	hid_t loc_id,
	const char * dataset_name,
	const hid_t type_id,
	const hid_t dataspace_id,
	const hid_t create_proplist
	);

herr_t
_h5_write_dataset (
	h5_file_t * const f,
	const hid_t dataset_id,
	const hid_t type_id,
	const hid_t memspace_id,
	const hid_t diskspace_id,
	const hid_t xfer_prop,
	const void * buf
	);

h5_err_t
_h5_read_dataset (
	h5_file_t * const f,
	const hid_t dataset_id,
	const hid_t type_id,
	const hid_t memspace_id,
	const hid_t diskspace_id,
	const hid_t xfer_prop,
	void * const buf );

hid_t
_h5_get_dataset_type ( 
	h5_file_t * const f,
	const hid_t dataset_id
	);

herr_t
_h5_close_dataset (
	h5_file_t * const f,
	const hid_t dataset_id
	);

hid_t
_h5_create_array_type (
	h5_file_t * const f,
	hid_t base_type_id,
	int rank,
	const hsize_t *dims
	);

hid_t
_h5_create_type (
	h5_file_t * const f,
	H5T_class_t _class,
	const size_t size
	);

herr_t
_h5_insert_type (
	h5_file_t * const f,
	hid_t dtype_id,
	const char * name,
	size_t offset,
	hid_t field_id
	);

herr_t
_h5_close_type (
	h5_file_t * const f,
	hid_t dtype_id
	);

hid_t
_h5_create_property (
	h5_file_t * const f,
	hid_t cls_id
	);

herr_t
_h5_set_chunk_property (
	h5_file_t * const f,
	hid_t plist,
	int ndims,
	const hsize_t * dim
	);

h5_err_t
_h5_close_property (
	h5_file_t * const f,
	hid_t prop
	);

herr_t
_h5_close_file (
	h5_file_t * const f,
	hid_t fileid
	);

herr_t
_h5_set_errorhandler (
	h5_file_t * const f,
	hid_t estack_id,
	H5E_auto_t func,
	void *client_data
	);

hid_t
_h5_open_attribute_by_name (
	hid_t loc_id,
	const char *attr_name
	);

hid_t
_h5_create_attribute (
	h5_file_t * const f,
	hid_t loc_id,
	const char *attr_name,
	hid_t type_id,
	hid_t space_id,
	hid_t acpl_id,
	hid_t aapl_id
	);

herr_t
_h5_read_attribute (
	h5_file_t * const f,
	hid_t attr_id,
	hid_t mem_type_id,
	void *buf
	);

herr_t
_h5_write_attribute (
	h5_file_t * const f,
	hid_t attr_id,
	hid_t mem_type_id,
	const void *buf
	);

ssize_t
_h5_get_attribute_name (
	h5_file_t * const f,
	hid_t attr_id,
	size_t buf_size,
	char *buf
	);

hid_t
_h5_get_attribute_type (
	h5_file_t * const f,
	hid_t attr_id
	);

hid_t
_h5_get_attribute_space (
	h5_file_t * const f,
	hid_t attr_id
	);

herr_t
_h5_close_attribute (
	h5_file_t * const f,
	hid_t attr_id
	);
#endif
