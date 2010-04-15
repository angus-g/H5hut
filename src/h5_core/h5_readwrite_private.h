#ifndef __H5_READWRITE_PRIVATE_H
#define __H5_READWRITE_PRIVATE_H

/*
  information about HDF5 dataset
*/
typedef struct h5_dataset_info {
	char name[256];
	int rank;
	hsize_t dims[4];
	hsize_t max_dims[4];
	hsize_t chunk_dims[4];
	hid_t type_id;
	hid_t create_prop;
	hid_t access_prop;
} h5_dsinfo_t;

h5_err_t
h5priv_write_dataset_by_name (
 	h5_file_t * const f,
	hid_t loc_id,
	h5_dsinfo_t *ds_info,
	hid_t (*set_memspace)(h5_file_t*,hid_t),
	hid_t (*set_diskspace)(h5_file_t*,hid_t),
	const void * const data
	);

h5_err_t
h5priv_read_dataset (
	h5_file_t * const f,
	hid_t dset_id,
	h5_dsinfo_t *ds_info,
	hid_t (*set_memspace)(h5_file_t*,hid_t),
	hid_t (*set_diskspace)(h5_file_t*,hid_t),
	void * const data
	);

h5_err_t
h5priv_read_dataset_by_name (
	h5_file_t * const f,
	hid_t loc_id,
	h5_dsinfo_t *ds_info,
	hid_t (*set_memspace)(h5_file_t*,hid_t),
	hid_t (*set_diskspace)(h5_file_t*,hid_t),
	void * const data
	);

#endif
