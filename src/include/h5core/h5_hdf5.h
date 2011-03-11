#ifndef __H5_HDF5_H
#define __H5_HDF5_H

ssize_t
hdf5_get_num_groups (
	const hid_t loc_id
	);

ssize_t
hdf5_get_num_groups_matching_prefix (
	const hid_t loc_id,
	char* prefix
	);

h5_err_t
hdf5_get_name_of_group_by_idx (
	hid_t loc_id,
	hsize_t idx,
	char *name,
	size_t len
	);

ssize_t
hdf5_get_num_datasets (
	const hid_t loc_id
	);

h5_err_t
hdf5_get_name_of_dataset_by_idx (
	hid_t loc_id,
	hsize_t idx,
	char *name,
	size_t len
	);

h5_err_t
hdf5_get_dataset_info_by_idx (
	const hid_t loc_id,
	const hsize_t idx,
	char* const name,
	const size_t len_name,
	h5_int64_t* const type,
	hsize_t* const npoints
	);

const char *
hdf5_get_objname (
	hid_t id
	);

#endif
