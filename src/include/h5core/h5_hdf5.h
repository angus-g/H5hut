#ifndef __H5_HDF5_H
#define __H5_HDF5_H

ssize_t
h5_get_num_hdf5_groups (
	h5_file_t* const f,
	const hid_t loc_id
	);

ssize_t
h5_get_num_hdf5_groups_matching_prefix (
	h5_file_t* const f,
	const hid_t loc_id,
	char* prefix
	);

h5_err_t
h5_get_hdf5_groupname_by_idx (
	h5_file_t* const f,
	hid_t loc_id,
	hsize_t idx,
	char *name,
	size_t size
	);

ssize_t
h5_get_num_hdf5_datasets (
	h5_file_t* const f,
	const hid_t loc_id
	);

h5_err_t
h5_get_hdf5_datasetname_by_idx (
	h5_file_t* const f,
	hid_t loc_id,
	hsize_t idx,
	char *name,
	size_t size
	);

const char *
h5_get_objname (
	hid_t id
	);

#endif
