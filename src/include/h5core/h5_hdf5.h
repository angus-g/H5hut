#ifndef __H5_HDF5_H
#define __H5_HDF5_H

ssize_t
h5_get_num_hdf5_groups (
	const hid_t loc_id
	);

ssize_t
h5_get_num_hdf5_groups_matching_prefix (
	const hid_t loc_id,
	char* prefix
	);

h5_err_t
h5_get_hdf5_groupname_by_idx (
	hid_t loc_id,
	hsize_t idx,
	char *name,
	size_t len
	);

ssize_t
h5_get_num_hdf5_datasets (
	const hid_t loc_id
	);

h5_err_t
h5_get_hdf5_datasetname_by_idx (
	hid_t loc_id,
	hsize_t idx,
	char *name,
	size_t len
	);

const char *
h5_get_objname (
	hid_t id
	);

#endif
