#ifndef __HDF5_MISC_H
#define __HDF5_MISC_H

h5_int64_t
hdf5_get_num_objects (
	hid_t group_id,
	const char *group_name,
	const hid_t type
	);

h5_int64_t
hdf5_get_num_objects_matching_pattern (
	hid_t group_id,
	const char *group_name,
	const hid_t type,
	char * const pattern
	);

const char *
hdf5_get_objname (
	hid_t id
	);

h5_int64_t
hdf5_get_object_name (
	hid_t group_id,
	const char *group_name,
	const hid_t type,
	const h5_int64_t idx,
	char *obj_name,
	const h5_int64_t len_obj_name
	);

#endif
