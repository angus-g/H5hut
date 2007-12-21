#ifndef __READWRITE_H
#define __READWRITE_H

h5part_int64_t
H5_write_data (
	h5_file *f,
	const char *name,
	const void *array,
	const hid_t type,
	const hid_t groupid,
	const hid_t shape,
	const hid_t memshape,
	const hid_t diskshape
	) ;

h5part_int64_t
H5_get_num_objects (
	hid_t group_id,
	const char *group_name,
	const hid_t type
	);

h5part_int64_t
H5_get_num_objects_matching_pattern (
	hid_t group_id,
	const char *group_name,
	const hid_t type,
	char * const pattern
	);

h5part_int64_t
H5_get_object_name (
	hid_t group_id,
	const char *group_name,
	const hid_t type,
	const h5part_int64_t idx,
	char *obj_name,
	const h5part_int64_t len_obj_name
	);

h5part_int64_t
H5_set_step (
	h5_file *f,			/*!< [in]  Handle to open file */
	const h5part_int64_t step	/*!< [in]  Time-step to set. */
	);

hid_t
H5_normalize_h5_type (
	hid_t type
	);

h5part_int64_t
H5_get_dataset_type(
	hid_t group_id,
	const char *dataset_name
	);

h5part_int64_t
H5_has_index (
	h5_file *f,		/*!< [in]  Handle to open file */
	h5part_int64_t step	/*!< [in]  Step number to query */
	);

#endif
