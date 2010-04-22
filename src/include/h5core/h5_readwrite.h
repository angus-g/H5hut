#ifndef __H5_READWRITE_H
#define __H5_READWRITE_H

h5_int64_t
h5_write_data (
	h5_file_t *f,
	const char *name,
	const void *array,
	const hid_t type_id,
	const hid_t group_id,
	const hid_t memspace_id,
	const hid_t diskspace_id
	) ;

h5_int64_t
h5_set_step (
	h5_file_t * const f,
	const h5_int64_t step	/*!< [in]  Time-step to set. */
	);

hid_t
h5_normalize_h5_type (
	h5_file_t * const f,
	hid_t type
	);

h5_int64_t
h5_get_dataset_type(
	h5_file_t * const f,
	hid_t group_id,
	const char *dataset_name
	);

h5_int64_t
h5_has_index (
	h5_file_t * const f,	/*!< [in]  Handle to open file */
	h5_int64_t step		/*!< [in]  Step number to query */
	);

#endif
