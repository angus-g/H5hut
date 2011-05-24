#ifndef __H5_READWRITE_H
#define __H5_READWRITE_H

#ifdef __cplusplus
extern "C" {
#endif

h5_int64_t
h5_set_step (
	h5_file_t * const f,
	const h5_int64_t step	/*!< [in]  Time-step to set. */
	);

h5_int64_t
h5_normalize_h5_type (
	hid_t type
	);

h5_int64_t
h5_get_dataset_type(
	const hid_t group_id,
	const char *dataset_name
	);

h5_int64_t
h5_has_index (
	h5_file_t * const f,	/*!< [in]  Handle to open file */
	h5_int64_t step		/*!< [in]  Step number to query */
	);

h5_err_t
h5_normalize_dataset_name (
	const char *name,
	char *name2
	);

h5_err_t
h5_set_throttle (
	h5_file_t *f,
	int factor
	);

h5_err_t
h5_start_throttle (
	h5_file_t *f
	);

h5_err_t
h5_end_throttle (
	h5_file_t *f
	);

#ifdef __cplusplus
}
#endif

#endif
