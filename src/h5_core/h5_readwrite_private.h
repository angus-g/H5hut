#ifndef __H5_READWRITE_PRIVATE_H
#define __H5_READWRITE_PRIVATE_H

h5_err_t
_h5_write (
 	h5_file_t * const f,
	hid_t loc_id,
	struct h5_dataset_info *ds_info,
	hid_t (*set_memspace)(h5_file_t*,hid_t),
	hid_t (*set_diskspace)(h5_file_t*,hid_t),
	const void * const data
	);

h5_err_t
_h5_read (
	h5_file_t * const f,
	hid_t loc_id,
	h5_dataset_info_t *ds_info,
	hid_t (*set_memspace)(h5_file_t*,hid_t),
	hid_t (*set_diskspace)(h5_file_t*,hid_t),
	void * const data
	);
#endif
