#ifndef __H5_READWRITE_PRIVATE_H
#define __H5_READWRITE_PRIVATE_H

h5_err_t
_h5_read_dataset (
	h5_file * const f,
	hid_t dataset_id,
	hid_t type_id,
	hid_t memspace_id,
	hid_t diskspace_id,
	void * const data );

#endif
