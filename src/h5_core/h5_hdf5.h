#ifndef __H5_HDF5_H
#define __H5_HDF5_H

hid_t
_h5_open_group (
	h5_file_t *f,
	const hid_t parent_gid,
	const char * const grpname
	);

h5_err_t
_h5_close_group (
	h5_file_t *f,
	hid_t group_id
	);
#endif
