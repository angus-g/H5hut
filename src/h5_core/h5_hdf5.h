#ifndef __H5_HDF5_H
#define __H5_HDF5_H

hid_t
_h5_open_group (
	h5_file *f,
	const hid_t parent_gid,
	const char * const grpname
	);

h5_err_t
_h5_close_group (
	hid_t group_id
	);
#endif
