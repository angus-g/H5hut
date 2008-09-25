#ifndef __H5T_READWRITE_PRIVATE_H
#define __H5T_READWRITE_PRIVATE_H

h5_err_t
_h5t_write_obj (
	h5_file * f,
	const hid_t	gid,
	const hsize_t  current_dims,
	const hsize_t  max_dims,
	const hid_t    tid,
	const void * const object,
	const char * const dsname
	);

h5_err_t
_h5t_read_mesh (
	h5_file *f
	);

#endif
