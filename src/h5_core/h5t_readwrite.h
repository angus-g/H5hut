#ifndef __H5T_READWRITE_H
#define __H5T_READWRITE_H

h5_err_t
_h5t_write_mesh (
	h5_file_t * f
	);

h5_err_t
_h5t_read_vertices (
	h5_file_t * f
	);

h5_err_t
_h5t_read_elems (
	h5_file_t * f
	);

#endif
