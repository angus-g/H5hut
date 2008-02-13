#ifndef __T_READWRITE_H
#define __T_READWRITE_H

h5_err_t
_h5t_init_step (
	h5_file * f
	);

h5_err_t
_h5t_close_step (
	h5_file * f
	);


h5_id_t
H5t_add_mesh (
	h5_file * f
	);

h5_id_t
_h5t_open_mesh (
	h5_file * f
	);

h5_id_t
_h5t_close_mesh (
	h5_file * f
	);

#endif
