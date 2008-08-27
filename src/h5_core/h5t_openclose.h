#ifndef __T_OPENCLOSE_H
#define __T_OPENCLOSE_H

h5_err_t
_h5t_open_file (
	h5_file * f			/*!< IN: file handle */
	);

h5_err_t
_h5t_close_file (
	h5_file *fh		/*!< IN: file handle */
	);

h5_err_t
_h5t_init_fdata (
	h5_file * f
	);
#endif
