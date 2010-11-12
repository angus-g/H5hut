#ifndef __H5T_OPENCLOSE_H
#define __H5T_OPENCLOSE_H

h5_err_t h5t_open_mesh (
	h5_file_t * const f,
	const h5_id_t id,
	const h5_oid_t type
	);
h5_err_t h5t_set_level (
	h5_file_t * const f,
	const h5t_lvl_idx_t id
	);
h5_err_t h5t_close_mesh (
	h5_file_t * const f
	);

#endif
