#ifndef __T_OPENCLOSE_H
#define __T_OPENCLOSE_H

h5_err_t
_h5t_open_file (
	h5_file_t * const f
	);

h5_err_t
_h5t_close_file (
	h5_file_t * const f
	);

h5_err_t
_h5t_init_fdata (
	h5_file_t * const f
	);

h5_err_t
h5t_open_mesh (
	h5_file_t * const f,
	const h5_id_t id,
	const h5_oid_t type
	);

h5_err_t
_h5t_open_topo_group (
	h5_file_t * const f
	);

h5_err_t
_h5t_open_meshes_group (
	h5_file_t * const f
	);

h5_err_t
_h5t_open_mesh_group (
	h5_file_t * const f
	);

h5_err_t
_h5t_close_mesh (
	h5_file_t * const f
	);

h5_err_t
h5t_open_level (
	h5_file_t * const f,
	const h5_id_t id
	);

h5_err_t
_h5t_init_step (
	h5_file_t * const f
	);

h5_err_t
_h5t_close_step (
	h5_file_t * const f
	);
#endif
