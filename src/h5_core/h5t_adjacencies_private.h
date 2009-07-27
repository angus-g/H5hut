#ifndef __H5T_ADJACENCIES_PRIVATE_H
#define __H5T_ADJACENCIES_PRIVATE_H

h5_err_t
_h5t_rebuild_adj_data (
	h5_file_t * const f
	);

h5_err_t
_h5t_find_te2 (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5_te_entry_t **retval
	);

h5_err_t
_h5t_find_td (
	h5_file_t * const f,
	h5_td_entry_t *item,
	h5_td_entry_t **retval
	);

h5_err_t
_h5t_find_td2 (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5_td_entry_t **rentry
	);
#endif
