#ifndef __H5T_HSEARCH_PRIVATE_H
#define __H5T_HSEARCH_PRIVATE_H

h5_err_t
_h5t_create_te_htab (
	h5_file_t * const f,
	size_t nel
	);

h5_err_t
_h5t_resize_te_htab (
	h5_file_t * const f,
	size_t nel
	);

h5_err_t
_h5t_search_te2 (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5_te_entry_t **entry
	);

h5_err_t
_h5t_find_te (
	h5_file_t * const f,
	h5_te_entry_t *item,
	h5_te_entry_t **retval
	);

h5_err_t
_h5t_find_te2 (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5_te_entry_t **retval
	);


h5_err_t
_h5t_create_td_htab (
	h5_file_t * const f,
	size_t nel
	);

h5_err_t
_h5t_resize_td_htab (
	h5_file_t * const f,
	size_t nel
	);

h5_err_t
_h5t_search_td2 (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5_td_entry_t **entry
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
