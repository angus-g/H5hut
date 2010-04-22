#ifndef __H5T_HSEARCH_PRIVATE_H
#define __H5T_HSEARCH_PRIVATE_H

typedef struct h5t_te_entry_key {
	h5_id_t vids[2];
} h5t_te_entry_key_t;

typedef struct h5_te_entry {
	h5_idlist_t value;
	h5t_te_entry_key_t key;
} h5t_te_entry_t;

typedef struct h5t_td_entry_key {
	h5_id_t vids[3];
} h5t_td_entry_key_t;

typedef struct h5_td_entry {
	h5_idlist_t value;
	h5t_td_entry_key_t key;
} h5t_td_entry_t;


h5_err_t
h5tpriv_create_te_htab (
	h5_file_t * const f,
	size_t nel
	);

h5_err_t
h5tpriv_resize_te_htab (
	h5_file_t * const f,
	size_t nel
	);

h5_err_t
h5tpriv_search_te2 (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5t_te_entry_t **entry
	);

h5_err_t
h5tpriv_find_te (
	h5_file_t * const f,
	h5t_te_entry_t *item,
	h5t_te_entry_t **retval
	);

h5_err_t
h5tpriv_find_te2 (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5t_te_entry_t **retval
	);


h5_err_t
h5tpriv_create_td_htab (
	h5_file_t * const f,
	size_t nel
	);

h5_err_t
h5tpriv_resize_td_htab (
	h5_file_t * const f,
	size_t nel
	);

h5_err_t
h5tpriv_search_td2 (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5t_td_entry_t **entry
	);

h5_err_t
h5tpriv_find_td (
	h5_file_t * const f,
	h5t_td_entry_t *item,
	h5t_td_entry_t **retval
	);

h5_err_t
h5tpriv_find_td2 (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5t_td_entry_t **rentry
	);

h5_err_t
h5tpriv_find_tv2 (
	h5_file_t * const f,
	h5_id_t cid,
	h5_id_t el_idx,
	h5_idlist_t **retval
	);
#endif
