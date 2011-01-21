#ifndef __H5T_HSEARCH_PRIVATE_H
#define __H5T_HSEARCH_PRIVATE_H

typedef struct h5t_te_entry_key {
	h5_loc_idx_t vids[2];
} h5t_te_entry_key_t;

typedef struct h5t_td_entry_key {
	h5_loc_idx_t vids[3];
} h5t_td_entry_key_t;

/*
  List of all upward adjacent elements of same coarsness of a specific face.
  The face is specified by its local vertex IDs.
 */
typedef struct h5_te_entry {
	h5t_te_entry_key_t key;
	h5_idlist_t* value;
} h5t_te_entry_t;

typedef struct h5_td_entry {
	h5t_td_entry_key_t key;
	h5_idlist_t* value;
} h5t_td_entry_t;

h5_err_t
h5tpriv_resize_te_htab (
	h5_file_t * const f,
	size_t nel
	);

h5_err_t
h5tpriv_search_tv2 (
	h5_file_t * const f,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx,
	h5_idlist_t **entry
	);

h5_err_t
h5tpriv_search_te2 (
	h5_file_t * const f,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx,
	h5_idlist_t **entry
	);

h5_err_t
h5tpriv_find_te (
	h5_file_t * const f,
	h5_loc_idx_t edge_id,
	h5_idlist_t **retval
	);

h5_err_t
h5tpriv_find_te2 (
	h5_file_t * const f,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx,
	h5_idlist_t **retval
	);

h5_err_t
h5tpriv_resize_td_htab (
	h5_file_t * const f,
	size_t nel
	);

h5_err_t
h5tpriv_search_td2 (
	h5_file_t * const f,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx,
	h5_idlist_t **entry
	);

h5_err_t
h5tpriv_find_td (
	h5_file_t * const f,
	h5_loc_idx_t triangle_id,
	h5_idlist_t **retval
	);

h5_err_t
h5tpriv_find_td2 (
	h5_file_t * const f,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx,
	h5_idlist_t **rentry
	);

h5_err_t
h5tpriv_find_tv2 (
	h5_file_t * const f,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx,
	h5_idlist_t **retval
	);
#endif
