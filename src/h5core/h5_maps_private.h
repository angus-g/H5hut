#ifndef __H5_MAPS_PRIVATE_H
#define __H5_MAPS_PRIVATE_H

typedef struct {
	size_t	size;
	size_t	num_items;
	char* items[1];
} h5_strlist_t;


h5_err_t
h5priv_alloc_idlist (
	h5_loc_idlist_t **list,
	const int32_t	size
	);

h5_err_t
h5priv_free_idlist (
	h5_loc_idlist_t **list
	);

h5_loc_id_t
h5priv_find_idlist (
	h5_loc_idlist_t* list,
	h5_loc_id_t item
	);

h5_loc_idx_t
h5priv_insert_idlist (
	h5_loc_idlist_t** list,
	h5_loc_id_t	item,
	h5_loc_idx_t idx
	);

h5_loc_idx_t
h5priv_search_idlist (
	h5_loc_idlist_t** list,
	h5_loc_id_t item
	);

h5_err_t
h5priv_alloc_strlist (h5_strlist_t**, const h5_size_t);

h5_err_t
h5priv_free_strlist (h5_strlist_t**);

ssize_t
h5priv_insert_strlist (h5_strlist_t**, const char const*, size_t);

ssize_t
h5priv_find_strlist (h5_strlist_t*, const char* const item);

ssize_t
h5priv_search_strlist (h5_strlist_t**, const char* const);

h5_err_t
h5priv_remove_strlist (h5_strlist_t*, const char* const);

h5_err_t
h5priv_alloc_idxmap (
	h5_idxmap_t *map,
	const h5_size_t	size
	);

h5_err_t
h5priv_insert_idxmap (
	h5_idxmap_t *map,
	h5_glb_idx_t glb_idx,
	h5_loc_idx_t loc_idx
	);

h5_loc_idx_t
h5priv_search_idxmap (
	h5_idxmap_t *map,
	h5_glb_idx_t value
	);

h5_err_t
h5priv_sort_idxmap (
	h5_idxmap_t *map
	);

#endif
