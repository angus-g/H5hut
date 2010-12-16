#ifndef __H5_MAPS_PRIVATE_H
#define __H5_MAPS_PRIVATE_H

h5_err_t
h5priv_alloc_idlist (
	h5_file_t * const f,
	h5_idlist_t **list,
	const h5_size_t	size
	);

h5_err_t
h5priv_free_idlist (
	h5_file_t * const f,
	h5_idlist_t **list
	);

h5_err_t
h5priv_free_idlist_items (
	h5_file_t * const f,
	h5_idlist_t *list
	);

h5_err_t
h5priv_append_to_idlist (
	h5_file_t * const f,
	h5_idlist_t *list,
	h5_loc_id_t id
	);

int
h5priv_cmp_ids_by_eid (
	const void *_id1,
	const void *_id2
	);

int
h5priv_cmp_ids (
	const void *_id1,
	const void *_id2
	);

h5_err_t
h5priv_sort_idlist_by_eid (
	h5_file_t * const f,
	h5_idlist_t *list
	);

h5_loc_id_t
h5priv_find_idlist (
	h5_file_t * const f,
	h5_idlist_t *list,
	h5_loc_id_t item
	);

h5_loc_idx_t
h5priv_insert_idlist (
	h5_file_t * const f,
	h5_idlist_t *list,
	h5_loc_id_t	item,
	h5_loc_idx_t idx
	);

h5_loc_idx_t
h5priv_search_idlist (
	h5_file_t * const f,
	h5_idlist_t *list,
	h5_loc_id_t item
	);

h5_err_t
h5priv_alloc_idxmap (
	h5_file_t * const f,
	h5_idxmap_t *map,
	const h5_size_t	size
	);

h5_err_t
h5priv_insert_idxmap (
	h5_file_t * const f,
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
