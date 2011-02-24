#ifndef __H5_MAPS_PRIVATE_H
#define __H5_MAPS_PRIVATE_H

h5_err_t
h5priv_alloc_idlist (
	h5_loc_idlist_t **list,
	const h5_size_t	size
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
