#ifndef __H5_MAPS_H
#define __H5_MAPS_H

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
h5priv_alloc_idlist_items (
	h5_file_t * const f,
	h5_idlist_t *list,
	const h5_size_t	size
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
	h5_id_t id
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

h5_id_t
h5priv_find_idlist (
	h5_file_t * const f,
	h5_idlist_t *list,
	h5_id_t	item
	);

h5_id_t
h5priv_insert_idlist (
	h5_file_t * const f,
	h5_idlist_t *list,
	h5_id_t	item,
	h5_id_t idx
	);

h5_id_t
h5priv_search_idlist (
	h5_file_t * const f,
	h5_idlist_t *list,
	h5_id_t	item
	);

h5_err_t
h5priv_alloc_idmap (
	h5_file_t * const f,
	h5_idmap_t *map,
	const h5_size_t	size
	);

h5_err_t
h5priv_insert_idmap (
	h5_file_t * const f,
	h5_idmap_t *map,
	h5_id_t global_id,
	h5_id_t local_id
	);

h5_id_t
h5priv_search_idmap (
	h5_idmap_t *map,
	h5_id_t value
	);

h5_err_t
h5priv_sort_idmap (
	h5_idmap_t *map
	);

#endif
