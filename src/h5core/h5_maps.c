#include <stdlib.h>
#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

h5_err_t
h5priv_alloc_idlist_items (
	h5_file_t* const f,
	h5_idlist_t* list,
	const h5_size_t	size	// new size of list
	) {
	int new = (list->items == NULL);
	size_t size_in_bytes = size * sizeof (list->items[0]);
	TRY( list->items = h5priv_alloc (f, list->items, size_in_bytes) );
	list->size = size;
	if (new) list->num_items = 0;
	return H5_SUCCESS;
}

h5_err_t
h5priv_free_idlist_items (
	h5_file_t* const f,
	h5_idlist_t* list
	) {
	UNUSED_ARGUMENT (f);
	if (list->items != NULL) free (list->items);
	list->items = NULL;
	list->size = 0;
	list->num_items = 0;
	return H5_SUCCESS;
}

h5_err_t
h5priv_alloc_idlist (
	h5_file_t* const f,
	h5_idlist_t** list,
	const h5_size_t	size
	) {
	TRY( (*list = h5priv_alloc (f, NULL, sizeof (**list))) );
	memset (*list, 0, sizeof (**list));
	size_t size_in_bytes = size * sizeof ((*list)->items[0]);
	TRY( (*list)->items = h5priv_alloc (f, (*list)->items, size_in_bytes) );
	(*list)->size = size;
	return H5_SUCCESS;
}

h5_err_t
h5priv_free_idlist (
	h5_file_t* const f,
	h5_idlist_t** list
	) {
	if (*list == NULL) return H5_SUCCESS;
	TRY( h5priv_free_idlist_items (f, *list) );
	TRY( h5priv_free( f, *list) );
	*list = NULL;
	return H5_SUCCESS;
}

h5_err_t
h5priv_append_to_idlist (
	h5_file_t* const f,
	h5_idlist_t* list,
	h5_loc_id_t id
	) {
	if (list->num_items == list->size) {
		h5_size_t size = list->size;
		if (size == 0) {
			size = 2;
		} else {
			size *= 2;
		}
		TRY( h5priv_alloc_idlist_items (f, list, size) );
	}
	list->items[list->num_items++] = id;
	return H5_SUCCESS;
}

int
h5priv_cmp_ids_by_eid (
	const void* _id1,
	const void* _id2
	) {
	h5_loc_id_t	id1 = h5tpriv_get_elem_idx (*(h5_loc_id_t*)_id1); 
	h5_loc_id_t	id2 = h5tpriv_get_elem_idx (*(h5_loc_id_t*)_id2); 
	
	if (id1 < id2) return -1;
	if (id1 > id2) return 1;
	return 0;
}

int
h5priv_cmp_ids (
	const void* _id1,
	const void* _id2
	) {
	h5_loc_id_t	*id1 = (h5_loc_id_t*)_id1;
	h5_loc_id_t	*id2 = (h5_loc_id_t*)_id2;
	
	if (*id1 < *id2) return -1;
	if (*id1 > *id2) return 1;
	return 0;
}

h5_err_t
h5priv_sort_idlist_by_eid (
	h5_file_t* const f,
	h5_idlist_t* list
	) {
	UNUSED_ARGUMENT (f);
	qsort (
		list->items,
		list->num_items,
		sizeof (list->items[0]),
		h5priv_cmp_ids_by_eid);
	
	return H5_SUCCESS;
}

/*
  Find ID in sorted list
*/
h5_loc_id_t
h5priv_find_idlist (
	h5_file_t* const f,
	h5_idlist_t* list,
	h5_loc_id_t item
	) {
	UNUSED_ARGUMENT (f);
	register h5_loc_idx_t low = 0;
	register h5_loc_idx_t high = list->num_items - 1;
	while (low <= high) {
		register h5_loc_idx_t mid = (low + high) / 2;
		register h5_loc_id_t diff = list->items[mid] - item;
           	if ( diff > 0 )
               		high = mid - 1;
           	else if ( diff < 0 )
               		low = mid + 1;
           	else
               		return mid; // found
       	}
       	return -(low+1);  // not found
}

/*
  Add item to list at position given by \c idx.
*/
h5_loc_idx_t
h5priv_insert_idlist (
	h5_file_t* const f,
	h5_idlist_t* list,
	h5_loc_id_t item,
	h5_loc_idx_t idx
	) {
	if (list->num_items == list->size) {
		h5_size_t size = list->size;
		if (size == 0) {
			size = 16;
		} else {
			size *= 2;
		}
		TRY( h5priv_alloc_idlist_items (f, list, size) );
	}
	memmove ( 
		&list->items[idx+1],
		&list->items[idx],
		(list->num_items - idx) * sizeof (list->items[0]));
	list->items[idx] = item;
	list->num_items++;
	return idx;
}

/*
  Search in sorted list. If item is not in list, add it.
 */
h5_loc_idx_t
h5priv_search_idlist (
	h5_file_t* const f,
	h5_idlist_t* list,
	h5_loc_id_t item
	) {
	h5_loc_idx_t idx = h5priv_find_idlist (f, list, item);
	if (idx < 0) {
		idx = -(idx+1);
		idx = h5priv_insert_idlist (f, list, item, idx);
	}
	return idx;
}


h5_err_t
h5priv_alloc_idxmap (
	h5_file_t* const f,
	h5_idxmap_t* map,
	const h5_size_t	size
	) {
	int new = (map->items == NULL);
	size_t size_in_bytes = size * sizeof (map->items[0]);
	TRY( map->items = h5priv_alloc (f, map->items, size_in_bytes) );
	map->size = size;
	if (new) map->num_items = 0;
	return H5_SUCCESS;
}

h5_err_t
h5priv_insert_idxmap (
	h5_file_t* const f,
	h5_idxmap_t* map,
	h5_glb_idx_t glb_idx,
	h5_loc_idx_t loc_idx
	) {
	if (map->num_items == map->size)
		return HANDLE_H5_OVERFLOW_ERR (f, "g2lmap", (long long)map->size);

	h5_loc_idx_t i = h5priv_search_idxmap (map, glb_idx);
	if (i >= 0)			/* global id already in use ? */
		return -1;

	i = -(i+1);

	memmove ( 
		&map->items[i+1],
		&map->items[i],
		(map->num_items - i) * sizeof(map->items[0]));
	map->items[i].glb_idx = glb_idx;
	map->items[i].loc_idx = loc_idx;
	map->num_items++;

	return H5_SUCCESS;
}

/*!

  \ingroup h5_core

  binary search in id map. 

  \return index in array if found, othwise \c -(result+1) is the index
  where \c value must be inserted.

 */
h5_loc_idx_t
h5priv_search_idxmap (
	h5_idxmap_t* map,
	h5_glb_idx_t value
	) {

	register h5_loc_idx_t low = 0;
	register h5_loc_idx_t high = map->num_items - 1;
	while (low <= high) {
		register h5_loc_idx_t mid = (low + high) / 2;
		register h5_glb_idx_t diff = map->items[mid].glb_idx - value;
           	if ( diff > 0 )
               		high = mid - 1;
           	else if ( diff < 0 )
               		low = mid + 1;
           	else
               		return mid; // found
       	}
       	return -(low+1);  // not found
}

static int
cmp_idxmap_items (
	const void* _item1,
	const void* _item2
	) {
	h5_idxmap_el_t* item1 = (h5_idxmap_el_t*)_item1;
	h5_idxmap_el_t* item2 = (h5_idxmap_el_t*)_item2;

	if (item1->glb_idx < item2->glb_idx) return -1;
	if (item1->glb_idx > item2->glb_idx) return 1;
	
	return 0;
}

h5_err_t
h5priv_sort_idxmap (
	h5_idxmap_t* map
	) {
	qsort (	map->items, map->num_items, sizeof (map->items[0]),
		cmp_idxmap_items);
	return H5_SUCCESS;
}
