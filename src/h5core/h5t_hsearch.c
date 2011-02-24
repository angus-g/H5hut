#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"


h5_err_t
h5tpriv_search_tv2 (
	h5_file_t* const f,
	h5_loc_idx_t face_idx,		// in
	h5_loc_idx_t elem_idx,		// in
	h5_loc_idlist_t** idlist	// out
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;

	h5_loc_idx_t vertex_idx;
	TRY (h5t_get_vertex_index_of_vertex2 (
		     f,
		     face_idx, elem_idx,
		     &vertex_idx));

	TRY (h5priv_search_idlist (
		     &t->adjacencies.tv.v[vertex_idx],
		     h5tpriv_build_vertex_id (face_idx, elem_idx)));
	if (idlist) {
		*idlist = t->adjacencies.tv.v[vertex_idx];
	}

	H5_PRIV_API_RETURN (H5_SUCCESS);
}

static int
cmp_te_entries (
	const void* __a,
	const void* __b
	) {
	h5t_te_entry_t* a = (h5t_te_entry_t*)__a;
	h5t_te_entry_t* b = (h5t_te_entry_t*)__b;
	return memcmp (a->key.vids, b->key.vids, sizeof (a->key.vids));
}

static unsigned int
compute_te_hashval (
	const void* __item
	) {
	h5t_te_entry_t* item = (h5t_te_entry_t*)__item;
	uint16_t* key =  (uint16_t*)item->key.vids;
	unsigned int count = 2*sizeof (item->key.vids[0]) / sizeof (uint16_t); 
	unsigned int hval = count;
	while (count--) {
		if (*key) {
			hval <<= 6;
			hval += *key;
		}
		key++;
	}
	return hval;
}

static h5_err_t
release_te_entry (
	const void* __entry
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	struct h5_te_entry* entry = *(struct h5_te_entry**)__entry;
	h5_loc_idlist_t* list = entry->value;
	TRY (h5priv_free_idlist (&list));
	TRY (h5_free (entry));
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

h5_err_t
h5tpriv_resize_te_htab (
	h5_file_t* const f,
	size_t nel
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	h5t_adjacencies_t* a = &f->t->adjacencies;
	if ( a->te_hash.size == 0 ) {
		TRY (h5priv_hcreate (
			     nel,
			     &a->te_hash,
			     cmp_te_entries,
			     compute_te_hashval,
			     release_te_entry));
	} else if (a->te_hash.size < nel) {
		TRY (h5priv_hresize (nel, &a->te_hash));
	}
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5tpriv_search_te2 (
	h5_file_t* const f,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx,
	h5_loc_idlist_t** idlist
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;
	h5t_adjacencies_t* a = &t->adjacencies;
	void* __retval;
	h5t_te_entry_t* entry;

	TRY (entry = h5_calloc (1, sizeof (*entry)));

	TRY (h5t_get_vertex_indices_of_edge2 (
		     f, face_idx, elem_idx, entry->key.vids));
	/*
	  resize hash table if more than 3/4 filled
	 */
	if ((a->te_hash.size*6) <= (a->te_hash.filled<<3)) {
		/*
		  Grow the hash table by 3*(num_elems - elem_idx) entries.
		  Why this number? We fill the hash table by looping over
		  all elements starting with 0. If we have to grow, we
		  still have num_elems-elem_idx elements to handle. Half the 
		  number of edges of the reference element times the number
		  of remaining elements is a good enough guess for the number
		  of edges we still have to add to the hash table.
		 */
		h5_loc_idx_t num_elems = t->num_elems[t->num_leaf_levels-1];
		TRY (h5priv_hresize (
			     3*(num_elems - elem_idx),
			     &a->te_hash));
	}
	TRY (h5priv_hsearch (
		     entry,
		     H5_ENTER,
		     &__retval,
		     &a->te_hash));
	h5t_te_entry_t* te_entry = (h5t_te_entry_t *)__retval;
	TRY (h5priv_search_idlist (
		     &te_entry->value,
		     h5tpriv_build_edge_id (face_idx, elem_idx)));
	if (te_entry->value->num_items > 1) {
		/* search returned existing entry */
		TRY (h5_free (entry));
	}
	if (idlist) {
		*idlist = te_entry->value;
	}
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

/*
  Find item in the T(E) hash table.

  Passing item with type entry type.
*/
static inline h5_err_t
find_te (
	h5_file_t* const f,
	h5t_te_entry_t* item,	// in: item to find
	h5_loc_idlist_t** idlist	// out: 
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	void* __entry;
	TRY (h5priv_hsearch (
		     item,
		     H5_FIND,
		     &__entry,
		     &f->t->adjacencies.te_hash));
	h5t_te_entry_t* entry = (h5t_te_entry_t*)__entry;
	if (entry ==NULL) {
		H5_PRIV_FUNC_LEAVE (H5_NOK);	// not found
	}
	if (idlist) {
		*idlist = entry->value;
	}
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}

/*
  Find item in the T(E) hash table.

  Passing item with face and local element ID.
*/
h5_err_t
h5tpriv_find_te (
	h5_file_t* const f,
	h5_loc_idx_t edge_id,	// in
	h5_loc_idlist_t** idlist	// out
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	h5t_te_entry_t item;
	TRY (h5t_get_vertex_indices_of_edge (
		     f,
		     edge_id,
		     item.key.vids));
	TRY (find_te (f, &item, idlist));
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5tpriv_find_te2 (
	h5_file_t* const f,
	h5_loc_idx_t face_idx,	// in
	h5_loc_idx_t elem_idx,	// in 
	h5_loc_idlist_t** idlist	// out
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	h5t_te_entry_t item;
	TRY (h5t_get_vertex_indices_of_edge2 (
		      f,
		      face_idx,
		      elem_idx,
		      item.key.vids));
	TRY (find_te (f, &item, idlist));
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

static int
cmp_td_entries (
	const void* __a,
	const void* __b
	) {
	h5t_td_entry_t* a = (h5t_td_entry_t*)__a;
	h5t_td_entry_t* b = (h5t_td_entry_t*)__b;
	return memcmp (a->key.vids, b->key.vids, sizeof(a->key.vids));
}

static unsigned int
compute_td_hashval (
	const void* __item
	) {
	h5t_td_entry_t* item = (h5t_td_entry_t*)__item;
	uint16_t* key =  (uint16_t*)item->key.vids;
	unsigned int count = 3 * sizeof (item->key.vids[0]) / sizeof (uint16_t); 
	unsigned int hval = count;
	while (count--) {
		if (*key) {
			hval <<= 6;
			hval += *key;
		}
		key++;
	}
	return hval;
}

static h5_err_t
release_td_entry (
	const void* __entry
	) {
	H5_PRIV_FUNC_ENTER (h5_err_t);
	struct h5_td_entry* entry = *(struct h5_td_entry**)__entry;
	h5_loc_idlist_t* list = entry->value;
	TRY (h5priv_free_idlist (&list));
	TRY (h5_free (entry));
	H5_PRIV_FUNC_RETURN (H5_SUCCESS);
}


h5_err_t
h5tpriv_resize_td_htab (
	h5_file_t* const f,
	size_t nel
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	h5t_adjacencies_t* a = &f->t->adjacencies;
	if (a->td_hash.size == 0) {
		TRY (h5priv_hcreate (
			     nel,
			     &a->td_hash,
			     cmp_td_entries,
			     compute_td_hashval,
			     release_td_entry));
	} else if (a->td_hash.size < nel) {
		TRY (h5priv_hresize (nel, &a->td_hash));
	}
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5tpriv_search_td2 (
	h5_file_t* const f,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx,
	h5_loc_idlist_t** idlist	// out
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	h5t_fdata_t* t = f->t;
	h5t_adjacencies_t* a = &f->t->adjacencies;
	void* __retval;
	h5t_td_entry_t* entry;

	TRY (entry = h5_calloc (1, sizeof(*entry)) );

	TRY (h5t_get_vertex_indices_of_triangle2 (
		     f, face_idx, elem_idx, entry->key.vids) );
	/*
	  resize hash table if more than 3/4 filled
	*/
	if ((a->td_hash.size*6) <= (a->td_hash.filled<<3)) {
		h5_loc_idx_t num_elems = t->num_elems[t->num_leaf_levels-1];
		TRY (h5priv_hresize (3*(num_elems-elem_idx), &a->td_hash));
	}
	/* search in directory, add if entry doesn't already exists */
	TRY (h5priv_hsearch (
		     entry,
		     H5_ENTER,
		     &__retval,
		     &a->td_hash));

	/* search ID in list of IDs for given triangle */
	h5t_td_entry_t *td_entry = (h5t_td_entry_t *)__retval;
	TRY (h5priv_search_idlist (
		     &td_entry->value,
		     h5tpriv_build_triangle_id (face_idx, elem_idx)));
	if (td_entry->value->num_items > 1) {
		TRY (h5_free (entry));
	}
	if (idlist) {
		*idlist = td_entry->value;
	}
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

static inline h5_err_t
find_td (
	h5_file_t* const f,
	h5t_td_entry_t* item,
	h5_loc_idlist_t** idlist	// out
	) {
	void* __entry;
	h5priv_hsearch (
		item,
		H5_FIND,
		&__entry,
		&f->t->adjacencies.td_hash);
	if (__entry == NULL) {
		return h5tpriv_error_local_triangle_nexist (item->key.vids);
	}
	h5t_td_entry_t* entry = (h5t_td_entry_t*)__entry;
	*idlist = entry->value;
	return H5_SUCCESS;
}

h5_err_t
h5tpriv_find_td (
	h5_file_t* const f,
	h5_loc_idx_t triangle_id,
	h5_loc_idlist_t** idlist
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	h5t_td_entry_t item;
	TRY (h5t_get_vertex_indices_of_triangle (
		     f,
		     triangle_id,
		     item.key.vids));
	TRY (find_td (f, &item, idlist));
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5tpriv_find_td2 (
	h5_file_t* const f,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx,
	h5_loc_idlist_t** idlist
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	h5t_td_entry_t item;
	TRY (h5t_get_vertex_indices_of_triangle2 (
		     f,
		     face_idx,
		     elem_idx,
		     item.key.vids));
	TRY (find_td (f, &item, idlist));
	H5_PRIV_API_RETURN (H5_SUCCESS);
}

/*
  Return list of elements sharing the same vertex.
 */
h5_err_t
h5tpriv_find_tv2 (
	h5_file_t* const f,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx,
	h5_loc_idlist_t** idlist
	) {
	H5_PRIV_API_ENTER (h5_err_t);
	h5_loc_idx_t idx;
	TRY (idx = h5tpriv_get_loc_elem_vertex_idx (f, elem_idx, face_idx));
	*idlist = f->t->adjacencies.tv.v[idx];
	H5_PRIV_API_RETURN (H5_SUCCESS);
}
