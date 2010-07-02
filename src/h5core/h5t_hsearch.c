#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

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
	char* key = (char*)item->key.vids;
	unsigned int count = 2 * sizeof (item->key.vids[0]);
	unsigned int hval = count;
	while (count-- > 0) {
		if (key[count]) {
			hval <<= 4;
			hval += key[count];
		}
	}
	return hval;
}

h5_err_t
h5tpriv_create_te_htab (
	h5_file_t* const f,
	size_t nel
	) {
	h5t_adjacencies_t* a = &f->t->adjacencies;
	return h5priv_hcreate (
		      f,
		      nel,
		      &a->te_hash,
		      cmp_te_entries,
		      compute_te_hashval);
}

h5_err_t
h5tpriv_resize_te_htab (
	h5_file_t* const f,
	size_t nel
	) {
	h5t_adjacencies_t* a = &f->t->adjacencies;
	if ( a->te_hash.size == 0 ) {
		TRY( h5tpriv_create_te_htab (f, nel) );
	} else if (a->te_hash.size < nel) {
		TRY( h5priv_hresize (f, nel, &a->te_hash) );
	}
	return H5_SUCCESS;
}

h5_err_t
h5tpriv_search_te2 (
	h5_file_t* const f,
	h5_id_t face_idx,
	h5_id_t elem_idx,
	h5_idlist_t** retval
	) {
	h5t_fdata_t* t = f->t;
	h5t_adjacencies_t* a = &t->adjacencies;
	void* __retval;
	h5t_te_entry_t* entry;

	TRY( entry = h5priv_calloc (f, 1, sizeof (*entry)) );

	TRY( h5t_get_vertex_indices_of_edge2 (
		f, face_idx, elem_idx, entry->key.vids) );
	/*
	  resize hash table if more than 3/4 filled
	 */
	if ((a->te_hash.size*6) <= (a->te_hash.filled<<3)) {
		/*
		  Grow the hash table by 3*(num_elems - elem_idx) entries.
		  Why this number? We fill the hash table by looping over
		  all elements starting with 0. So if we have to grow, we
		  still have num_elems-elem_idx elements to handle. Half the 
		  number of edges of the reference element times the number
		  of remaining elements is a good enough guess for the number
		  of edges we still have to add to the hash table.

		  Thus for a tetrahedal mesh we 3 time the remaining elements!
		  @@@
		 */
		h5_id_t num_elems = t->num_elems[t->num_levels-1];
		TRY( h5priv_hresize (
			     f,
			     3*(num_elems - elem_idx),
			     &a->te_hash) );
	}
	TRY( h5priv_hsearch (
		     f,
		     entry,
		     H5_ENTER,
		     &__retval,
		     &a->te_hash) );
	h5t_te_entry_t* te_entry = (h5t_te_entry_t *)__retval;
	TRY( h5priv_search_idlist (
		     f,
		     &te_entry->value,
		     h5tpriv_build_edge_id (face_idx, elem_idx)) );
	if (entry->value.num_items > 1) {
		/* search returned an existing entry */
		TRY( h5priv_free (f, entry) );
	}
	*retval = &te_entry->value;
	return H5_SUCCESS;
}

/*
  Find item in the T(E) hash table.

  Passing item with type entry type.
*/
h5_err_t
h5tpriv_find_te (
	h5_file_t* const f,
	h5t_te_entry_t* item,
	h5_idlist_t** retval
	) {
	void* __entry;
	TRY( h5priv_hsearch (
		     f,
		     item,
		     H5_FIND,
		     &__entry,
		     &f->t->adjacencies.te_hash) );
	h5t_te_entry_t* entry = (h5t_te_entry_t*)__entry;
	if (entry ==NULL) {
		// not found
		return H5_NOK;
	}
	*retval = &entry->value;
	return H5_SUCCESS;
}

/*
  Find item in the T(E) hash table.

  Passing item with face and local element ID.
*/
h5_err_t
h5tpriv_find_te2 (
	h5_file_t* const f,
	h5_id_t face_idx,
	h5_id_t elem_idx,
	h5_idlist_t** retval
	) {
	h5t_te_entry_t item;
	TRY( h5t_get_vertex_indices_of_edge2 (
		     f,
		     face_idx,
		     elem_idx,
		     item.key.vids) );
	return h5tpriv_find_te (f, &item, retval);
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
	h5t_te_entry_t* item = (h5t_te_entry_t*)__item;
	char* key = (char*)item->key.vids;
	unsigned int count = sizeof (h5_3id_t);
	unsigned int hval = count;
	while (count-- > 0) {
		if (key[count]) {
			hval <<= 4;
			hval += key[count];
		}
	}
	return hval;
}

h5_err_t
h5tpriv_create_td_htab (
	h5_file_t* const f,
	size_t nel
	) {
	h5t_adjacencies_t* a = &f->t->adjacencies;
	return h5priv_hcreate (
		f,
		nel,
		&a->td_hash,
		cmp_td_entries,
		compute_td_hashval);
}

h5_err_t
h5tpriv_resize_td_htab (
	h5_file_t* const f,
	size_t nel
	) {
	h5t_adjacencies_t* a = &f->t->adjacencies;
	if (a->td_hash.size == 0) {
		TRY( h5tpriv_create_td_htab (f, nel) );
	} else if (a->td_hash.size < nel) {
		TRY( h5priv_hresize (f, nel, &a->td_hash) );
	}
	return H5_SUCCESS;
}

h5_err_t
h5tpriv_search_td2 (
	h5_file_t* const f,
	h5_id_t face_idx,
	h5_id_t elem_idx,
	h5_idlist_t** retval
	) {
	h5t_fdata_t* t = f->t;
	h5t_adjacencies_t* a = &f->t->adjacencies;
	void* __retval;
	h5t_td_entry_t* entry;

	TRY( entry = h5priv_calloc (f, 1, sizeof(*entry)) );

	TRY( h5t_get_vertex_indices_of_triangle2 (
		     f, face_idx, elem_idx, entry->key.vids) );
	/*
	  resize hash table if more than 3/4 filled
	*/
	if ((a->td_hash.size*6) <= (a->td_hash.filled<<3)) {
		h5_id_t num_elems = t->num_elems[t->num_levels-1];
		TRY( h5priv_hresize (
			     f,
			     3*(num_elems-elem_idx),
			     &a->td_hash));
	}

	TRY( h5priv_hsearch (
		     f,
		     entry,
		     H5_ENTER,
		     &__retval,
		     &a->td_hash) );
	h5t_td_entry_t *te_entry = (h5t_td_entry_t *)__retval;
	TRY( h5priv_search_idlist (
		     f,
		     &te_entry->value,
		     h5tpriv_build_triangle_id (face_idx, elem_idx)) );
	if (te_entry->value.num_items > 1) {
		TRY( h5priv_free (f, entry) );
	}
	return H5_SUCCESS;
}

h5_err_t
h5tpriv_find_td (
	h5_file_t* const f,
	h5t_td_entry_t* item,
	h5_idlist_t** retval
	) {
	void* __entry;
	h5priv_hsearch (
		f,
		item,
		H5_FIND,
		&__entry,
		&f->t->adjacencies.td_hash);
	if (__entry == NULL) {
		return h5tpriv_error_local_triangle_nexist (f, item->key.vids);
	}
	h5t_td_entry_t* entry = (h5t_td_entry_t*)__entry;
	*retval = &entry->value;
	return H5_SUCCESS;
}

h5_err_t
h5tpriv_find_td2 (
	h5_file_t* const f,
	h5_id_t face_idx,
	h5_id_t elem_idx,
	h5_idlist_t** retval
	) {
	h5t_td_entry_t item;
	TRY( h5t_get_vertex_indices_of_triangle2 (
		     f,
		     face_idx,
		     elem_idx,
		     item.key.vids) );
	return h5tpriv_find_td (f, &item, retval);
}

h5_err_t
h5tpriv_find_tv2 (
	h5_file_t* const f,
	h5_id_t face_idx,
	h5_id_t elem_idx,
	h5_idlist_t** retval
	) {
	/*
	  map (cid,el_idx) to local index of vertex
	*/
	h5_id_t vertex_idx = f->t->elems_ldta[elem_idx].local_vertex_indices[face_idx];
	*retval = &f->t->vertices_data[vertex_idx].tv;

	return H5_SUCCESS;
}
