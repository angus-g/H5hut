#include <stdlib.h>
#include <string.h>
#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

static int
_cmp_te_entries (
	const void *__a,
	const void *__b
	) {
	return memcmp ( __a, __b, sizeof(h5_2id_t) );
}

static unsigned int
_compute_te_hashval (
	const void *__item
	) {
	h5t_te_entry_t *item = (h5t_te_entry_t*)__item;
	char *key = (char *)item->key.vids;
	unsigned int count = 2 * sizeof ( item->key.vids[0] );
	unsigned int hval = count;
	while ( count-- > 0 ) {
		if ( key[count] ) {
			hval <<= 4;
			hval += key[count];
		}
	}
	return hval;
}

h5_err_t
h5tpriv_create_te_htab (
	h5_file_t * const f,
	size_t nel
	) {
	h5t_adjacencies_t *a = &f->t->adjacencies;
	return h5priv_hcreate (
		      f,
		      nel,
		      &a->te_hash,
		      _cmp_te_entries,
		      _compute_te_hashval );
}

h5_err_t
h5tpriv_resize_te_htab (
	h5_file_t * const f,
	size_t nel
	) {
	h5t_adjacencies_t *a = &f->t->adjacencies;
	if ( a->te_hash.size == 0 ) {
		TRY ( h5tpriv_create_te_htab ( f, nel ) );
	} else if ( a->te_hash.size < nel ) {
		TRY ( h5priv_hresize ( f, nel, &a->te_hash ) );
	}
	return H5_SUCCESS;
}

h5_err_t
h5tpriv_search_te2 (
	h5_file_t * const f,
	h5_id_t face,
	h5_id_t eid,
	h5t_te_entry_t **entry
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	void *__retval;

	TRY ( *entry = h5priv_calloc ( f, 1, sizeof(**entry) ) );

	TRY ( h5t_get_local_vids_of_edge2 (
		f,
		face,
		eid,
		(*entry)->key.vids ) );
	/*
	  resize hash table if more than 3/4 filled
	 */
	if ( (a->te_hash.size*6) <= (a->te_hash.filled<<3) ) {
		h5_id_t num_elems = t->num_elems[t->num_levels-1];
		TRY ( h5priv_hresize (
			      f,
			      3*(num_elems - eid),
			      &a->te_hash ) );
	}
	TRY ( h5priv_hsearch (
		      f,
		      *entry,
		      H5_ENTER,
		      &__retval,
		      &a->te_hash ) );
	h5t_te_entry_t *retval = (h5t_te_entry_t *)__retval;
	TRY ( h5priv_search_idlist (
		      f,
		      &retval->value,
		      h5tpriv_build_edge_id ( face, eid ) ) );
	if ( retval->value.num_items > 1 ) {
		/* search returned an existing entry */
		TRY ( h5priv_free ( f, *entry ) );
	}
	return H5_SUCCESS;
}

/*
  Find item in the T(E) hash table.

  Passing item with type entry type.
*/
h5_err_t
h5tpriv_find_te (
	h5_file_t * const f,
	h5t_te_entry_t *item,
	h5t_te_entry_t **retval
	) {
	void *__ret;
	TRY ( h5priv_hsearch (
		      f,
		      item,
		      H5_FIND,
		      &__ret,
		      &f->t->adjacencies.te_hash ) );
	*retval = (h5t_te_entry_t *)__ret;
	return H5_SUCCESS;
}

/*
  Find item in the T(E) hash table.

  Passing item with face and local element ID.
*/
h5_err_t
h5tpriv_find_te2 (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5t_te_entry_t **retval
	) {
	h5t_te_entry_t item;
	TRY ( h5t_get_local_vids_of_edge2 (
		f,
		face_id,
		local_eid,
		item.key.vids
		      ) );
	return h5tpriv_find_te ( f, &item, retval );
}

static int
_cmp_td_entries (
	const void *__a,
	const void *__b
	) {
	return memcmp ( __a, __b, sizeof(h5_3id_t) );
}

static unsigned int
_compute_td_hashval (
	const void *__item
	) {
	h5t_te_entry_t *item = (h5t_te_entry_t*)__item;
	char *key = (char *)item->key.vids;
	unsigned int count = sizeof ( h5_3id_t );
	unsigned int hval = count;
	while ( count-- > 0 ) {
		if ( key[count] ) {
			hval <<= 4;
			hval += key[count];
		}
	}
	return hval;
}

h5_err_t
h5tpriv_create_td_htab (
	h5_file_t * const f,
	size_t nel
	) {
	h5t_adjacencies_t *a = &f->t->adjacencies;
	return h5priv_hcreate (
		      f,
		      nel,
		      &a->td_hash,
		      _cmp_td_entries,
		      _compute_td_hashval );
}

h5_err_t
h5tpriv_resize_td_htab (
	h5_file_t * const f,
	size_t nel
	) {
	h5t_adjacencies_t *a = &f->t->adjacencies;
	if ( a->td_hash.size == 0 ) {
		TRY ( h5tpriv_create_td_htab ( f, nel ) );
	} else if ( a->td_hash.size < nel ) {
		TRY ( h5priv_hresize ( f, nel, &a->td_hash ) );
	}
	return H5_SUCCESS;
}

h5_err_t
h5tpriv_search_td2 (
	h5_file_t * const f,
	h5_id_t face,
	h5_id_t eid,
	h5t_td_entry_t **entry
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &f->t->adjacencies;
	void *__retval;

	TRY ( *entry = h5priv_calloc ( f, 1, sizeof(**entry) ) );

	TRY ( h5t_get_local_vids_of_triangle2 (
		f,
		face,
		eid,
		(*entry)->key.vids ) );
	/*
	  resize hash table if more than 3/4 filled
	*/
	if ( (a->td_hash.size*6) <= (a->td_hash.filled<<3) ) {
		h5_id_t num_elems = t->num_elems[t->num_levels-1];
		TRY ( h5priv_hresize (
			      f,
			      3*(num_elems-eid),
			      &a->td_hash ) );
	}

	TRY ( h5priv_hsearch (
		      f,
		      *entry,
		      H5_ENTER,
		      &__retval,
		      &a->td_hash ) );
	h5t_td_entry_t *retval = (h5t_td_entry_t *)__retval;
	TRY ( h5priv_search_idlist (
		      f,
		      &retval->value,
		      h5tpriv_build_triangle_id ( face, eid ) ) );
	if ( retval->value.num_items > 1 ) {
		TRY ( h5priv_free ( f, *entry ) );
	}
	return H5_SUCCESS;
}

h5_err_t
h5tpriv_find_td (
	h5_file_t * const f,
	h5t_td_entry_t *item,
	h5t_td_entry_t **retval
	) {
	void *__ret;
	h5priv_hsearch (
		      f,
		      item,
		      H5_FIND,
		      &__ret,
		      &f->t->adjacencies.td_hash );
	if ( __ret == NULL ) {
		return h5tpriv_error_local_triangle_nexist( f, item->key.vids );
	}
	*retval = (h5t_td_entry_t *)__ret;
	return H5_SUCCESS;
}

h5_err_t
h5tpriv_find_td2 (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5t_td_entry_t **retval
	) {
	h5t_td_entry_t item;
	TRY ( h5t_get_local_vids_of_triangle2 (
		f,
		face_id,
		local_eid,
		item.key.vids ) );
	return h5tpriv_find_td ( f, &item, retval );
}

h5_err_t
h5tpriv_find_tv2 (
	h5_file_t * const f,
	h5_id_t cid,
	h5_id_t el_idx,
	h5_idlist_t **retval
	) {
	/*
	  map (cid,el_idx) to local index of vertex
	*/
	h5_id_t vidx = f->t->elems_ldta[el_idx].local_vids[cid];
	*retval = &f->t->vertices_data[vidx].tv;

	return H5_SUCCESS;
}
