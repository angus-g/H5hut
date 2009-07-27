/*
  Copyright 2006-2009
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Achim Gsell
  
  Warning
	This code is under development.
*/

#include <stdlib.h>
#include <string.h>
#include <search.h>
#include <time.h>
#include <hdf5.h>
#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

/*
  compute T(V)
*/
static h5_err_t
_compute_tets_of_vertices (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5_elem_ldta_t *tet;

	h5_id_t local_tid;
	h5_id_t local_vid;
	int i;

	for ( local_tid = 0, tet = &t->elems_ldta[0];
	      local_tid < t->num_elems[t->num_levels-1];
	      local_tid++, tet++ ) {
		for ( i = 0; i < 4; i++ ) {
			local_vid = tet->local_vids[i];
			TRY ( _h5_append_to_idlist (
				      f,
				      &t->vertices_data[local_vid].tv,
				      _h5t_build_vertex_id( i, local_tid ) ) );
		}
	}
	return H5_SUCCESS;
}

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
	h5_te_entry_t *item = (h5_te_entry_t*)__item;
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
_h5t_search_te2 (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5_te_entry_t **entry

	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	void *__retval;
	if ( *entry == NULL ) {
		TRY ( *entry = _h5_calloc ( f, 1, sizeof(**entry) ) );
	}
	_h5t_get_local_vids_of_edge (
		&t->elems_ldta[local_eid],
		face_id,
		(*entry)->key.vids );
	TRY ( _h5_hsearch_r (
		      f,
		      *entry,
		      H5_ENTER,
		      &__retval,
		      &a->te_hash ) );
	h5_te_entry_t *retval = (h5_te_entry_t *)__retval;
	TRY ( _h5_append_to_idlist (
		      f,
		      &retval->value,
		      _h5t_build_edge_id ( face_id, local_eid ) ) );
	if ( retval->value.num_items == 1 ) {
		*entry = NULL;
	}
	return H5_SUCCESS;
}

/*
  Find item in the T(E) hash table.

  Passing item with type entry type.
*/
h5_err_t
_h5t_find_te (
	h5_file_t * const f,
	h5_te_entry_t *item,
	h5_te_entry_t **retval
	) {
	void *__ret;
	TRY ( _h5_hsearch_r (
		      f,
		      item,
		      H5_FIND,
		      &__ret,
		      &f->t->adjacencies.te_hash ) );
	*retval = (h5_te_entry_t *)__ret;
	return H5_SUCCESS;
}

/*
  Find item in the T(E) hash table.

  Passing item with face and local element ID.
*/
h5_err_t
_h5t_find_te2 (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5_te_entry_t **retval
	) {
	h5_te_entry_t item;
	_h5t_get_local_vids_of_edge (
		&f->t->elems_ldta[local_eid],
		face_id,
		item.key.vids
		);
	return _h5t_find_te ( f, &item, retval );
}

static void
_sort_telist (
	const void *_entry
	) {
	h5_te_entry_t *entry = *(h5_te_entry_t **)_entry;
	h5_idlist_t *list = &entry->value;
	qsort (
		list->items,
		list->num_items,
		sizeof(list->items[0]),
		_h5_cmp_ids_by_eid );
}

/*
  Compute T(E) from current level up to highest levels.
 */
static h5_err_t
_compute_tets_of_edges (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	h5_te_entry_t *entry = NULL;
	h5_elem_ldta_t *tet = &t->elems_ldta[0];
	h5_id_t cur_lvl = t->cur_level < 0 ? 0 : t->cur_level;
	h5_id_t num_elems = t->num_elems[t->num_levels-1];
	h5_id_t local_eid = (cur_lvl == 0 ) ? 0 : t->num_elems[cur_lvl-1];
	TRY ( _h5_hcreate_r (
		      f,
		      5*(num_elems - local_eid),
		      &a->te_hash,
		      _cmp_te_entries,
		      _compute_te_hashval ) );
	for ( ; local_eid < num_elems; local_eid++, tet++ ) {
		h5_id_t face_id;
		for ( face_id = 0; face_id < 6; face_id++ ) {
			if ( (a->te_hash.size*6) <= (a->te_hash.filled<<3) ) {
				TRY ( _h5_hresize_r (
					      f,
					      3*(num_elems - local_eid),
					      &a->te_hash ) );
			}
			TRY ( _h5t_search_te2 (
				      f,
				      face_id,
				      local_eid,
				      &entry ) );
		}
	}
	_h5_hwalk_r ( f, &a->te_hash, _sort_telist ); 
	if ( entry && entry->value.items == NULL ) {
		_h5_free ( f, entry );
	}
	return H5_SUCCESS;
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
	h5_te_entry_t *item = (h5_te_entry_t*)__item;
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
_h5t_search_td2 (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5_td_entry_t **entry
	) {
	void *__retval;
	if ( *entry == NULL ) {
		TRY ( *entry = _h5_calloc ( f, 1, sizeof(**entry) ) );
	}
	_h5t_get_local_vids_of_triangle (
		&f->t->elems_ldta[local_eid],
		face_id,
		(*entry)->key.vids );
	TRY ( _h5_hsearch_r (
		      f,
		      *entry,
		      H5_ENTER,
		      &__retval,
		      &f->t->adjacencies.td_hash ) );
	h5_td_entry_t *retval = (h5_td_entry_t *)__retval;
	TRY ( _h5_append_to_idlist (
		      f,
		      &retval->value,
		      _h5t_build_triangle_id ( face_id, local_eid ) ) );
	if ( retval->value.num_items == 1 ) {
		*entry = NULL;
	}
	return H5_SUCCESS;
}

h5_err_t
_h5t_find_td (
	h5_file_t * const f,
	h5_td_entry_t *item,
	h5_td_entry_t **retval
	) {
	void *__ret;
	_h5_hsearch_r (
		      f,
		      item,
		      H5_FIND,
		      &__ret,
		      &f->t->adjacencies.td_hash );
	if ( __ret == NULL ) {
		return _h5t_error_local_triangle_nexist( f, item->key.vids );
	}
	*retval = (h5_td_entry_t *)__ret;
	return H5_SUCCESS;
}

h5_err_t
_h5t_find_td2 (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5_td_entry_t **retval
	) {
	h5_td_entry_t item;
	_h5t_get_local_vids_of_triangle (
		&f->t->elems_ldta[local_eid],
		face_id,
		item.key.vids );
	return _h5t_find_td ( f, &item, retval );
}

/*
  Sort ID list according their tetrahedra ID.

  Called by twalk().
*/
static void
_sort_tdlist (
	const void *_entry
	) {
	h5_td_entry_t *entry = *(h5_td_entry_t **)_entry;
	h5_idlist_t *list = &entry->value;
	qsort (
		list->items,
		list->num_items,
		sizeof(list->items[0]),
		_h5_cmp_ids_by_eid );
}


static h5_err_t
_compute_tets_of_triangles (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	h5_td_entry_t *entry = NULL;
	h5_elem_ldta_t *tet = &t->elems_ldta[0];
	h5_id_t cur_lvl = t->cur_level < 0 ? 0 : t->cur_level;
	h5_size_t num_elems = t->num_elems[t->num_levels-1];
	h5_id_t local_eid = (cur_lvl == 0 ) ? 0 : t->num_elems[cur_lvl-1];
	TRY ( _h5_hcreate_r (
		      f,
		      5*(num_elems-local_eid),
		      &a->td_hash,
		      _cmp_td_entries,
		      _compute_td_hashval ) );
	for ( ; local_eid < num_elems; local_eid++, tet++ ) {
		h5_id_t face_id;
		for ( face_id = 0; face_id < 4; face_id++ ) {
			if ( (a->td_hash.size*6) <= (a->td_hash.filled<<3) ) {
				TRY ( _h5_hresize_r (
					      f,
					      3*(num_elems-local_eid),
					      &a->td_hash ) );
			}
			TRY (
				_h5t_search_td2 (
					f,
					face_id,
					local_eid,
					&entry )
				);
		}
	}
	_h5_hwalk_r ( f, &a->td_hash, _sort_tdlist ); 
	if ( entry && entry->value.items == NULL ) {
		_h5_free ( f, entry );
	}
	return H5_SUCCESS;
}

h5_err_t
_h5t_rebuild_adj_data (
	h5_file_t * const f
	) {
	clock_t t1 = clock();
	TRY ( _compute_tets_of_vertices ( f ) );
	clock_t t2 = clock();
	fprintf ( stderr, "_compute_tets_of_vertices(): %f\n",
		  (float)(t2-t1)/CLOCKS_PER_SEC );
	t1 = clock();
	TRY ( _compute_tets_of_edges ( f ) );
	t2 = clock();
	fprintf ( stderr, "_compute_tets_of_edge(): %f\n",
		  (float)(t2-t1)/CLOCKS_PER_SEC );
	t1 = clock();
	TRY ( _compute_tets_of_triangles ( f ) );
	t2 = clock();
	fprintf ( stderr, "_compute_tets_of_triangle(): %f\n",
		  (float)(t2-t1)/CLOCKS_PER_SEC );

	return H5_SUCCESS;
}

/*!
  \param[in]	f		file handle
  \param[in]	local_kid	local edge ID we search children of
  \param[in]	local_eid	local element ID of first children 
  \param[out]	kids		direct children

*/
h5_err_t
_compute_direct_children_of_edge (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5_id_t	kids[2]
	) {
	int off[6][2] = { {0,1}, {1,2}, {0,2}, {0,3}, {1,3}, {2,3} };

	if ( ( face_id < 0 ) || ( face_id >= 6 ) ) {
		return  h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
	kids[0] = _h5t_build_edge_id ( face_id, local_eid+off[face_id][0] );
	kids[1] = _h5t_build_edge_id ( face_id, local_eid+off[face_id][1] );
	return H5_SUCCESS;
}

static h5_err_t
_compute_children_of_edge (
	h5_file_t * const f,
	h5_id_t local_kid,
	h5_idlist_t *children
	) {
	h5t_fdata_t *t = f->t;
	h5_te_entry_t *te;

	TRY ( _h5t_find_te2 (
		      f,
		      _h5t_get_face_id ( local_kid ),
		      _h5t_get_elem_id ( local_kid ),
		      &te ) 
		);
	h5_id_t *edge = te->value.items;
	h5_id_t *end = te->value.items+te->value.num_items;
	for ( ; edge < end; edge++ ) {
		h5_id_t local_eid = _h5t_get_elem_id ( *edge );
		h5_id_t face_id =  _h5t_get_face_id ( *edge );
		h5_elem_ldta_t *tet = &t->elems_ldta[local_eid];
		if ( _h5t_elem_is_on_cur_level ( f, tet ) == H5_OK ) {
			TRY ( _h5_append_to_idlist (
				      f, children, *edge )
				);
		} else {
			h5_id_t local_kids[2];
			TRY ( _compute_direct_children_of_edge (
				      f,
				      face_id,
				      tet->local_child_eid,
				      local_kids ) );
			TRY ( _compute_children_of_edge (
				      f, local_kids[0], children ) );
			TRY ( _compute_children_of_edge (
				      f, local_kids[1], children ) );
		}
	}
	return H5_SUCCESS;
}

/* 
   Compute all sections of an edge.
*/
static h5_err_t
_compute_sections_of_edge (
	h5_file_t * const f,
	h5_id_t local_kid,
	h5_idlist_t *children
	) {
	h5t_fdata_t *t = f->t;
	h5_te_entry_t *te;

	TRY ( _h5t_find_te2 (
		      f,
		      _h5t_get_face_id ( local_kid ),
		      _h5t_get_elem_id ( local_kid ),
		      &te )
		);
	h5_id_t *edge = te->value.items;
	h5_id_t *end = te->value.items+te->value.num_items;
	int refined = 0;
	for ( ; edge < end; edge++ ) {
		h5_id_t local_eid = _h5t_get_elem_id ( *edge );
		h5_id_t face_id =  _h5t_get_face_id ( *edge );
		h5_elem_ldta_t *tet = &t->elems_ldta[local_eid];
		if ( ! _h5t_elem_is_on_cur_level ( f, tet ) == H5_OK ) {
			refined = 1;
			h5_id_t local_kids[2];
			TRY ( _compute_direct_children_of_edge (
				      f,
				      face_id,
				      tet->local_child_eid,
				      local_kids ) );
			TRY ( _compute_sections_of_edge (
				      f, local_kids[0], children ) );
			TRY ( _compute_sections_of_edge (
				      f, local_kids[1], children ) );
		}
	}
	if ( ! refined ) {
		TRY ( _h5_append_to_idlist ( f, children, te->value.items[0] ) );
	}
	return H5_SUCCESS;
}

h5_err_t
_compute_direct_children_of_triangle (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid,
	h5_id_t	dids[4]
	) {
	int off[4][4] = { {1,2,3,7}, {0,2,3,6}, {0,1,3,4}, {0,1,2,5} };

	if ( ( face_id < 0 ) || ( face_id >= 4 ) ) {
		return  h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
	dids[0] = _h5t_build_edge_id ( face_id, local_eid+off[face_id][0] );
	dids[1] = _h5t_build_edge_id ( face_id, local_eid+off[face_id][1] );
	dids[2] = _h5t_build_edge_id ( face_id, local_eid+off[face_id][2] );
	dids[3] = _h5t_build_edge_id ( face_id, local_eid+off[face_id][3] );
	return H5_SUCCESS;
}

h5_err_t
_compute_children_of_triangle (
	h5_file_t * const f,
	h5_id_t local_did,
	h5_idlist_t *children 
	) {

	h5t_fdata_t *t = f->t;
	h5_td_entry_t *td;

	TRY ( _h5t_find_td2 (
		      f,
		      _h5t_get_face_id ( local_did ),
		      _h5t_get_elem_id ( local_did ),
		      &td
		      ) );
	h5_id_t *tri = td->value.items;
	h5_id_t *end = td->value.items+td->value.num_items;
	for ( ; tri < end; tri++ ) {
		h5_id_t local_eid = _h5t_get_elem_id ( *tri );
		h5_id_t face_id =  _h5t_get_face_id ( *tri );
		h5_elem_ldta_t *tet = &t->elems_ldta[local_eid];
		if ( _h5t_elem_is_on_cur_level ( f, tet ) == H5_OK ) {
			TRY ( _h5_append_to_idlist (
				      f, children, *tri )
				);
		} else {
			h5_id_t local_dids[4];
			TRY ( _compute_direct_children_of_triangle (
				      f,
				      face_id,
				      tet->local_child_eid,
				      local_dids ) );
			TRY ( _compute_children_of_triangle (
				      f, local_dids[0], children ) );
			TRY ( _compute_children_of_triangle (
				      f, local_dids[1], children ) );
			TRY ( _compute_children_of_triangle (
				      f, local_dids[2], children ) );
			TRY ( _compute_children_of_triangle (
				      f, local_dids[3], children ) );
		}
	}
	return H5_SUCCESS;
}

static h5_err_t
_compute_sections_of_triangle (
	h5_file_t * const f,
	h5_id_t local_did,
	h5_idlist_t *children
	) {
	h5t_fdata_t *t = f->t;
	h5_td_entry_t *td;

	TRY ( _h5t_find_td2 (
		      f,
		      _h5t_get_face_id ( local_did ),
		      _h5t_get_elem_id ( local_did ), &td ) );
	h5_id_t *tri = td->value.items;
	h5_id_t *end = td->value.items+td->value.num_items;
	int refined = 0;
	for ( ; tri < end; tri++ ) {
		h5_id_t local_eid = _h5t_get_elem_id ( *tri );
		h5_id_t face_id =  _h5t_get_face_id ( *tri );
		h5_elem_ldta_t *tet = &t->elems_ldta[local_eid];
		if ( ! _h5t_elem_is_on_cur_level ( f, tet ) == H5_OK ) {
			refined = 1;
			h5_id_t local_dids[4];
			TRY ( _compute_direct_children_of_triangle (
				      f,
				      face_id,
				      tet->local_child_eid,
				      local_dids ) );
			TRY ( _compute_sections_of_triangle (
				      f, local_dids[0], children ) );
			TRY ( _compute_sections_of_triangle (
				      f, local_dids[1], children ) );
			TRY ( _compute_sections_of_triangle (
				      f, local_dids[2], children ) );
			TRY ( _compute_sections_of_triangle (
				      f, local_dids[3], children ) );

		}
	}
	if ( ! refined ) {
		TRY ( _h5_append_to_idlist ( f, children, td->value.items[0] ) );
	}
	return H5_SUCCESS;
}
/*
  map edge ID to unique ID
  if unique ID not in list: add
*/
static h5_err_t
_add_edge (
	h5_file_t * const f,
	h5_idlist_t *list,
	h5_id_t face_id,
	h5_id_t local_eid
	) {
	h5_te_entry_t *te;
	TRY ( _h5t_find_te2 ( f, face_id, local_eid, &te ) );
	TRY ( _h5_search_idlist ( f, list, te->value.items[0] ) );
	return H5_SUCCESS;
}


h5_err_t
h5t_get_edges_upadjacent_to_vertex (
	h5_file_t * const f,
	const h5_id_t local_vid,
	h5_idlist_t **list
	) {
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	
	h5t_fdata_t *t = f->t;
	h5_idlist_t *tv = &t->vertices_data[local_vid].tv;
	h5_size_t i;

	h5_id_t *vid = tv->items;
	for ( i = 0; i < tv->num_items; i++, vid++ ) {
		h5_id_t local_eid = _h5t_get_elem_id ( *vid );
		h5_id_t face_id = _h5t_get_face_id ( *vid );
		h5_elem_ldta_t *tet = &t->elems_ldta[local_eid];

		if ( _h5t_elem_is_on_cur_level ( f, tet ) == H5_NOK ) {
			continue;
		}
		int map[4][3] = { { 0, 2, 3 }, { 0, 1, 4 }, { 2, 1, 5 }, { 3, 4, 5} };
		TRY ( _add_edge ( f, *list, map[face_id][0], local_eid ) );
		TRY ( _add_edge ( f, *list, map[face_id][1], local_eid ) );
		TRY ( _add_edge ( f, *list, map[face_id][2], local_eid ) );
	}
	return H5_SUCCESS;
}

static h5_err_t
_add_triangle (
	h5_file_t * const f,
	h5_idlist_t *list,
	h5_id_t face_id,
	h5_id_t local_eid
	) {
	h5_td_entry_t *td;
	TRY ( _h5t_find_td2 ( f, face_id, local_eid, &td ) );
	TRY ( _h5_search_idlist ( f, list, td->value.items[0] ) );

	return H5_SUCCESS;
}

h5_err_t
h5t_get_triangles_upadjacent_to_vertex (
	h5_file_t * const f,
	const h5_id_t local_vid,
	h5_idlist_t **list
	) {
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	
	h5t_fdata_t *t = f->t;
	h5_idlist_t *tv = &t->vertices_data[local_vid].tv;
	h5_size_t i;
	h5_id_t *vid = tv->items;
	for ( i = 0; i < tv->num_items; i++, vid++ ) {
		h5_id_t local_eid = _h5t_get_elem_id ( *vid );
		h5_id_t face_id = _h5t_get_face_id ( *vid );
		h5_elem_ldta_t *tet = &t->elems_ldta[local_eid];

		if ( _h5t_elem_is_on_cur_level ( f, tet ) == H5_NOK ) {
			continue;
		}
		int map[4][3] = { { 1, 2, 3 }, { 0, 2, 3 }, { 0, 1, 3 }, { 0, 1, 2} };
		TRY ( _add_triangle ( f, *list, map[face_id][0], local_eid ) );
		TRY ( _add_triangle ( f, *list, map[face_id][1], local_eid ) );
		TRY ( _add_triangle ( f, *list, map[face_id][2], local_eid ) );
	}
	return H5_SUCCESS;
}

h5_err_t
h5t_get_tets_upadjacent_to_vertex (
	h5_file_t * const f,
	const h5_id_t local_vid,
	h5_idlist_t **list
	) {
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	
	h5t_fdata_t *t = f->t;
	h5_idlist_t *tv = &t->vertices_data[local_vid].tv;
	h5_size_t i;
	h5_id_t *vid = tv->items;
	for ( i = 0; i < tv->num_items; i++, vid++ ) {
		h5_id_t local_eid = _h5t_get_elem_id ( *vid );
		h5_elem_ldta_t *tet = &t->elems_ldta[local_eid];

		if ( _h5t_elem_is_on_cur_level ( f, tet ) == H5_NOK ) {
			continue;
		}
		TRY ( _h5_search_idlist ( f, *list, local_eid ) );
	}
	return H5_SUCCESS;
}

h5_err_t
h5t_get_triangles_upadjacent_to_edge (
	h5_file_t * const f,
	const h5_id_t local_kid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );
	TRY ( _compute_children_of_edge ( f, local_kid, children ) );
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	h5_id_t *edge = children->items;
	h5_id_t *end = children->items+children->num_items;
	int map[6][2] = { {2,3}, {0,3}, {1,3}, {1,2}, {0,2}, {0,1} };
	for ( ; edge < end; edge++ ) {
		h5_id_t local_eid = _h5t_get_elem_id ( *edge );
		h5_id_t face_id = _h5t_get_face_id ( *edge );
		TRY ( _add_triangle ( f, *list, map[face_id][0], local_eid ) );
		TRY ( _add_triangle ( f, *list, map[face_id][1], local_eid ) );
	}
	TRY ( _h5_free_idlist( f, &children ) );

	return H5_SUCCESS;
}

h5_err_t
h5t_get_tets_upadjacent_to_edge (
	h5_file_t * const f,
	const h5_id_t local_kid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );
	TRY( _compute_children_of_edge ( f, local_kid, children ) );
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	int i;
	h5_id_t *kid = children->items;
	for ( i = 0; i < children->num_items; i++, kid++ ) {
		h5_id_t local_eid = _h5t_get_elem_id ( *kid );
		TRY ( _h5_search_idlist ( f, *list, local_eid ) );
	}
	TRY ( _h5_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

h5_err_t
h5t_get_tets_upadjacent_to_triangle (
	h5_file_t * const f,
	const h5_id_t local_did,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );
	TRY( _compute_children_of_triangle ( f, local_did, children ) );
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	int i;
	h5_id_t *did = children->items;
	for ( i = 0; i < children->num_items; i++ , did++) {
		h5_id_t local_eid = _h5t_get_elem_id ( *did );
		TRY ( _h5_search_idlist ( f, *list, local_eid ) );
	}
	TRY ( _h5_free_idlist( f, &children ) );
	return H5_SUCCESS;
}


h5_err_t
h5t_get_vertices_downadjacent_to_edge (
	h5_file_t * const f,
	const h5_id_t local_kid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );
	TRY( _compute_sections_of_edge ( f, local_kid, children ) );
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	int i;
	h5_id_t *kid = children->items;
	for ( i = 0; i < children->num_items; i++, kid++ ) {
		h5_2id_t local_vids;
		TRY ( h5t_get_local_vids_of_entity ( f, *kid, local_vids ) );
		TRY ( _h5_search_idlist ( f, *list, local_vids[0] ) );
		TRY ( _h5_search_idlist ( f, *list, local_vids[1] ) );
	}
	TRY ( _h5_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

/*
  Compute downward adjacent vertices of all edges of triangle.
 */
h5_err_t
h5t_get_vertices_downadjacent_to_triangle (
	h5_file_t * const f,
	const h5_id_t local_did,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );

	int map[4][3] = { {1,4,5}, {2,3,5}, {0,3,4}, {0,1,2} };
	h5_id_t face_id = _h5t_get_face_id ( local_did );
	h5_id_t local_eid = _h5t_get_elem_id ( local_did );

	h5_id_t i;
	for ( i = 0; i < 3; i++ ) {
		TRY( _compute_sections_of_edge (
			     f,
			     _h5t_build_edge_id ( map[face_id][i], local_eid ),
			     children ) );
	}
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	h5_id_t *kid = children->items;
	for ( i = 0; i < children->num_items; i++, kid++ ) {
		h5_2id_t local_vids;
		TRY ( h5t_get_local_vids_of_entity ( f, *kid, local_vids ) );
		TRY ( _h5_search_idlist ( f, *list, local_vids[0] ) );
		TRY ( _h5_search_idlist ( f, *list, local_vids[1] ) );
	}
	TRY ( _h5_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

/*
  Compute downward adjacent vertices of all edges of tetrahedron.
 */
h5_err_t
h5t_get_vertices_downadjacent_to_tet (
	h5_file_t * const f,
	const h5_id_t local_eid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );

	h5_id_t i;
	for ( i = 0; i < 6; i++ ) {
		TRY( _compute_sections_of_edge (
			     f,
			     _h5t_build_edge_id ( i, local_eid ),
			     children ) );
	}
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	h5_id_t *kid = children->items;
	for ( i = 0; i < children->num_items; i++, kid++ ) {
		h5_2id_t local_vids;
		TRY ( h5t_get_local_vids_of_entity ( f, *kid, local_vids ) );
		TRY ( _h5_search_idlist ( f, *list, local_vids[0] ) );
		TRY ( _h5_search_idlist ( f, *list, local_vids[1] ) );
	}
	TRY ( _h5_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

h5_err_t
h5t_get_edges_downadjacent_to_triangle (
	h5_file_t * const f,
	const h5_id_t local_did,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );

	int map[4][3] = { {1,4,5}, {2,3,5}, {0,3,4}, {0,1,2} };
	h5_id_t face_id = _h5t_get_face_id ( local_did );
	h5_id_t local_eid = _h5t_get_elem_id ( local_did );

	h5_id_t i;
	for ( i = 0; i < 3; i++ ) {
		TRY( _compute_sections_of_edge (
			     f,
			     _h5t_build_edge_id ( map[face_id][i], local_eid ),
			     children ) );
	}
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	h5_id_t *kid = children->items;
	for ( i = 0; i < children->num_items; i++, kid++ ) {
		TRY ( _h5_search_idlist ( f, *list, *kid ) );
	}
	TRY ( _h5_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

h5_err_t
h5t_get_edges_downadjacent_to_tet (
	h5_file_t * const f,
	const h5_id_t local_eid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );

	h5_id_t i;
	for ( i = 0; i < 6; i++ ) {
		TRY( _compute_sections_of_edge (
			     f,
			     _h5t_build_edge_id ( i, local_eid ),
			     children ) );
	}
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	h5_id_t *kid = children->items;
	for ( i = 0; i < children->num_items; i++, kid++ ) {
		TRY ( _h5_search_idlist ( f, *list, *kid ) );
	}
	TRY ( _h5_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

h5_err_t
h5t_get_triangles_downadjacent_to_tet (
	h5_file_t * const f,
	const h5_id_t local_eid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );

	h5_id_t i;
	for ( i = 0; i < 4; i++ ) {
		TRY( _compute_sections_of_triangle (
			     f,
			     _h5t_build_edge_id ( i, local_eid ),
			     children ) );
	}
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	h5_id_t *did = children->items;
	for ( i = 0; i < children->num_items; i++, did++ ) {
		TRY ( _h5_search_idlist ( f, *list, *did ) );
	}
	TRY ( _h5_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

h5_err_t
h5t_release_list_of_adjacencies (
	h5_file_t * const f,
	h5_idlist_t **list
	) {
	TRY ( _h5_free_idlist ( f, list ) );
	return H5_SUCCESS;
}
