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
#include <hdf5.h>
#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

static h5_err_t
_calc_tets_of_vertices (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5_tet_data_t *tet;

	h5_id_t local_tid;
	h5_id_t local_vid;
	int i;

	for ( local_tid = 0, tet = &t->elems_data.tets[0];
	      local_tid < t->num_elems[t->num_levels-1];
	      local_tid++, tet++ ) {
		for ( i = 0; i < 3; i++ ) {
			local_vid = tet->local_vids[i];
			TRY ( _h5_append_to_idlist (
				      f,
				      &t->vertices_data[local_vid].tv,
				      local_tid ) );
		}
	}
	return H5_SUCCESS;
}

static int
_cmp_te_node (
	const void *a,
	const void *b
	) {
	h5_te_node_key_t *key0 = (h5_te_node_key_t*)a;
	h5_te_node_key_t *key1 = (h5_te_node_key_t*)b;

	if ( key0->vid0 < key1->vid0 )
		return -1;
	if ( key0->vid0 > key1->vid0 )
		return 1;
	if ( key0->vid1 < key1->vid1 )
		return -1;
	if ( key0->vid1 > key1->vid1 )
		return 1;
	return 0;
}

static h5_err_t
_search_te (
	h5_file_t * const f,
	h5_te_node_t **node,
	h5_id_t local_eid,
	h5_id_t local_vid0,
	h5_id_t local_vid1
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	void *vnode;

	if ( *node ==NULL ) {
		TRY ( *node = _h5_alloc ( f, NULL, sizeof(**node) ) );
		memset ( *node, 0, sizeof(**node) );
	}

	if ( local_vid0 < local_vid1 ) {
		(*node)->key.vid0 = local_vid0; 
		(*node)->key.vid1 = local_vid1;
	} else {
		(*node)->key.vid0 = local_vid1;
		(*node)->key.vid1 = local_vid0;
	}

	TRY ( vnode = _h5_tsearch (
		      f,
		      *node,
		      (void**)&a->te_tree,
		      _cmp_te_node ) );
	h5_te_node_t *rnode = *(h5_te_node_t **)vnode;
	TRY ( _h5_append_to_idlist (
		      f,
		      &rnode->value,
		      local_eid ) );
	if ( rnode->value.num_items == 1 ) {
		*node = NULL;
	}
	return H5_SUCCESS;
}

static h5_err_t
_find_te (
	h5_file_t * const f,
	h5_te_node_t **rnode,
	h5_id_t local_vid0,
	h5_id_t local_vid1
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	h5_te_node_t node;
	void *vnode = &node;

	if ( local_vid0 < local_vid1 ) {
		node.key.vid0 = local_vid0; 
		node.key.vid1 = local_vid1;
	} else {
		node.key.vid0 = local_vid1;
		node.key.vid1 = local_vid0;
	}

	TRY ( vnode = _h5_tfind (
		      f,
		      vnode,
		      (void**)&a->te_tree,
		      _cmp_te_node ) );
	*rnode = *(h5_te_node_t **)vnode;

	return H5_SUCCESS;
}

static h5_err_t
_calc_tets_of_edges (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	h5_id_t local_tid;
	h5_size_t num_tets = t->num_elems[t->num_levels-1];
	h5_te_node_t *node = NULL;
	h5_tet_data_t *tet = &t->elems_data.tets[0];
	int i, j;

	a->te_tree = NULL;

	for ( local_tid = 0; local_tid < num_tets; local_tid++, tet++ ) {
		/* for each edge of tet */
		TRY ( _search_te ( f, &node, local_tid,
				   tet->local_vids[0],
				   tet->local_vids[1] ) );
		TRY ( _search_te ( f, &node, local_tid,
				   tet->local_vids[0],
				   tet->local_vids[2] ) );
		TRY ( _search_te ( f, &node, local_tid,
				   tet->local_vids[0],
				   tet->local_vids[3] ) );
		TRY ( _search_te ( f, &node, local_tid,
				   tet->local_vids[1],
				   tet->local_vids[2] ) );
		TRY ( _search_te ( f, &node, local_tid,
				   tet->local_vids[1],
				   tet->local_vids[3] ) );
		TRY ( _search_te ( f, &node, local_tid,
				   tet->local_vids[2],
				   tet->local_vids[3] ) );
	}
	if ( node && node->value.items == NULL ) {
		_h5_free ( f, node );
	}

#define DEBUG
#ifdef DEBUG
	node = NULL;
	tet = &t->elems_data.tets[0];
	for ( local_tid = 0; local_tid < num_tets; local_tid++, tet++ ) {
		/* for each edge of tet */
		for ( i = 1; i <= 3; i++ ) {
			for ( j = 0; j < i; j++ ) {
				TRY ( _find_te (
					      f,
					      &node,
					      tet->local_vids[i],
					      tet->local_vids[j] ) );
				h5_debug ( f, "Edge (%lld,%lld):",
					   node->key.vid0,
					   node->key.vid1 );
				h5_size_t k;
				for ( k = 0; k < node->value.num_items; k++ ) {
					h5_debug ( f, "    %lld",
						   node->value.items[k] );
				}
			}
		}
	}
#endif
	return H5_SUCCESS;
}

static int
_cmp_td_node (
	const void *a,
	const void *b
	) {
	h5_td_node_key_t *key0 = (h5_td_node_key_t*)a;
	h5_td_node_key_t *key1 = (h5_td_node_key_t*)b;

	if ( key0->vid0 < key1->vid0 )
		return -1;
	if ( key0->vid0 > key1->vid0 )
		return 1;
	if ( key0->vid1 < key1->vid1 )
		return -1;
	if ( key0->vid1 > key1->vid1 )
		return 1;
	if ( key0->vid2 < key1->vid2 )
		return -1;
	if ( key0->vid2 > key1->vid2 )
		return 1;
	return 0;
}

static h5_err_t
_search_td (
	h5_file_t * const f,
	h5_td_node_t **node,
	h5_id_t local_eid,
	h5_id_t local_vid0,
	h5_id_t local_vid1,
	h5_id_t local_vid2
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	void *vnode;

	if ( *node ==NULL ) {
		TRY ( *node = _h5_alloc ( f, NULL, sizeof(**node) ) );
		memset ( *node, 0, sizeof(**node) );
	}

	if ( local_vid0 <= local_vid1 ) {
		if ( local_vid0 <= local_vid2 ) {
			(*node)->key.vid0 = local_vid0;
			if ( local_vid1 <= local_vid2 ) {
				(*node)->key.vid1 = local_vid1;
				(*node)->key.vid2 = local_vid2;
			} else {
				(*node)->key.vid1 = local_vid2;
				(*node)->key.vid2 = local_vid1;
			}
		} else { // v2 < v0 && v0 <= v1
			(*node)->key.vid0 = local_vid2;
			if ( local_vid0 <= local_vid1 ) {
				(*node)->key.vid1 = local_vid0;
				(*node)->key.vid2 = local_vid1;
			} else {
				(*node)->key.vid1 = local_vid1;
				(*node)->key.vid2 = local_vid0;
			}
		}
	} else { // v1 < v0
		if ( local_vid1 < local_vid2 ) {
			(*node)->key.vid0 = local_vid1;
			if ( local_vid0 <= local_vid2 ) {
				(*node)->key.vid1 = local_vid0;
				(*node)->key.vid2 = local_vid2;
			} else {
				(*node)->key.vid1 = local_vid2;
				(*node)->key.vid2 = local_vid0;
			}
		} else {
			(*node)->key.vid0 = local_vid2;
			if ( local_vid0 <= local_vid1 ) {
				(*node)->key.vid1 = local_vid0;
				(*node)->key.vid2 = local_vid1;
			} else {
				(*node)->key.vid1 = local_vid1;
				(*node)->key.vid2 = local_vid0;
			}
		}
	}
	TRY ( vnode = _h5_tsearch (
		      f,
		      *node,
		      (void**)&a->td_tree,
		      _cmp_td_node ) );
	h5_te_node_t *rnode = *(h5_te_node_t **)vnode;
	TRY ( _h5_append_to_idlist (
		      f,
		      &rnode->value,
		      local_eid ) );
	if ( rnode->value.num_items == 1 ) {
		*node = NULL;
	}
	return H5_SUCCESS;
}

static h5_err_t
_calc_tets_of_triangles (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	h5_id_t local_tid;
	h5_size_t num_tets = t->num_elems[t->num_levels-1];
	h5_td_node_t *node = NULL;
	h5_tet_data_t *tet = &t->elems_data.tets[0];

	a->td_tree = NULL;

	for ( local_tid = 0; local_tid < num_tets; local_tid++, tet++ ) {
		/* for each triangle of tet */
		TRY ( _search_td ( f, &node, local_tid,
				   tet->local_vids[0],
				   tet->local_vids[1],
				   tet->local_vids[2] ) );
		TRY ( _search_td ( f, &node, local_tid,
				   tet->local_vids[0],
				   tet->local_vids[1],
				   tet->local_vids[3] ) );
		TRY ( _search_td ( f, &node, local_tid,
				   tet->local_vids[0],
				   tet->local_vids[2],
				   tet->local_vids[3] ) );
		TRY ( _search_td ( f, &node,
				   local_tid,
				   tet->local_vids[1], 
				   tet->local_vids[2],
				   tet->local_vids[3] ) );
	}
	if ( node && node->value.items == NULL ) {
		_h5_free ( f, node );
	}

	return H5_SUCCESS;
}

h5_err_t
_h5t_rebuild_adj_data (
	h5_file_t * const f
	) {
	TRY ( _calc_tets_of_vertices ( f ) );
	TRY ( _calc_tets_of_edges ( f ) );
	TRY ( _calc_tets_of_triangles ( f ) );

	return H5_SUCCESS;
}
