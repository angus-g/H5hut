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
#include <hdf5.h>
#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

h5_id_t *
_h5t_get_edge_of_tet (
	const h5_elem_ldta_t *tet,
	const h5_id_t face_id,
	h5_2id_t edge
	) {
	int map[6][2] = { { 0,1 }, {1,2}, {0,2}, {0,3}, {1,3}, {2,3} };

 	edge[0] = tet->local_vids[map[face_id][0]];
	edge[1] = tet->local_vids[map[face_id][1]];
	return edge;
}

h5_id_t *
_h5t_get_triangle_of_tet (
	const h5_elem_ldta_t *tet,
	const h5_id_t face_id,
	h5_2id_t tri
	) {
	int map[4][3] = { { 1, 2, 3 }, { 0, 2, 3 }, { 0, 1, 3 }, { 0, 1, 2} };
	
 	tri[0] = tet->local_vids[map[face_id][0]];
	tri[1] = tet->local_vids[map[face_id][1]];
	tri[2] = tet->local_vids[map[face_id][2]];
	return tri;
}


/*
  compute T(V)
*/
static h5_err_t
_calc_tets_of_vertices (
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
_cmp_te_node (
	const void *a,
	const void *b
	) {
	h5_te_node_key_t *key0 = (h5_te_node_key_t*)a;
	h5_te_node_key_t *key1 = (h5_te_node_key_t*)b;

	if ( key0->vids[0] < key1->vids[0] )
		return -1;
	if ( key0->vids[0] > key1->vids[0] )
		return 1;
	if ( key0->vids[1] < key1->vids[1] )
		return -1;
	if ( key0->vids[1] > key1->vids[1] )
		return 1;
	return 0;
}

static h5_err_t
_search_te2 (
	h5_file_t * const f,
	h5_te_node_t **node,
	h5_id_t face_id,
	h5_id_t local_eid
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	void *vnode;

	h5_elem_ldta_t *tet_data = &t->elems_ldta[local_eid];
	h5_id_t vid;
	int map[6][2] = { { 0,1 }, {1,2}, {0,2}, {0,3}, {1,3}, {2,3} };

	if ( *node ==NULL ) {
		TRY ( *node = _h5_alloc ( f, NULL, sizeof(**node) ) );
		memset ( *node, 0, sizeof(**node) );
	}
	h5_id_t *edge = (*node)->key.vids;
 	edge[0] = tet_data->local_vids[map[face_id][0]];
	edge[1] = tet_data->local_vids[map[face_id][1]];

	if ( edge[0] > edge[1] ) {
		vid = edge[0]; edge[0] = edge[1]; edge[1] = vid;
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
		      _h5t_build_edge_id ( face_id, local_eid ) ) );
	if ( rnode->value.num_items == 1 ) {
		*node = NULL;
	}
	return H5_SUCCESS;
}

h5_err_t
_h5t_find_te (
	h5_file_t * const f,
	h5_te_node_t **rnode,
	h5_2id_t edge
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	h5_te_node_t node;
	void *vnode = &node;

	if ( edge[0] > edge[1] ) {
		h5_id_t vid = edge[0]; edge[0] = edge[1]; edge[1] = vid;
	}
	memcpy ( node.key.vids, edge, 2*sizeof(*edge) );
	TRY ( vnode = _h5_tfind (
		      f,
		      vnode,
		      (void**)&a->te_tree,
		      _cmp_te_node ) );
	*rnode = *(h5_te_node_t **)vnode;

	return H5_SUCCESS;
}

h5_err_t
_h5t_find_te2 (
	h5_file_t * const f,
	h5_te_node_t **rnode,
	h5_id_t face_id,
	h5_id_t local_eid
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	h5_te_node_t node;
	void *vnode = &node;

	h5_elem_ldta_t *tet_data = &t->elems_ldta[local_eid];
	h5_id_t *edge = node.key.vids;
	int map[6][2] = { { 0,1 }, {1,2}, {0,2}, {0,3}, {1,3}, {2,3} };

	edge[0] = tet_data->local_vids[map[face_id][0]];
	edge[1] = tet_data->local_vids[map[face_id][1]];

	if ( edge[0] > edge[1] ) {
		h5_id_t vid = edge[0]; edge[0] = edge[1]; edge[1] = vid;
	}
	TRY ( vnode = _h5_tfind (
		      f,
		      vnode,
		      (void**)&a->te_tree,
		      _cmp_te_node ) );
	*rnode = *(h5_te_node_t **)vnode;

	return H5_SUCCESS;
}

/*
  Sort ID list according their tetrahedra ID.

  Called by twalk().
*/
static void
_sort_telist (
	const void *_node,
	const VISIT order,
	const int depth
	) {
	if ( order == postorder || order == leaf ) {
		h5_te_node_t *node = *(h5_te_node_t **)_node;
		h5_idlist_t *list = &node->value;
		qsort (
			list->items,
			list->num_items,
			sizeof(list->items[0]),
			_h5_cmp_ids_by_eid );
	}
}

static h5_err_t
_calc_tets_of_edges (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	h5_id_t local_eid;
	h5_size_t num_tets = t->num_elems[t->num_levels-1];
	h5_te_node_t *node = NULL;
	h5_elem_ldta_t *tet = &t->elems_ldta[0];
	h5_id_t face_id;

	a->te_tree = NULL;

	for ( local_eid = 0; local_eid < num_tets; local_eid++, tet++ ) {
		for ( face_id = 0; face_id < 6; face_id++ ) {
			TRY (
				_search_te2 (
					f,
					&node,
					face_id,
					local_eid )
				);
		}
	}
	twalk ( (void*)a->te_tree, _sort_telist ); 
	if ( node && node->value.items == NULL ) {
		_h5_free ( f, node );
	}
	return H5_SUCCESS;
}

static int
_cmp_td_node (
	const void *a,
	const void *b
	) {
	h5_td_node_key_t *key0 = (h5_td_node_key_t*)a;
	h5_td_node_key_t *key1 = (h5_td_node_key_t*)b;

	if ( key0->vids[0] < key1->vids[0] )
		return -1;
	if ( key0->vids[0] > key1->vids[0] )
		return 1;
	if ( key0->vids[1] < key1->vids[1] )
		return -1;
	if ( key0->vids[1] > key1->vids[1] )
		return 1;
	if ( key0->vids[2] < key1->vids[2] )
		return -1;
	if ( key0->vids[2] > key1->vids[2] )
		return 1;
	return 0;
}

static h5_err_t
_search_td2 (
	h5_file_t * const f,
	h5_td_node_t **node,
	h5_id_t face_id,
	h5_id_t local_eid
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	void *vnode;

	h5_elem_ldta_t *tet_data = &t->elems_ldta[local_eid];
	h5_id_t vid;
	int map[4][3] = { { 1,2,3 }, {0,2,3}, {0,1,3}, {0,1,2} };

	if ( *node ==NULL ) {
		TRY ( *node = _h5_alloc ( f, NULL, sizeof(**node) ) );
		memset ( *node, 0, sizeof(**node) );
	}
	h5_id_t *triangle = (*node)->key.vids;
	triangle[0] = tet_data->local_vids[map[face_id][0]];
	triangle[1] = tet_data->local_vids[map[face_id][1]];
	triangle[2] = tet_data->local_vids[map[face_id][2]];

	if ( triangle[0] > triangle[1] ) {
		vid = triangle[0]; triangle[0] = triangle[1]; triangle[1] = vid;
	}
	if ( triangle[1] > triangle[2] ) {
		vid = triangle[1]; triangle[1] = triangle[2]; triangle[2] = vid;
	}
	if ( triangle[0] > triangle[1] ) {
		vid = triangle[0]; triangle[0] = triangle[1]; triangle[1] = vid;
	}

	TRY ( vnode = _h5_tsearch (
		      f,
		      *node,
		      (void**)&a->td_tree,
		      _cmp_td_node ) );
	h5_td_node_t *rnode = *(h5_td_node_t **)vnode;
	TRY ( _h5_append_to_idlist (
		      f,
		      &rnode->value,
		      _h5t_build_triangle_id ( face_id, local_eid ) ) );
	if ( rnode->value.num_items == 1 ) {
		*node = NULL;
	}
	return H5_SUCCESS;
}

h5_err_t
_h5t_find_td (
	h5_file_t * const f,
	h5_td_node_t **rnode,
	h5_3id_t tri
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	h5_td_node_t node;
	void *vnode = &node;

	h5_id_t vid;
	if ( tri[0] > tri[1] ) {
		vid = tri[0]; tri[0] = tri[1]; tri[1] = vid;
	}
	if ( tri[1] > tri[2] ) {
		vid = tri[1]; tri[1] = tri[2]; tri[2] = vid;
	}
	if ( tri[0] > tri[1] ) {
		vid = tri[0]; tri[0] = tri[1]; tri[1] = vid;
	}
	memcpy ( node.key.vids, tri, 3*sizeof(*tri) );
	vnode = _h5_tfind (
		      f,
		      vnode,
		      (void**)&a->td_tree,
		      _cmp_td_node );
	if ( (h5_err_t)(ptrdiff_t)(vnode) == H5_ERR ) {
		return _h5t_error_local_triangle_nexist( f, tri );
	}

	*rnode = *(h5_td_node_t **)vnode;

	return H5_SUCCESS;
}

h5_err_t
_h5t_find_td2 (
	h5_file_t * const f,
	h5_td_node_t **rnode,
	h5_id_t face_id,
	h5_id_t local_eid
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	h5_td_node_t node;
	void *vnode = &node;

	h5_elem_ldta_t *tet_data = &t->elems_ldta[local_eid];
	h5_id_t *tri = node.key.vids;

	_h5t_get_triangle_of_tet ( tet_data, face_id, tri );

	h5_id_t vid;
	if ( tri[0] > tri[1] ) {
		vid = tri[0]; tri[0] = tri[1]; tri[1] = vid;
	}
	if ( tri[1] > tri[2] ) {
		vid = tri[1]; tri[1] = tri[2]; tri[2] = vid;
	}
	if ( tri[0] > tri[1] ) {
		vid = tri[0]; tri[0] = tri[1]; tri[1] = vid;
	}
	vnode = _h5_tfind (
		f,
		vnode,
		(void**)&a->td_tree,
		_cmp_td_node );
	if ( (h5_err_t)(ptrdiff_t)(vnode) == H5_ERR ) {
		return _h5t_error_local_triangle_nexist( f, tri );
	}

	*rnode = *(h5_td_node_t **)vnode;

	return H5_SUCCESS;
}

/*
  Sort ID list according their tetrahedra ID.

  Called by twalk().
*/
static void
_sort_tdlist (
	const void *_node,
	const VISIT order,
	const int depth
	) {
	if ( order == postorder || order == leaf ) {
		h5_td_node_t *node = *(h5_td_node_t **)_node;
		h5_idlist_t *list = &node->value;
		qsort (
			list->items,
			list->num_items,
			sizeof(list->items[0]),
			_h5_cmp_ids_by_eid );
	}
}


static h5_err_t
_calc_tets_of_triangles (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	h5_id_t local_eid;
	h5_size_t num_tets = t->num_elems[t->num_levels-1];
	h5_td_node_t *node = NULL;
	h5_elem_ldta_t *tet = &t->elems_ldta[0];
	h5_id_t face_id;

	a->td_tree = NULL;

	for ( local_eid = 0; local_eid < num_tets; local_eid++, tet++ ) {
		for ( face_id = 0; face_id < 4; face_id++ ) {
			TRY (
				_search_td2 (
					f,
					&node,
					face_id,
					local_eid )
				);
		}
	}
	twalk ( (void*)a->td_tree, _sort_tdlist ); 
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
	h5_te_node_t *te;

	TRY ( _h5t_find_te2 (
		      f,
		      &te,
		      _h5t_get_face_id ( local_kid ),
		      _h5t_get_elem_id ( local_kid ) ) 
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
	h5_te_node_t *te;

	TRY ( _h5t_find_te2 (
		      f,
		      &te,
		      _h5t_get_face_id ( local_kid ),
		      _h5t_get_elem_id ( local_kid ) ) 
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
	h5_td_node_t *td;

	TRY ( _h5t_find_td2 (
		      f,
		      &td,
		      _h5t_get_face_id ( local_did ),
		      _h5t_get_elem_id ( local_did ) ) 
		      );
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
	h5_td_node_t *td;

	TRY ( _h5t_find_td2 (
		      f,
		      &td,
		      _h5t_get_face_id ( local_did ),
		      _h5t_get_elem_id ( local_did ) ) 
		);
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
	h5_te_node_t *te;
	TRY ( _h5t_find_te2 ( f, &te, face_id, local_eid ) );
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
	h5_td_node_t *td;
	TRY ( _h5t_find_td2 ( f, &td, face_id, local_eid ) );
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
