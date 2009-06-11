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

static h5_err_t
_compute_children_of_edge (
	h5_file_t * const f,
	h5_id_t local_kid,
	h5_idlist_t *children
	);

/*
  compute T(V)
*/
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

	h5_tet_data_t *tet_data = &t->elems_data.tets[local_eid];
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

static h5_err_t
_find_te (
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

/*
  pass edge by:
  edge ID
  vertices
  face and element ID

  
*/

static h5_err_t
_find_te2 (
	h5_file_t * const f,
	h5_te_node_t **rnode,
	h5_id_t face_id,
	h5_id_t local_eid
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	h5_te_node_t node;
	void *vnode = &node;

	h5_tet_data_t *tet_data = &t->elems_data.tets[local_eid];
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
	h5_tet_data_t *tet = &t->elems_data.tets[0];
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

	h5_tet_data_t *tet_data = &t->elems_data.tets[local_eid];
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

h5_id_t *
_h5t_get_triangle_of_tet (
	const h5_tet_data_t *tet,
	const h5_id_t face_id,
	h5_2id_t tri
	) {
	int map[4][3] = { { 1, 2, 3 }, { 0, 2, 3 }, { 0, 1, 3 }, { 0, 1, 2} };
	
 	tri[0] = tet->local_vids[map[face_id][0]];
	tri[1] = tet->local_vids[map[face_id][1]];
	tri[2] = tet->local_vids[map[face_id][2]];
	return tri;
}

static h5_err_t
_find_td (
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
	TRY ( vnode = _h5_tfind (
		      f,
		      vnode,
		      (void**)&a->td_tree,
		      _cmp_td_node ) );
	*rnode = *(h5_td_node_t **)vnode;

	return H5_SUCCESS;
}

static h5_err_t
_find_td2 (
	h5_file_t * const f,
	h5_td_node_t **rnode,
	h5_id_t face_id,
	h5_id_t local_eid
	) {
	h5t_fdata_t *t = f->t;
	h5t_adjacencies_t *a = &t->adjacencies;
	h5_td_node_t node;
	void *vnode = &node;

	h5_tet_data_t *tet_data = &t->elems_data.tets[local_eid];
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
	TRY ( vnode = _h5_tfind (
		      f,
		      vnode,
		      (void**)&a->td_tree,
		      _cmp_td_node ) );
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
	h5_tet_data_t *tet = &t->elems_data.tets[0];
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
	h5_id_t local_kid,
	h5_id_t local_eid,
	h5_id_t	kids[2]
	) {
	h5_id_t face_id = _h5t_get_face_id ( local_kid );

	if ( face_id == 0 ) {
		kids[0] = _h5t_build_edge_id ( 0, local_eid+0 );
		kids[1] = _h5t_build_edge_id ( 0, local_eid+1 );
	} else if ( face_id == 1 ) {
		kids[0] = _h5t_build_edge_id ( 1, local_eid+1 );
		kids[1] = _h5t_build_edge_id ( 1, local_eid+2 );
	} else if ( face_id == 2 ) {
		kids[0] = _h5t_build_edge_id ( 2, local_eid+0 );
		kids[1] = _h5t_build_edge_id ( 2, local_eid+2 );
	} else if ( face_id == 3 ) {
		kids[0] = _h5t_build_edge_id ( 3, local_eid+0 );
		kids[1] = _h5t_build_edge_id ( 3, local_eid+3 );
	} else if ( face_id == 4 ) {
		kids[0] = _h5t_build_edge_id ( 4, local_eid+1 );
		kids[1] = _h5t_build_edge_id ( 4, local_eid+3 );
	} else if ( face_id == 5 ) {
		kids[0] = _h5t_build_edge_id ( 5, local_eid+2 );
		kids[1] = _h5t_build_edge_id ( 5, local_eid+3 );
	} else {
		return  h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
	return H5_SUCCESS;
}

/*
def _compute_children_of_edge ( E, children ):
	for E' = in =T(E):
		T' = tetrahedron bound to E'
		if T' has no children on level ≤ L: next
		(E'0, E'1) = _compute_direct_children_of edge ( E' )
		for i in [0,1]:
			T'i = tetrahedron bound to D'i
			if T'i has children on level ≤ L:
				_compute_children_of_edge ( E'i, children )
			else:
				children.append ( E'i ) 
*/
h5_err_t
_compute_children_of_edge (
	h5_file_t * const f,
	h5_id_t local_kid,
	h5_idlist_t *children
	) {
	h5t_fdata_t *t = f->t;
	h5_te_node_t *te;
	int i,k;

	TRY ( _find_te2 (
		      f,
		      &te,
		      _h5t_get_face_id ( local_kid ),
		      _h5t_get_elem_id ( local_kid ) ) 
		);
	for ( k = 0; k < te->value.num_items; k++ ) {
		h5_id_t local_eid = _h5t_get_elem_id ( te->value.items[k] );
		h5_tet_data_t *tet = &t->elems_data.tets[local_eid];

		/* no children? */
		if ( tet->local_child_eid < 0 )
			continue;
		/* no children on current level? */
		h5_tet_data_t *child_tet = &t->elems_data.tets[tet->local_child_eid];
		if ( child_tet->level_id > t->cur_level )
			continue;

		h5_id_t local_kids[2];
		TRY ( _compute_direct_children_of_edge (
			f,
			te->value.items[k],	/* parent edge */
			tet->local_child_eid,	/* first children of element */
			local_kids ) );		/* result */
		for ( i = 0; i < 2; i++ ) {
			local_eid = _h5t_get_elem_id ( local_kids[i] );
			tet = &t->elems_data.tets[local_eid];

			/* no children? */
			h5_id_t l_chld_eid = tet->local_child_eid;
			if ( ( l_chld_eid < 0 ) ||
			     ( t->elems_data.tets[l_chld_eid].level_id > t->cur_level)) {
				TRY ( _h5_append_to_idlist (
					      f, children, local_kids[i] )
					);
			} else {
				TRY ( _compute_children_of_edge (
					      f, local_kids[i], children ) );
			}
		}
	}
	return H5_SUCCESS;
}


h5_err_t
_compute_direct_children_of_triangle (
	h5_file_t * const f,
	h5_id_t local_did,
	h5_id_t	children_dids[4]
	) {

	h5_id_t local_eid = _h5t_get_elem_id ( local_did );
	h5_id_t face_id = _h5t_get_face_id ( local_did );
	if ( face_id == 0 ) {
		children_dids[0] = _h5t_build_triangle_id ( 0, local_eid+1 );
		children_dids[1] = _h5t_build_triangle_id ( 0, local_eid+2 );
		children_dids[2] = _h5t_build_triangle_id ( 0, local_eid+3 );
		children_dids[3] = _h5t_build_triangle_id ( 0, local_eid+7 );
	} else if ( face_id == 1 ) {
		children_dids[0] = _h5t_build_triangle_id ( 1, local_eid+0 );
		children_dids[1] = _h5t_build_triangle_id ( 1, local_eid+2 );
		children_dids[2] = _h5t_build_triangle_id ( 1, local_eid+3 );
		children_dids[3] = _h5t_build_triangle_id ( 1, local_eid+6 );
	} else if ( face_id == 2 ) {
		children_dids[0] = _h5t_build_triangle_id ( 2, local_eid+0 );
		children_dids[1] = _h5t_build_triangle_id ( 2, local_eid+1 );
		children_dids[2] = _h5t_build_triangle_id ( 2, local_eid+3 );
		children_dids[3] = _h5t_build_triangle_id ( 2, local_eid+4 );
	} else if ( face_id == 3 ) {
		children_dids[0] = _h5t_build_triangle_id ( 3, local_eid+0 );
		children_dids[1] = _h5t_build_triangle_id ( 3, local_eid+1 );
		children_dids[2] = _h5t_build_triangle_id ( 3, local_eid+2 );
		children_dids[3] = _h5t_build_triangle_id ( 3, local_eid+5 );
	} else {
		return  h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
	return H5_SUCCESS;
}

/*
def _compute_children_of_triangle ( D, children ):
	for D' in T(D):
		T' = tetrahedron bound to D'
		if T' has no children on level ≤ L: next
		(D'0,D'1,D'2,D'3) = _compute_direct_children_of_triangle ( D' )
		for i in [0,1,2,3]:
			T'i = tetrahedron bound to D'i
			if T'i has children on level ≤ L:
				_compute_children_of_triangle ( D'i, children )
			else:
				children.append ( D'i) 
*/
h5_err_t
_compute_children_of_triangle (
	h5_file_t * const f,
	h5_id_t local_did,
	h5_idlist_t *children 
	) {

	h5t_fdata_t *t = f->t;
	h5_td_node_t *td;
	int i, k;

	TRY ( _find_td2 (
		      f,
		      &td,
		      _h5t_get_face_id ( local_did ),
		      _h5t_get_elem_id ( local_did ) ) 
		      );
	for ( k = 0; k < td->value.num_items; k++ ) {
		h5_id_t local_eid = _h5t_get_elem_id ( td->value.items[k] );
		h5_tet_data_t *tet = &t->elems_data.tets[local_eid];

		/* no children? */
		if ( tet->local_child_eid < 0 )
			continue;
		/* no children on current level? */
		h5_tet_data_t *child = &t->elems_data.tets[tet->local_child_eid];
		if ( child->level_id > t->cur_level )
			continue;

		h5_id_t local_dids[4];
		TRY ( _compute_direct_children_of_triangle (
			f,
			td->value.items[k],	/* parent triangle */
			local_dids ) );		/* result */
		for ( i = 0; i < 4; i++ ) {
			local_eid = _h5t_get_elem_id ( local_dids[i] );
			tet = &t->elems_data.tets[local_eid];
			/* no children? */
			h5_id_t l_chld_eid = tet->local_child_eid;
			if ( ( l_chld_eid < 0 ) ||
			     ( t->elems_data.tets[l_chld_eid].level_id > t->cur_level)) {
				TRY ( _h5_append_to_idlist (
					      f, children, local_dids[i] )
					);
			} else {
				TRY ( _compute_children_of_triangle (
					      f, local_dids[i], children ) );
			}
		}
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
	h5_2id_t edge
	) {
	h5_te_node_t *te;
	TRY ( _find_te ( f, &te, edge ) );
	/* search/add unique ID */
	_h5_search_idlist ( f, list, te->value.items[0] );

	return H5_SUCCESS;
}

h5_err_t
_h5t_tet_is_on_level (
	h5_file_t * const f,
	h5_tet_data_t *tet_dta 
	) {
	h5t_fdata_t *t = f->t;

	if ( tet_dta->level_id > t->cur_level ) {
		/* No. Tetrahedron has been defined on higher level */
		return H5_ERR;
	}

	h5_id_t local_child_eid = tet_dta->local_child_eid;
	if ( ( local_child_eid >= 0 ) &&
	     ( t->elems_data.tets[local_child_eid].level_id <= t->cur_level ) ) {
		/* No. Tetrahedron has children an a level <= current level */
		return H5_ERR;
	}

	return H5_SUCCESS;
}

h5_id_t *
_h5t_get_edge_of_tet (
	const h5_tet_data_t *tet,
	const h5_id_t face_id,
	h5_2id_t edge
	) {
	int map[6][2] = { { 0,1 }, {1,2}, {0,2}, {0,3}, {1,3}, {2,3} };

 	edge[0] = tet->local_vids[map[face_id][0]];
	edge[1] = tet->local_vids[map[face_id][1]];
	return edge;
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

	for ( i = 0; i < tv->num_items; i++ ) {
		h5_id_t local_eid = _h5t_get_elem_id ( tv->items[i] );
		h5_id_t vertex_id = _h5t_get_face_id ( tv->items[i] );
		h5_tet_data_t *tet = &t->elems_data.tets[local_eid];

		if ( _h5t_tet_is_on_level ( f, tet ) == H5_ERR ) {
			continue;
		}
		int map[4][3] = { { 0, 2, 3 }, { 0, 1, 4 }, { 2, 1, 5 }, { 3, 4, 5} };
		h5_2id_t edge;
		TRY ( _add_edge ( f, *list,
				  _h5t_get_edge_of_tet(tet, map[vertex_id][0], edge) ) );
		TRY ( _add_edge ( f, *list,
				  _h5t_get_edge_of_tet(tet, map[vertex_id][1], edge) ) );
		TRY ( _add_edge ( f, *list,
				  _h5t_get_edge_of_tet(tet, map[vertex_id][2], edge) ) );
	}
	return H5_SUCCESS;
}

static h5_err_t
_add_triangle (
	h5_file_t * const f,
	h5_idlist_t *list,
	h5_3id_t tri
	) {
	h5_td_node_t *td;
	TRY ( _find_td ( f, &td, tri ) );
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

	for ( i = 0; i < tv->num_items; i++ ) {
		h5_id_t local_eid = _h5t_get_elem_id ( tv->items[i] );
		h5_id_t vertex_id = _h5t_get_face_id ( tv->items[i] );
		h5_tet_data_t *tet = &t->elems_data.tets[local_eid];

		if ( _h5t_tet_is_on_level ( f, tet ) == H5_ERR ) {
			continue;
		}
		int map[4][3] = { { 1, 2, 3 }, { 0, 2, 3 }, { 0, 1, 3 }, { 0, 1, 2} };
		h5_3id_t tri;
		TRY ( _add_triangle (
			      f, *list,
			      _h5t_get_triangle_of_tet(tet, map[vertex_id][0], tri) ) );
		TRY ( _add_triangle (
			      f, *list,
			      _h5t_get_triangle_of_tet(tet, map[vertex_id][1], tri) ) );
		TRY ( _add_triangle (
			      f, *list,
			      _h5t_get_triangle_of_tet(tet, map[vertex_id][2], tri) ) );
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

	for ( i = 0; i < tv->num_items; i++ ) {
		h5_id_t local_eid = _h5t_get_elem_id ( tv->items[i] );
		h5_tet_data_t *tet = &t->elems_data.tets[local_eid];

		if ( _h5t_tet_is_on_level ( f, tet ) == H5_ERR ) {
			continue;
		}
		TRY ( _h5_search_idlist ( f, *list, local_eid ) );
	}
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
