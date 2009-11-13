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
  compute T(V) from current level up to highest levels.
*/
static h5_err_t
_compute_tets_of_vertices (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t eid = (t->cur_level <= 0 ) ? 0 : t->num_elems[t->cur_level-1];
	h5_elem_ldta_t *tet = tet = &t->elems_ldta[eid];
	h5_id_t num_elems = t->num_elems[t->num_levels-1];
	for ( ;eid < num_elems; eid++, tet++ ) {
		int i;
		for ( i = 0; i < 4; i++ ) {
			h5_id_t vid = tet->local_vids[i];
			TRY ( _h5_append_to_idlist (
				      f,
				      &t->vertices_data[vid].tv,
				      _h5t_build_vertex_id( i, eid ) ) );
		}
	}
	return H5_SUCCESS;
}

/*
  Compute T(E) from current level up to highest levels.
 */
static h5_err_t
_compute_tets_of_edges (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t eid = (t->cur_level <= 0 ) ? 0 : t->num_elems[t->cur_level-1];
	h5_elem_ldta_t *tet = tet = &t->elems_ldta[eid];
	h5_id_t num_elems = t->num_elems[t->num_levels-1];
	h5t_te_entry_t *retval = NULL;
	TRY ( _h5t_resize_te_htab ( f, 4*(num_elems-eid) ) );
	for ( ; eid < num_elems; eid++, tet++ ) {
		h5_id_t face;
		for ( face = 0; face < 6; face++ ) {
			TRY ( _h5t_search_te2 ( f, face, eid, &retval ) );
		}
	}
	return H5_SUCCESS;
}

static h5_err_t
_compute_tets_of_triangles (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t eid = (t->cur_level <= 0 ) ? 0 : t->num_elems[t->cur_level-1];
	h5_elem_ldta_t *tet = tet = &t->elems_ldta[eid];
	h5_id_t num_elems = t->num_elems[t->num_levels-1];
	h5t_td_entry_t *retval = NULL;
	TRY ( _h5t_resize_td_htab ( f, 4*(num_elems-eid) ) );
	for ( ; eid < num_elems; eid++, tet++ ) {
		h5_id_t face;
		for ( face = 0; face < 4; face++ ) {
			TRY ( _h5t_search_td2 ( f, face, eid, &retval ) );
		}
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

static h5_err_t
_compute_children_of_edge (
	h5_file_t * const f,
	h5_id_t kid,
	h5_idlist_t *children
	) {
	h5t_fdata_t *t = f->t;
	h5t_te_entry_t *te;

	TRY ( _h5t_find_te2 (
		      f,
		      _h5t_get_face_id ( kid ),
		      _h5t_get_elem_idx ( kid ),
		      &te ) 
		);
	h5_id_t *edge = te->value.items;
	h5_id_t *end = te->value.items+te->value.num_items;
	for ( ; edge < end; edge++ ) {
		h5_id_t eid = _h5t_get_elem_idx ( *edge );
		h5_id_t face_id =  _h5t_get_face_id ( *edge );
		h5_elem_ldta_t *tet = &t->elems_ldta[eid];
		if ( _h5t_elem_is_on_cur_level ( f, tet ) == H5_OK ) {
			TRY ( _h5_append_to_idlist (
				      f, children, *edge )
				);
		} else {
			h5_id_t kids[2];
			TRY ( _h5t_compute_direct_children_of_edge (
				      f,
				      face_id,
				      tet->local_child_eid,
				      kids ) );
			TRY ( _compute_children_of_edge (
				      f, kids[0], children ) );
			TRY ( _compute_children_of_edge (
				      f, kids[1], children ) );
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
	h5_id_t kid,
	h5_idlist_t *children
	) {
	h5t_fdata_t *t = f->t;
	h5t_te_entry_t *te;

	TRY ( _h5t_find_te2 (
		      f,
		      _h5t_get_face_id ( kid ),
		      _h5t_get_elem_idx ( kid ),
		      &te )
		);
	h5_id_t *edge = te->value.items;
	h5_id_t *end = te->value.items+te->value.num_items;
	int refined = 0;
	for ( ; edge < end; edge++ ) {
		h5_id_t eid = _h5t_get_elem_idx ( *edge );
		h5_id_t face_id =  _h5t_get_face_id ( *edge );
		h5_elem_ldta_t *tet = &t->elems_ldta[eid];
		if ( ! _h5t_elem_is_on_cur_level ( f, tet ) == H5_OK ) {
			refined = 1;
			h5_id_t kids[2];
			TRY ( _h5t_compute_direct_children_of_edge (
				      f,
				      face_id,
				      tet->local_child_eid,
				      kids ) );
			TRY ( _compute_sections_of_edge (
				      f, kids[0], children ) );
			TRY ( _compute_sections_of_edge (
				      f, kids[1], children ) );
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
	h5_id_t eid,
	h5_id_t	dids[4]
	) {
	int off[4][4] = { {1,2,3,7}, {0,2,3,6}, {0,1,3,4}, {0,1,2,5} };

	if ( ( face_id < 0 ) || ( face_id >= 4 ) ) {
		return  h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
	dids[0] = _h5t_build_edge_id ( face_id, eid+off[face_id][0] );
	dids[1] = _h5t_build_edge_id ( face_id, eid+off[face_id][1] );
	dids[2] = _h5t_build_edge_id ( face_id, eid+off[face_id][2] );
	dids[3] = _h5t_build_edge_id ( face_id, eid+off[face_id][3] );
	return H5_SUCCESS;
}

h5_err_t
_compute_children_of_triangle (
	h5_file_t * const f,
	h5_id_t did,
	h5_idlist_t *children 
	) {

	h5t_fdata_t *t = f->t;
	h5t_td_entry_t *td;

	TRY ( _h5t_find_td2 (
		      f,
		      _h5t_get_face_id ( did ),
		      _h5t_get_elem_idx ( did ),
		      &td
		      ) );
	h5_id_t *tri = td->value.items;
	h5_id_t *end = td->value.items+td->value.num_items;
	for ( ; tri < end; tri++ ) {
		h5_id_t eid = _h5t_get_elem_idx ( *tri );
		h5_id_t face_id =  _h5t_get_face_id ( *tri );
		h5_elem_ldta_t *tet = &t->elems_ldta[eid];
		if ( _h5t_elem_is_on_cur_level ( f, tet ) == H5_OK ) {
			TRY ( _h5_append_to_idlist (
				      f, children, *tri )
				);
		} else {
			h5_id_t dids[4];
			TRY ( _compute_direct_children_of_triangle (
				      f,
				      face_id,
				      tet->local_child_eid,
				      dids ) );
			TRY ( _compute_children_of_triangle (
				      f, dids[0], children ) );
			TRY ( _compute_children_of_triangle (
				      f, dids[1], children ) );
			TRY ( _compute_children_of_triangle (
				      f, dids[2], children ) );
			TRY ( _compute_children_of_triangle (
				      f, dids[3], children ) );
		}
	}
	return H5_SUCCESS;
}

static h5_err_t
_compute_sections_of_triangle (
	h5_file_t * const f,
	h5_id_t did,
	h5_idlist_t *children
	) {
	h5t_fdata_t *t = f->t;
	h5t_td_entry_t *td;

	TRY ( _h5t_find_td2 (
		      f,
		      _h5t_get_face_id ( did ),
		      _h5t_get_elem_idx ( did ), &td ) );
	h5_id_t *tri = td->value.items;
	h5_id_t *end = td->value.items+td->value.num_items;
	int refined = 0;
	for ( ; tri < end; tri++ ) {
		h5_id_t eid = _h5t_get_elem_idx ( *tri );
		h5_id_t face_id =  _h5t_get_face_id ( *tri );
		h5_elem_ldta_t *tet = &t->elems_ldta[eid];
		if ( ! _h5t_elem_is_on_cur_level ( f, tet ) == H5_OK ) {
			refined = 1;
			h5_id_t dids[4];
			TRY ( _compute_direct_children_of_triangle (
				      f,
				      face_id,
				      tet->local_child_eid,
				      dids ) );
			TRY ( _compute_sections_of_triangle (
				      f, dids[0], children ) );
			TRY ( _compute_sections_of_triangle (
				      f, dids[1], children ) );
			TRY ( _compute_sections_of_triangle (
				      f, dids[2], children ) );
			TRY ( _compute_sections_of_triangle (
				      f, dids[3], children ) );

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
	h5_id_t eid
	) {
	h5t_te_entry_t *te;
	TRY ( _h5t_find_te2 ( f, face_id, eid, &te ) );
	TRY ( _h5_search_idlist ( f, list, te->value.items[0] ) );
	return H5_SUCCESS;
}


h5_err_t
h5t_get_edges_upadjacent_to_vertex (
	h5_file_t * const f,
	const h5_id_t vid,
	h5_idlist_t **list
	) {
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	
	h5t_fdata_t *t = f->t;
	h5_idlist_t *tv = &t->vertices_data[vid].tv;
	h5_size_t i;

	h5_id_t *vidp = tv->items;
	for ( i = 0; i < tv->num_items; i++, vidp++ ) {
		h5_id_t eid = _h5t_get_elem_idx ( *vidp );
		h5_id_t face = _h5t_get_face_id ( *vidp );
		h5_elem_ldta_t *tet = &t->elems_ldta[eid];

		if ( _h5t_elem_is_on_cur_level ( f, tet ) == H5_NOK ) {
			continue;
		}
		int map[4][3] = { {0,2,3}, {0,1,4}, {2,1,5}, {3,4,5} };
		TRY ( _add_edge ( f, *list, map[face][0], eid ) );
		TRY ( _add_edge ( f, *list, map[face][1], eid ) );
		TRY ( _add_edge ( f, *list, map[face][2], eid ) );
	}
	return H5_SUCCESS;
}

static h5_err_t
_add_triangle (
	h5_file_t * const f,
	h5_idlist_t *list,
	h5_id_t face,
	h5_id_t eid
	) {
	h5t_td_entry_t *td;
	TRY ( _h5t_find_td2 ( f, face, eid, &td ) );
	TRY ( _h5_search_idlist ( f, list, td->value.items[0] ) );

	return H5_SUCCESS;
}

h5_err_t
h5t_get_triangles_upadjacent_to_vertex (
	h5_file_t * const f,
	const h5_id_t vid,
	h5_idlist_t **list
	) {
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	
	h5t_fdata_t *t = f->t;
	h5_idlist_t *tv = &t->vertices_data[vid].tv;
	h5_size_t i;
	h5_id_t *vidp = tv->items;
	for ( i = 0; i < tv->num_items; i++, vidp++ ) {
		h5_id_t eid = _h5t_get_elem_idx ( *vidp );
		h5_id_t face = _h5t_get_face_id ( *vidp );
		h5_elem_ldta_t *tet = &t->elems_ldta[eid];

		if ( _h5t_elem_is_on_cur_level ( f, tet ) == H5_NOK ) {
			continue;
		}
		int map[4][3] = { {1,2,3}, {0,2,3}, {0,1,3}, {0,1,2} };
		TRY ( _add_triangle ( f, *list, map[face][0], eid ) );
		TRY ( _add_triangle ( f, *list, map[face][1], eid ) );
		TRY ( _add_triangle ( f, *list, map[face][2], eid ) );
	}
	return H5_SUCCESS;
}

h5_err_t
h5t_get_tets_upadjacent_to_vertex (
	h5_file_t * const f,
	const h5_id_t vid,
	h5_idlist_t **list
	) {
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	
	h5t_fdata_t *t = f->t;
	h5_idlist_t *tv = &t->vertices_data[vid].tv;
	h5_size_t i;
	h5_id_t *vidp = tv->items;
	for ( i = 0; i < tv->num_items; i++, vidp++ ) {
		h5_id_t eid = _h5t_get_elem_idx ( *vidp );
		h5_elem_ldta_t *tet = &t->elems_ldta[eid];

		if ( _h5t_elem_is_on_cur_level ( f, tet ) == H5_NOK ) {
			continue;
		}
		TRY ( _h5_search_idlist ( f, *list, eid ) );
	}
	return H5_SUCCESS;
}

h5_err_t
h5t_get_triangles_upadjacent_to_edge (
	h5_file_t * const f,
	const h5_id_t kid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );
	TRY ( _compute_children_of_edge ( f, kid, children ) );
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	h5_id_t *edge = children->items;
	h5_id_t *end = children->items+children->num_items;
	int map[6][2] = { {2,3}, {0,3}, {1,3}, {1,2}, {0,2}, {0,1} };
	for ( ; edge < end; edge++ ) {
		h5_id_t eid = _h5t_get_elem_idx ( *edge );
		h5_id_t face_id = _h5t_get_face_id ( *edge );
		TRY ( _add_triangle ( f, *list, map[face_id][0], eid ) );
		TRY ( _add_triangle ( f, *list, map[face_id][1], eid ) );
	}
	TRY ( _h5_free_idlist( f, &children ) );

	return H5_SUCCESS;
}

h5_err_t
h5t_get_tets_upadjacent_to_edge (
	h5_file_t * const f,
	const h5_id_t kid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );
	TRY( _compute_children_of_edge ( f, kid, children ) );
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	int i;
	h5_id_t *kidp = children->items;
	for ( i = 0; i < children->num_items; i++, kidp++ ) {
		h5_id_t eid = _h5t_get_elem_idx ( *kidp );
		TRY ( _h5_search_idlist ( f, *list, eid ) );
	}
	TRY ( _h5_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

h5_err_t
h5t_get_tets_upadjacent_to_triangle (
	h5_file_t * const f,
	const h5_id_t did,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );
	TRY( _compute_children_of_triangle ( f, did, children ) );
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	int i;
	h5_id_t *didp = children->items;
	for ( i = 0; i < children->num_items; i++ , didp++) {
		h5_id_t eid = _h5t_get_elem_idx ( *didp );
		TRY ( _h5_search_idlist ( f, *list, eid ) );
	}
	TRY ( _h5_free_idlist( f, &children ) );
	return H5_SUCCESS;
}


h5_err_t
h5t_get_vertices_downadjacent_to_edge (
	h5_file_t * const f,
	const h5_id_t kid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );
	TRY( _compute_sections_of_edge ( f, kid, children ) );
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	int i;
	h5_id_t *kidp = children->items;
	for ( i = 0; i < children->num_items; i++, kidp++ ) {
		h5_id_t vids[2];
		TRY ( h5t_get_local_vids_of_edge ( f, *kidp, vids ) );
		TRY ( _h5_search_idlist ( f, *list, vids[0] ) );
		TRY ( _h5_search_idlist ( f, *list, vids[1] ) );
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
	const h5_id_t did,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );

	int map[4][3] = { {1,4,5}, {2,3,5}, {0,3,4}, {0,1,2} };
	h5_id_t face = _h5t_get_face_id ( did );
	h5_id_t eid = _h5t_get_elem_idx ( did );

	h5_id_t i;
	for ( i = 0; i < 3; i++ ) {
		TRY( _compute_sections_of_edge (
			     f,
			     _h5t_build_edge_id ( map[face][i], eid ),
			     children ) );
	}
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	h5_id_t *kid = children->items;
	for ( i = 0; i < children->num_items; i++, kid++ ) {
		h5_id_t vids[2];
		TRY ( h5t_get_local_vids_of_edge ( f, *kid, vids ) );
		TRY ( _h5_search_idlist ( f, *list, vids[0] ) );
		TRY ( _h5_search_idlist ( f, *list, vids[1] ) );
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
	const h5_id_t eid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );

	h5_id_t i;
	for ( i = 0; i < 6; i++ ) {
		TRY( _compute_sections_of_edge (
			     f,
			     _h5t_build_edge_id ( i, eid ),
			     children ) );
	}
	TRY ( _h5_alloc_idlist ( f, list, 8 ) );
	h5_id_t *kid = children->items;
	for ( i = 0; i < children->num_items; i++, kid++ ) {
		h5_id_t vids[2];
		TRY ( h5t_get_local_vids_of_edge ( f, *kid, vids ) );
		TRY ( _h5_search_idlist ( f, *list, vids[0] ) );
		TRY ( _h5_search_idlist ( f, *list, vids[1] ) );
	}
	TRY ( _h5_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

h5_err_t
h5t_get_edges_downadjacent_to_triangle (
	h5_file_t * const f,
	const h5_id_t did,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );

	int map[4][3] = { {1,4,5}, {2,3,5}, {0,3,4}, {0,1,2} };
	h5_id_t face_id = _h5t_get_face_id ( did );
	h5_id_t eid = _h5t_get_elem_idx ( did );

	h5_id_t i;
	for ( i = 0; i < 3; i++ ) {
		TRY( _compute_sections_of_edge (
			     f,
			     _h5t_build_edge_id ( map[face_id][i], eid ),
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
	const h5_id_t eid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );

	h5_id_t i;
	for ( i = 0; i < 6; i++ ) {
		TRY( _compute_sections_of_edge (
			     f,
			     _h5t_build_edge_id ( i, eid ),
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
	const h5_id_t eid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( _h5_alloc_idlist ( f, &children, 8 ) );

	h5_id_t i;
	for ( i = 0; i < 4; i++ ) {
		TRY( _compute_sections_of_triangle (
			     f,
			     _h5t_build_edge_id ( i, eid ),
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

h5_err_t
_h5t_release_adjacencies (
	h5_file_t * const f
	) {
	/* TO BE WRITTEN @@@ */
	return H5_SUCCESS;
}
