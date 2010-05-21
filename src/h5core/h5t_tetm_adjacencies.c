/*
  Copyright 2006-2010
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Achim Gsell
  
  Warning
	This code is under development.
*/

#include <time.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*
  compute T(V) from current level up to highest levels.
*/
static h5_err_t
compute_tets_of_vertices (
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
			TRY ( h5priv_append_to_idlist (
				      f,
				      &t->vertices_data[vid].tv,
				      h5tpriv_build_vertex_id( i, eid ) ) );
		}
	}
	return H5_SUCCESS;
}

/*
  Compute T(E) from current level up to highest levels.
 */
static h5_err_t
compute_tets_of_edges (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t eid = (t->cur_level <= 0 ) ? 0 : t->num_elems[t->cur_level-1];
	h5_elem_ldta_t *tet = tet = &t->elems_ldta[eid];
	h5_id_t num_elems = t->num_elems[t->num_levels-1];
	h5_idlist_t *retval = NULL;
	TRY ( h5tpriv_resize_te_htab ( f, 4*(num_elems-eid) ) );
	for ( ; eid < num_elems; eid++, tet++ ) {
		h5_id_t face;
		for ( face = 0; face < 6; face++ ) {
			TRY ( h5tpriv_search_te2 ( f, face, eid, &retval ) );
		}
	}
	return H5_SUCCESS;
}

static h5_err_t
compute_tets_of_triangles (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t eid = (t->cur_level <= 0 ) ? 0 : t->num_elems[t->cur_level-1];
	h5_elem_ldta_t *tet = tet = &t->elems_ldta[eid];
	h5_id_t num_elems = t->num_elems[t->num_levels-1];
	h5_idlist_t *retval = NULL;
	TRY ( h5tpriv_resize_td_htab ( f, 4*(num_elems-eid) ) );
	for ( ; eid < num_elems; eid++, tet++ ) {
		h5_id_t face;
		for ( face = 0; face < 4; face++ ) {
			TRY ( h5tpriv_search_td2 ( f, face, eid, &retval ) );
		}
	}
	return H5_SUCCESS;
}

static h5_err_t
rebuild_internal_structs (
	h5_file_t * const f
	) {
	clock_t t1 = clock();
	TRY ( compute_tets_of_vertices ( f ) );
	clock_t t2 = clock();
	fprintf ( stderr, "compute_tets_of_vertices(): %f\n",
		  (float)(t2-t1)/CLOCKS_PER_SEC );
	t1 = clock();
	TRY ( compute_tets_of_edges ( f ) );
	t2 = clock();
	fprintf ( stderr, "compute_tets_of_edge(): %f\n",
		  (float)(t2-t1)/CLOCKS_PER_SEC );
	t1 = clock();
	TRY ( compute_tets_of_triangles ( f ) );
	t2 = clock();
	fprintf ( stderr, "compute_tets_of_triangle(): %f\n",
		  (float)(t2-t1)/CLOCKS_PER_SEC );

	return H5_SUCCESS;
}

static h5_err_t
compute_children_of_edge (
	h5_file_t * const f,
	h5_id_t kid,
	h5_idlist_t *children
	) {
	h5t_fdata_t *t = f->t;
	h5_idlist_t *te;

	TRY ( h5tpriv_find_te2 (
		      f,
		      h5tpriv_get_face_idx ( kid ),
		      h5tpriv_get_elem_idx ( kid ),
		      &te ) 
		);
	h5_id_t *edge = te->items;
	h5_id_t *end = te->items+te->num_items;
	for ( ; edge < end; edge++ ) {
		h5_id_t elem_idx = h5tpriv_get_elem_idx ( *edge );
		h5_id_t face_idx =  h5tpriv_get_face_idx ( *edge );
		h5_elem_ldta_t *tet = &t->elems_ldta[elem_idx];
		if ( h5tpriv_elem_is_on_cur_level ( f, tet ) == H5_OK ) {
			TRY ( h5priv_append_to_idlist (
				      f, children, *edge )
				);
		} else {
			h5_id_t kids[2];
			TRY ( t->methods.store->get_direct_children_of_edge (
				      f,
				      face_idx,
				      tet->local_child_eid,
				      kids ) );
			TRY ( compute_children_of_edge (
				      f, kids[0], children ) );
			TRY ( compute_children_of_edge (
				      f, kids[1], children ) );
		}
	}
	return H5_SUCCESS;
}

/* 
   Compute all sections of an edge.
*/
static h5_err_t
compute_sections_of_edge (
	h5_file_t * const f,
	h5_id_t kid,
	h5_idlist_t *children
	) {
	h5t_fdata_t *t = f->t;
	h5_idlist_t *te;

	TRY ( h5tpriv_find_te2 (
		      f,
		      h5tpriv_get_face_idx ( kid ),
		      h5tpriv_get_elem_idx ( kid ),
		      &te )
		);
	h5_id_t *edge = te->items;
	h5_id_t *end = te->items+te->num_items;
	int refined = 0;
	for ( ; edge < end; edge++ ) {
		h5_id_t eid = h5tpriv_get_elem_idx ( *edge );
		h5_id_t face_id =  h5tpriv_get_face_idx ( *edge );
		h5_elem_ldta_t *tet = &t->elems_ldta[eid];
		if ( ! h5tpriv_elem_is_on_cur_level ( f, tet ) == H5_OK ) {
			refined = 1;
			h5_id_t kids[2];
			TRY ( t->methods.store->get_direct_children_of_edge (
				      f,
				      face_id,
				      tet->local_child_eid,
				      kids ) );
			TRY ( compute_sections_of_edge (
				      f, kids[0], children ) );
			TRY ( compute_sections_of_edge (
				      f, kids[1], children ) );
		}
	}
	if ( ! refined ) {
		TRY ( h5priv_append_to_idlist ( f, children, te->items[0] ) );
	}
	return H5_SUCCESS;
}

/*
  Compute direct children of a triangle.

  Face 0	      Face 1		  Face 2	      Face 3	    
  1		      1	      	          2		      1	    
   +		       +	      	   +		       +	    
   |\		       |\	      	   |\		       |\	    
   | \		       | \	      	   | \		       | \	    
   |  \		       |  \	      	   |  \		       |  \	    
   |   \	       |   \	      	   |   \	       |   \	    
  4+----+7	      4+----+8     	  5+----+9	      7+----+8     
   |\   |\	       |\   |\     	   |\   |\	       |\   |\     
   | \  | \	       | \  | \    	   | \  | \	       | \  | \    
   |  \ |  \	       |  \ |  \   	   |  \ |  \	       |  \ |  \   
   |   \|   \	       |   \|   \  	   |   \|   \	       |   \|   \  
   +----+----+	       +----+----+ 	   +----+----+	       +----+----+ 
  0     5     2	      0     6     3	  0     6     3	      2     9     3

  z ^                 z ^                 y ^                 z ^
    |                   |                   |                   |
    +-->                +-->                +-->                +-->
       y                    x                  x                   x

  Triangle: face idx, #child

  [0,4,5]: 0, 0       [0,4,6]: 1, 0       [0,5,6]: 2, 0       [1,7,8]: 3, 1
  [1,4,7]: 0, 1       [1,4,8]: 1, 1       [2,5,9]: 2, 2       [2,7,9]: 3, 2
  [2,5,7]: 0, 2       [3,6,8]: 1, 3       [3,6,9]: 2, 3       [3,8,9]: 3, 3
  [4,5,7]: 0, 5       [4,6,8]: 2, 4       [5,6,9]: 1, 7       [7,8,9]: 3, 6

*/
static h5_err_t
compute_direct_children_of_triangle (
	h5_file_t * const f,
	h5_id_t face_idx,
	h5_id_t elem_idx,
	h5_id_t	children[4]
	) {
	h5_id_t map[4][4][2] = {
		{{0,0},{0,1},{0,2},{0,5}},
		{{1,0},{1,1},{1,3},{2,4}},
		{{2,0},{2,2},{2,3},{1,7}},
		{{3,1},{3,2},{3,3},{3,6}}
	};
	int num_faces = f->t->ref_element->num_faces[2];
	if ( (face_idx < 0) || (face_idx >= num_faces) ) {
		return  h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
	children[0] = h5tpriv_build_edge_id (
		map[face_idx][0][0], elem_idx+map[face_idx][0][1]);
	children[1] = h5tpriv_build_edge_id (
		map[face_idx][1][0], elem_idx+map[face_idx][1][1]);
	children[2] = h5tpriv_build_edge_id (
		map[face_idx][2][0], elem_idx+map[face_idx][2][1]);
	children[3] = h5tpriv_build_edge_id (
		map[face_idx][3][0], elem_idx+map[face_idx][3][1]);
	return H5_SUCCESS;
}

static h5_err_t
compute_children_of_triangle (
	h5_file_t * const f,
	h5_id_t did,
	h5_idlist_t *children 
	) {

	h5t_fdata_t *t = f->t;
	h5_idlist_t *td;

	TRY ( h5tpriv_find_td2 (
		      f,
		      h5tpriv_get_face_idx ( did ),
		      h5tpriv_get_elem_idx ( did ),
		      &td
		      ) );
	h5_id_t *tri = td->items;
	h5_id_t *end = td->items+td->num_items;
	for ( ; tri < end; tri++ ) {
		h5_id_t eid = h5tpriv_get_elem_idx ( *tri );
		h5_id_t face_idx =  h5tpriv_get_face_idx ( *tri );
		h5_elem_ldta_t *tet = &t->elems_ldta[eid];
		if ( h5tpriv_elem_is_on_cur_level ( f, tet ) == H5_OK ) {
			TRY ( h5priv_append_to_idlist (
				      f, children, *tri )
				);
		} else {
			h5_id_t dids[4];
			TRY ( compute_direct_children_of_triangle (
				      f,
				      face_idx,
				      tet->local_child_eid,
				      dids ) );
			TRY ( compute_children_of_triangle (
				      f, dids[0], children ) );
			TRY ( compute_children_of_triangle (
				      f, dids[1], children ) );
			TRY ( compute_children_of_triangle (
				      f, dids[2], children ) );
			TRY ( compute_children_of_triangle (
				      f, dids[3], children ) );
		}
	}
	return H5_SUCCESS;
}

static h5_err_t
compute_sections_of_triangle (
	h5_file_t * const f,
	h5_id_t did,
	h5_idlist_t *children
	) {
	h5t_fdata_t *t = f->t;
	h5_idlist_t *td;

	TRY ( h5tpriv_find_td2 (
		      f,
		      h5tpriv_get_face_idx ( did ),
		      h5tpriv_get_elem_idx ( did ), &td ) );
	h5_id_t *tri = td->items;
	h5_id_t *end = td->items+td->num_items;
	int refined = 0;
	for ( ; tri < end; tri++ ) {
		h5_id_t eid = h5tpriv_get_elem_idx ( *tri );
		h5_id_t face_idx =  h5tpriv_get_face_idx ( *tri );
		h5_elem_ldta_t *tet = &t->elems_ldta[eid];
		if ( ! h5tpriv_elem_is_on_cur_level ( f, tet ) == H5_OK ) {
			refined = 1;
			h5_id_t dids[4];
			TRY ( compute_direct_children_of_triangle (
				      f,
				      face_idx,
				      tet->local_child_eid,
				      dids ) );
			TRY ( compute_sections_of_triangle (
				      f, dids[0], children ) );
			TRY ( compute_sections_of_triangle (
				      f, dids[1], children ) );
			TRY ( compute_sections_of_triangle (
				      f, dids[2], children ) );
			TRY ( compute_sections_of_triangle (
				      f, dids[3], children ) );

		}
	}
	if ( ! refined ) {
		TRY ( h5priv_append_to_idlist ( f, children, td->items[0] ) );
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
	h5_id_t face_idx,
	h5_id_t eid
	) {
	h5_idlist_t *te;
	TRY ( h5tpriv_find_te2 ( f, face_idx, eid, &te ) );
	TRY ( h5priv_search_idlist ( f, list, te->items[0] ) );
	return H5_SUCCESS;
}


static h5_err_t
get_edges_upadjacent_to_vertex (
	h5_file_t * const f,
	const h5_id_t vid,
	h5_idlist_t **list
	) {
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	
	h5t_fdata_t *t = f->t;
	h5_idlist_t *tv = &t->vertices_data[vid].tv;
	h5_size_t i;

	h5_id_t *vidp = tv->items;
	for ( i = 0; i < tv->num_items; i++, vidp++ ) {
		h5_id_t eid = h5tpriv_get_elem_idx ( *vidp );
		h5_id_t face = h5tpriv_get_face_idx ( *vidp );
		h5_elem_ldta_t *tet = &t->elems_ldta[eid];

		if ( h5tpriv_elem_is_on_cur_level ( f, tet ) == H5_NOK ) {
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
	h5_idlist_t *td;
	TRY ( h5tpriv_find_td2 ( f, face, eid, &td ) );
	TRY ( h5priv_search_idlist ( f, list, td->items[0] ) );

	return H5_SUCCESS;
}

static h5_err_t
get_triangles_upadjacent_to_vertex (
	h5_file_t * const f,
	const h5_id_t vid,
	h5_idlist_t **list
	) {
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	
	h5t_fdata_t *t = f->t;
	h5_idlist_t *tv = &t->vertices_data[vid].tv;
	h5_size_t i;
	h5_id_t *vidp = tv->items;
	for ( i = 0; i < tv->num_items; i++, vidp++ ) {
		h5_id_t eid = h5tpriv_get_elem_idx ( *vidp );
		h5_id_t face = h5tpriv_get_face_idx ( *vidp );
		h5_elem_ldta_t *tet = &t->elems_ldta[eid];

		if ( h5tpriv_elem_is_on_cur_level ( f, tet ) == H5_NOK ) {
			continue;
		}
		int map[4][3] = { {1,2,3}, {0,2,3}, {0,1,3}, {0,1,2} };
		TRY ( _add_triangle ( f, *list, map[face][0], eid ) );
		TRY ( _add_triangle ( f, *list, map[face][1], eid ) );
		TRY ( _add_triangle ( f, *list, map[face][2], eid ) );
	}
	return H5_SUCCESS;
}

static h5_err_t
get_tets_upadjacent_to_vertex (
	h5_file_t * const f,
	const h5_id_t vid,
	h5_idlist_t **list
	) {
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	
	h5t_fdata_t *t = f->t;
	h5_idlist_t *tv = &t->vertices_data[vid].tv;
	h5_size_t i;
	h5_id_t *vidp = tv->items;
	for ( i = 0; i < tv->num_items; i++, vidp++ ) {
		h5_id_t eid = h5tpriv_get_elem_idx ( *vidp );
		h5_elem_ldta_t *tet = &t->elems_ldta[eid];

		if ( h5tpriv_elem_is_on_cur_level ( f, tet ) == H5_NOK ) {
			continue;
		}
		TRY ( h5priv_search_idlist ( f, *list, eid ) );
	}
	return H5_SUCCESS;
}

static h5_err_t
get_triangles_upadjacent_to_edge (
	h5_file_t * const f,
	const h5_id_t kid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( h5priv_alloc_idlist ( f, &children, 8 ) );
	TRY ( compute_children_of_edge ( f, kid, children ) );
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	h5_id_t *edge = children->items;
	h5_id_t *end = children->items+children->num_items;
	int map[6][2] = { {2,3}, {0,3}, {1,3}, {1,2}, {0,2}, {0,1} };
	for ( ; edge < end; edge++ ) {
		h5_id_t eid = h5tpriv_get_elem_idx ( *edge );
		h5_id_t face_idx = h5tpriv_get_face_idx ( *edge );
		TRY ( _add_triangle ( f, *list, map[face_idx][0], eid ) );
		TRY ( _add_triangle ( f, *list, map[face_idx][1], eid ) );
	}
	TRY ( h5priv_free_idlist( f, &children ) );

	return H5_SUCCESS;
}

static h5_err_t
get_tets_upadjacent_to_edge (
	h5_file_t * const f,
	const h5_id_t kid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( h5priv_alloc_idlist ( f, &children, 8 ) );
	TRY( compute_children_of_edge ( f, kid, children ) );
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	int i;
	h5_id_t *kidp = children->items;
	for ( i = 0; i < children->num_items; i++, kidp++ ) {
		h5_id_t eid = h5tpriv_get_elem_idx ( *kidp );
		TRY ( h5priv_search_idlist ( f, *list, eid ) );
	}
	TRY ( h5priv_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

static h5_err_t
get_tets_upadjacent_to_triangle (
	h5_file_t * const f,
	const h5_id_t did,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( h5priv_alloc_idlist ( f, &children, 8 ) );
	TRY( compute_children_of_triangle ( f, did, children ) );
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	int i;
	h5_id_t *didp = children->items;
	for ( i = 0; i < children->num_items; i++ , didp++) {
		h5_id_t eid = h5tpriv_get_elem_idx ( *didp );
		TRY ( h5priv_search_idlist ( f, *list, eid ) );
	}
	TRY ( h5priv_free_idlist( f, &children ) );
	return H5_SUCCESS;
}


static h5_err_t
get_vertices_downadjacent_to_edge (
	h5_file_t * const f,
	const h5_id_t kid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( h5priv_alloc_idlist ( f, &children, 8 ) );
	TRY( compute_sections_of_edge ( f, kid, children ) );
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	int i;
	h5_id_t *kidp = children->items;
	for ( i = 0; i < children->num_items; i++, kidp++ ) {
		h5_id_t vids[2];
		TRY ( h5t_get_vertex_indices_of_edge ( f, *kidp, vids ) );
		TRY ( h5priv_search_idlist ( f, *list, vids[0] ) );
		TRY ( h5priv_search_idlist ( f, *list, vids[1] ) );
	}
	TRY ( h5priv_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

/*
  Compute downward adjacent vertices of all edges of triangle.
 */
static h5_err_t
get_vertices_downadjacent_to_triangle (
	h5_file_t * const f,
	const h5_id_t did,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( h5priv_alloc_idlist ( f, &children, 8 ) );

	int map[4][3] = { {1,4,5}, {2,3,5}, {0,3,4}, {0,1,2} };
	h5_id_t face = h5tpriv_get_face_idx ( did );
	h5_id_t eid = h5tpriv_get_elem_idx ( did );

	h5_id_t i;
	for ( i = 0; i < 3; i++ ) {
		TRY( compute_sections_of_edge (
			     f,
			     h5tpriv_build_edge_id ( map[face][i], eid ),
			     children ) );
	}
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	h5_id_t *kid = children->items;
	for ( i = 0; i < children->num_items; i++, kid++ ) {
		h5_id_t vids[2];
		TRY ( h5t_get_vertex_indices_of_edge ( f, *kid, vids ) );
		TRY ( h5priv_search_idlist ( f, *list, vids[0] ) );
		TRY ( h5priv_search_idlist ( f, *list, vids[1] ) );
	}
	TRY ( h5priv_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

/*
  Compute downward adjacent vertices of all edges of tetrahedron.
 */
static h5_err_t
get_vertices_downadjacent_to_tet (
	h5_file_t * const f,
	const h5_id_t eid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( h5priv_alloc_idlist ( f, &children, 8 ) );

	h5_id_t i;
	for ( i = 0; i < 6; i++ ) {
		TRY( compute_sections_of_edge (
			     f,
			     h5tpriv_build_edge_id ( i, eid ),
			     children ) );
	}
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	h5_id_t *kid = children->items;
	for ( i = 0; i < children->num_items; i++, kid++ ) {
		h5_id_t vids[2];
		TRY ( h5t_get_vertex_indices_of_edge ( f, *kid, vids ) );
		TRY ( h5priv_search_idlist ( f, *list, vids[0] ) );
		TRY ( h5priv_search_idlist ( f, *list, vids[1] ) );
	}
	TRY ( h5priv_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

static h5_err_t
get_edges_downadjacent_to_triangle (
	h5_file_t * const f,
	const h5_id_t did,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( h5priv_alloc_idlist ( f, &children, 8 ) );

	int map[4][3] = { {1,4,5}, {2,3,5}, {0,3,4}, {0,1,2} };
	h5_id_t face_idx = h5tpriv_get_face_idx ( did );
	h5_id_t eid = h5tpriv_get_elem_idx ( did );

	h5_id_t i;
	for ( i = 0; i < 3; i++ ) {
		TRY( compute_sections_of_edge (
			     f,
			     h5tpriv_build_edge_id ( map[face_idx][i], eid ),
			     children ) );
	}
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	h5_id_t *kid = children->items;
	for ( i = 0; i < children->num_items; i++, kid++ ) {
		TRY ( h5priv_search_idlist ( f, *list, *kid ) );
	}
	TRY ( h5priv_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

static h5_err_t
get_edges_downadjacent_to_tet (
	h5_file_t * const f,
	const h5_id_t eid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( h5priv_alloc_idlist ( f, &children, 8 ) );

	h5_id_t i;
	for ( i = 0; i < 6; i++ ) {
		TRY( compute_sections_of_edge (
			     f,
			     h5tpriv_build_edge_id ( i, eid ),
			     children ) );
	}
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	h5_id_t *kid = children->items;
	for ( i = 0; i < children->num_items; i++, kid++ ) {
		TRY ( h5priv_search_idlist ( f, *list, *kid ) );
	}
	TRY ( h5priv_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

static h5_err_t
get_triangles_downadjacent_to_tet (
	h5_file_t * const f,
	const h5_id_t eid,
	h5_idlist_t **list
	) {
	h5_idlist_t *children;
	TRY ( h5priv_alloc_idlist ( f, &children, 8 ) );

	h5_id_t i;
	for ( i = 0; i < 4; i++ ) {
		TRY( compute_sections_of_triangle (
			     f,
			     h5tpriv_build_edge_id ( i, eid ),
			     children ) );
	}
	TRY ( h5priv_alloc_idlist ( f, list, 8 ) );
	h5_id_t *did = children->items;
	for ( i = 0; i < children->num_items; i++, did++ ) {
		TRY ( h5priv_search_idlist ( f, *list, *did ) );
	}
	TRY ( h5priv_free_idlist( f, &children ) );
	return H5_SUCCESS;
}

static h5_err_t
release_internal_structs (
	h5_file_t * const f
	) {
	/* TO BE WRITTEN @@@ */
	return H5_SUCCESS;
}

struct h5t_adjacency_methods h5tpriv_tetm_adjacency_methods = {
	rebuild_internal_structs,
	release_internal_structs,
	get_edges_upadjacent_to_vertex,
	get_triangles_upadjacent_to_vertex,
	get_tets_upadjacent_to_vertex,
	get_triangles_upadjacent_to_edge,
	get_tets_upadjacent_to_edge,
	get_tets_upadjacent_to_triangle,
	get_vertices_downadjacent_to_edge,
	get_vertices_downadjacent_to_triangle,
	get_vertices_downadjacent_to_tet,
	get_edges_downadjacent_to_triangle,
	get_edges_downadjacent_to_tet,
	get_triangles_downadjacent_to_tet
};

