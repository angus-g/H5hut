#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

/*!
  Compare to vertices given by their 3-dimensional coordinates
*/
static int
_cmp_vertices (
	h5_float64_t P0[3],
	h5_float64_t P1[3]
	) {
	int i;
	for ( i = 0; i < 3; i++ ) {
		int diff = _h5_fcmp ( P0[i], P1[i], 10 );
		if ( diff < 0 ) 	return -1;
		else if ( diff > 0 )	return 1;
	}
	return 0;
}

static int
_qsort_cmp_vertices (
	void * _f,
	const void* _local_vid1,
	const void* _local_vid2
	) {
	h5_file_t *f = (h5_file_t*)_f;
	h5_id_t local_vid1 = *(h5_id_t*)_local_vid1;
	h5_id_t local_vid2 = *(h5_id_t*)_local_vid2;

	return _cmp_vertices (
		f->t->vertices[local_vid1].P,
		f->t->vertices[local_vid2].P );
}


/*!
  We assume that the number of vertices is already known.
*/
h5_err_t
_h5t_sort_vertices (
	h5_file_t * const f
	) {

	struct h5t_fdata *t = f->t;
	h5_id_t num_vertices = t->num_vertices[t->num_levels-1];

	h5_id_t i;
	for ( i = 0; i < num_vertices; i++ ) {
		t->sorted_lvertices.items[i] = i;
	}
	t->sorted_lvertices.num_items = num_vertices;

	_h5_qsort_r (
		t->sorted_lvertices.items,
		num_vertices,
		sizeof(t->sorted_lvertices.items[0]),
		f,
		_qsort_cmp_vertices );

	return H5_SUCCESS;
}


static h5_id_t
_search_vertex (
	h5_file_t * const f,
	h5_float64_t P0[3]
	) {
	struct h5t_fdata *t = f->t;

	register h5_id_t low = 0;
	register h5_id_t high = t->sorted_lvertices.num_items - 1;
	while (low <= high) {
		register int mid = (low + high) / 2;

		h5_id_t local_vid = t->sorted_lvertices.items[mid];
		h5_float64_t *P1 = t->vertices[local_vid].P;
		int diff = _cmp_vertices ( P0, P1 );
           	if ( diff < 0 )
               		high = mid - 1;
           	else if ( diff > 0 )
               		low = mid + 1;
           	else
               		return mid; // found
       	}
       	return -(low+1);  // not found
}

h5_id_t
_h5t_get_local_vid (
	h5_file_t * const f,
	h5_float64_t P[3]
	) {
	struct h5t_fdata *t = f->t;

	h5_id_t local_vid;
	TRY2( local_vid = _search_vertex ( f, P ), fail );
	return t->sorted_lvertices.items[local_vid];
fail:
	return H5_ERR;
}

/*!
  Returns the local vertex id of the i-th vertex of an element. For triangles
  i is in [0,1,2], for tetraheda i is in [0,1,2,3].
*/

static h5_id_t 
_get_local_vid_of_elem (
	h5_file_t * f,
	int  ith_vertex,
	h5_id_t local_eid
	) {
	struct h5t_fdata *t = f->t;

	h5_id_t local_vid = -1;
	h5_id_t global_vid = -1;

	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON: {
		local_vid = t->elems_ldta.tets[local_eid].vids[ith_vertex];
		if ( local_vid == -1 ) {
			global_vid =
				t->elems.tets[local_eid].vids[ith_vertex];
			local_vid = _h5_search_idmap (
				&t->map_vertex_g2l, global_vid );
			t->elems_ldta.tets[local_eid].vids[ith_vertex] =
				local_vid;
		}
		break;
	}
	case H5_OID_TRIANGLE: {
		local_vid = t->elems_ldta.tris[local_eid].vids[ith_vertex];
		if ( local_vid == -1 ) {
			global_vid =
				t->elems.tris[local_eid].vids[ith_vertex];
			local_vid = _h5_search_idmap (
				&t->map_vertex_g2l, global_vid );
			t->elems_ldta.tris[local_eid].vids[ith_vertex] =
				local_vid;
		}
		break;
	}
	default:
		return  h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
	return local_vid;
}

/*!
  Return the coordinates of the i-th vertex of the element given by its local 
  element id. For triangles \c i is in \c [0,1,2], for tetraheda \c i is in
  \c [0,1,2,3].
*/
static h5_float64_t*
_get_vertex_of_elem (
	h5_file_t * f,
	int  ith_vertex,
	h5_id_t local_eid
	) {
	struct h5t_fdata *t = f->t;

	h5_id_t local_vid = _get_local_vid_of_elem (
		f, ith_vertex, local_eid );
	if ( local_vid == -1 ) 
		return NULL;
	return t->vertices[local_vid].P;
}



/*!
  Compare two elems given by their local vertex ids
*/
static int
_vcmp_elems (
	h5_file_t *f,
	h5_id_t *e1,
	h5_id_t *e2
	) {
	struct h5t_fdata *t = f->t;
	int i;

	for ( i = 0; i < t->mesh_type; i++ ) {
		int r = _cmp_vertices (
			t->vertices[e1[i]].P,
			t->vertices[e2[i]].P );
		if ( r < 0 )		return -1;
		else if ( r > 0 ) 	return 1;
	}
	return 0;
}

/*!
  compare two elems given by their local id
*/
static int
_cmp_elems (
	h5_file_t * f,
	h5_id_t	local_eid1,
	h5_id_t local_eid2
	) {
	struct h5t_fdata *t = f->t;
	int i;
	for ( i = 0; i < t->mesh_type; i++ ) {
		int r = _cmp_vertices (
			_get_vertex_of_elem ( f, i, local_eid1 ),
			_get_vertex_of_elem ( f, i, local_eid2 ) );
		if ( r < 0 )		return -1;
		else if ( r > 0 ) 	return 1;
	}
	return 0;
}

static int
_cmp_elems1 (
	h5_file_t * f,
	h5_id_t	local_eid1,
	h5_id_t local_eid2
	) {
	struct h5t_fdata *t = f->t;
	int imap[] = { 1, 0, 2, 3 };
	int i;
	for ( i = 0; i < t->mesh_type; i++ ) {
		int r = _cmp_vertices (
			_get_vertex_of_elem ( f, imap[i], local_eid1 ),
			_get_vertex_of_elem ( f, imap[i], local_eid2 ) );
		if ( r < 0 )		return -1;
		else if ( r > 0 ) 	return 1;
	}
	return 0;
}


static int
_qsort_cmp_elems0 (
	void *_f,
	const void* _local_eid1,
	const void* _local_eid2
	) {
	h5_file_t *f = (h5_file_t*)_f;
	h5_id_t local_eid1 = *(h5_id_t*)_local_eid1;
	h5_id_t local_eid2 = *(h5_id_t*)_local_eid2;
	return _cmp_elems ( f, local_eid1, local_eid2 );
}

static int
_qsort_cmp_elems1 (
	void *_f,
	const void* _local_eid1,
	const void* _local_eid2
	) {
	h5_file_t *f = (h5_file_t*)_f;
	h5_id_t local_eid1 = *(h5_id_t*)_local_eid1;
	h5_id_t local_eid2 = *(h5_id_t*)_local_eid2;
	return _cmp_elems1 ( f, local_eid1, local_eid2 );
}

/*!
  Sort elems geometrically.
*/
h5_err_t
_h5t_sort_elems (
	h5_file_t *f
	) {

	struct h5t_fdata *t = f->t;
	h5_size_t num_elems;
	TRY( num_elems = h5t_get_num_elems_total ( f, f->myproc, -1 ) );

	int k;
	h5_id_t i;
	for ( k = 0; k < 2; k++ ) {
		TRY( _h5_alloc_smap ( f, &t->sorted_elems_ldta[k], num_elems ) );
		for ( i = 0; i < num_elems; i++ ) {
			t->sorted_elems_ldta[k].items[i] = i;
		}
		t->sorted_elems_ldta[k].num_items = num_elems;
	}

	_h5_qsort_r (
		t->sorted_elems_ldta[0].items,
		num_elems,
		sizeof(t->sorted_elems_ldta[0].items[0]),
		f,    
		_qsort_cmp_elems0 );
	_h5_qsort_r (
		t->sorted_elems_ldta[1].items,
		num_elems,
		sizeof(t->sorted_elems_ldta[1].items[0]),
		f,
		_qsort_cmp_elems1 );

	return H5_SUCCESS;
}

/*!
  Sort (small) array of local vertex ids geometrically. 
 */
h5_err_t
_h5t_sort_local_vids (
	h5_file_t * const f,
	h5_id_t * const local_vids,		/* IN/OUT: local vertex ids */	
	const h5_size_t size			/* size of array */
	) {
	struct h5t_fdata *t = f->t;

	h5_size_t i;
	for ( i = 1; i < size; ++i ) {
		h5_id_t local_vid = local_vids[i];

		h5_id_t j = i;
		while ( (j >= 1 ) && _cmp_vertices (
				t->vertices[local_vid].P,
				t->vertices[local_vids[j-1]].P 
				) < 0 ) {
			local_vids[j] = local_vids[j-1];
			--j;
		}
		local_vids[j] = local_vid;
	}
	return H5_SUCCESS;
}

h5_err_t
_h5t_sort_global_vids (
	h5_file_t * const f,
	h5_id_t * const global_vids,		/* IN/OUT: global vertex ids */	
	const h5_size_t size			/* size of array */
	) {

	h5_id_t local_vids[H5_MAX_VERTICES_PER_ELEM];
	const h5_id_t *global_vid = global_vids;
	h5_id_t *local_vid = local_vids;

	h5_id_t i;
	for ( i = 0; i < size; i++, local_vid++, global_vid++ ) {
		TRY(
			*local_vid = h5t_map_global_vid2local(
				f, *global_vid )
			);
	}
	TRY( _h5t_sort_local_vids ( f, local_vids, size ) );
	for ( i = 0; i < size; i++ ) {
		global_vids[i] = h5t_map_local_vid2global (
			f, local_vids[i] );
	}
	return H5_SUCCESS;
}


/*!
  Binary search an element given by its local vertex ids.

  \result	index in t->map_elem_s2l[0].items
 */
static h5_id_t
_search_elem (
	h5_file_t *f,
	h5_id_t * const local_vids	/* local vertex ids */
	) {
	struct h5t_fdata *t = f->t;

	_h5t_sort_local_vids ( f, local_vids, t->mesh_type );

	register h5_id_t low = 0;
	register h5_id_t high = t->sorted_elems_ldta[0].num_items - 1;
	register h5_id_t *elem1 = local_vids;
	while (low <= high) {
		register int mid = (low + high) / 2;

		h5_id_t local_eid = t->sorted_elems_ldta[0].items[mid];
		h5_id_t *elem2 = t->elems_ldta.tets[local_eid].vids;
		int diff = _vcmp_elems ( f, elem1, elem2 );
           	if ( diff < 0 )
               		high = mid - 1;
           	else if ( diff > 0 )
               		low = mid + 1;
           	else
               		return mid; // found
       	}
       	return -(low+1);  // not found
}

 /*!
   Get local id of elem given by local vertex id's
 */
h5_id_t
h5t_get_local_eid (
	h5_file_t *f,
	h5_id_t * const local_vids	/* IN/OUT: local vertex id's */
	) {
	struct h5t_fdata *t = f->t;

	h5_id_t local_eid;
	TRY2( local_eid = _search_elem ( f, local_vids ), fail );
	return t->sorted_elems_ldta[0].items[local_eid];
fail:
	return _h5t_handle_get_local_eid_err ( f, local_vids );
}

/*!
  Map a global vertex id to corresponding local vertex id.
*/
h5_id_t
h5t_map_global_vid2local (
	h5_file_t *f,
	const h5_id_t global_id
	) {
	struct h5t_fdata *t = f->t;

	h5_id_t local_id = _h5_search_idmap ( &t->map_vertex_g2l, global_id );
	if ( local_id < 0 ) 
		return _h5t_error_global_id_nexist ( f, "vertex", global_id );
	return local_id;
}

h5_id_t
h5t_map_local_vid2global (
	h5_file_t *f,
	const h5_id_t local_vid
	) {
	struct h5t_fdata *t = f->t;
	
	if ( local_vid < 0 || local_vid > t->num_vertices[t->num_levels-1] )
		return HANDLE_H5_OUT_OF_RANGE_ERR ( f, "vertex", local_vid );
	return t->vertices[local_vid].id;
}

h5_err_t
h5t_map_global_vids2local (
	h5_file_t *f,
	const h5_id_t * const global_vids,
	const h5_id_t size,
	h5_id_t * const local_vids
	) {
	h5_id_t i;

	for ( i = 0; i < size; i++ ) {
		local_vids[i] = h5t_map_global_vid2local (
			f, global_vids[i] );
		if ( local_vids[i] < 0 ) 
			return _h5t_error_global_id_nexist (
				f,
				"vertex", global_vids[i] );
	}
	return H5_SUCCESS;
}

/*!
  Map a local elem id to corresponding global id.
*/
h5_id_t
h5t_map_local_eid2global (
	h5_file_t *f,
	const h5_id_t local_eid
	) {
	struct h5t_fdata *t = f->t;

	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON:
		if ( local_eid < 0 || local_eid > t->num_elems[t->num_levels-1] )
			return HANDLE_H5_OUT_OF_RANGE_ERR ( f, "tet", local_eid );
		return t->elems.tets[local_eid].id;
	case H5_OID_TRIANGLE:
		if ( local_eid < 0 || local_eid > t->num_elems[t->num_levels-1] )
			return HANDLE_H5_OUT_OF_RANGE_ERR (
				f, "triangle", local_eid );
		return t->elems.tris[local_eid].id;
	default:
		return  h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
}

/*!
  Get global id of elem given by global vertex id's
*/
h5_id_t
h5t_get_global_eid (
	h5_file_t *f,
	const h5_id_t * const global_vids	/* global vertex id's */
	) {
	struct h5t_fdata *t = f->t;
	h5_id_t local_vids[H5_MAX_VERTICES_PER_ELEM];

	if ( t->vertices == NULL ) {
		TRY( _h5t_read_mesh ( f ) );
	}
	TRY2( h5t_map_global_vids2local (
		f,
		global_vids,
		t->mesh_type,
		local_vids ), fail );
	h5_id_t local_eid;
	TRY2( local_eid = h5t_get_local_eid ( f, local_vids ), fail );
	return h5t_map_local_eid2global ( f, local_eid );
fail:
	return _h5t_handle_get_global_eid_err ( f, global_vids );
}

static int
_search_ith_vertex_in_elem (
	h5_file_t * const f,
	const int i,
	h5_id_t	local_vid
	) {
	struct h5t_fdata *t = f->t;

	register h5_id_t low = 0;
	register h5_id_t high = t->sorted_elems_ldta[i].num_items - 1;
	register h5_float64_t *vertex1 = t->vertices[local_vid].P;

	while (low <= high) {
		register int mid = (low + high) / 2;

		h5_id_t local_eid = t->sorted_elems_ldta[i].items[mid];
		h5_id_t local_vid2 = t->elems_ldta.tets[local_eid].vids[i];
		h5_float64_t *vertex2 = t->vertices[local_vid2].P;
		int diff = _cmp_vertices ( vertex1, vertex2 );

           	if ( diff < 0 )
               		high = mid - 1;
           	else if ( diff > 0 )
               		low = mid + 1;
           	else
               		return mid; // found
       	}
       	return -(low+1);  // not found
}

static h5_id_t
_tetm_contain_triangle (
	h5_file_t *f,
	const h5_id_t * const local_vids,
	int i,
	h5_id_t local_eid
	) {
	struct h5t_fdata *t = f->t;

	h5_id_t *local_vids_of_elem = t->elems_ldta.tets[local_eid].vids;
	if ( i == 0 && 
	     local_vids[1] == local_vids_of_elem[1] && 
	     local_vids[2] == local_vids_of_elem[2]
		) return 0;
	else if ( i == 0 && 
	     local_vids[1] == local_vids_of_elem[1] && 
	     local_vids[2] == local_vids_of_elem[3]
		) return 1;
	else if ( i == 0 && 
	     local_vids[1] == local_vids_of_elem[2] && 
	     local_vids[2] == local_vids_of_elem[3]
		) return 2;
	else if ( i == 1 && 
	     local_vids[1] == local_vids_of_elem[2] && 
	     local_vids[2] == local_vids_of_elem[3]
		) return 3;
	return -1;
}

/*!
  Search for triangle given by local vertex id in tetrahdral mesh.

  \return unique local triangle id
 */
h5_id_t
_tetm_search_triangle (
	h5_file_t *f,
	h5_id_t * const local_vids
	) {
	struct h5t_fdata *t = f->t;

	_h5t_sort_local_vids ( f, local_vids, 3 );

	/*
	  search for vid(tri,0) in the tuple of all 0th vertices of all tets
	  if there is one:
		take the smallest tet with vid(tri,0) == vid(tet,0)
		loop over all tets with vid(tri,0) == vid(tet,0)
		until we find a tet the triangle is belonging to.
	  else
		search for vid(tri,0) in the tuple of all 1st vertices of
		all tets
		if there is one:
			take the smallest tet with
			vid(tri,0) == vid(tet,0)
			loop over all tets with
			vid(tri,0) == vid(tet,0)
			until we find a tet the triangle is belonging to.
	*/

	int i;
	h5_id_t idx[2];
	h5_id_t local_eid[2];
	h5_id_t tidx[2];
	for ( i = 0; i < 2; i++ ) {
		idx[i] = _search_ith_vertex_in_elem ( f, i, local_vids[0] );

		while ( idx[i] > 0 &&		/* get leftmost */
			local_vids[0] == _get_local_vid_of_elem (
				f, i, t->sorted_elems_ldta[0].items[idx[i]-1] ) )
			idx[i]--;

		do {
			/* check whether triangle is in elem given by local id */
			local_eid[i] = t->sorted_elems_ldta[i].items[idx[i]];
			tidx[i] = _tetm_contain_triangle (
				f, local_vids, i, local_eid[i] );
			if ( tidx[i] >= 0 ) break;
			idx[i]++;
		} while ( local_vids[0] == _get_local_vid_of_elem (
				  f, i, local_eid[i] ) );
	}
	if ( idx[0] < 0 && idx[1] < 0 ) return -1;
	if ( idx[0] < 0 ) return _h5t_build_triangle_id ( tidx[1], local_eid[1] );
	if ( idx[1] < 0 ) return _h5t_build_triangle_id ( tidx[0], local_eid[0] );

	if ( _cmp_elems( f, local_eid[0], local_eid[1] ) < 0 )
		return _h5t_build_triangle_id ( tidx[0], local_eid[0] );
	else
		return _h5t_build_triangle_id ( tidx[1], local_eid[1] );

	return -1;
}

/*!
  
 */
h5_id_t
h5t_get_global_triangle_id  (
	h5_file_t * const f,
	h5_id_t * const global_vids
	) {
	struct h5t_fdata *t = f->t;

	if ( t->vertices == NULL ) {
		h5_err_t h5err = _h5t_read_mesh ( f );
		if ( h5err < 0 ) return h5err;
	}

	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON: {
		h5_id_t local_vids[3];
		h5_err_t h5err = h5t_map_global_vids2local (
			f, global_vids, 3, local_vids );
		if ( h5err < 0 ) return h5err;
		h5_id_t local_tid = h5t_get_local_triangle_id ( f, local_vids );
		if ( local_tid < 0 )
			return _h5t_error_global_triangle_id_nexist (
				f, global_vids );
		return h5t_map_local_triangle_id2global ( f, local_tid );
	}
	case H5_OID_TRIANGLE:
		return h5t_get_global_eid ( f, global_vids );
	default:
		return  h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
}

/*!
  
 */
h5_id_t
h5t_get_local_triangle_id  (
	h5_file_t * const f,
	h5_id_t * const local_vids
	) {
	struct h5t_fdata *t = f->t;
	
	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON: {
		h5_err_t h5err = _h5t_read_mesh ( f );
		if ( h5err < 0 ) return h5err;
		h5_id_t local_tid = _tetm_search_triangle ( f, local_vids );
		if ( local_tid == -1 ) {
			return _h5t_error_local_triangle_id_nexist( f, local_vids );
		}
		return local_tid;
	}
	case H5_OID_TRIANGLE:
		return h5t_get_local_eid ( f, local_vids );
	default:
		return  h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
}

h5_id_t
h5t_map_global_eid2local (
	h5_file_t * const f,
	const h5_id_t global_eid
	) {
	struct h5t_fdata *t = f->t;
	h5_id_t local_eid = _h5_search_idmap ( &t->map_elem_g2l, global_eid );
	if ( local_eid < 0 ) 
		return _h5t_error_global_id_nexist ( f, "elem", global_eid );
	return local_eid;
}

/*!
  Map global triangle id to local id. 

  \return local id of triangle
 */
h5_id_t
h5t_map_global_triangle_id2local (
	h5_file_t * const f,
	const h5_id_t global_tri_id
	) {
	struct h5t_fdata *t = f->t;
	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON: {
		h5_id_t global_tet_id = global_tri_id & H5_TET_MASK;
		h5_id_t local_tet_id = h5t_map_global_eid2local (
			f, global_tet_id );
		if ( local_tet_id < 0 ) 
			return _h5t_error_global_id_nexist (
				f, "triangle", global_tri_id );
		return local_tet_id | (global_tri_id & ~H5_TET_MASK);
	}
	case H5_OID_TRIANGLE:
		return h5t_map_global_eid2local ( f, global_tri_id );
	default:
		return  h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
}

/*!
  Map local triangle id to global id.

  \return global id of triangle 
 */
h5_id_t
h5t_map_local_triangle_id2global (
	h5_file_t * const f,
	const h5_id_t local_tri_id
	) {
	struct h5t_fdata *t = f->t;
	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON: {
		h5_id_t local_tet_id = local_tri_id & H5_TET_MASK;
		h5_id_t global_tet_id = h5t_map_local_eid2global (
			f, local_tet_id );
		if ( global_tet_id < 0 )
			return HANDLE_H5_OUT_OF_RANGE_ERR(
				f, "triangle", local_tri_id );
		return global_tet_id | (local_tri_id & ~H5_TET_MASK); 
	}
	case H5_OID_TRIANGLE:
		return h5t_map_local_eid2global ( f, local_tri_id );
	default:
		return  h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
}
