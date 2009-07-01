#include <string.h>
#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

/*
  Mapping of global to local id's:

  Before adding a new level or closing the mesh, we must define global id's
  for the vertices and elements. This we have to do only for the last stored
  level.
*/  


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
		h5_int64_t diff = _h5_fcmp ( P0[i], P1[i], 10 );
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
  Sort vertices. Store local id's in a sorted array for binary search. 
*/
h5_err_t
_h5t_sort_vertices (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	if ( t->num_levels <= 0 ) return H5_SUCCESS;

	h5_id_t local_vid = t->cur_level > 0 ?
		t->num_vertices[t->cur_level-1] : 0;

	h5_id_t num_vertices = t->num_vertices[t->num_levels-1];
	for ( ; local_vid < num_vertices; local_vid++ ) {
		t->sorted_lvertices.items[local_vid] = local_vid;
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


/*!
  Return local vertex id of a vertex given by its coordinates.

  \return	local vertex id if found
  \return	else negativ value
 */
h5_id_t
_h5t_get_local_vid (
	h5_file_t * const f,
	h5_float64_t P[3]
	) {
	h5t_fdata_t *t = f->t;
	register h5_id_t low = 0;
	register h5_id_t high = t->sorted_lvertices.num_items - 1;
	while (low <= high) {
		register int mid = (low + high) / 2;

		h5_id_t local_vid = t->sorted_lvertices.items[mid];
		h5_float64_t *P1 = t->vertices[local_vid].P;
	        int diff = _cmp_vertices ( P, P1 );
           	if ( diff < 0 )
               		high = mid - 1;
           	else if ( diff > 0 )
               		low = mid + 1;
           	else
               		return t->sorted_lvertices.items[mid]; // found
       	}
       	return -(low+1);  // not found
}

/*!
  Return the coordinates of the i-th vertex of the element given by its local 
  element id. For triangles \c i is in \c [0,1,2], for tetraheda \c i is in
  \c [0,1,2,3].

  t->elems_ldta[local_eid].local_vids[i]
*/
#define _get_vertex_of_elem( f, i, local_eid ) \
	(f->t->vertices[ f->t->elems_ldta[local_eid].local_vids[i] ].P)



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
  Sort elements. Store local id's in a sorted array so we can run a
  binary search. 
*/
h5_err_t
_h5t_sort_elems (
	h5_file_t *f
	) {
	h5t_fdata_t *t = f->t;
	if ( t->num_levels <= 0 ) return H5_SUCCESS;
	h5_id_t local_eid = t->cur_level > 0 ? t->num_elems[t->cur_level-1] : 0;
	h5_size_t num_elems = t->num_elems[t->num_levels-1];

	int k;
	h5_id_t i;
	for ( k = 0; k < 2; k++ ) {
		TRY( _h5_alloc_idlist_items ( f, &t->sorted_elems[k], num_elems ) );
		for ( i = local_eid; i < num_elems; i++ ) {
			t->sorted_elems[k].items[i] = i;
		}
		t->sorted_elems[k].num_items = num_elems;
	}

	_h5_qsort_r (
		t->sorted_elems[0].items,
		num_elems,
		sizeof(t->sorted_elems[0].items[0]),
		f,    
		_qsort_cmp_elems0 );
	_h5_qsort_r (
		t->sorted_elems[1].items,
		num_elems,
		sizeof(t->sorted_elems[1].items[0]),
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
	h5t_fdata_t *t = f->t;

	_h5t_sort_local_vids ( f, local_vids, t->mesh_type );

	register h5_id_t low = 0;
	register h5_id_t high = t->sorted_elems[0].num_items - 1;
	register h5_id_t *elem1 = local_vids;
	while (low <= high) {
		register int mid = (low + high) / 2;

		h5_id_t local_eid = t->sorted_elems[0].items[mid];
		h5_id_t *elem2 = t->elems_ldta[local_eid].local_vids;
		int diff = _vcmp_elems ( f, elem1, elem2 );
           	if ( diff < 0 )
               		high = mid - 1;
           	else if ( diff > 0 )
               		low = mid + 1;
           	else
               		return mid; // found
       	}
	return _h5t_error_local_elem_nexist ( f, local_vids );
}

 /*!
   Get local element id given by its local vertex id's.
   
 */
h5_id_t
h5t_get_local_eid (
	h5_file_t *f,
	h5_id_t * const local_vids	/* IN/OUT: local vertex id's */
	) {
	h5t_fdata_t *t = f->t;

	h5_id_t local_eid;
	TRY ( local_eid = _search_elem ( f, local_vids ) );
	return t->sorted_elems[0].items[local_eid];
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
	return t->vertices[local_vid].global_vid;
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
	h5t_fdata_t *t = f->t;

	if ( local_eid < 0 || local_eid > t->num_elems[t->num_levels-1] )
		return HANDLE_H5_OUT_OF_RANGE_ERR (
			f, _h5t_oid_names[t->mesh_type], local_eid );

	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON:
		return t->elems.tets[local_eid].global_eid;
	case H5_OID_TRIANGLE:
		return t->elems.tris[local_eid].global_eid;
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

	TRY ( h5t_map_global_vids2local (
		f,
		global_vids,
		t->mesh_type,
		local_vids ) );
	h5_id_t local_eid;
	TRY ( local_eid = h5t_get_local_eid ( f, local_vids ) );
	return h5t_map_local_eid2global ( f, local_eid );
}

/*!
  
 */
h5_id_t
h5t_get_global_triangle_id  (
	h5_file_t * const f,
	h5_id_t * const global_vids
	) {
	struct h5t_fdata *t = f->t;

	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON: {
		h5_id_t local_vids[3];
		TRY ( h5t_map_global_vids2local (
			      f, global_vids, 3, local_vids ) );
		h5_id_t local_tid = h5t_get_local_triangle_id ( f, local_vids );
		if ( local_tid < 0 )
			return _h5t_error_global_triangle_id_nexist (
				f, global_vids );
		return h5t_map_local_triangle_id2global ( f, local_tid );
	}
	case H5_OID_TRIANGLE:
		return h5t_get_global_eid ( f, global_vids );
	default:
		return h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
}

/*!
  
 */
h5_id_t
h5t_get_local_triangle_id  (
	h5_file_t * const f,
	h5_id_t * const local_vids
	) {
	h5t_fdata_t *t = f->t;

	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON: {
		h5_td_node_t *td;
		TRY ( _h5t_find_td (
			      f,
			      &td,
			      local_vids ) );
		return td->value.items[0];
	}
	case H5_OID_TRIANGLE:
		return h5t_get_local_eid ( f, local_vids );
	default:
		return h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
}

h5_id_t
h5t_get_local_triangle_id2 (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t local_eid
	) {
	h5t_fdata_t *t = f->t;

	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON: {
		h5_td_node_t *td;
		TRY ( _h5t_find_td2 (
			      f,
			      &td,
			      face_id,
			      local_eid) );
		return td->value.items[0];
	}
	case H5_OID_TRIANGLE:
		return local_eid;
	default:
		return h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
}

/*!
  Get local element id for an element given by its global id.

  \param[in]	f		File handle
  \param[in]	global_eid	Global element id

  \return	Local element id or error code.
*/
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
		h5_id_t global_tet_id = _h5t_get_elem_id ( global_tri_id );
		h5_id_t local_tet_id = h5t_map_global_eid2local (
			f, global_tet_id );
		if ( local_tet_id < 0 ) 
			return _h5t_error_global_id_nexist (
				f, "triangle", global_tri_id );
		return local_tet_id | (global_tri_id & ~H5T_ELEM_MASK);
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
		h5_id_t local_tet_id = _h5t_get_elem_id ( local_tri_id );
		h5_id_t global_tet_id = h5t_map_local_eid2global (
			f, local_tet_id );
		if ( global_tet_id < 0 )
			return HANDLE_H5_OUT_OF_RANGE_ERR(
				f, "triangle", local_tri_id );
		return global_tet_id | (local_tri_id & ~H5T_ELEM_MASK); 
	}
	case H5_OID_TRIANGLE:
		return h5t_map_local_eid2global ( f, local_tri_id );
	default:
		return h5_error_internal ( f, __FILE__, __func__, __LINE__ ); 
	}
}


h5_err_t
_h5t_rebuild_global_2_local_map_of_vertices (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = f->t;
	if ( t->num_levels <= 0 ) return H5_SUCCESS;
	h5_id_t local_vid = t->cur_level > 0 ?
		t->num_vertices[t->cur_level-1] : 0;
	for ( ; local_vid < t->num_vertices[t->num_levels-1]; local_vid++ ) {
		t->map_vertex_g2l.items[local_vid].global_id =
			t->vertices[local_vid].global_vid; 
		t->map_vertex_g2l.items[local_vid].local_id = local_vid;
		t->map_vertex_g2l.num_items++;
	}
	_h5_sort_idmap ( &t->map_vertex_g2l );

	return H5_SUCCESS;
}

h5_err_t
_h5t_rebuild_global_2_local_map_of_elems (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	if ( t->num_levels <= 0 ) return H5_SUCCESS;
	h5_id_t local_eid = t->cur_level > 0 ? t->num_elems[t->cur_level-1] : 0;
	h5_id_t num_elems = t->num_elems[t->num_levels-1];
	h5_idmap_el_t *item = &t->map_elem_g2l.items[local_eid];
	size_t offset = _h5t_sizeof_elem[t->mesh_type];
	void *elemp = t->elems.data + local_eid*offset;
	for ( ;
	      local_eid < num_elems;
	      local_eid++,
		      item++,
		      elemp+=offset ) {
		h5_elem_t *elem = (h5_elem_t*)elemp;
		
		item->global_id = elem->global_eid; 
		item->local_id = local_eid;
		t->map_elem_g2l.num_items++;
	}
	_h5_sort_idmap ( &t->map_elem_g2l );

	return H5_SUCCESS;
}

/*!
  \param[in]	f		file handle
  \param[in]	local_id	local ID of entity
  \param[out]	local_vids	array of local vertex IDs of entity
 */
h5_err_t
h5t_get_local_vids_of_entity (
	h5_file_t * const f,
	h5_id_t local_id,
	h5_id_t *local_vids
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t face_id = _h5t_get_face_id ( local_id );
	h5_id_t local_eid = _h5t_get_elem_id ( local_id );
	h5_elem_ldta_t	*tet_dta = &t->elems_ldta[local_eid];

	switch ( _h5t_get_entity_type ( local_id ) ) {
	case H5T_ELEM_TYPE_VERTEX: {
		local_vids[0] = tet_dta->local_vids[face_id];
		break;
	}
	case H5T_ELEM_TYPE_EDGE: {
		int map[6][2] = { { 0,1 }, {1,2}, {0,2}, {0,3}, {1,3}, {2,3} };
		local_vids[0] = tet_dta->local_vids[map[face_id][0]];
		local_vids[1] = tet_dta->local_vids[map[face_id][1]];
		break;
	}
	case H5T_ELEM_TYPE_TRIANGLE: {
		int map[4][3] = { { 1,2,3 }, {0,2,3}, {0,1,3}, {0,1,2} };
		local_vids[0] = tet_dta->local_vids[map[face_id][0]];
		local_vids[1] = tet_dta->local_vids[map[face_id][1]];
		local_vids[2] = tet_dta->local_vids[map[face_id][2]];
		break;
	}
	case 0:
	case H5T_ELEM_TYPE_TET: {
		memcpy ( local_vids, tet_dta->local_vids, sizeof(h5_id_t)*4 );
		break;
	}
	default:
		return h5_error_internal (
			f, __FILE__, __func__, __LINE__ );
	}
	return H5_SUCCESS;
}

h5_id_t
h5t_map_local_vids_to_entity_id (
	h5_file_t * const f,
	h5_id_t *local_vids,
	h5_oid_t etype
	) {
	return h5_error_not_implemented ( f, __FILE__, __func__, __LINE__ );
}
