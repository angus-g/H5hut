#include <hdf5.h>

#include "h5/h5_core.h"
#include "h5/h5_private.h"
#include "h5/t_map.h"

/*!
  Returns the local vertex id of the i-th vertex of an entity. For triangles
  i is in [0,1,2], for tetraheda i is in [0,1,2,3].
*/

static h5_id_t 
_get_local_vertex_id_of_entity (
	h5_file * f,
	int  ith_vertex,
	h5_id_t local_eid
	) {
	struct h5t_fdata *t = &f->t;

	h5_id_t local_vid = -1;
	h5_id_t global_vid = -1;

	switch ( t->mesh_type ) {
	case TETRAHEDRAL_MESH: {
		local_vid = t->lentities.tets[local_eid].vertex_ids[ith_vertex];
		if ( local_vid == -1 ) {
			global_vid =
				t->entities.tets[local_eid].vertex_ids[ith_vertex];
			local_vid = _h5_search_idmap (
				&t->map_vertex_g2l, global_vid );
			t->lentities.tets[local_eid].vertex_ids[ith_vertex] =
				local_vid;
		}
		break;
	}
	case TRIANGLE_MESH: {
		local_vid = t->lentities.tris[local_eid].vertex_ids[ith_vertex];
		if ( local_vid == -1 ) {
			global_vid =
				t->entities.tris[local_eid].vertex_ids[ith_vertex];
			local_vid = _h5_search_idmap (
				&t->map_vertex_g2l, global_vid );
			t->lentities.tris[local_eid].vertex_ids[ith_vertex] =
				local_vid;
		}
		break;
	}
	}
	return local_vid;
}

/*!
  Return the coordinates of the i-th vertex of the entity given by its local 
  entity id. For triangles \c i is in \c [0,1,2], for tetraheda \c i is in
  \c [0,1,2,3].
*/
static h5_float64_t*
_get_vertex_of_entity (
	h5_file * f,
	int  ith_vertex,
	h5_id_t local_entity_id
	) {
	struct h5t_fdata *t = &f->t;

	h5_id_t local_vid = _get_local_vertex_id_of_entity (
		f, ith_vertex, local_entity_id );
	if ( local_vid == -1 ) 
		return NULL;
	return t->vertices[local_vid].P;
}

/*!
  Compare to vertices given by their 3-dimensional coordinates
*/
static int
_cmp_vertices (
	h5_float64_t	*P1,
	h5_float64_t	*P2
	) {
	int i;
	for ( i = 0; i < 3; i++ ) {
		if ( P1[i] < P2[i] ) 	return -1;
		else if (P1[i] > P2[i] )return 1;
	}
	return 0;
}

/*!
  Compare two entities given by their local vertex ids
*/
static int
_vcmp_entities (
	h5_file *f,
	h5_id_t *e1,
	h5_id_t *e2
	) {
	struct h5t_fdata *t = &f->t;
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
  compare two entities given by their local id
*/
static int
_cmp_entities (
	h5_file * f,
	h5_id_t	local_eid1,
	h5_id_t local_eid2
	) {
	struct h5t_fdata *t = &f->t;
	int i;
	for ( i = 0; i < t->mesh_type; i++ ) {
		int r = _cmp_vertices (
			_get_vertex_of_entity ( f, i, local_eid1 ),
			_get_vertex_of_entity ( f, i, local_eid2 ) );
		if ( r < 0 )		return -1;
		else if ( r > 0 ) 	return 1;
	}
	return 0;
}

static int
_cmp_entities1 (
	h5_file * f,
	h5_id_t	local_eid1,
	h5_id_t local_eid2
	) {
	struct h5t_fdata *t = &f->t;
	int imap[] = { 1, 0, 2, 3 };
	int i;
	for ( i = 0; i < t->mesh_type; i++ ) {
		int r = _cmp_vertices (
			_get_vertex_of_entity ( f, imap[i], local_eid1 ),
			_get_vertex_of_entity ( f, imap[i], local_eid2 ) );
		if ( r < 0 )		return -1;
		else if ( r > 0 ) 	return 1;
	}
	return 0;
}

/*
  The re-rentrant version of qsort(3) is not available on many systems, thus ...
*/
static h5_file *_f;
static int
_qsort_cmp_entities0 ( const void* _local_eid1, const void* _local_eid2 ) {
	h5_id_t local_eid1 = *(h5_id_t*)_local_eid1;
	h5_id_t local_eid2 = *(h5_id_t*)_local_eid2;
	return _cmp_entities ( _f, local_eid1, local_eid2 );
}

static int
_qsort_cmp_entities1 ( const void* _local_eid1, const void* _local_eid2 ) {
	h5_id_t local_eid1 = *(h5_id_t*)_local_eid1;
	h5_id_t local_eid2 = *(h5_id_t*)_local_eid2;
	return _cmp_entities1 ( _f, local_eid1, local_eid2 );
}

/*!
  Sort entities geometrically.
*/
static h5_err_t
_sort_entities (
	h5_file *f
	) {

	struct h5t_fdata *t = &f->t;
	h5_size_t num_entities = h5t_get_num_entities ( f );
	if ( num_entities < 0 ) return num_entities;

	int k;
	h5_id_t i;
	for ( k = 0; k < 2; k++ ) {
		h5_err_t h5err = _h5_alloc_smap (
			&t->sorted_lentities[k], num_entities );
		if ( h5err < 0 ) return h5err;
		for ( i = 0; i < num_entities; i++ ) {
			t->sorted_lentities[k].items[i] = i;
		}
		t->sorted_lentities[k].num_items = num_entities;
	}

	_f = f;
	qsort ( t->sorted_lentities[0].items,
		num_entities,
		sizeof(t->sorted_lentities[0].items[0]),
		_qsort_cmp_entities0 );
	qsort ( t->sorted_lentities[1].items,
		num_entities,
		sizeof(t->sorted_lentities[1].items[0]),
		_qsort_cmp_entities1 );

	return H5_SUCCESS;
}

/*!
  Sort (small) array of local vertex ids geometrically. 
 */
h5_err_t
_h5t_sort_local_vertex_ids (
	h5_file * const f,
	h5_id_t * const local_vids,		/* IN/OUT: local vertex ids */	
	const h5_size_t size			/* size of array */
	) {
	struct h5t_fdata *t = &f->t;

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
_h5t_sort_global_vertex_ids (
	h5_file * const f,
	h5_id_t * const global_vids,		/* IN/OUT: global vertex ids */	
	const h5_size_t size			/* size of array */
	) {

	h5_id_t local_vids[H5_MAX_VERTICES_PER_ENTITY];
	const h5_id_t *global_vid = global_vids;
	h5_id_t *local_vid = local_vids;

	h5_id_t i;
	for ( i = 0; i < size; i++, local_vid++, global_vid++ ) {
		*local_vid = h5t_map_global_vertex_id2local ( f, *global_vid );
		if ( *local_vid < 0 )
			return *local_vid;
	}
	h5_err_t h5err = _h5t_sort_local_vertex_ids ( f, local_vids, size );
	if ( h5err < 0 ) return h5err;

	for ( i = 0; i < size; i++ ) {
		global_vids[i] = h5t_map_local_vertex_id2global ( f, local_vids[i] );
	}
	return H5_SUCCESS;
}


/*!
  Binary search an entity given by its local vertex ids.

  \result	index in t->map_entity_s2l[0].items
 */
static h5_id_t
_search_entity (
	h5_file *f,
	h5_id_t * const local_vids	/* local vertex ids */
	) {
	struct h5t_fdata *t = &f->t;

	_h5t_sort_local_vertex_ids ( f, local_vids, t->mesh_type );

	register h5_id_t low = 0;
	register h5_id_t high = t->sorted_lentities[0].num_items - 1;
	register h5_id_t *entity1 = local_vids;
	while (low <= high) {
		register int mid = (low + high) / 2;

		h5_id_t local_eid = t->sorted_lentities[0].items[mid];
		h5_id_t *entity2 = t->lentities.tets[local_eid].vertex_ids;
		int diff = _vcmp_entities ( f, entity1, entity2 );
           	if ( diff < 0 )
               		high = mid - 1;
           	else if ( diff > 0 )
               		low = mid + 1;
           	else
               		return mid; // found
       	}
       	return -(low+1);  // not found
}

h5_err_t
_h5t_read_mesh (
	h5_file *f
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->vertices == NULL ) {
		h5_err_t h5err = _h5t_read_vertices ( f );
		if ( h5err < 0 ) return h5err;
	}

	if ( t->entities.data == NULL ) {
		h5_err_t h5err = _h5t_read_entities ( f );
		if ( h5err < 0 ) return h5err;
	}

	if ( t->sorted_lentities[0].items == NULL ) {
		_sort_entities ( f );
	}
	return H5_SUCCESS;
}

 /*!
   Get local id of entity given by local vertex id's
 */
h5_id_t
h5t_get_local_entity_id (
	h5_file *f,
	h5_id_t * const local_vids	/* IN/OUT: local vertex id's */
	) {
	struct h5t_fdata *t = &f->t;

	h5_id_t local_eid = _search_entity ( f, local_vids );
	if ( local_eid < 0 ) {
		return _h5t_handle_get_local_entity_id_err ( f, local_vids );
	}

	return t->sorted_lentities[0].items[local_eid];
}

/*!
  Map a global vertex id to corresponding local vertex id.
*/
h5_id_t
h5t_map_global_vertex_id2local (
	h5_file *f,
	const h5_id_t global_id
	) {
	struct h5t_fdata *t = &f->t;

	h5_id_t local_id = _h5_search_idmap ( &t->map_vertex_g2l, global_id );
	if ( local_id < 0 ) 
		return _h5t_handle_global_id_not_exist_err ("vertex", global_id );
	return local_id;
}

h5_id_t
h5t_map_local_vertex_id2global (
	h5_file *f,
	const h5_id_t local_vid
	) {
	struct h5t_fdata *t = &f->t;
	
	if ( local_vid < 0 || local_vid > t->num_vertices[t->num_levels-1] )
		return HANDLE_H5_OUT_OF_RANGE_ERR ( "vertex", local_vid );
	return t->vertices[local_vid].id;
}

h5_err_t
h5t_map_global_vertex_ids2local (
	h5_file *f,
	const h5_id_t * const global_vids,
	const h5_id_t size,
	h5_id_t * const local_vids
	) {
	h5_id_t i;

	for ( i = 0; i < size; i++ ) {
		local_vids[i] = h5t_map_global_vertex_id2local (
			f, global_vids[i] );
		if ( local_vids[i] < 0 ) 
			return _h5t_handle_global_id_not_exist_err (
				"vertex", global_vids[i] );
	}
	return H5_SUCCESS;
}

/*!
  Map a local entity id to corresponding global id.
*/
h5_id_t
h5t_map_local_entity_id2global (
	h5_file *f,
	const h5_id_t local_eid
	) {
	struct h5t_fdata *t = &f->t;

	switch ( t->mesh_type ) {
	case TETRAHEDRAL_MESH:
		if ( local_eid < 0 || local_eid > t->num_entities[t->num_levels-1] )
			return HANDLE_H5_OUT_OF_RANGE_ERR ( "tet", local_eid );
		return t->entities.tets[local_eid].id;
	case TRIANGLE_MESH:
		if ( local_eid < 0 || local_eid > t->num_entities[t->num_levels-1] )
			return HANDLE_H5_OUT_OF_RANGE_ERR ( "triangle", local_eid );
		return t->entities.tris[local_eid].id;
	}
	return -1;
}

/*!
  Get global id of entity given by global vertex id's
*/
h5_id_t
h5t_get_global_entity_id (
	h5_file *f,
	const h5_id_t * const global_vids	/* global vertex id's */
	) {
	struct h5t_fdata *t = &f->t;
	h5_id_t local_vids[H5_MAX_VERTICES_PER_ENTITY];

	if ( t->vertices == NULL ) {
		h5_err_t h5err = _h5t_read_mesh ( f );
		if ( h5err < 0 ) return h5err;
	}

	h5_err_t h5err = h5t_map_global_vertex_ids2local (
		f,
		global_vids,
		t->mesh_type,
		local_vids );
	if ( h5err < 0 )
		return _h5t_handle_get_global_entity_id_err ( f, global_vids );

	h5_id_t local_eid = h5t_get_local_entity_id ( f, local_vids );
	if ( local_eid < 0 )
		return _h5t_handle_get_global_entity_id_err ( f, global_vids );

	return h5t_map_local_entity_id2global ( f, local_eid );
}

static int
_search_ith_vertex_in_entity (
	h5_file * const f,
	const int i,
	h5_id_t	local_vid
	) {
	struct h5t_fdata *t = &f->t;

	register h5_id_t low = 0;
	register h5_id_t high = t->sorted_lentities[i].num_items - 1;
	register h5_float64_t *vertex1 = t->vertices[local_vid].P;

	while (low <= high) {
		register int mid = (low + high) / 2;

		h5_id_t local_eid = t->sorted_lentities[i].items[mid];
		h5_id_t local_vid2 = t->lentities.tets[local_eid].vertex_ids[i];
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
	h5_file *f,
	const h5_id_t * const local_vids,
	int i,
	h5_id_t local_eid
	) {
	struct h5t_fdata *t = &f->t;

	h5_id_t *local_vids_of_entity = t->lentities.tets[local_eid].vertex_ids;
	if ( i == 0 && 
	     local_vids[1] == local_vids_of_entity[1] && 
	     local_vids[2] == local_vids_of_entity[2]
		) return 0;
	else if ( i == 0 && 
	     local_vids[1] == local_vids_of_entity[1] && 
	     local_vids[2] == local_vids_of_entity[3]
		) return 1;
	else if ( i == 0 && 
	     local_vids[1] == local_vids_of_entity[2] && 
	     local_vids[2] == local_vids_of_entity[3]
		) return 2;
	else if ( i == 1 && 
	     local_vids[1] == local_vids_of_entity[2] && 
	     local_vids[2] == local_vids_of_entity[3]
		) return 3;
	return -1;
}

/*!
  Search for triangle given by local vertex id in tetrahdral mesh.

  \return unique local triangle id
 */
h5_id_t
_tetm_search_triangle (
	h5_file *f,
	h5_id_t * const local_vids
	) {
	struct h5t_fdata *t = &f->t;

	_h5t_sort_local_vertex_ids ( f, local_vids, 3 );

	/*
	  search for Vertex_id(tri,0) in the tuple of all 0th vertices of all tets
	  if there is one:
		take the smallest tet with Vertex_id(tri,0) == Vertex_id(tet,0)
		loop over all tets with Vertex_id(tri,0) == Vertex_id(tet,0)
		until we find a tet the triangle is belonging to.
	  else
		search for Vertex_id(tri,0) in the tuple of all 1st vertices of
		all tets
		if there is one:
			take the smallest tet with
			Vertex_id(tri,0) == Vertex_id(tet,0)
			loop over all tets with
			Vertex_id(tri,0) == Vertex_id(tet,0)
			until we find a tet the triangle is belonging to.
	*/

	int i;
	h5_id_t idx[2];
	h5_id_t local_eid[2];
	h5_id_t tidx[2];
	for ( i = 0; i < 2; i++ ) {
		idx[i] = _search_ith_vertex_in_entity ( f, i, local_vids[0] );

		while ( idx[i] > 0 &&		/* get leftmost */
			local_vids[0] == _get_local_vertex_id_of_entity (
				f, i, t->sorted_lentities[0].items[idx[i]-1] ) )
			idx[i]--;

		do {
			/* check whether triangle is in entity given by local id */
			local_eid[i] = t->sorted_lentities[i].items[idx[i]];
			tidx[i] = _tetm_contain_triangle (
				f, local_vids, i, local_eid[i] );
			if ( tidx[i] >= 0 ) break;
			idx[i]++;
		} while ( local_vids[0] == _get_local_vertex_id_of_entity (
				  f, i, local_eid[i] ) );
	}
	if ( idx[0] < 0 && idx[1] < 0 ) return -1;
	if ( idx[0] < 0 ) return _h5t_build_triangle_id ( tidx[1], local_eid[1] );
	if ( idx[1] < 0 ) return _h5t_build_triangle_id ( tidx[0], local_eid[0] );

	if ( _cmp_entities( f, local_eid[0], local_eid[1] ) < 0 )
		return _h5t_build_triangle_id ( tidx[0], local_eid[0] );
	else
		return _h5t_build_triangle_id ( tidx[1], local_eid[1] );

	return -1;
}

/*!
  
 */
h5_id_t
h5t_get_global_triangle_id  (
	h5_file * const f,
	h5_id_t * const global_vids
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->vertices == NULL ) {
		h5_err_t h5err = _h5t_read_mesh ( f );
		if ( h5err < 0 ) return h5err;
	}

	switch ( t->mesh_type ) {
	case TETRAHEDRAL_MESH: {
		h5_id_t local_vids[3];
		h5_err_t h5err = h5t_map_global_vertex_ids2local (
			f, global_vids, 3, local_vids );
		if ( h5err < 0 ) return h5err;
		h5_id_t local_tid = h5t_get_local_triangle_id ( f, local_vids );
		if ( local_tid < 0 )
			return _h5t_handle_get_global_triangle_id_err( global_vids );
		return h5t_map_local_triangle_id2global ( f, local_tid );
	}
	case TRIANGLE_MESH:
		return h5t_get_global_entity_id ( f, global_vids );
	}
	return -1;
}

/*!
  
 */
h5_id_t
h5t_get_local_triangle_id  (
	h5_file * const f,
	h5_id_t * const local_vids
	) {
	struct h5t_fdata *t = &f->t;
	
	switch ( t->mesh_type ) {
	case TETRAHEDRAL_MESH: {
		struct h5t_fdata *t = &f->t;

		if ( t->vertices == NULL ) {
			h5_err_t h5err = _h5t_read_vertices ( f );
			if ( h5err < 0 ) return h5err;
		}

		if ( t->entities.data == NULL ) {
			h5_err_t h5err = _h5t_read_entities ( f );
			if ( h5err < 0 ) return h5err;
		}

		if ( t->sorted_lentities[0].items == NULL ) {
			_sort_entities ( f );
		}
		h5_id_t local_tid = _tetm_search_triangle ( f, local_vids );
		if ( local_tid == -1 ) {
			return _h5t_handle_get_local_triangle_id_err (
				local_vids );
		}
		return local_tid;
	}
	case TRIANGLE_MESH:
		return h5t_get_local_entity_id ( f, local_vids );
	}
	return -1;
}

h5_id_t
h5t_map_global_entity_id2local (
	h5_file * const f,
	const h5_id_t global_eid
	) {
	struct h5t_fdata *t = &f->t;
	h5_id_t local_eid = _h5_search_idmap ( &t->map_entity_g2l, global_eid );
	if ( local_eid < 0 ) 
		return _h5t_handle_global_id_not_exist_err ( "entity", global_eid );
	return local_eid;
}

/*!
  Map global triangle id to local id. 

  \return local id of triangle
 */
h5_id_t
h5t_map_global_triangle_id2local (
	h5_file * const f,
	const h5_id_t global_tri_id
	) {
	struct h5t_fdata *t = &f->t;
	switch ( t->mesh_type ) {
	case TETRAHEDRAL_MESH: {
		h5_id_t global_tet_id = global_tri_id & H5_TET_MASK;
		h5_id_t local_tet_id = h5t_map_global_entity_id2local (
			f, global_tet_id );
		if ( local_tet_id < 0 ) 
			return _h5t_handle_global_id_not_exist_err (
				"triangle", global_tri_id );
		return local_tet_id | (global_tri_id & ~H5_TET_MASK);
	}
	case TRIANGLE_MESH:
		return h5t_map_global_entity_id2local ( f, global_tri_id );
	}
	return -1;
}

/*!
  Map local triangle id to global id.

  \return global id of triangle 
 */
h5_id_t
h5t_map_local_triangle_id2global (
	h5_file * const f,
	const h5_id_t local_tri_id
	) {
	struct h5t_fdata *t = &f->t;
	switch ( t->mesh_type ) {
	case TETRAHEDRAL_MESH: {
		h5_id_t local_tet_id = local_tri_id & H5_TET_MASK;
		h5_id_t global_tet_id = h5t_map_local_entity_id2global (
			f, local_tet_id );
		if ( global_tet_id < 0 )
			return HANDLE_H5_OUT_OF_RANGE_ERR(
				"triangle", local_tri_id );
		return global_tet_id | (local_tri_id & ~H5_TET_MASK); 
	}
	case TRIANGLE_MESH:
		return h5t_map_local_entity_id2global ( f, local_tri_id );
	}
	return -1;
}
