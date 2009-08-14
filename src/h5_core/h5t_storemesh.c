#include <stdlib.h>
#include <string.h>
#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_types_private.h"
#include "h5_core/h5t_types_private.h"
#include "h5_core/h5_core_private.h"

/*!
  Add new mesh

  \return mesh id
*/
h5_id_t
h5t_add_mesh (
	h5_file_t * const f,
	const h5_oid_t mesh_type
	) {
	h5_id_t mesh_id = 0;

	TRY ( (mesh_id = h5t_open_mesh ( f, -1, mesh_type )) ); 
	TRY ( h5t_add_level ( f ) );
	
	f->t->mesh_changed = 1;

	return mesh_id;
}

/*

 * Assign unique global id's to vertices. Vertices already have
   unique id's assigned by the mesher but this id's may not be
   consecutive numbered starting from 0.
 * Set the global vertex id's in element definitions.
*/
h5_err_t
_h5t_assign_global_vertex_ids (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t local_id;

	if ( t->cur_level < 0 ) return H5_SUCCESS; /* no level defined */

	/*
	  simple in serial runs: global_id = local_id
	*/
	for ( local_id = 0;
	      local_id < t->num_vertices[t->num_levels-1];
	      local_id++ ) {
		t->vertices[local_id].global_vid = local_id;
	}

	return H5_SUCCESS;
}

/*!
 Assign unique global IDs to new elements.
*/
static h5_err_t
_assign_global_elem_ids (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t local_eid;

	if ( t->cur_level < 0 ) return H5_SUCCESS; /* no level defined */

	/*
	  simple in serial runs: global_id = local_id
	*/
	for ( local_eid = (t->cur_level == 0) ? 0 : t->num_elems[t->cur_level-1];
	      local_eid < t->num_elems[t->cur_level];
	      local_eid++ ) {
		h5_elem_t *elem;
		h5_elem_ldta_t *elem_ldta = &t->elems_ldta[local_eid];
		switch ( t->mesh_type ) {
		case H5_OID_TETRAHEDRON:
			elem = (h5_elem_t*)&t->elems.tets[local_eid];
			break;
		case H5_OID_TRIANGLE:
			elem = (h5_elem_t*)&t->elems.tris[local_eid];
			break;
		default:
			return h5_error_internal (
				f, __FILE__, __func__, __LINE__ );
		}
		elem->global_eid = local_eid;
		elem->global_parent_eid = elem_ldta->local_parent_eid;
		elem->global_child_eid = elem_ldta->local_child_eid;
		int i;
		for ( i = 0; i < t->mesh_type; i++ ) {
			elem->global_vids[i] = elem_ldta->local_vids[i];
		}
	}

	return H5_SUCCESS;
}

h5_id_t
h5t_add_level (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;

	if ( f->mode == H5_O_RDONLY ) {
		return H5_ERR_INVAL;
	}

	/* t->num_levels will be set to zero on file creation(!) */
	if ( t->num_levels == -1 ) {	/* unknown number of levels	*/
		/* determine number of levels */
		return h5_error_not_implemented (
			f, __FILE__, __func__, __LINE__ );
	
	}
	t->cur_level = t->num_levels++;
	t->dsinfo_num_vertices.dims[0] = t->num_levels;
	t->dsinfo_num_elems.dims[0] = t->num_levels;
	t->dsinfo_num_elems_on_level.dims[0] = t->num_levels;

	ssize_t num_bytes = t->num_levels*sizeof ( h5_size_t );
	t->num_vertices = realloc ( t->num_vertices, num_bytes );
	t->num_vertices[t->cur_level] = -1;

	TRY ( t->num_elems = _h5_alloc ( f, t->num_elems, num_bytes ) );
	t->num_elems[t->cur_level] = -1;
	TRY ( t->num_elems_on_level = _h5_alloc (
		      f, t->num_elems_on_level, num_bytes ) );
	t->num_elems_on_level[t->cur_level] = -1;

	t->new_level = t->cur_level;
	if ( t->cur_level == 0 ) {
		/* nothing stored yet */
		t->last_stored_vid = -1;
		t->last_stored_eid = -1;
	}

	return t->cur_level;
}

h5_err_t
_h5t_alloc_num_vertices (
	h5_file_t * const f,
	const h5_size_t num_vertices
	) {
	h5t_fdata_t *t = f->t;

	ssize_t size = num_vertices * sizeof ( t->vertices[0] );
	TRY ( t->vertices = _h5_alloc (	f, t->vertices, size ) );
	size = num_vertices * sizeof ( t->vertices_data[0] );
	TRY ( t->vertices_data = _h5_alloc (	f, t->vertices_data, size ) );
	TRY( _h5_alloc_idmap ( f, &t->map_vertex_g2l, num_vertices ) );
	TRY( _h5_alloc_idlist_items ( f, &t->sorted_lvertices, num_vertices ) );

	return H5_SUCCESS;
}

/*!
  Allocate memory for (more) vertices.
*/
h5_err_t
h5t_begin_store_vertices (
	h5_file_t * const f,
	const h5_size_t num
	) {
	h5t_fdata_t *t = f->t;

	if ( t->cur_level < 0 ) {
		return _h5t_error_undef_level( f );
	}
	t->storing_data = 1;
	h5_size_t cur_num_vertices = ( t->cur_level > 0 ?
				       t->num_vertices[t->cur_level-1] : 0 );
	t->num_vertices[t->cur_level] = cur_num_vertices+num;
	t->dsinfo_vertices.dims[0] = cur_num_vertices+num;
	return _h5t_alloc_num_vertices ( f, cur_num_vertices+num );
}

h5_id_t
h5t_store_vertex (
	h5_file_t * const f,		/*!< file handle		*/
	const h5_id_t global_vid,     	/*!< global vertex id or -1	*/
	const h5_float64_t P[3]		/*!< coordinates		*/
	) {
	struct h5t_fdata *t = f->t;
	
	/*
	  more than allocated
	*/
	if ( t->last_stored_vid+1 >= t->num_vertices[t->cur_level] ) 
		return HANDLE_H5_OVERFLOW_ERR(
			f, "vertex", t->num_vertices[t->cur_level] );
	
	/*
	  missing call to add the first level
	 */
	if ( t->cur_level < 0 )
		return _h5t_error_undef_level( f );

	t->level_changed = 1;
	h5_id_t local_id = ++t->last_stored_vid;
	h5_vertex_t *vertex = &t->vertices[local_id];
	vertex->global_vid = global_vid;     /* ID from mesher, replaced later!*/
	memcpy ( &vertex->P, P, sizeof ( vertex->P ) );
	return local_id;
}

h5_err_t
h5t_end_store_vertices (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	t->storing_data = 0;

	t->num_vertices[t->cur_level] = t->last_stored_vid+1;
	TRY ( _h5t_assign_global_vertex_ids ( f ) );
	TRY ( _h5t_sort_vertices ( f ) );
	TRY ( _h5t_rebuild_global_2_local_map_of_vertices ( f ) );
	return H5_SUCCESS;
}

h5_err_t
_h5t_alloc_tris (
	h5_file_t * const f,
	const size_t cur,
	const size_t new
	) {
	h5t_fdata_t *t = f->t;
	const size_t nvertices = 3;

	/* alloc mem for elements */
	TRY ( t->elems.tris = _h5_alloc (
		      f,
		      t->elems.tris,
		      new * sizeof(t->elems.tris[0]) ) );
	memset (
		t->elems.tris + cur,
		-1,
		(new-cur) * sizeof(t->elems.tris[0]) );

	/* alloc mem for local data of elements */
	TRY ( t->elems_ldta = _h5_alloc (
		      f,
		      t->elems_ldta,
		      new * sizeof (t->elems_ldta[0]) ) );
	memset (
		t->elems_ldta + cur,
		-1,
		(new-cur) * sizeof (t->elems_ldta[0]) );

	/* alloc mem for local vertex IDs of elements */
	TRY ( t->elems_lvids = _h5_alloc (
		      f,
		      t->elems_lvids,
		      new * sizeof(t->elems_lvids[0]) * nvertices ) );
	memset (
		t->elems_lvids + cur * sizeof(t->elems_lvids[0]) * nvertices,
		-1,
		(new-cur) * sizeof(t->elems_lvids[0]) * nvertices );

	/* re-init pointer to local vertex id in local data structure */
	h5_id_t *p = t->elems_lvids;
	h5_id_t id;
	for ( id = 0; id < new; id++, p+=nvertices ) {
		t->elems_ldta[id].local_vids = p;
	}

	/* alloc mem for global to local ID mapping */
	TRY ( _h5_alloc_idmap ( f, &t->map_elem_g2l, new ) );

	return  H5_SUCCESS;
}

h5_err_t
_h5t_alloc_tets (
	h5_file_t * const f,
	const size_t cur,
	const size_t new
	) {
	h5t_fdata_t *t = f->t;
	const size_t nvertices = 4;

	/* alloc mem for elements */
	TRY ( t->elems.tets = _h5_alloc (
		      f,
		      t->elems.tets,
		      new * sizeof(t->elems.tets[0]) ) );
	memset (
		t->elems.tets + cur,
		-1,
		(new-cur) * sizeof(t->elems.tets[0]) );

	/* alloc mem for local data of elements */
	TRY ( t->elems_ldta = _h5_alloc (
		      f,
		      t->elems_ldta,
		      new * sizeof (t->elems_ldta[0]) ) );
	memset (
		t->elems_ldta + cur,
		-1,
		(new-cur) * sizeof (t->elems_ldta[0]) );

	/* alloc mem for local vertex IDs of elements */
	TRY ( t->elems_lvids = _h5_alloc (
		      f,
		      t->elems_lvids,
		      new * sizeof(t->elems_lvids[0]) * nvertices ) );
	memset (
		t->elems_lvids + cur * nvertices,
		-1,
		(new-cur) * sizeof(t->elems_lvids[0]) * nvertices );

	/* re-init pointer to local vertex id in local data structure */
	h5_id_t *p = t->elems_lvids;
	h5_id_t id;
	for ( id = 0; id < new; id++, p+=nvertices ) {
		t->elems_ldta[id].local_vids = p;
	}

	/* alloc mem for global to local ID mapping */
	TRY ( _h5_alloc_idmap ( f, &t->map_elem_g2l, new ) );

	return  H5_SUCCESS;
}

/*!
  Initialize everything so that we can begin to store elements.

  \param[in]	f	file handle
  \param[in]	num	number of elements to add
 */
h5_err_t
h5t_begin_store_elems (
	h5_file_t * const f,
	const h5_size_t num
	) {
	h5t_fdata_t *t = f->t;

	t->storing_data = 1;
	size_t cur = t->cur_level > 0 ? t->num_elems[t->cur_level-1] : 0;
	size_t new = num + cur;
	t->num_elems[t->cur_level] = new;
	t->dsinfo_elems.dims[0] = new;

	t->num_elems_on_level[t->cur_level] = t->cur_level > 0 ?
		num + t->num_elems_on_level[t->cur_level-1] : num;
	/*
	  We allocate a hash table for a minimum of 2^21 edges to
	  prevent resizing.
	 */
	size_t nel = 2097152 > 5*new ? 2097152 : 5*new;
	TRY ( _h5t_resize_te_htab ( f, nel ) );
	return (*t->methods._alloc_elems) ( f, cur, new );
}

/*!
  \param[in] local_parent_eid	local parent id of element
				if level \c >0 else \c -1
  \param[in] local_vids		local vertex id's defining the element
 */
h5_id_t
h5t_store_elem    (
	h5_file_t * const f,
	const h5_id_t local_parent_eid,
	const h5_id_t local_vids[]
	) {
	h5t_fdata_t *t = f->t;

	/* level set? */
	if ( t->cur_level < 0 )
		return _h5t_error_undef_level( f );

	/*  more than allocated? */
	if ( t->last_stored_eid+1 >= t->num_elems[t->cur_level] ) 
		return HANDLE_H5_OVERFLOW_ERR(
			f, _h5t_map_oid2str(t->mesh_type),
			t->num_elems[t->cur_level] );

	/* check parent id */
	if (
		( t->cur_level == 0 && local_parent_eid != -1 ) ||
		( t->cur_level >  0 && local_parent_eid < 0 ) ||
		( t->cur_level >  0
		  && local_parent_eid >= t->num_elems[t->cur_level-1] )
		) {
		return HANDLE_H5_PARENT_ID_ERR (
			f,
			_h5t_map_oid2str(t->mesh_type),
			local_parent_eid );
	}

	return (*t->methods._store_elem)( f, local_parent_eid, local_vids );
}

/*!
  Store tetrahedron. The vertices are given with there *local* id's!

  \param[in]	f		File handle.
  \param[in]	local_parent_id	Local id of the parent element or \c -1.
  \param[in]	local_vids	Local vertex id's defining the tetrahedron.
 */
h5_id_t
_h5t_store_tet (
	h5_file_t * const f,
	const h5_id_t local_parent_eid,
	const h5_id_t *local_vids
	) {

	h5t_fdata_t *t = f->t;
	t->level_changed = 1;
	h5_id_t local_eid = ++t->last_stored_eid;
	h5_elem_ldta_t *elem_ldta = &t->elems_ldta[local_eid];

	elem_ldta->local_parent_eid = local_parent_eid;
	elem_ldta->local_child_eid = -1;
	elem_ldta->level_id = t->cur_level;
	memcpy ( elem_ldta->local_vids, local_vids,
		 sizeof (*local_vids) * t->mesh_type );
	_h5t_sort_local_vids ( f, elem_ldta->local_vids, t->mesh_type );
	h5_id_t face_id;
	h5_te_entry_t *retval;
	for ( face_id = 0; face_id < 6; face_id++ ) {
		TRY ( _h5t_search_te2 (
			      f,
			      face_id,
			      local_eid,
			      &retval ) );
	}
	return local_eid;
}

h5_id_t
_h5t_store_tri (
	h5_file_t * const f,
	const h5_id_t local_parent_eid,
	const h5_id_t *local_vids
	) {

	h5t_fdata_t *t = f->t;
	t->level_changed = 1;
	h5_id_t local_eid = ++t->last_stored_eid;
	h5_elem_ldta_t *elem_ldta = &t->elems_ldta[local_eid];

	elem_ldta->local_parent_eid = local_parent_eid;
	elem_ldta->local_child_eid = -1;
	elem_ldta->level_id = t->cur_level;
	memcpy ( elem_ldta->local_vids, local_vids,
		 sizeof (*local_vids) * t->mesh_type );
	_h5t_sort_local_vids ( f, elem_ldta->local_vids, t->mesh_type );
	h5_id_t face_id;
	h5_te_entry_t *retval;
	for ( face_id = 0; face_id < 3; face_id++ ) {
		TRY ( _h5t_search_te2 (
			      f,
			      face_id,
			      local_eid,
			      &retval ) );
	}
	return local_eid;
}

h5_err_t
h5t_end_store_elems (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	t->storing_data = 0;

	t->num_elems[t->cur_level] = t->last_stored_eid+1;
	TRY ( _assign_global_elem_ids ( f ) );

	TRY ( _h5t_sort_elems ( f ) );
	TRY ( _h5t_rebuild_global_2_local_map_of_elems ( f ) );

	return H5_SUCCESS;
}


h5_err_t
h5t_begin_refine_elems (
	h5_file_t * const f,
	const h5_size_t num_elems_to_refine
	) {
	h5_size_t num_elems_to_add = 0;
	h5_size_t num_vertices_to_add = 0;

	f->t->storing_data = 1;
	/*
	  Now we have to guess the number of vertices ...
	  If we are going to refine one tetrahedron, we have 8 new tetrahedra
	  and 6 new vertices. Thus the numbers below are definitely upper
	  limits!
	 */
	switch ( f->t->mesh_type ) {
	case H5_OID_TETRAHEDRON:
		num_vertices_to_add = num_elems_to_refine*6;
		num_elems_to_add = num_elems_to_refine*8;
		break;
	case H5_OID_TRIANGLE:
		num_vertices_to_add = num_elems_to_refine*3;
		num_elems_to_add = num_elems_to_refine*4;
		break;
	default:
		return h5_error_internal ( f, __FILE__, __func__, __LINE__ );
	}
	TRY ( h5t_begin_store_vertices ( f, num_vertices_to_add ) );
	TRY ( h5t_begin_store_elems ( f, num_elems_to_add ) );

	return H5_SUCCESS;
}

h5_err_t
h5t_end_refine_elems (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	t->storing_data = 0;

	TRY ( h5t_end_store_vertices ( f ) );
	TRY ( h5t_end_store_elems ( f ) );

	return H5_SUCCESS;
}

/*!
  Refine edge. Store vertex, if new.

  \return local id of vertex
*/
h5_id_t
_h5t_bisect_edge (
	h5_file_t * const f,
	h5_id_t face_id,
	h5_id_t el_id
	) {
	h5t_fdata_t *t = f->t;
	h5_te_entry_t item;
	h5_id_t *vids = item.key.vids;
	h5_te_entry_t *retval;
	/*
	  get all elements sharing the given edge
	 */
	TRY ( h5t_get_local_vids_of_edge2 ( f, face_id, el_id, vids ) );
	TRY ( _h5t_find_te ( f, &item, &retval ) );
	/*
	  check wether one of these elements has been refined
	 */
	size_t i;
	for ( i = 0; i < retval->value.num_items; i++ ) {
		h5_id_t local_id = _h5t_get_elem_id ( retval->value.items[i] );
		h5_elem_ldta_t *tet = &t->elems_ldta[local_id];
		if ( tet->local_child_eid >= 0 ) {
			/*
			  this element has been refined!
			  return bisecting point
			 */
			h5_id_t face_id = _h5t_get_face_id ( retval->value.items[i] );
			h5_id_t kids[2], edge0[2], edge1[2];
			TRY ( _h5t_compute_direct_children_of_edge (
				      f,
				      face_id,
				      tet->local_child_eid,
				      kids ) );
			TRY ( h5t_get_local_vids_of_edge ( f, kids[0], edge0 ) );
			TRY ( h5t_get_local_vids_of_edge ( f, kids[1], edge1 ) );
			if ( (edge0[0] == edge1[0]) || (edge0[0] == edge1[1]) )
				return edge0[0];
			else
				return edge0[1];
		}
	}
	/*
	  None of the elements has been refined -> add new vertex.
	 */
	h5_float64_t *P0 = t->vertices[vids[0]].P;
	h5_float64_t *P1 = t->vertices[vids[1]].P;
	h5_float64_t P[3];

	P[0] = ( P0[0] + P1[0] ) / 2.0;
	P[1] = ( P0[1] + P1[1] ) / 2.0;
	P[2] = ( P0[2] + P1[2] ) / 2.0;

	return h5t_store_vertex ( f, -1, P );
}

/*!
  Refine element \c local_eid

  \return Local id of first new element or \c -1
*/
h5_id_t
h5t_refine_elem (
	h5_file_t * const f,
	const h5_id_t local_eid
	) {
	return (*f->t->methods._refine_elem)( f, local_eid );

}

/*!
  Refine triangle \c local_eid

  \return Local id of first new triangle or \c -1
*/
h5_id_t
_h5t_refine_tri (
	h5_file_t * const f,
	const h5_id_t local_eid
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t local_vids[6];
	h5_id_t local_child_eid;
	h5_elem_ldta_t *el = &t->elems_ldta[local_eid];

	if ( el->local_child_eid >= 0 )
		return h5_error (
			f,
			H5_ERR_INVAL,
			"Tetrahedron %lld already refined.",
			local_eid );

	local_vids[0] = el->local_vids[0];
	local_vids[1] = el->local_vids[1];
	local_vids[2] = el->local_vids[2];

	local_vids[3] = _h5t_bisect_edge( f, 0, local_eid ); /* 1,2 */
	local_vids[4] = _h5t_bisect_edge( f, 1, local_eid ); /* 0,2 */
	local_vids[5] = _h5t_bisect_edge( f, 2, local_eid ); /* 0,1 */

	h5_id_t new_el[3];

	/* 0 */
	new_el[0] = local_vids[0];
	new_el[1] = local_vids[4]; 
	new_el[2] = local_vids[5];
	TRY ( local_child_eid = _h5t_store_tri ( f, local_eid, new_el ) );

	/* 1 */
	new_el[0] = local_vids[1];
	new_el[1] = local_vids[3]; 
	new_el[2] = local_vids[5];
	TRY ( _h5t_store_tri ( f, local_eid, new_el ) );

	/* 2 */
	new_el[0] = local_vids[2];
	new_el[1] = local_vids[3]; 
	new_el[2] = local_vids[4];
	TRY ( _h5t_store_tri ( f, local_eid, new_el ) );

	/* 3 */
	new_el[0] = local_vids[3];
	new_el[1] = local_vids[4];
	new_el[2] = local_vids[5];
	TRY ( _h5t_store_tri ( f, local_eid, new_el ) );

	return local_child_eid;
}

/*!
  \param[in]	f		file handle
  \param[in]	local_kid	local edge ID we search children of
  \param[in]	local_eid	local element ID of first children 
  \param[out]	kids		direct children

*/
h5_err_t
_h5t_compute_direct_children_of_edge (
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


/*!
  Refine tetrahedron \c local_eid

  \return Local id of first new tetrahedron or \c -1
*/
h5_id_t
_h5t_refine_tet (
	h5_file_t * const f,
	const h5_id_t local_eid
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t local_vids[10];
	h5_id_t local_child_eid;
	h5_elem_ldta_t *tet = &t->elems_ldta[local_eid];

	if ( tet->local_child_eid >= 0 )
		return h5_error (
			f,
			H5_ERR_INVAL,
			"Tetrahedron %lld already refined.",
			local_eid );
	local_vids[0] = tet->local_vids[0];
	local_vids[1] = tet->local_vids[1];
	local_vids[2] = tet->local_vids[2];
	local_vids[3] = tet->local_vids[3];

	local_vids[4] = _h5t_bisect_edge( f, 0, local_eid );
	local_vids[5] = _h5t_bisect_edge( f, 2, local_eid );
	local_vids[6] = _h5t_bisect_edge( f, 3, local_eid );
	local_vids[7] = _h5t_bisect_edge( f, 1, local_eid );
	local_vids[8] = _h5t_bisect_edge( f, 4, local_eid );
	local_vids[9] = _h5t_bisect_edge( f, 5, local_eid );

	/* 
	   add new tets
	*/
	h5_id_t new_tet_local_vids[4];

	/* 0 */
	new_tet_local_vids[0] = local_vids[0];
	new_tet_local_vids[1] = local_vids[6];  // (03)
	new_tet_local_vids[2] = local_vids[5];  // (02)
	new_tet_local_vids[3] = local_vids[4];  // (01)
	TRY ( local_child_eid = _h5t_store_tet (
		      f, local_eid, new_tet_local_vids ) );

	/* 1 */
	new_tet_local_vids[0] = local_vids[4];  // (01)
	new_tet_local_vids[1] = local_vids[8];  // (13)
	new_tet_local_vids[2] = local_vids[7];  // (12)
	new_tet_local_vids[3] = local_vids[1];
	TRY ( _h5t_store_tet ( f, local_eid, new_tet_local_vids ) );

	/* 2 */
	new_tet_local_vids[0] = local_vids[5];  // (02)
	new_tet_local_vids[1] = local_vids[9];  // (23)
	new_tet_local_vids[2] = local_vids[2];
	new_tet_local_vids[3] = local_vids[7];  // (12)
	TRY ( _h5t_store_tet ( f, local_eid, new_tet_local_vids ) );

	/* 3 */
	new_tet_local_vids[0] = local_vids[6];  // (03)
	new_tet_local_vids[1] = local_vids[3];
	new_tet_local_vids[2] = local_vids[9];  // (23)
	new_tet_local_vids[3] = local_vids[8];  // (13)
	TRY ( _h5t_store_tet ( f, local_eid, new_tet_local_vids ) );

	/* 4 */
	new_tet_local_vids[0] = local_vids[6];  // (03)
	new_tet_local_vids[1] = local_vids[5];  // (02)
	new_tet_local_vids[2] = local_vids[4];  // (01)
	new_tet_local_vids[3] = local_vids[8];  // (13)
	TRY ( _h5t_store_tet ( f, local_eid, new_tet_local_vids ) );

	/* 5 */
	new_tet_local_vids[0] = local_vids[5];  // (02)
	new_tet_local_vids[1] = local_vids[4];  // (01)
	new_tet_local_vids[2] = local_vids[8];  // (13)
	new_tet_local_vids[3] = local_vids[7];  // (12)
	TRY ( _h5t_store_tet ( f, local_eid, new_tet_local_vids ) );

	/* 6 */
	new_tet_local_vids[0] = local_vids[6];  // (03)
	new_tet_local_vids[1] = local_vids[5];  // (02)
	new_tet_local_vids[2] = local_vids[9];  // (23)
	new_tet_local_vids[3] = local_vids[8];  // (13)
	TRY ( _h5t_store_tet ( f, local_eid, new_tet_local_vids ) );

	/* 7 */
	new_tet_local_vids[0] = local_vids[5];  // (02)
	new_tet_local_vids[1] = local_vids[9];  // (23)
	new_tet_local_vids[2] = local_vids[8];  // (13)
	new_tet_local_vids[3] = local_vids[7];  // (12)
	TRY ( _h5t_store_tet ( f, local_eid, new_tet_local_vids ) );

	t->elems.tets[local_eid].global_child_eid = local_child_eid;
	t->elems_ldta[local_eid].local_child_eid = local_child_eid;
	t->num_elems_on_level[t->cur_level]--;

	return local_child_eid;
}
  
