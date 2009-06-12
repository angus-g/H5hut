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
 Assign unique global id's to elements.

 Nothing to do in serial case.
*/
h5_err_t
_h5t_assign_global_elem_ids (
	h5_file_t * const f
	) {
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
	TRY ( t->num_elems_on_level = _h5_alloc ( f, t->num_elems_on_level, num_bytes ) );
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
	vertex->global_vid = global_vid;     /* global id from mesher,replaced later!*/
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
_h5t_alloc_num_elems (
	h5_file_t * const f,
	const size_t cur_num_elems,
	const size_t new_num_elems
	) {
	h5t_fdata_t *t = f->t;
	size_t sizeof_elem = 0;
	size_t sizeof_lelem = 0;

	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON:
		sizeof_elem = sizeof ( t->elems.tets[0] );
		sizeof_lelem = sizeof ( t->elems_data.tets[0] );
		break;
	case H5_OID_TRIANGLE:
		sizeof_elem = sizeof ( t->elems.tris[0] );
		sizeof_lelem = sizeof ( t->elems_data.tris[0] );
		break;
	default:
		return h5_error_internal (
			f, __FILE__, __func__, __LINE__ );
	}

	size_t size = new_num_elems * sizeof_elem;
	TRY ( t->elems.data = _h5_alloc ( f, t->elems.data, size ) );

	size = new_num_elems * sizeof_lelem;
	TRY ( t->elems_data.data = _h5_alloc ( f, t->elems_data.data, size ) );

	memset (
		t->elems_data.data+cur_num_elems*sizeof_lelem,
		-1,
		(new_num_elems-cur_num_elems) * sizeof_lelem );

	return  _h5_alloc_idmap ( f, &t->map_elem_g2l, new_num_elems );
}

h5_err_t
h5t_begin_store_elems (
	h5_file_t * const f,
	const h5_size_t num
	) {
	struct h5t_fdata *t = f->t;

	t->storing_data = 1;
	size_t cur_num_elems = t->cur_level > 0 ?
		t->num_elems[t->cur_level-1] : 0;
	size_t new_num_elems = t->cur_level > 0 ?
		num + t->num_elems[t->cur_level-1] : num;
	t->num_elems[t->cur_level] = new_num_elems;
	t->dsinfo_elems.dims[0] = new_num_elems;

	t->num_elems_on_level[t->cur_level] = t->cur_level > 0 ?
		num + t->num_elems_on_level[t->cur_level-1] : num;

	return _h5t_alloc_num_elems ( f, cur_num_elems, new_num_elems );
}

/*!
  \param[in] local_parent_eid	local parent id of element if level \c >0 else \c -1
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

	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON:
		return _h5t_store_tet ( f, local_parent_eid, local_vids );
	case H5_OID_TRIANGLE:
		return _h5t_store_triangle ( f, local_parent_eid, local_vids );
	default:
		return h5_error_internal (
			f, __FILE__, __func__, __LINE__ );
	}
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
	const h5_id_t local_vids[4]
	) {

	struct h5t_fdata *t = f->t;
	t->level_changed = 1;	
	h5_id_t local_eid = ++t->last_stored_eid;
	h5_tetrahedron_t *tet = &t->elems.tets[local_eid];
	h5_tet_data_t *tet_data = &t->elems_data.tets[local_eid];
	tet->global_eid = local_eid;
	tet->global_parent_eid = local_parent_eid;
	tet->global_child_eid = -1;

	memcpy ( &tet->global_vids, local_vids, sizeof ( tet->global_vids ) );
	_h5t_sort_local_vids ( f, tet->global_vids, 4 );
	memcpy ( &tet_data->local_vids, &tet->global_vids,
		 sizeof ( tet->global_vids ) );

	return local_eid;
}


h5_id_t
_h5t_store_triangle (
	h5_file_t * const f,
	const h5_id_t local_parent_eid,
	const h5_id_t vids[3]
	) {

	struct h5t_fdata *t = f->t;
	t->level_changed = 1;
	h5_id_t local_eid = ++t->last_stored_eid;
	h5_triangle_t *tri = &t->elems.tris[local_eid];
	h5_triangle_data_t *tri_data = &t->elems_data.tris[local_eid];
	tri->global_eid = local_eid;
	tri->global_parent_eid = local_parent_eid;
	tri->global_child_eid = -1;

	memcpy ( &tri->global_vids, vids, sizeof ( tri->global_vids ) );
	_h5t_sort_local_vids ( f, tri->global_vids, 3 );
	memcpy ( &tri_data->local_vids, &tri->global_vids,
		 sizeof ( tri->global_vids ) );

	return local_eid;
}

h5_err_t
h5t_end_store_elems (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	t->storing_data = 0;

	t->num_elems[t->cur_level] = t->last_stored_eid+1;
	TRY ( _h5t_assign_global_elem_ids ( f ) );
	TRY ( _h5t_sort_elems ( f ) );
	TRY ( _h5t_rebuild_global_2_local_map_of_elems ( f ) );
	TRY ( _h5t_rebuild_elems_data ( f ) );

	return H5_SUCCESS;
}


h5_err_t
h5t_begin_refine_elems (
	h5_file_t * const f,
	const h5_size_t num_elems_to_refine
	) {
	h5_size_t num_elems_to_add = 0;
	h5t_fdata_t *t = f->t;

	t->storing_data = 1;
	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON:
		num_elems_to_add = num_elems_to_refine*8;
		break;
	case H5_OID_TRIANGLE:
		num_elems_to_add = num_elems_to_refine*4;
		break;
	default:
		return h5_error_internal ( f, __FILE__, __func__, __LINE__ );
	}
	h5_size_t num_vertices = (num_elems_to_add>>2)*3; /* this is an upper limit */
	TRY ( h5t_begin_store_vertices ( f, num_vertices ) );
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
	h5_id_t	local_vid0,
	h5_id_t	local_vid1
	) {
	struct h5t_fdata *t = f->t;
	h5_id_t local_vid = -1;
	h5_float64_t *P0 = t->vertices[local_vid0].P;
	h5_float64_t *P1 = t->vertices[local_vid1].P;
	h5_float64_t P[3];

	P[0] = ( P0[0] + P1[0] ) / 2.0;
	P[1] = ( P0[1] + P1[1] ) / 2.0;
	P[2] = ( P0[2] + P1[2] ) / 2.0;

	/* add vertex if not already exist */
	if ( (local_vid = _h5t_get_local_vid( f, P )) < 0 ) {
		TRY ( local_vid = h5t_store_vertex ( f, -1, P ) );
	}
	return local_vid;
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
	struct h5t_fdata *t = f->t;
	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON:
		return _h5t_refine_tet ( f, local_eid );
	case H5_OID_TRIANGLE:
		return _h5t_refine_triangle ( f, local_eid );
	default:
		return h5_error_internal (
			f, __FILE__, __func__, __LINE__ );
	}
}

/*!
  Refine triangle \c local_eid

  \return Local id of first new triangle or \c -1
*/
h5_id_t
_h5t_refine_triangle (
	h5_file_t * const f,
	const h5_id_t local_eid
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t local_vids[3];
	h5_id_t local_child_eid;

	local_vids[0] = _h5t_bisect_edge(
		f,
		t->elems_data.tris[local_eid].local_vids[0],
		t->elems_data.tris[local_eid].local_vids[1] );
	local_vids[1] = _h5t_bisect_edge(
		f,
		t->elems_data.tris[local_eid].local_vids[0],
		t->elems_data.tris[local_eid].local_vids[2] );
	local_vids[2] = _h5t_bisect_edge(
		f,
		t->elems_data.tris[local_eid].local_vids[1],
		t->elems_data.tris[local_eid].local_vids[2] );


	h5_id_t elem_local_vids[4];

	/* 0 */
	elem_local_vids[0] = t->elems_data.tets[local_eid].local_vids[0];
	elem_local_vids[1] = local_vids[0];
	elem_local_vids[2] = local_vids[1];
	TRY ( local_child_eid = _h5t_store_triangle (
		      f,
		      local_eid, elem_local_vids ) );
	if ( local_eid >= 0 ) {
		t->elems.tris[local_eid].global_child_eid = local_child_eid;
		t->elems_data.tris[local_eid].local_child_eid = local_child_eid;
		t->num_elems_on_level[t->cur_level]--;
	}

	/* 1 */
	elem_local_vids[0] = t->elems_data.tets[local_eid].local_vids[1];
	elem_local_vids[1] = local_vids[0];
	elem_local_vids[2] = local_vids[2];
	TRY ( _h5t_store_triangle (
		      f,
		      local_eid, elem_local_vids ) );

	/* 2 */
	elem_local_vids[0] = t->elems_data.tets[local_eid].local_vids[2];
	elem_local_vids[1] = local_vids[1];
	elem_local_vids[2] = local_vids[2];
	TRY ( _h5t_store_triangle (
		      f,
		      local_eid, elem_local_vids ) );

	/* 3 */
	elem_local_vids[0] = local_vids[0];
	elem_local_vids[1] = local_vids[1];
	elem_local_vids[2] = local_vids[2];
	TRY ( _h5t_store_triangle (
		      f,
		      local_eid, elem_local_vids ) );

	return local_child_eid;
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

	if ( t->elems.tets[local_eid].global_child_eid >= 0 )
		return h5_error (
			f,
			H5_ERR_INVAL,
			"Tetrahedron %lld already refined.",
			local_eid );
	local_vids[0] = t->elems_data.tets[local_eid].local_vids[0];
	local_vids[1] = t->elems_data.tets[local_eid].local_vids[1];
	local_vids[2] = t->elems_data.tets[local_eid].local_vids[2];
	local_vids[3] = t->elems_data.tets[local_eid].local_vids[3];
	local_vids[4] = _h5t_bisect_edge(
		f,
		t->elems_data.tets[local_eid].local_vids[0],
		t->elems_data.tets[local_eid].local_vids[1] );
	local_vids[5] = _h5t_bisect_edge(
		f,
		t->elems_data.tets[local_eid].local_vids[0],
		t->elems_data.tets[local_eid].local_vids[2] );
	local_vids[6] = _h5t_bisect_edge(
		f,
		t->elems_data.tets[local_eid].local_vids[0],
		t->elems_data.tets[local_eid].local_vids[3] );
	local_vids[7] = _h5t_bisect_edge(
		f,
		t->elems_data.tets[local_eid].local_vids[1],
		t->elems_data.tets[local_eid].local_vids[2] );
	local_vids[8] = _h5t_bisect_edge(
		f,
		t->elems_data.tets[local_eid].local_vids[1],
		t->elems_data.tets[local_eid].local_vids[3] );
	local_vids[9] = _h5t_bisect_edge(
		f,
		t->elems_data.tets[local_eid].local_vids[2],
		t->elems_data.tets[local_eid].local_vids[3] );

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
	t->elems_data.tets[local_eid].local_child_eid = local_child_eid;
	t->num_elems_on_level[t->cur_level]--;

	return local_child_eid;
}
  
