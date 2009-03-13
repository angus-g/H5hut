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
	const h5_size_t num_elems,
	const h5_oid_t mesh_type
	) {
	h5_id_t mesh_id = 0;

	TRY( (mesh_id = h5t_open_mesh ( f, -1, mesh_type )) ); 
	TRY( h5t_add_level ( f, num_elems+3, num_elems ) );
	
	f->t->mesh_changed = 1;

	return mesh_id;
}


h5_id_t
h5t_add_level (
	h5_file_t * const f,
	const h5_size_t num_vertices,
	const h5_size_t num_elems
	) {
	struct h5t_fdata *t = f->t;

	if ( f->mode == H5_O_RDONLY ) {
		return H5_ERR_INVAL;
	}

	/* t->num_levels will be set to zero on file creation(!) */
	if ( t->num_levels == -1 ) {	/* unknown number of levels	*/
		/* determine number of levels */
		return -1;		/* not implemented		*/
	}
	t->cur_level = t->num_levels++;
	t->dsinfo_num_vertices.dims[0] = t->num_levels;
	t->dsinfo_num_elems.dims[0] = t->num_levels;
	t->dsinfo_num_elems_on_level.dims[0] = t->num_levels;

	ssize_t num_bytes = t->num_levels*sizeof ( h5_size_t );
	t->num_vertices = realloc ( t->num_vertices, num_bytes );
	t->num_vertices[t->cur_level] = -1;

	t->num_elems = realloc ( t->num_elems, num_bytes );
	t->num_elems[t->cur_level] = -1;
	t->num_elems_on_level = realloc ( t->num_elems_on_level, num_bytes );
	t->num_elems_on_level[t->cur_level] = -1;

	t->new_level = t->cur_level;
	if ( t->cur_level == 0 ) {
		/* nothing stored yet */
		t->last_stored_vid = -1;
		t->last_stored_eid = -1;
	}

	TRY( _h5t_add_num_vertices ( f, num_vertices ) );
	TRY( _h5t_add_num_elems ( f, num_elems ) );

	return t->cur_level;
}

h5_err_t
_h5t_alloc_num_vertices (
	h5_file_t * const f,
	const h5_size_t num_vertices
	) {
	struct h5t_fdata *t = f->t;

	ssize_t num_bytes = num_vertices*sizeof ( t->vertices[0] );
	h5_debug ( f, "Allocating %ld bytes.", num_bytes ); 
	t->vertices = realloc (	t->vertices, num_bytes );
	if ( t->vertices == NULL ) {
		return HANDLE_H5_NOMEM_ERR ( f );
	}

	TRY( _h5_alloc_idmap ( f, &t->map_vertex_g2l, num_vertices ) );
	TRY( _h5_alloc_smap ( f, &t->sorted_lvertices, num_vertices ) );

	return H5_SUCCESS;
}

h5_err_t
_h5t_add_num_vertices (
	h5_file_t * const f,
	const h5_size_t num
	) {
	struct h5t_fdata *t = f->t;

	if ( t->cur_level < 0 ) {
		return _h5t_error_undef_level( f );
	}
	ssize_t num_vertices = (t->cur_level > 0 ?
			     t->num_vertices[t->cur_level-1] + num : num);
	t->num_vertices[t->cur_level] = num_vertices;
	t->dsinfo_vertices.dims[0] = num_vertices;
	return _h5t_alloc_num_vertices ( f, num_vertices );
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

	h5_id_t local_id = ++t->last_stored_vid;
	h5_vertex_t *vertex = &t->vertices[local_id];
	vertex->vid = global_vid;	/* global id from mesher not yet handled! */
	vertex->vid = local_id;		/* serial case: global id == local id */
	memcpy ( &vertex->P, P, sizeof ( vertex->P ) );

	return local_id;
}

h5_err_t
_h5t_alloc_num_elems (
	h5_file_t * const f,
	const size_t cur_num_elems,
	const size_t new_num_elems
	) {
	struct h5t_fdata *t = f->t;
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
		return -1;
	}

	t->elems.data = realloc (
		t->elems.data, new_num_elems * sizeof_elem );
	if ( t->elems.data == NULL ) {
		return H5_ERR_NOMEM;
	}

	t->elems_data.data = realloc (
		t->elems_data.data, new_num_elems*sizeof_lelem );
	if ( t->elems_data.data == NULL ) {
		return H5_ERR_NOMEM;
	}
	memset (
		t->elems_data.data+cur_num_elems*sizeof_lelem,
		-1,
		(new_num_elems-cur_num_elems) * sizeof_lelem );

	return  _h5_alloc_idmap ( f, &t->map_elem_g2l, new_num_elems );
}

h5_err_t
_h5t_add_num_elems (
	h5_file_t * const f,
	const h5_size_t num
	) {
	struct h5t_fdata *t = f->t;

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
	struct h5t_fdata *t = f->t;

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
		return -1;
	}
}

h5_id_t
_h5t_store_tet (
	h5_file_t * const f,
	const h5_id_t local_parent_eid,	/*!< local parent id
					     if level \c >0 else \c -1	*/
	const h5_id_t vids[4]	/*!< tuple with local vertex id's	*/
	) {

	struct h5t_fdata *t = f->t;
	
	h5_id_t local_eid = ++t->last_stored_eid;
	h5_tetrahedron_t *tet = &t->elems.tets[local_eid];
	tet->eid = local_eid;
	tet->parent_eid = local_parent_eid;
	tet->refined_on_level = -1;

	memcpy ( &tet->vids, vids, sizeof ( tet->vids ) );

	_h5t_sort_local_vids ( f, tet->vids, 4 );

	if ( local_parent_eid >= 0 ) {
		if ( t->elems.tets[local_parent_eid].refined_on_level < 0 ) {
			t->elems.tets[local_parent_eid].refined_on_level =
				t->cur_level;
			t->num_elems_on_level[t->cur_level]--;
		}
	}
	return local_eid;
}


h5_id_t
_h5t_store_triangle (
	h5_file_t * const f,
	const h5_id_t local_parent_eid,
	const h5_id_t vids[3]
	) {

	struct h5t_fdata *t = f->t;
	
	h5_id_t local_eid = ++t->last_stored_eid;
	h5_triangle_t *tri = &t->elems.tris[local_eid];
	tri->eid = local_eid;
	tri->parent_eid = local_parent_eid;
	tri->refined_on_level = -1;

	memcpy ( &tri->vids, vids, sizeof ( tri->vids ) );

	if ( local_parent_eid >= 0 ) {
		if ( t->elems.tris[local_parent_eid].refined_on_level < 0 ) {
			t->elems.tris[local_parent_eid].refined_on_level =
				t->cur_level;
			t->num_elems_on_level[t->cur_level]--;
		}
	}
	return local_eid;
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
	h5_id_t local_id = -1;
	h5_float64_t *P0 = t->vertices[local_vid0].P;
	h5_float64_t *P1 = t->vertices[local_vid1].P;

	local_id = ++t->last_stored_vid;
	h5_vertex_t *vertex = &t->vertices[local_id];
	vertex->vid = local_id;
	vertex->P[0] = ( P0[0] + P1[0] ) / 2.0;
	vertex->P[1] = ( P0[1] + P1[1] ) / 2.0;
	vertex->P[2] = ( P0[2] + P1[2] ) / 2.0;

	if ( _h5t_get_local_vid( f, vertex->P ) < 0 ) {
		t->num_vertices[t->cur_level]++;
	}

	return local_id;
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
		return -1;
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
	return -1;
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
	struct h5t_fdata *t = f->t;
	h5_id_t local_vids[6];

	local_vids[0] = _h5t_bisect_edge(
		f,
		t->elems_data.tets[local_eid].vids[0],
		t->elems_data.tets[local_eid].vids[1] );
	local_vids[1] = _h5t_bisect_edge(
		f,
		t->elems_data.tets[local_eid].vids[0],
		t->elems_data.tets[local_eid].vids[2] );
	local_vids[2] = _h5t_bisect_edge(
		f,
		t->elems_data.tets[local_eid].vids[0],
		t->elems_data.tets[local_eid].vids[3] );
	local_vids[3] = _h5t_bisect_edge(
		f,
		t->elems_data.tets[local_eid].vids[1],
		t->elems_data.tets[local_eid].vids[2] );
	local_vids[4] = _h5t_bisect_edge(
		f,
		t->elems_data.tets[local_eid].vids[1],
		t->elems_data.tets[local_eid].vids[3] );
	local_vids[5] = _h5t_bisect_edge(
		f,
		t->elems_data.tets[local_eid].vids[2],
		t->elems_data.tets[local_eid].vids[3] );

	/* 
	   add new tets
	*/
	h5_id_t new_tet_local_vids[4];

	/* 0 */
	h5_id_t new_tet_local_id = h5t_get_num_elems( f, f->myproc, -1 );
	new_tet_local_vids[0] = t->elems_data.tets[local_eid].vids[0];
	new_tet_local_vids[1] = local_vids[0];  // (01)
	new_tet_local_vids[2] = local_vids[1];  // (02)
	new_tet_local_vids[3] = local_vids[2];  // (03)
	TRY2( _h5t_store_tet (
		f,
		new_tet_local_id,
		new_tet_local_vids ), fail );

	/* 1 */
	new_tet_local_id = h5t_get_num_elems( f, f->myproc, -1 );
	new_tet_local_vids[0] = t->elems_data.tets[local_eid].vids[1];
	new_tet_local_vids[1] = local_vids[0];  // (01)
	new_tet_local_vids[2] = local_vids[3];  // (12)
	new_tet_local_vids[3] = local_vids[4];  // (13)
	TRY2( _h5t_store_tet (
		f,
		new_tet_local_id,
		new_tet_local_vids ), fail );

	/* 2 */
	new_tet_local_id = h5t_get_num_elems( f, f->myproc, -1 );
	new_tet_local_vids[0] = t->elems_data.tets[local_eid].vids[2];
	new_tet_local_vids[1] = local_vids[1];  // (02)
	new_tet_local_vids[2] = local_vids[3];  // (12)
	new_tet_local_vids[3] = local_vids[5];  // (23)
	TRY2( _h5t_store_tet (
		f,
		new_tet_local_id,
		new_tet_local_vids ), fail );

	/* 3 */
	new_tet_local_id = h5t_get_num_elems( f, f->myproc, -1 );
	new_tet_local_vids[0] = t->elems_data.tets[local_eid].vids[3];
	new_tet_local_vids[1] = local_vids[2];  // (03)
	new_tet_local_vids[2] = local_vids[4];  // (13)
	new_tet_local_vids[3] = local_vids[5];  // (23)
	TRY2( _h5t_store_tet (
		f,
		new_tet_local_id,
		new_tet_local_vids ), fail );

	/* 4 */
	new_tet_local_id = h5t_get_num_elems( f, f->myproc, -1 );
	new_tet_local_vids[0] = local_vids[0];  // (01)
	new_tet_local_vids[1] = local_vids[1];  // (02)
	new_tet_local_vids[2] = local_vids[2];  // (03)
	new_tet_local_vids[3] = local_vids[4];  // (13)
	TRY2( _h5t_store_tet (
		f,
		new_tet_local_id,
		new_tet_local_vids ), fail );

	/* 5 */
	new_tet_local_id = h5t_get_num_elems( f, f->myproc, -1 );
	new_tet_local_vids[0] = local_vids[0];  // (01)
	new_tet_local_vids[1] = local_vids[1];  // (02)
	new_tet_local_vids[2] = local_vids[3];  // (12)
	new_tet_local_vids[3] = local_vids[4];  // (13)
	TRY2( _h5t_store_tet (
		f,
		new_tet_local_id,
		new_tet_local_vids ), fail );

	/* 6 */
	new_tet_local_id = h5t_get_num_elems( f, f->myproc, -1 );
	new_tet_local_vids[0] = local_vids[1];  // (02)
	new_tet_local_vids[1] = local_vids[2];  // (03)
	new_tet_local_vids[2] = local_vids[4];  // (13)
	new_tet_local_vids[3] = local_vids[5];  // (23)
	TRY2( _h5t_store_tet (
		f,
		new_tet_local_id,
		new_tet_local_vids ), fail );

	/* 7 */
	new_tet_local_id = h5t_get_num_elems( f, f->myproc, -1 );
	new_tet_local_vids[0] = local_vids[1];  // (02)
	new_tet_local_vids[1] = local_vids[3];  // (12)
	new_tet_local_vids[2] = local_vids[4];  // (13)
	new_tet_local_vids[3] = local_vids[5];  // (23)
	TRY2( _h5t_store_tet (
		f,
		new_tet_local_id,
		new_tet_local_vids ), fail );

	return H5_SUCCESS;

fail:
	return H5_ERR;
}
  
