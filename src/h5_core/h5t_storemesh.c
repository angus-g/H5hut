#include <hdf5.h>
#include "h5_core/h5_core.h"
#include "h5_core/h5_types_private.h"
#include "h5_core/h5t_types_private.h"
#include "h5_core/h5_core_private.h"

h5_id_t
h5t_add_level (
	h5_file_t * const f
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

	return _h5t_alloc_num_vertices ( f, num_vertices );
}

h5_id_t
h5t_store_vertex (
	h5_file_t * const f,		/*!< file handle		*/
	const h5_id_t global_id,       	/*!< global vertex id or -1	*/
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

	/*
	  check id
	*/
	if ( (t->cur_level == 0) && (
		     (global_id < 0) || (global_id >= t->num_vertices[0]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( f, "vertex", global_id );
	}
	if ( (t->cur_level > 0) && (
		     (global_id <  t->num_vertices[t->cur_level-1]) ||
		     (global_id >= t->num_vertices[t->cur_level]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( f, "vertex", global_id );
	}

	h5_id_t local_id = ++t->last_stored_vid;
	h5_vertex_t *vertex = &t->vertices[local_id];
	vertex->id = global_id;
	memcpy ( &vertex->P, P, sizeof ( vertex->P ) );

	_h5_insert_idmap ( f, &t->map_vertex_g2l, global_id, local_id );
	
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
		sizeof_lelem = sizeof ( t->elems_ldta.tets[0] );
		break;
	case H5_OID_TRIANGLE:
		sizeof_elem = sizeof ( t->elems.tris[0] );
		sizeof_lelem = sizeof ( t->elems_ldta.tris[0] );
		break;
	default:
		return -1;
	}

	t->elems.data = realloc (
		t->elems.data, new_num_elems * sizeof_elem );
	if ( t->elems.data == NULL ) {
		return H5_ERR_NOMEM;
	}

	t->elems_ldta.data = realloc (
		t->elems_ldta.data, new_num_elems*sizeof_lelem );
	if ( t->elems_ldta.data == NULL ) {
		return H5_ERR_NOMEM;
	}
	memset (
		t->elems_ldta.data+cur_num_elems*sizeof_lelem,
		-1,
		(new_num_elems-cur_num_elems) * sizeof_lelem );

	return  _h5_alloc_idmap ( f, &t->map_elem_g2l, new_num_elems );
}

h5_err_t
_h5t_add_num_elements (
	h5_file_t * const f,
	const h5_size_t num
	) {
	struct h5t_fdata *t = f->t;

	size_t cur_num_elems = t->cur_level > 0 ?
		t->num_elems[t->cur_level-1] : 0;
	size_t new_num_elems = t->cur_level > 0 ?
		num + t->num_elems[t->cur_level-1] : num;
	t->num_elems[t->cur_level] = new_num_elems;

	t->num_elems_on_level[t->cur_level] = t->cur_level > 0 ?
		num + t->num_elems_on_level[t->cur_level-1] : num;

	return _h5t_alloc_num_elems ( f, cur_num_elems, new_num_elems );
}

h5_err_t
h5t_add_num_elements (
	h5_file_t * const f,
	const h5_size_t num
	) {
	TRY( _h5t_add_num_vertices ( f, num+3 ) );
	TRY( _h5t_add_num_elements ( f, num ) );

	return H5_SUCCESS;
}

h5_id_t
h5t_store_element (
	h5_file_t * const f,
	const h5_id_t local_parent_cid,	/*!< local parent id of element
					     if level \c >0 else \c -1	*/
	const h5_id_t local_vids[]	/*!< local vertex id's	*/
	) {
	struct h5t_fdata *t = f->t;

	/* level set? */
	if ( t->cur_level < 0 )
		return _h5t_error_undef_level( f );

	/*  more than allocated? */
	if ( t->last_stored_eid+1 >= t->num_elems[t->cur_level] ) 
		return HANDLE_H5_OVERFLOW_ERR(
			f, _h5t_map_oid2str(t->type),
			t->num_elems[t->cur_level] );

	/* check parent id */
	if (
		( t->cur_level == 0 && local_parent_cid != -1 ) ||
		( t->cur_level >  0 && local_parent_cid < 0 ) ||
		( t->cur_level >  0
		  && local_parent_cid >= t->num_elems[t->cur_level-1] )
		) {
		return HANDLE_H5_PARENT_ID_ERR (
			f,
			_h5t_map_oid2str(t->type),
			local_parent_cid );
	}
	
	h5_id_t local_id = ++t->last_stored_eid;

	h5_tetrahedron_t *tet = &t->elems.tets[local_id];
	tet->id = global_id;
	tet->parent_id = parent_id;
	tet->refined_on_level = -1;
	tet->unused = 0;

	memcpy ( &tet->vids, vids, sizeof ( tet->vids ) );

	_h5t_sort_global_vids ( f, tet->vids, 4 );
	_h5_insert_idmap ( f, &t->map_elem_g2l, global_id, local_id );

	if ( parent_id >= 0 ) {
		h5_id_t local_parent_id = _h5_search_idmap (
			&t->map_elem_g2l, parent_id );
		if ( t->elems.tets[local_parent_id].refined_on_level < 0 ) {
			t->elems.tets[local_parent_id].refined_on_level =
				t->cur_level;
			t->num_elems_on_level[t->cur_level]--;
		}
	}
	return local_id;

}

h5_id_t
_h5t_store_tet (
	h5_file_t * const f,
	const h5_id_t parent_id,	/*!< global parent id
					     if level \c >0 else \c -1	*/
	const h5_id_t vids[4]	/*!< tuple with vertex id's	*/
	) {

	struct h5t_fdata *t = f->t;

	/*
	  more than allocated
	*/
	if ( t->last_stored_eid+1 >= t->num_elems[t->cur_level] ) 
		return HANDLE_H5_OVERFLOW_ERR(
			f, "tet", t->num_elems[t->cur_level] );

	/*
	  level not set
	 */
	if ( t->cur_level < 0 )
		return _h5t_error_undef_level( f );

	/*
	  check parent id
	*/
	if ( (t->cur_level == 0) &&
	     (parent_id != -1) ) {
		return HANDLE_H5_PARENT_ID_ERR ( f, "tet", global_id, parent_id );
	} 
	if ( (t->cur_level >  0) &&
	     (parent_id < 0) ) {
		return HANDLE_H5_PARENT_ID_ERR ( f, "tet", global_id, parent_id );
	}
	if ( (t->cur_level >  0) &&
	     (parent_id >= t->num_elems[t->cur_level-1]) ) {
		return HANDLE_H5_PARENT_ID_ERR ( f, "tet", global_id, parent_id );
	}
	/*
	  check id
	*/
	if ( (t->cur_level == 0) && (
		     (global_id < 0) || (global_id >= t->num_elems[0]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( f, "tet", global_id );
	}
	if ( (t->cur_level > 0) && (
		     (global_id <  t->num_elems[t->cur_level-1]) ||
		     (global_id >= t->num_elems[t->cur_level]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( f, "tet", global_id );
	}
	
	h5_id_t local_id = ++t->last_stored_eid;
	h5_tetrahedron_t *tet = &t->elems.tets[local_id];
	tet->id = global_id;
	tet->parent_id = parent_id;
	tet->refined_on_level = -1;
	tet->unused = 0;

	memcpy ( &tet->vids, vids, sizeof ( tet->vids ) );

	_h5t_sort_global_vids ( f, tet->vids, 4 );
	_h5_insert_idmap ( f, &t->map_elem_g2l, global_id, local_id );

	if ( parent_id >= 0 ) {
		h5_id_t local_parent_id = _h5_search_idmap (
			&t->map_elem_g2l, parent_id );
		if ( t->elems.tets[local_parent_id].refined_on_level < 0 ) {
			t->elems.tets[local_parent_id].refined_on_level =
				t->cur_level;
			t->num_elems_on_level[t->cur_level]--;
		}
	}
	return local_id;
}


h5_id_t
h5t_store_triangle (
	h5_file_t * const f,
	const h5_id_t parent_id,	/*!< global parent id
					     if level \c >0 else \c -1	*/
	const h5_id_t vids[3]	/*!< tuple with vertex id's	*/
	) {

	struct h5t_fdata *t = f->t;

	/*
	  more than allocated
	*/
	if ( t->last_stored_eid+1 >= t->num_elems[t->cur_level] ) 
		return HANDLE_H5_OVERFLOW_ERR(
			f, "triangle", t->num_elems[t->cur_level] );

	/*
	  missing call to add the first level
	 */
	if ( t->cur_level < 0 )
		return _h5t_error_undef_level( f );

	/*
	  check parent id
	*/
	if ( (t->cur_level == 0) && (parent_id != -1) ) {
		return HANDLE_H5_PARENT_ID_ERR (
			f, "triangle", global_id, parent_id );
	} 
	if ( (t->cur_level >  0) && (parent_id < 0) ) {
		return HANDLE_H5_PARENT_ID_ERR (
			f, "triangle", global_id, parent_id );
	}
	if ( (t->cur_level>0) && (parent_id >= t->num_elems[t->cur_level-1]) ) {
		return HANDLE_H5_PARENT_ID_ERR (
			f, "triangle", global_id, parent_id );
	}
	/*
	  check id
	*/
	if ( (t->cur_level == 0) && (
		     (global_id < 0) || (global_id >= t->num_elems[0]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( f, "triangle", global_id );
	}
	if ( (t->cur_level > 0) && (
		     (global_id <  t->num_elems[t->cur_level-1]) ||
		     (global_id >= t->num_elems[t->cur_level]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( f, "triangle", global_id );
	}
	
	h5_id_t local_id = ++t->last_stored_eid;
	h5_triangle_t *tri = &t->elems.tris[local_id];
	tri->id = global_id;
	tri->parent_id = parent_id;
	tri->refined_on_level = -1;
	memcpy ( &tri->vids, vids, sizeof ( tri->vids ) );

	_h5_insert_idmap ( f, &t->map_elem_g2l, global_id, local_id );

	if ( parent_id >= 0 ) {
		h5_id_t local_parent_id = _h5_search_idmap (
			&t->map_elem_g2l, parent_id );
		if ( t->elems.tris[local_parent_id].refined_on_level < 0 ) {
			t->elems.tris[local_parent_id].refined_on_level =
				t->cur_level;
			t->num_elems_on_level[t->cur_level]--;
		}
	}
	return local_id;
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

	/*
	  Get number of vertices on this node. This will be used as local id.
	  Get number of vertices. This will be used as (temporary) global id.
	  Compute vertice
	  if new vertice: store
	*/
	local_id = h5t_get_num_vertices( f, f->myproc, -1 );
	h5_vertex_t *vertex = &t->vertices[local_id];
	vertex->id = h5t_get_num_vertices( f, -1, -1 );
	vertex->P[0] = ( P0[0] + P1[0] ) / 2.0;
	vertex->P[1] = ( P0[1] + P1[1] ) / 2.0;
	vertex->P[2] = ( P0[2] + P1[2] ) / 2.0;

	if ( _h5t_get_local_vid( f, vertex->P ) == H5_ERR ) {
		t->num_vertices[t->cur_level]++;
	}

	return local_id;
}

/*!
  Refine tetrahedron \c global_tid

  \return Local id of first new tetrahedron or \c -1
*/
h5_id_t
h5t_refine_tet (
	h5_file_t * const f,
	const h5_id_t parent_tet_global_id
	) {
	struct h5t_fdata *t = f->t;
	h5_id_t local_vids[6];
	h5_id_t parent_tet_local_id;
	TRY( parent_tet_local_id = h5t_map_global_eid2local(
		     f, parent_tet_global_id ) );

	local_vids[0] = _h5t_bisect_edge(
		f,
		t->elems_ldta.tets[parent_tet_local_id].vids[0],
		t->elems_ldta.tets[parent_tet_local_id].vids[1] );
	local_vids[1] = _h5t_bisect_edge(
		f,
		t->elems_ldta.tets[parent_tet_local_id].vids[0],
		t->elems_ldta.tets[parent_tet_local_id].vids[2] );
	local_vids[2] = _h5t_bisect_edge(
		f,
		t->elems_ldta.tets[parent_tet_local_id].vids[0],
		t->elems_ldta.tets[parent_tet_local_id].vids[3] );
	local_vids[3] = _h5t_bisect_edge(
		f,
		t->elems_ldta.tets[parent_tet_local_id].vids[1],
		t->elems_ldta.tets[parent_tet_local_id].vids[2] );
	local_vids[4] = _h5t_bisect_edge(
		f,
		t->elems_ldta.tets[parent_tet_local_id].vids[1],
		t->elems_ldta.tets[parent_tet_local_id].vids[3] );
	local_vids[5] = _h5t_bisect_edge(
		f,
		t->elems_ldta.tets[parent_tet_local_id].vids[2],
		t->elems_ldta.tets[parent_tet_local_id].vids[3] );

	/* 
	   add new tets
	*/
	h5_id_t new_tet_local_vids[4];

	/* 0 */
	h5_id_t new_tet_local_id = h5t_get_num_elems( f, f->myproc, -1 );
	new_tet_local_vids[0] = t->elems_ldta.tets[parent_tet_local_id].vids[0];
	new_tet_local_vids[1] = local_vids[0];  // (01)
	new_tet_local_vids[2] = local_vids[1];  // (02)
	new_tet_local_vids[3] = local_vids[2];  // (03)
	TRY2( h5t_store_tet (
		f,
		new_tet_local_id,
		parent_tet_global_id,
		new_tet_local_vids ), fail );

	/* 1 */
	new_tet_local_id = h5t_get_num_elems( f, f->myproc, -1 );
	new_tet_local_vids[0] = t->elems_ldta.tets[parent_tet_local_id].vids[1];
	new_tet_local_vids[1] = local_vids[0];  // (01)
	new_tet_local_vids[2] = local_vids[3];  // (12)
	new_tet_local_vids[3] = local_vids[4];  // (13)
	TRY2( h5t_store_tet (
		f,
		new_tet_local_id,
		parent_tet_global_id,
		new_tet_local_vids ), fail );

	/* 2 */
	new_tet_local_id = h5t_get_num_elems( f, f->myproc, -1 );
	new_tet_local_vids[0] = t->elems_ldta.tets[parent_tet_local_id].vids[2];
	new_tet_local_vids[1] = local_vids[1];  // (02)
	new_tet_local_vids[2] = local_vids[3];  // (12)
	new_tet_local_vids[3] = local_vids[5];  // (23)
	TRY2( h5t_store_tet (
		f,
		new_tet_local_id,
		parent_tet_global_id,
		new_tet_local_vids ), fail );

	/* 3 */
	new_tet_local_id = h5t_get_num_elems( f, f->myproc, -1 );
	new_tet_local_vids[0] = t->elems_ldta.tets[parent_tet_local_id].vids[3];
	new_tet_local_vids[1] = local_vids[2];  // (03)
	new_tet_local_vids[2] = local_vids[4];  // (13)
	new_tet_local_vids[3] = local_vids[5];  // (23)
	TRY2( h5t_store_tet (
		f,
		new_tet_local_id,
		parent_tet_global_id,
		new_tet_local_vids ), fail );

	/* 4 */
	new_tet_local_id = h5t_get_num_elems( f, f->myproc, -1 );
	new_tet_local_vids[0] = local_vids[0];  // (01)
	new_tet_local_vids[1] = local_vids[1];  // (02)
	new_tet_local_vids[2] = local_vids[2];  // (03)
	new_tet_local_vids[3] = local_vids[4];  // (13)
	TRY2( h5t_store_tet (
		f,
		new_tet_local_id,
		parent_tet_global_id,
		new_tet_local_vids ), fail );

	/* 5 */
	new_tet_local_id = h5t_get_num_elems( f, f->myproc, -1 );
	new_tet_local_vids[0] = local_vids[0];  // (01)
	new_tet_local_vids[1] = local_vids[1];  // (02)
	new_tet_local_vids[2] = local_vids[3];  // (12)
	new_tet_local_vids[3] = local_vids[4];  // (13)
	TRY2( h5t_store_tet (
		f,
		new_tet_local_id,
		parent_tet_global_id,
		new_tet_local_vids ), fail );

	/* 6 */
	new_tet_local_id = h5t_get_num_elems( f, f->myproc, -1 );
	new_tet_local_vids[0] = local_vids[1];  // (02)
	new_tet_local_vids[1] = local_vids[2];  // (03)
	new_tet_local_vids[2] = local_vids[4];  // (13)
	new_tet_local_vids[3] = local_vids[5];  // (23)
	TRY2( h5t_store_tet (
		f,
		new_tet_local_id,
		parent_tet_global_id,
		new_tet_local_vids ), fail );

	/* 7 */
	new_tet_local_id = h5t_get_num_elems( f, f->myproc, -1 );
	new_tet_local_vids[0] = local_vids[1];  // (02)
	new_tet_local_vids[1] = local_vids[3];  // (12)
	new_tet_local_vids[2] = local_vids[4];  // (13)
	new_tet_local_vids[3] = local_vids[5];  // (23)
	TRY2( h5t_store_tet (
		f,
		new_tet_local_id,
		parent_tet_global_id,
		new_tet_local_vids ), fail );

	return H5_SUCCESS;

fail:
	return H5_ERR;
}
  
