#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

h5_id_t
h5t_add_level (
	h5_file_t * const f
	) {
	struct h5t_fdata *t = &f->t;

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

	t->num_entities = realloc ( t->num_entities, num_bytes );
	t->num_entities[t->cur_level] = -1;
	t->num_entities_on_level = realloc ( t->num_entities_on_level, num_bytes );
	t->num_entities_on_level[t->cur_level] = -1;

	t->new_level = t->cur_level;
	if ( t->cur_level == 0 ) {
		/* nothing stored yet */
		t->last_stored_vertex_id = -1;
		t->last_stored_entity_id = -1;
	}
	return t->cur_level;
}

h5_err_t
_h5t_alloc_num_vertices (
	h5_file_t * const f,
	const h5_size_t num_vertices
	) {
	struct h5t_fdata *t = &f->t;

	ssize_t num_bytes = num_vertices*sizeof ( t->vertices[0] );
	h5_debug ( "Allocating %ld bytes.", num_bytes ); 
	t->vertices = realloc (	t->vertices, num_bytes );
	if ( t->vertices == NULL ) {
		return HANDLE_H5_NOMEM_ERR;
	}

	TRY( _h5_alloc_idmap (&t->map_vertex_g2l, num_vertices ), error_exit );
	TRY( _h5_alloc_smap (&t->sorted_lvertices, num_vertices ), error_exit );

	return H5_SUCCESS;

error_exit:
	return h5_get_errno();
}

h5_err_t
_h5t_add_num_vertices (
	h5_file_t * const f,
	const h5_size_t num
	) {
	struct h5t_fdata *t = &f->t;

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
	struct h5t_fdata *t = &f->t;

	/*
	  more than allocated
	*/
	if ( t->last_stored_vertex_id+1 >= t->num_vertices[t->cur_level] ) 
		return HANDLE_H5_OVERFLOW_ERR( "vertex",
					       t->num_vertices[t->cur_level] );

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
		return HANDLE_H5_OUT_OF_RANGE_ERR( "vertex", global_id );
	}
	if ( (t->cur_level > 0) && (
		     (global_id <  t->num_vertices[t->cur_level-1]) ||
		     (global_id >= t->num_vertices[t->cur_level]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( "vertex", global_id );
	}

	h5_id_t local_id = ++t->last_stored_vertex_id;
	h5_vertex *vertex = &t->vertices[local_id];
	vertex->id = global_id;
	memcpy ( &vertex->P, P, sizeof ( vertex->P ) );

	_h5_insert_idmap ( &t->map_vertex_g2l, global_id, local_id );
	
	return local_id;
}

h5_err_t
_h5t_alloc_num_entities (
	h5_file_t * const f,
	const size_t cur_num_entities,
	const size_t new_num_entities
	) {
	struct h5t_fdata *t = &f->t;
	size_t sizeof_entity = 0;
	size_t sizeof_lentity = 0;

	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON:
		sizeof_entity = sizeof ( t->entities.tets[0] );
		sizeof_lentity = sizeof ( t->lentities.tets[0] );
		break;
	case H5_OID_TRIANGLE:
		sizeof_entity = sizeof ( t->entities.tris[0] );
		sizeof_lentity = sizeof ( t->lentities.tris[0] );
		break;
	default:
		return -1;
	}

	t->entities.data = realloc (
		t->entities.data, new_num_entities * sizeof_entity );
	if ( t->entities.data == NULL ) {
		return H5_ERR_NOMEM;
	}

	t->lentities.data = realloc (
		t->lentities.data, new_num_entities*sizeof_lentity );
	if ( t->lentities.data == NULL ) {
		return H5_ERR_NOMEM;
	}
	memset (
		t->lentities.data+cur_num_entities*sizeof_lentity,
		-1,
		(new_num_entities-cur_num_entities) * sizeof_lentity );

	return  _h5_alloc_idmap (&t->map_entity_g2l, new_num_entities );
}

h5_err_t
h5_add_num_tets (
	h5_file_t * const f,
	const h5_size_t num
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->mesh_type != H5_OID_TETRAHEDRON ) {
		_h5t_error_illegal_object_type ( f, H5_OID_TETRAHEDRON );
	}

	TRY( _h5t_add_num_vertices ( f, num+3 ), error_exit );
	TRY( _h5t_add_num_entities ( f, num ), error_exit );
	return H5_SUCCESS;
error_exit:
	return h5_get_errno();
}

h5_err_t
h5_add_num_triangles (
	h5_file_t * const f,
	const h5_size_t num
	) {
	struct h5t_fdata *t = &f->t;

	if ( t->mesh_type != H5_OID_TRIANGLE ) {
		_h5t_error_illegal_object_type ( f, H5_OID_TRIANGLE );
	}
	TRY( _h5t_add_num_vertices ( f, num+2 ), error_exit );
	TRY( _h5t_add_num_entities ( f, num ), error_exit );
	return H5_SUCCESS;
error_exit:
	return h5_get_errno();
}

h5_err_t
_h5t_add_num_entities (
	h5_file_t * const f,
	const h5_size_t num
	) {
	struct h5t_fdata *t = &f->t;

	size_t cur_num_entities = t->cur_level > 0 ?
		t->num_entities[t->cur_level-1] : 0;
	size_t new_num_entities = t->cur_level > 0 ?
		num + t->num_entities[t->cur_level-1] : num;
	t->num_entities[t->cur_level] = new_num_entities;

	t->num_entities_on_level[t->cur_level] = t->cur_level > 0 ?
		num + t->num_entities_on_level[t->cur_level-1] : num;

	return _h5t_alloc_num_entities ( f, cur_num_entities, new_num_entities );
}

h5_id_t
h5t_store_tet (
	h5_file_t * const f,
	const h5_id_t global_id,	/*!< global tetrahedron id	*/
	const h5_id_t parent_id,	/*!< global parent id
					     if level \c >0 else \c -1	*/
	const h5_id_t vertex_ids[4]	/*!< tuple with vertex id's	*/
	) {

	struct h5t_fdata *t = &f->t;

	/*
	  more than allocated
	*/
	if ( t->last_stored_entity_id+1 >= t->num_entities[t->cur_level] ) 
		return HANDLE_H5_OVERFLOW_ERR(
			"tet", t->num_entities[t->cur_level] );

	/*
	  missing call to add the first level
	 */
	if ( t->cur_level < 0 )
		return _h5t_error_undef_level( f );

	/*
	  check parent id
	*/
	if ( (t->cur_level == 0) &&
	     (parent_id != -1) ) {
		return HANDLE_H5_PARENT_ID_ERR ( "tet", global_id, parent_id );
	} 
	if ( (t->cur_level >  0) &&
	     (parent_id < 0) ) {
		return HANDLE_H5_PARENT_ID_ERR ( "tet", global_id, parent_id );
	}
	if ( (t->cur_level >  0) &&
	     (parent_id >= t->num_entities[t->cur_level-1]) ) {
		return HANDLE_H5_PARENT_ID_ERR ( "tet", global_id, parent_id );
	}
	/*
	  check id
	*/
	if ( (t->cur_level == 0) && (
		     (global_id < 0) || (global_id >= t->num_entities[0]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( "tet", global_id );
	}
	if ( (t->cur_level > 0) && (
		     (global_id <  t->num_entities[t->cur_level-1]) ||
		     (global_id >= t->num_entities[t->cur_level]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( "tet", global_id );
	}
	
	h5_id_t local_id = ++t->last_stored_entity_id;
	h5_tetrahedron *tet = &t->entities.tets[local_id];
	tet->id = global_id;
	tet->parent_id = parent_id;
	tet->refined_on_level = -1;
	tet->unused = 0;

	memcpy ( &tet->vertex_ids, vertex_ids, sizeof ( tet->vertex_ids ) );

	_h5t_sort_global_vertex_ids ( f, tet->vertex_ids, 4 );
	_h5_insert_idmap ( &t->map_entity_g2l, global_id, local_id );

	if ( parent_id >= 0 ) {
		h5_id_t local_parent_id = _h5_search_idmap (
			&t->map_entity_g2l, parent_id );
		if ( t->entities.tets[local_parent_id].refined_on_level < 0 ) {
			t->entities.tets[local_parent_id].refined_on_level =
				t->cur_level;
			t->num_entities_on_level[t->cur_level]--;
		}
	}
	return local_id;
}


h5_id_t
h5t_store_triangle (
	h5_file_t * const f,
	const h5_id_t global_id,	/*!< global triangle id		*/
	const h5_id_t parent_id,	/*!< global parent id
					     if level \c >0 else \c -1	*/
	const h5_id_t vertex_ids[3]	/*!< tuple with vertex id's	*/
	) {

	struct h5t_fdata *t = &f->t;

	/*
	  more than allocated
	*/
	if ( t->last_stored_entity_id+1 >= t->num_entities[t->cur_level] ) 
		return HANDLE_H5_OVERFLOW_ERR(
			"triangle", t->num_entities[t->cur_level] );

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
			"triangle", global_id, parent_id );
	} 
	if ( (t->cur_level >  0) && (parent_id < 0) ) {
		return HANDLE_H5_PARENT_ID_ERR (
			"triangle", global_id, parent_id );
	}
	if ( (t->cur_level>0) && (parent_id >= t->num_entities[t->cur_level-1]) ) {
		return HANDLE_H5_PARENT_ID_ERR (
			"triangle", global_id, parent_id );
	}
	/*
	  check id
	*/
	if ( (t->cur_level == 0) && (
		     (global_id < 0) || (global_id >= t->num_entities[0]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( "triangle", global_id );
	}
	if ( (t->cur_level > 0) && (
		     (global_id <  t->num_entities[t->cur_level-1]) ||
		     (global_id >= t->num_entities[t->cur_level]) ) ) {
		return HANDLE_H5_OUT_OF_RANGE_ERR( "triangle", global_id );
	}
	
	h5_id_t local_id = ++t->last_stored_entity_id;
	h5_triangle *tri = &t->entities.tris[local_id];
	tri->id = global_id;
	tri->parent_id = parent_id;
	tri->refined_on_level = -1;
	memcpy ( &tri->vertex_ids, vertex_ids, sizeof ( tri->vertex_ids ) );

	_h5_insert_idmap ( &t->map_entity_g2l, global_id, local_id );

	if ( parent_id >= 0 ) {
		h5_id_t local_parent_id = _h5_search_idmap (
			&t->map_entity_g2l, parent_id );
		if ( t->entities.tris[local_parent_id].refined_on_level < 0 ) {
			t->entities.tris[local_parent_id].refined_on_level =
				t->cur_level;
			t->num_entities_on_level[t->cur_level]--;
		}
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
	const h5_id_t global_tid
	) {
	/* 
	   get local id of tet
	   compute vertices
	   add new vertices
	   add new tets
	*/

error_exit:
	return h5_get_errno();
}
  
