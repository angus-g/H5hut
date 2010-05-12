#include <stdlib.h>
#include <string.h>
#include <hdf5.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

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
static h5_err_t
assign_global_vertex_ids (
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
assign_global_elem_ids (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5_id_t local_eid;

	if ( t->cur_level < 0 ) return H5_SUCCESS; /* no level defined */

	/*
	  simple in serial runs: global_id = local_id
	*/
	for (local_eid = (t->cur_level == 0) ? 0 : t->num_elems[t->cur_level-1];
	      local_eid < t->num_elems[t->cur_level];
	      local_eid++) {
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
		int num_vertices = t->ref_element->num_faces[0];
		for ( i = 0; i < num_vertices; i++ ) {
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
	TRY ( t->num_vertices = h5priv_alloc ( f, t->num_vertices, num_bytes ) );
	t->num_vertices[t->cur_level] = -1;

	TRY ( t->num_elems = h5priv_alloc ( f, t->num_elems, num_bytes ) );
	t->num_elems[t->cur_level] = -1;
	TRY ( t->num_elems_on_level = h5priv_alloc (
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
		return h5tpriv_error_undef_level( f );
	}
	t->storing_data = 1;
	h5_size_t cur_num_vertices = ( t->cur_level > 0 ?
				       t->num_vertices[t->cur_level-1] : 0 );
	t->num_vertices[t->cur_level] = cur_num_vertices+num;
	t->dsinfo_vertices.dims[0] = cur_num_vertices+num;
	return h5tpriv_alloc_num_vertices ( f, cur_num_vertices+num );
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
		return h5tpriv_error_undef_level( f );

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
	TRY ( assign_global_vertex_ids ( f ) );
	TRY ( h5tpriv_sort_vertices ( f ) );
	TRY ( h5tpriv_rebuild_global_2_local_map_of_vertices ( f ) );
	return H5_SUCCESS;
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
	TRY ( h5tpriv_resize_te_htab ( f, nel ) );
	return (*t->methods.store->alloc_elems) ( f, cur, new );
}


/*!
  Store element. The vertices are given via their local indices.

  \param[in]	f			File handle.
  \param[in]	elem_idx_of_parent	Local indexd of the parent element
					or \c -1.
  \param[in]	vertices		Local vertex indices defining the
					tetrahedron.
 */
h5_id_t
h5t_store_elem (
	h5_file_t* const f,
	const h5_id_t elem_idx_of_parent,
	const h5_id_t* vertices
	) {
	h5t_fdata_t *t = f->t;

	/* level set? */
	if ( t->cur_level < 0 )
		return h5tpriv_error_undef_level( f );

	/*  more than allocated? */
	if ( t->last_stored_eid+1 >= t->num_elems[t->cur_level] ) 
		return HANDLE_H5_OVERFLOW_ERR(
			f, h5tpriv_map_oid2str(t->mesh_type),
			t->num_elems[t->cur_level] );

	/* check parent id */
	if (
		( t->cur_level == 0 && elem_idx_of_parent != -1 ) ||
		( t->cur_level >  0 && elem_idx_of_parent < 0 ) ||
		( t->cur_level >  0
		  && elem_idx_of_parent >= t->num_elems[t->cur_level-1] )
		) {
		return HANDLE_H5_PARENT_ID_ERR (
			f,
			h5tpriv_map_oid2str(t->mesh_type),
			elem_idx_of_parent );
	}
	t->level_changed = 1;
	h5_id_t elem_idx = ++t->last_stored_eid;
	h5_elem_ldta_t* elem_ldta = &t->elems_ldta[elem_idx];

	elem_ldta->local_parent_eid = elem_idx_of_parent;
	elem_ldta->local_child_eid = -1;
	elem_ldta->level_id = t->cur_level;
	int num_vertices = t->ref_element->num_faces[0];
	memcpy (elem_ldta->local_vids, vertices,
		 sizeof (*vertices) * num_vertices);
	h5tpriv_sort_local_vids (f, elem_ldta->local_vids, num_vertices);
	h5_id_t face_idx;
	int num_faces = t->ref_element->num_faces[1];
	h5_idlist_t* retval;
	for (face_idx = 0; face_idx < num_faces; face_idx++) {
		// add edges to neighbour struct
		TRY( h5tpriv_search_te2 (
			      f,
			      face_idx,
			      elem_idx,
			      &retval) );
	}
	return elem_idx;
}

h5_err_t
h5t_end_store_elems (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	t->storing_data = 0;

	t->num_elems[t->cur_level] = t->last_stored_eid+1;
	TRY ( assign_global_elem_ids ( f ) );

	TRY ( h5tpriv_sort_elems ( f ) );
	TRY ( h5tpriv_rebuild_global_2_local_map_of_elems ( f ) );

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


/*!
  Refine element \c local_eid

  \return local id of first new element or \c -1
*/
h5_id_t
h5t_refine_elem (
	h5_file_t * const f,
	const h5_id_t local_eid
	) {
	return (*f->t->methods.store->refine_elem)( f, local_eid );
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





  
