#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

h5_err_t
h5t_begin_traverse_vertices (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	h5t_vertex_iterator_t *iter = &t->iters.vertex; 

	iter->cur_vid = -1;
	return H5_SUCCESS;
}

h5_id_t
h5t_traverse_vertices (
	h5_file_t * f,			/*!< file handle		*/
	h5_id_t * const id,		/*!< OUT: global vertex id	*/
	h5_float64_t P[3]		/*!< OUT: coordinates		*/
	) {
	h5t_fdata_t *t = f->t;
	h5t_vertex_iterator_t *iter = &t->iters.vertex; 

	if ( iter->cur_vid+1 >= t->num_vertices[t->cur_level] ) {
		h5_debug ( f, "Traversing done!" );
		return H5_NOK;
	}
	h5_vertex_t *vertex = &t->vertices[++iter->cur_vid];
	*id = vertex->global_vid;
	memcpy ( P, &vertex->P, sizeof ( vertex->P ) );

	return iter->cur_vid;
}

h5_err_t
h5t_end_traverse_vertices (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	h5t_vertex_iterator_t *iter = &t->iters.vertex; 

	iter->cur_vid = -1;
	return H5_SUCCESS;
}

/*
  Test whether given element is on current level. This is the case, if
  - the level_id of the element is <= the current level
  - and, if any, the direct children is on a level > the current level
*/
h5_err_t
_h5t_elem_is_on_cur_level (
	h5_file_t * const f,
	h5_elem_ldta_t *el_dta 
	) {
	h5t_fdata_t *t = f->t;
	if ( ( el_dta->level_id > t->cur_level ) ||
	     ( (el_dta->local_child_eid >= 0) &&
	       (el_dta->local_child_eid < t->num_elems[t->cur_level]) ) ) {
		return H5_NOK;
	}
	return H5_SUCCESS;
}

/*
  Skip elements which have been refined on a level <= the current one.
*/
h5_id_t
_skip_to_next_elem_on_level (
	h5_file_t * f,
	h5_id_t *eid
	) {
	h5t_fdata_t *t = f->t;
	h5_elem_ldta_t *el_dta;

	do {
		(*eid)++;
		if ( *eid >= t->num_elems[t->cur_level] ) {
			return h5_error_internal (
				f, __FILE__, __func__, __LINE__ );
		}
		el_dta = &t->elems_ldta[*eid];
	}
	while ( _h5t_elem_is_on_cur_level ( f, el_dta ) == H5_NOK );
	return *eid;
}

h5_err_t
h5t_begin_traverse_edges (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.edge; 

	iter->cur_eid = -1;
	TRY ( _skip_to_next_elem_on_level ( f, &iter->cur_eid ) );
	iter->cur_fid = -1;
	return H5_SUCCESS;
}

h5_id_t
h5t_traverse_edges (
	h5_file_t * const f,
	h5_id_t *local_vids
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.edge; 
	h5_te_node_t *te;
	h5_size_t i;
	do {
		if ( iter->cur_fid >= 5 ) {
			if ( iter->cur_eid+1 >= t->num_elems[t->cur_level] ) {
				h5_debug ( f, "Traversing done!" );
				return H5_NOK;
			}
			TRY ( _skip_to_next_elem_on_level (
				      f, &iter->cur_eid ) );
			iter->cur_fid = 0;
		} else {
			iter->cur_fid++;
		}
		TRY ( _h5t_find_te2 (
			      f, &te, iter->cur_fid, iter->cur_eid ) );
		/* skip to first element which is on current level */
		i = -1;
		h5_elem_ldta_t *el_dta;
		do {
			i++;
			h5_id_t eid = _h5t_get_elem_id ( te->value.items[i] );
			el_dta = &t->elems_ldta[eid];
		} while ( _h5t_elem_is_on_cur_level ( f, el_dta ) == H5_NOK );
	} while ( iter->cur_eid != _h5t_get_elem_id(te->value.items[i]) );
	memcpy ( local_vids, te->key.vids, 2*sizeof(h5_id_t) );

	return te->value.items[0];
}

h5_err_t
h5t_end_traverse_edges (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.edge; 

	iter->cur_eid = 0;
	iter->cur_fid = -1;
	return H5_SUCCESS;
}

h5_err_t
h5t_begin_traverse_triangles (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.triangle; 

	iter->cur_eid = -1;
	TRY ( _skip_to_next_elem_on_level ( f, &iter->cur_eid ) );
	iter->cur_fid = -1;
	return H5_SUCCESS;
}

h5_id_t
h5t_traverse_triangles (
	h5_file_t * const f,
	h5_id_t *local_vids
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.triangle; 
	h5_td_node_t *td;
	h5_size_t i;
	do {
		if ( iter->cur_fid >= 3 ) {
			if ( iter->cur_eid+1 >= t->num_elems[t->cur_level] ) {
				h5_debug ( f, "Traversing done!" );
				return H5_NOK;
			}
			TRY ( _skip_to_next_elem_on_level (
				      f, &iter->cur_eid ) );
			iter->cur_fid = 0;
		} else {
			iter->cur_fid++;
		}
		TRY ( _h5t_find_td2 (
			      f, &td, iter->cur_fid, iter->cur_eid ) );
		/* skip to first element which is on current level */
		i = -1;
		h5_elem_ldta_t *el_dta;
		do {
			i++;
			h5_id_t eid = _h5t_get_elem_id ( td->value.items[i] );
			el_dta = &t->elems_ldta[eid];
		} while ( _h5t_elem_is_on_cur_level ( f, el_dta ) == H5_NOK );
	} while ( iter->cur_eid != _h5t_get_elem_id(td->value.items[i]) );
	memcpy ( local_vids, td->key.vids, 3*sizeof(h5_id_t) );

	return td->value.items[0];
}

h5_err_t
h5t_end_traverse_triangles (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.triangle; 

	iter->cur_eid = 0;
	iter->cur_fid = -1;
	return H5_SUCCESS;
}

h5_err_t
h5t_begin_traverse_elems (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	h5t_elem_iterator_t *iter = &t->iters.elem; 

	iter->cur_eid = -1;
	return H5_SUCCESS;
}

/*!
  \param[in]	f		file handle
  \param[out]	global_eid	Global element id
  \param[out]	local_parent_id	Local parent id
  \param[out]	vids		Local vertex id
*/
h5_id_t
h5t_traverse_elems (
	h5_file_t * const f,
	h5_id_t * const global_eid,
	h5_id_t * const local_parent_id,
	h5_id_t *local_vids
	) {
	h5t_fdata_t *t = f->t;
	h5t_elem_iterator_t *iter = &t->iters.elem; 
	h5_elem_t *elem;
	h5_elem_ldta_t *elem_data;
	h5_id_t local_child_eid;
	h5_id_t refined_on_level = -1;

	if ( iter->cur_eid+1 >= t->num_elems[t->cur_level] ) {
		h5_debug ( f, "Traversing done!" );
		return H5_NOK;
	}

	/*
	  Skip elements which have been refined on a level <= the current one.
	*/
	do {
		++iter->cur_eid;
		elem_data = &t->elems_ldta[iter->cur_eid];
		local_child_eid = elem_data->local_child_eid;
		refined_on_level = ( local_child_eid >= 0 ) ?
			t->elems_ldta[local_child_eid].level_id :
			t->cur_level+1;   /* this means "not refined" */
	}
	while ( refined_on_level <= t->cur_level );

	switch ( t->mesh_type ) {
	case H5_OID_TETRAHEDRON:
		elem = (h5_elem_t*)&t->elems.tets[iter->cur_eid];
		break;
	case H5_OID_TRIANGLE:
		elem = (h5_elem_t*)&t->elems.tris[iter->cur_eid];
		break;
	default:
		return h5_error_internal (
			f, __FILE__, __func__, __LINE__ );
	}

	if ( iter->cur_eid >= t->num_elems[t->cur_level] ) {
		return h5_error_internal (
			f, __FILE__, __func__, __LINE__ );
	}
	*global_eid = elem->global_eid;
	*local_parent_id = elem_data->local_parent_eid;
	memcpy (
		local_vids,
		elem_data->local_vids,
		sizeof ( elem_data->local_vids[0] ) * t->mesh_type );

	return iter->cur_eid;
}

h5_err_t
h5t_end_traverse_elems (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	h5t_elem_iterator_t *iter = &t->iters.elem; 
	iter->cur_eid = -1;
	return H5_SUCCESS;
}

