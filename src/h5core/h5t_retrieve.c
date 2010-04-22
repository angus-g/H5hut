#include <stdlib.h>
#include <string.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

/*
  Skip elements which have been refined on a level <= the current one.
*/
static h5_id_t
skip_to_next_elem_on_level (
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
	} while ( h5tpriv_elem_is_on_cur_level ( f, el_dta ) == H5_NOK );
	return *eid;
}

/*
  Test whether given element is on current level. This is the case, if
  - the level_id of the element is <= the current level
  - and, if any, the direct children is on a level > the current level
*/
h5_err_t
h5tpriv_elem_is_on_cur_level (
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

h5_err_t
h5t_begin_traverse_vertices (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.vertex; 

	iter->cur_cid = -1;
	iter->cur_eid = -1;
	TRY ( skip_to_next_elem_on_level ( f, &iter->cur_eid ) );
	return H5_SUCCESS;
}

h5_id_t
h5t_traverse_vertices (
	h5_file_t * const f,		/*!< file handle		*/
	h5_float64_t P[3]		/*!< OUT: coordinates		*/
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.vertex; 
	h5_idlist_t *tv;
	size_t i;
	do {
		if ( iter->cur_cid >= 3 ) {
			if ( iter->cur_eid+1 >= t->num_elems[t->cur_level] ) {
				h5_debug ( f, "Traversing done!" );
				return H5_NOK;
			}
			TRY ( skip_to_next_elem_on_level (
				      f, &iter->cur_eid ) );
			iter->cur_cid = 0;
		} else {
			iter->cur_cid++;
		}
		TRY ( h5tpriv_find_tv2 ( f, iter->cur_cid, iter->cur_eid, &tv ) );
		/* skip to first element which is on current level */
		i = -1;
		h5_elem_ldta_t *el_dta;
		do {
			i++;
			h5_id_t eid = h5tpriv_get_elem_idx ( tv->items[i] );
			el_dta = &t->elems_ldta[eid];
		} while ( h5tpriv_elem_is_on_cur_level ( f, el_dta ) == H5_NOK );
	} while ( iter->cur_eid != h5tpriv_get_elem_idx(tv->items[i]) );

	h5_id_t vidx = t->elems_ldta[iter->cur_eid].local_vids[iter->cur_cid];
	h5_vertex_t *vertex = &t->vertices[vidx];
	memcpy ( P, &vertex->P, sizeof ( vertex->P ) );

	return h5tpriv_build_vertex_id ( iter->cur_cid, iter->cur_eid );
}

h5_err_t
h5t_end_traverse_vertices (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.vertex; 

	iter->cur_cid = -1;
	iter->cur_eid = -1;
	return H5_SUCCESS;
}

h5_err_t
h5t_begin_traverse_edges (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.edge; 

	iter->cur_cid = -1;
	iter->cur_eid = -1;
	TRY ( skip_to_next_elem_on_level ( f, &iter->cur_eid ) );
	return H5_SUCCESS;
}

h5_id_t
h5t_traverse_edges (
	h5_file_t * const f,
	h5_id_t *local_vids
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.edge; 
	h5t_te_entry_t *te;
	h5_size_t i;
	do {
		if ( iter->cur_cid >= 5 ) {
			if ( iter->cur_eid+1 >= t->num_elems[t->cur_level] ) {
				h5_debug ( f, "Traversing done!" );
				return H5_NOK;
			}
			TRY ( skip_to_next_elem_on_level (
				      f, &iter->cur_eid ) );
			iter->cur_cid = 0;
		} else {
			iter->cur_cid++;
		}
		TRY( h5tpriv_find_te2 (f, iter->cur_cid, iter->cur_eid, &te) );
		/* skip to first element which is on current level */
		i = -1;
		h5_elem_ldta_t *el_dta;
		do {
			i++;
			h5_id_t eid = h5tpriv_get_elem_idx (te->value.items[i]);
			el_dta = &t->elems_ldta[eid];
		} while (h5tpriv_elem_is_on_cur_level (f, el_dta) == H5_NOK);
	} while ( iter->cur_eid != h5tpriv_get_elem_idx (te->value.items[i]));
	memcpy (local_vids, te->key.vids, 2*sizeof(h5_id_t));

	return te->value.items[0];
}

h5_err_t
h5t_end_traverse_edges (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.edge; 

	iter->cur_cid = -1;
	iter->cur_eid = -1;
	return H5_SUCCESS;
}

h5_err_t
h5t_begin_traverse_triangles (
	h5_file_t * const f
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.triangle; 

	iter->cur_cid = -1;
	iter->cur_eid = -1;
	TRY ( skip_to_next_elem_on_level ( f, &iter->cur_eid ) );
	return H5_SUCCESS;
}

h5_id_t
h5t_traverse_triangles (
	h5_file_t * const f,
	h5_id_t *local_vids
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.triangle; 
	h5t_td_entry_t *td;
	h5_size_t i;
	do {
		if (iter->cur_cid >= 3) {
			if (iter->cur_eid+1 >= t->num_elems[t->cur_level]) {
				h5_debug (f, "Traversing done!");
				return H5_NOK;
			}
			TRY( skip_to_next_elem_on_level (f, &iter->cur_eid) );
			iter->cur_cid = 0;
		} else {
			iter->cur_cid++;
		}
		TRY( h5tpriv_find_td2 (f, iter->cur_cid, iter->cur_eid, &td) );
		/* skip to first element which is on current level */
		i = -1;
		h5_elem_ldta_t *el_dta;
		do {
			i++;
			h5_id_t eid = h5tpriv_get_elem_idx (td->value.items[i]);
			el_dta = &t->elems_ldta[eid];
		} while (h5tpriv_elem_is_on_cur_level (f, el_dta) == H5_NOK);
	} while (iter->cur_eid != h5tpriv_get_elem_idx(td->value.items[i]));
	memcpy (local_vids, td->key.vids, 3*sizeof(h5_id_t));

	return td->value.items[0];
}


#if 0
/*!
  Function for traversing entities with 0 < co-dim < co-dim(vertex).
  In a tetrahedral mesh this means edges and triangle, in triangle
  mesh only edges.
 */
typedef struct {
	h5_id_t cur_elem_id;	// local element id
	h5_id_t	cur_face_id;	// face id according reference element
	int32_t codim;		// co-dimension of faces to traverse
	int32_t num_faces;	// number of faces
	
} h5t_entity_iterator2;

typedef struct h5t_idlisthash_key {
	h5_id_t ids[1];
} h5t_idlisthash_key_t;

/*
  List of all upward adjacent elements of same coarsness of a specific face.
  The face is specified by its local vertex IDs.
 */
typedef struct h5t_idlisthash_entry {
	h5_idlist_t value;
	h5t_idlisthash_key_t key;
} h5t_idlisthash_entry_t;

h5_id_t
h5t_traverse_faces (
	h5_file_t * const f,
	h5t_entity_iterator2_t *iter;
	) {
	h5t_fdata_t *t = f->t;
	h5t_idlisthash_entry_t *entry;
	h5_size_t i;
	do {
		if (iter->cur_fid >= iter->num_faces) {
			if (iter->cur_eid+1 >= t->num_elems[t->cur_level]) {
				h5_debug (f, "Traversing done!");
				return H5_NOK;
			}
			TRY( skip_to_next_elem_on_level (f, &iter->cur_eid) );
			iter->cur_fid = 0;
		} else {
			iter->cur_fid++;
		}
		TRY( h5tpriv_find_td2 (f, iter->cur_fid, iter->cur_eid, &entry) );
		/* skip to first element which is on current level */
		i = -1;
		h5_elem_ldta_t *el_dta;
		do {
			i++;
			h5_id_t eid = h5tpriv_get_elem_idx (td->value.items[i]);
			el_dta = &t->elems_ldta[eid];
		} while (h5tpriv_elem_is_on_cur_level (f, el_dta) == H5_NOK);
	} while (iter->cur_eid != h5tpriv_get_elem_idx(td->value.items[i]));

	return td->value.items[0];
}
#endif

h5_err_t
h5t_end_traverse_triangles (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.triangle; 

	iter->cur_cid = -1;
	iter->cur_eid = -1;
	return H5_SUCCESS;
}

h5_err_t
h5t_begin_traverse_elems (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.elem; 

	iter->cur_cid = -1;
	iter->cur_eid = -1;
	return H5_SUCCESS;
}

/*!
  \param[in]	f		file handle
  \param[out]	vids		Local vertex id
*/
h5_id_t
h5t_traverse_elems (
	h5_file_t * const f,
	h5_id_t *local_vids
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.elem; 
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

	memcpy (
		local_vids,
		elem_data->local_vids,
		sizeof ( elem_data->local_vids[0] ) * t->mesh_type );

	return h5tpriv_build_elem_id ( iter->cur_eid );
}

h5_err_t
h5t_end_traverse_elems (
	h5_file_t * f
	) {
	h5t_fdata_t *t = f->t;
	h5t_entity_iterator_t *iter = &t->iters.elem; 
	iter->cur_cid = -1;
	iter->cur_eid = -1;
	return H5_SUCCESS;
}

