/*
  Copyright 2006-2010
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Achim Gsell
  
  Warning
	This code is under development.
*/

#include <time.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"

h5_err_t
h5t_get_adjacencies (
	h5_file_t* const f,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_idlist_t** list
	) {
	return h5tpriv_get_adjacencies (f, entity_id, dim, list);
}

h5_err_t
h5t_release_list_of_adjacencies (
	h5_file_t* const f,
	h5_idlist_t** list
	) {
	TRY( h5priv_free_idlist (f, list) );
	return H5_SUCCESS;
}

h5_err_t
h5t_find_te2 (
	h5_file_t* const f,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx,
	h5_idlist_t** retval
	) {
    return h5tpriv_find_te2 (f,face_idx,elem_idx,retval);
}
