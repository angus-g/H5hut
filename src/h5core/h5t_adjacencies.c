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
	h5t_mesh_t* const m,
	const h5_loc_id_t entity_id,
	const h5_int32_t dim,
	h5_loc_idlist_t** list
	) {
	H5_CORE_API_ENTER (h5_err_t,
			   "m=%p, entity_id=%llu, dim=%d, list=%p",
			   m, (long long unsigned)entity_id, dim, list);
	H5_CORE_API_RETURN (h5tpriv_get_adjacencies (m, entity_id, dim, list));
}

h5_err_t
h5t_release_list_of_adjacencies (
	h5t_mesh_t* const m,
	h5_loc_idlist_t** list
	) {
	H5_CORE_API_ENTER (h5_err_t, "m=%p, list=%p", m, list);
	UNUSED_ARGUMENT (m);
	H5_CORE_API_RETURN (h5priv_free_idlist (list));
}

h5_err_t
h5t_find_te2 (
	h5t_mesh_t* const m,
	h5_loc_idx_t face_idx,
	h5_loc_idx_t elem_idx,
	h5_loc_idlist_t** retval
	) {
	H5_CORE_API_ENTER (h5_err_t,
			   "m=%p, face_idx=%lld, elem_idx=%lld, retval=%p",
			   m,
			   (long long)face_idx,
			   (long long)elem_idx,
			   retval);
	H5_CORE_API_RETURN (h5tpriv_find_te2 (m,face_idx,elem_idx,retval));
}
