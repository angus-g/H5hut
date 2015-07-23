/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5T_ADJACENCIES_PRIVATE_H
#define __H5T_ADJACENCIES_PRIVATE_H

#include "h5core/h5_types.h"
#include "h5t_types_private.h"
#include "h5_debug_private.h"

struct h5t_adjacency_methods {
	h5_err_t (*get_adjacencies)(
	        h5t_mesh_t* const,
	        const h5_loc_id_t, const h5_int32_t, h5_loc_idlist_t**);
};

extern struct h5t_adjacency_methods h5tpriv_trim_adjacency_methods;
extern struct h5t_adjacency_methods h5tpriv_tetm_adjacency_methods;

static inline h5_err_t
h5tpriv_get_adjacencies (
        h5t_mesh_t* const m,
        const h5_loc_id_t entity_id,
        const h5_int32_t dim,
        h5_loc_idlist_t** list
        ) {
	H5_PRIV_API_ENTER (h5_err_t,
	                   "m=%p, entity_id=%lld, dim=%d, list=%p",
	                   m, (long long)entity_id, dim, list);
	if (m->methods->adjacency == NULL) {
		H5_PRIV_API_LEAVE (h5_error_internal ());
	}
	H5_PRIV_API_RETURN (m->methods->adjacency->get_adjacencies(
	                            m, entity_id, dim, list));
}


#endif
