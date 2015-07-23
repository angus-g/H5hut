/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "h5t_types_private.h"
#include "h5t_errorhandling_private.h"
#include "h5t_core_private.h"
#include "h5t_map_private.h"
#include "h5t_model_private.h"
#include "h5t_access_private.h"
#include "h5t_tags_private.h"
#include "h5t_retrieve_private.h"

static h5_err_t
init_entity_iterator (
        h5t_iterator_t* iter,
        const int codim
        ) {
	h5t_leaf_iterator_t* it = (h5t_leaf_iterator_t*)iter;
	switch (h5tpriv_ref_elem_get_dim (it) - codim) {
	case 0: // iterate vertices
		it->find = h5tpriv_find_tv2;
		break;
	case 1: // iterate edges
		it->find = h5tpriv_find_te2;
		break;
	case 2: // iterate elems
		it->find = NULL;
		break;
	default:
		return h5_error_internal ();
	}
	return H5_SUCCESS;
}

struct h5t_retrieve_methods h5tpriv_trim_retrieve_methods = {
	init_entity_iterator
};
