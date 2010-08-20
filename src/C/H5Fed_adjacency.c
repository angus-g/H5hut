/*
  Copyright 2006-2010
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Benedikt Oswald;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Achim Gsell
  
  Warning
	This code is under development.
 
 */

#include "h5core/h5_core.h"
#include "H5Fed.h"

/******	UPWARD ADJACENCY routines *********************************************/

/*!
  \return	number of upward adjacent edges 
 */
h5_err_t
H5FedGetAdjacencies (
	h5_file_t* const f,
	const h5_id_t entity_id,
	const h5_int32_t dim,
	h5_idlist_t** list
	) {
	SET_FNAME (f, __func__);
	return h5t_get_adjacencies (f, entity_id, dim, list);
}

h5_err_t
H5FedReleaseListOfAdjacencies (
	h5_file_t* const f,
	h5_idlist_t** list
	) {
	SET_FNAME (f, __func__);
	return h5priv_free_idlist (f, list);
}
