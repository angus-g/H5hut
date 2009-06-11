/*
  Copyright 2007-2009
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Benedikt Oswald;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Achim Gsell
  
  Warning
	This code is under development.
 
 */
/*!
  \ingroup h5fed_c_api
  \defgroup h5fed_map
*/

#include <stdarg.h>
#include <hdf5.h>
#include "h5_core/h5_core.h"
#include "H5Fed.h"

h5_id_t
H5FedMapTet2GlobalID (
	h5_file_t * const f,
	h5_id_t * const global_vids
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_global_eid ( f, global_vids );
}


h5_id_t
H5FedMapTriangle2GlobalID (
	h5_file_t * const f,
	h5_id_t * const global_vids
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_global_triangle_id ( f, global_vids );
}

h5_err_t
H5FedMapEntityID2LocalVids (
	h5_file_t * const f,
	h5_id_t local_id,
	h5_id_t *local_vids
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_local_vids_of_entity ( f, local_id, local_vids );
}
