/*
  Copyright 2007-2008
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Benedikt Oswald;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Achim Gsell
  
  Warning
	This code is under development.
 
 */

#include <stdarg.h>
#include <hdf5.h>
#include "h5/h5_core.h"
#include "h5/h5_private.h"
#include "H5Fed.h"

h5_err_t
H5FedOpenMesh (
	h5_file * f,
	const h5_id_t id,
	const enum h5_mesh_types type
	) {
	SET_FNAME ( __func__ );
	return h5t_open_mesh ( f, id, type );
}

h5_id_t
H5FedAddMesh (
	h5_file * f,
	const enum h5_mesh_types type
	) {
	SET_FNAME ( __func__ );
	return h5t_open_mesh ( f, -1, type );
}

h5_err_t
H5FedSetLevel (
	h5_file * f,
	const h5_id_t id
	) {
	SET_FNAME ( __func__ );
	return h5t_open_level ( f, id );
}
