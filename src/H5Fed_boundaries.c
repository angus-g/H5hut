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
#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"
#include "H5Fed.h"

h5_err_t
H5FedAddBoundary (
	h5_file_t * const f
	) {
	SET_FNAME ( __func__ );
	return h5t_open_boundary ( f, -1 );
}

h5_err_t
H5FedOpenBoundary (
	h5_file_t * const f,
	const h5_id_t boundary_id
	) {
	SET_FNAME ( __func__ );
	return h5t_open_boundary ( f, boundary_id );
}

h5_err_t
H5FedCloseBoundary (
	h5_file_t * const f
	) {
	SET_FNAME ( __func__ );
	return h5t_close_boundary ( f );
}

h5_err_t
H5FedAddNumBoundaryfaces (
	h5_file_t * const f,
	const h5_id_t num_boundaryfaces
	) {
	SET_FNAME ( __func__ );
	return h5t_add_num_boundaryfaces ( f, num_boundaryfaces );
}

h5_err_t
H5FedStoreBoundaryface (
	h5_file_t *f,
	h5_id_t *global_vids
	) {
	SET_FNAME ( __func__ );
	return h5t_store_boundaryface ( f, global_vids );
}

h5_err_t
H5FedStoreBoundaryfaceGlobalID (
	h5_file_t *f,
	h5_id_t global_fid
	) {
	SET_FNAME ( __func__ );
	return h5t_store_boundaryface_global_id ( f, global_fid );
}

h5_err_t
H5FedStoreBoundaryfaceLocalID (
	h5_file_t *f,
	h5_id_t local_fid
	) {
	SET_FNAME ( __func__ );
	return h5t_store_boundaryface_local_id ( f, local_fid );
}

h5_err_t
H5FedStartTraverseBoundaryfaces (
	h5_file_t * const f
	) {
	SET_FNAME ( __func__ );
	return h5t_start_traverse_boundary_faces( f );
}

h5_id_t
H5FedTraverseBoundaryfaces (
	h5_file_t * const f,
	h5_id_t * const id,		/*!< OUT: global face id	*/
	h5_id_t * const parent_id,	/*!< OUT: parent id if level
					     \c >0 else \c -1		*/
	h5_id_t vertex_ids[3]		/*!< OUT: vertex id's		*/
	) {
	SET_FNAME ( __func__ );
	return h5t_traverse_boundary_faces( f, id, parent_id, vertex_ids );
}
