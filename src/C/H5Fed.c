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
#include "h5core/h5_core.h"
#include "H5Fed.h"

h5_err_t
H5FedOpenMesh (
	h5_file_t* const f,
	const h5_id_t mesh_id,
	const h5_oid_t mesh_type_id
	) {
	H5_API_ENTER3 (h5_err_t,
		       "f=0x%p, mesh_id=%lld, mesh_type_id=%d",
		       f,
		       (long long)mesh_id,
		       mesh_type_id);
	H5_API_RETURN (h5t_open_mesh (f, mesh_id, mesh_type_id));
}

h5_err_t
H5FedCloseMesh (
	h5_file_t* const f
	) {
	H5_API_ENTER1 (h5_err_t, "f=0x%p", f);
	H5_API_RETURN (h5t_close_mesh (f));
}

h5_err_t
H5FedSetLevel (
	h5_file_t* const f,
	const h5t_lvl_idx_t level_id
	) {
	H5_API_ENTER2 (h5_err_t, "f=0x%p, level_id=%d", f, level_id);
	H5_API_RETURN (h5t_set_level (f, level_id));
}

h5_err_t
H5FedLinkMeshToStep (
	h5_file_t* const f,
	const h5_id_t mesh_id
	) {
	H5_API_ENTER2 (h5_err_t, "f=0x%p, mesh_id=%lld", f, (long long)mesh_id);
	H5_API_RETURN (h5_error_not_implemented ());
}
