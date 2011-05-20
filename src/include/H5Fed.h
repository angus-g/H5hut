/*
  Header file for declaring the H5Fed application programming
  interface (API) in the C language.
  
  Copyright 2006-2007
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Benedikt Oswald;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Achim Gsell
  
  Warning
	This code is under development.
 
 */

#ifndef __H5FED_H
#define __H5FED_H

#include "H5Fed_adjacency.h"
#include "H5Fed_inquiry.h"
#include "H5Fed_retrieve.h"
#include "H5Fed_store.h"
#include "H5Fed_tags.h"

#ifdef __cplusplus
extern "C" {
#endif

/******	General routines *****************************************************/

static inline h5_err_t
H5FedOpenTetrahedralMesh (
	h5_file_t* const f,
	const h5_id_t mesh_id
	) {
	H5_API_ENTER2 (h5_err_t, "f=0x%p, mesh_id=%lld", f, (long long)mesh_id);
	H5_API_RETURN (h5t_open_tetrahedral_mesh (f, mesh_id));
}

static inline h5_err_t
H5FedOpenTriangleMesh (
	h5_file_t* const f,
	const h5_id_t mesh_id
	) {
	H5_API_ENTER2 (h5_err_t, "f=0x%p, mesh_id=%lld", f, (long long)mesh_id);
	H5_API_RETURN (h5t_open_triangle_mesh (f, mesh_id));
}

h5_err_t
H5FedCloseMesh (
	h5_file_t * const f
	);

h5_err_t
H5FedSetLevel (
	h5_file_t * f,
	const h5t_lvl_idx_t level_id
	);

h5_err_t
H5FedLinkMeshToStep (
	h5_file_t * f,
	const h5_id_t mesh_id
	);

#ifdef __cplusplus
}
#endif

#endif












