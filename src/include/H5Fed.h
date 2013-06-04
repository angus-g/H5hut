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
	const char* name,
	h5t_mesh_t** mesh
	) {
	H5_API_ENTER (h5_err_t, "f=%p, name=%s, mesh=%p", f, name, mesh);
	H5_API_RETURN (h5t_open_tetrahedral_mesh (f, name, mesh));
}

static inline h5_err_t
H5FedOpenTetrahedralMeshByIndex (
	h5_file_t* const f,
	const h5_id_t idx,
	h5t_mesh_t** mesh
	) {
	H5_API_ENTER (h5_err_t, "f=%p, idx=%lld, mesh=%p", f, (long long)idx, mesh);
	H5_API_RETURN (h5t_open_tetrahedral_mesh_by_idx (f, idx, mesh));
}

static inline h5_err_t
H5FedOpenTriangleMesh (
	h5_file_t* const f,
	const char* name,
	h5t_mesh_t** mesh
	) {
	H5_API_ENTER (h5_err_t, "f=%p, name=%s, mesh=%p", f, name, mesh);
	H5_API_RETURN (h5t_open_triangle_mesh (f, name, mesh));
}

static inline h5_err_t
H5FedOpenTriangleMeshByIndex (
	h5_file_t* const f,
	const h5_id_t idx,
	h5t_mesh_t** mesh
	) {
	H5_API_ENTER (h5_err_t, "f=%p, idx=%lld, mesh=%p", f, (long long)idx, mesh);
	H5_API_RETURN (h5t_open_triangle_mesh_by_idx (f, idx, mesh));
}

static inline h5_err_t
H5FedCloseMesh (
	h5t_mesh_t* const m
	) {
	H5_API_ENTER (h5_err_t, "m=%p", m);
	H5_API_RETURN (h5t_close_mesh (m));
}

static inline h5_err_t
H5FedSetLevel (
	h5t_mesh_t* const m,
	const h5t_lvl_idx_t level_id
	) {
	H5_API_ENTER (h5_err_t, "m=%p, level_id=%d", m, level_id);
	H5_API_RETURN (h5t_set_level (m, level_id));
}

static inline h5_err_t
H5FedLinkMeshToStep (
	h5_file_t* const m,
	const h5_id_t mesh_id
	) {
	H5_API_ENTER (h5_err_t, "m=%p, mesh_id=%lld", m, (long long)mesh_id);
	H5_API_RETURN (h5_error_not_implemented ());
}

#ifdef __cplusplus
}
#endif

#endif
