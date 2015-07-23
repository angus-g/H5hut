/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5FED_H
#define __H5FED_H

#include "H5Fed_adjacency.h"
#include "H5Fed_model.h"
#include "H5Fed_retrieve.h"
#include "H5Fed_store.h"
#include "H5Fed_tags.h"

#ifdef __cplusplus
extern "C" {
#endif

/******	General routines *****************************************************/

static inline h5_err_t
H5FedOpenTetrahedralMesh (
	const h5_file_t f,
	const char* name,
	h5t_mesh_t** mesh
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name=%s, mesh=%p", 
                      (h5_file_p)f, name, mesh);
	H5_API_RETURN (h5t_open_tetrahedral_mesh (f, name, mesh));
}

static inline h5_err_t
H5FedOpenTetrahedralMeshByIndex (
	const h5_file_t f,
	const h5_id_t idx,
	h5t_mesh_t** mesh
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, idx=%lld, mesh=%p",
                      (h5_file_p)f, (long long)idx, mesh);
	H5_API_RETURN (h5t_open_tetrahedral_mesh_by_idx (f, idx, mesh));
}

static inline h5_err_t
H5FedOpenTriangleMesh (
	const h5_file_t f,
	const char* name,
	h5t_mesh_t** mesh
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name=%s, mesh=%p",
                      (h5_file_p)f, name, mesh);
	H5_API_RETURN (h5t_open_triangle_mesh (f, name, mesh));
}

static inline h5_err_t
H5FedOpenTriangleMeshPart (
        const h5_file_t f,
        const char* name,
        h5t_mesh_t** mesh,
        h5_glb_idx_t* const elem_indices,
        const h5_glb_idx_t num_elems
        ) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name=%s, mesh=%p",
                      (h5_file_p)f, name, mesh);
	H5_API_RETURN (h5t_open_triangle_mesh_part (f, name, mesh, elem_indices, num_elems));
}

static inline h5_err_t
H5FedOpenTriangleMeshByIndex (
	const h5_file_t f,
	const h5_id_t idx,
	h5t_mesh_t** mesh
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, idx=%lld, mesh=%p",
                      (h5_file_p)f, (long long)idx, mesh);
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
        const h5_lvl_idx_t level_id
        ) {
	H5_API_ENTER (h5_err_t, "m=%p, level_id=%d", m, level_id);
	H5_API_RETURN (h5t_set_level (m, level_id));
}

static inline h5_err_t
H5FedSetMeshChanged (
        h5t_mesh_t* const m
        ) {
	H5_API_ENTER (h5_err_t, "m=%p", m);
	H5_API_RETURN (h5t_set_mesh_changed (m));
}

#ifdef __cplusplus
}
#endif

#endif












