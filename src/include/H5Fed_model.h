/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5FED_MODEL_H
#define __H5FED_MODEL_H

#include "h5core/h5_types.h"
#include "h5core/h5.h"
#include "h5core/h5_debug.h"
#include "h5core/h5t_model.h"

#ifdef __cplusplus
extern "C" {
#endif

/*!
  Get number of meshes of given type.

  \param[in]	f	File handle
  \param[in]	type_id	Type of mesh we want the number of.

  \return	Number of meshes of type \c type_id or error code.
 */
static inline h5_ssize_t
H5FedGetNumTetrahedralMeshes ( 
	const h5_file_t f
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5t_get_num_tetmeshes (f));
}

static inline h5_ssize_t
H5FedGetNumTriangleMeshes ( 
	const h5_file_t f
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5t_get_num_trimeshes (f));
}

/*!
  Get the number of hierarchical mesh levels.

  \param[in]	f	File handle

  \return	Number of hierarchical mesh levels or error code.
 */
static inline h5_ssize_t
H5FedGetNumLevels (
	h5t_mesh_t* const m
	) {
	H5_API_ENTER (h5_ssize_t, "m=%p", m);
	H5_API_RETURN (h5t_get_num_leaf_levels (m));
}

/*!
  Get current mesh levels.

  \param[in]	f	File handle

  \return	ID of current mesh levels or error code.
 */
static inline h5_lvl_idx_t
H5FedGetLevel (
	h5t_mesh_t* const m
	) {
	H5_API_ENTER (h5_lvl_idx_t, "m=%p", m);
	H5_API_RETURN (h5t_get_level (m));
}

/*!
  Returns the number of vertices used for defining the (sub-)mesh
  at current level on this compute node.

  \param[in]	f	file handle

  \return	Number of vertices or error code.
*/
static inline h5_ssize_t
H5FedGetNumVertices (
	h5t_mesh_t* const m		/*!< file handle		*/
	) {
	H5_API_ENTER (h5_ssize_t, "m=%p", m);
	H5_API_RETURN (h5t_get_num_vertices (m, -1));
}

/*!
  Returns the number of vertices used for defining the (sub-)mesh
  at current level on compute node \c cnode.

  \param[in]	f	file handle
  \param[in]	cnode	compute node

  \return	Number of vertices or error code.
*/
static inline h5_ssize_t
H5FedGetNumVerticesCnode (
	h5t_mesh_t* const m,
	const int cnode
	) {
	H5_API_ENTER (h5_ssize_t, "m=%p, cnode=%d", m, cnode);
	H5_API_RETURN (h5t_get_num_vertices (m, cnode));
}

/*!
  Returns the number of vertices used for defining the (sub-)mesh
  at current level overl all compute nodes.

  \param[in]	f	file handle

  \return	Total number of vertices or error code.
*/
static inline h5_ssize_t
H5FedGetNumVerticesTotal (
	h5t_mesh_t* const m
	) {
	H5_API_ENTER (h5_ssize_t, "m=%p", m);
	H5_API_RETURN (h5t_get_num_vertices (m, -1));
}

/*!
  Returns the number of elements present in the (sub-)mesh
  at current level on this compute node.

  \param[in]	f	file handle

  \return	Number of elements or error code.
*/
static inline h5_ssize_t
H5FedGetNumElements (
	h5t_mesh_t* const m
	) {
	H5_API_ENTER (h5_ssize_t, "m=%p", m);
	H5_API_RETURN (h5t_get_num_leaf_elems (m, -1));
}

/*!
  Returns the number of elements present in the (sub-)mesh
  at current level on compute node \c cnode.

  \param[in]	f	file handle
  \param[in]	cnode	Compute node

  \return	Number of elements or error code.
*/
static inline h5_ssize_t
H5FedGetNumElementsCnode (
	h5t_mesh_t* const m,
	const int cnode
	) {
	H5_API_ENTER (h5_ssize_t, "m=%p, cnode=%d", m, cnode);
	H5_API_RETURN (h5t_get_num_leaf_elems (m, cnode));
}

/*!
  Returns the number of elements present in the mesh
  at current level over all compute nodes.

  \param[in]	f	File handle.

  \return	Number of elements or error code.
*/
static inline h5_ssize_t
H5FedGetNumElementsTotal (
	h5t_mesh_t* const m
	) {
	H5_API_ENTER (h5_ssize_t, "m=%p", m);
	H5_API_RETURN (h5t_get_num_leaf_elems (m, -1));
}

#ifdef __cplusplus
}
#endif

#endif
