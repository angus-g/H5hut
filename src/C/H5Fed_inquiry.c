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
/*!
  \ingroup h5fed_c_api
  \defgroup h5fed_mesh_inquiry
*/

#include "h5core/h5_core.h"
#include "H5Fed.h"

/*!
  Get number of meshes of given type.

  \param[in]	f	File handle
  \param[in]	type_id	Type of mesh we want the number of.

  \return	Number of meshes of type \c type_id or error code.
 */
h5_size_t
H5FedGetNumMeshes ( 
	h5_file_t* const f,
	const h5_oid_t type_id
	) {
	SET_FNAME (f, __func__);
	return h5t_get_num_meshes (f, type_id);
}

/*!
  Get the number of hierarchical mesh levels.

  \param[in]	f	File handle

  \return	Number of hierarchical mesh levels or error code.
 */
h5_size_t
H5FedGetNumLevels (
	h5_file_t* const f
	) {
	SET_FNAME (f, __func__);
	return h5t_get_num_levels (f);
}

/*!
  Get current mesh levels.

  \param[in]	f	File handle

  \return	ID of current mesh levels or error code.
 */
h5_id_t
H5FedGetLevel (
	h5_file_t* const f
	) {
	SET_FNAME (f, __func__);
	return h5t_get_level (f);
}

/*!
  Returns the number of vertices used for defining the (sub-)mesh
  at current level on this compute node.

  \param[in]	f	file handle

  \return	Number of vertices or error code.
*/
h5_size_t
H5FedGetNumVertices (
	h5_file_t* const f		/*!< file handle		*/
	) {
	SET_FNAME (f, __func__);
	return h5t_get_num_vertices (f, f->myproc);
}

/*!
  Returns the number of vertices used for defining the (sub-)mesh
  at current level on compute node \c cnode.

  \param[in]	f	file handle
  \param[in]	cnode	compute node

  \return	Number of vertices or error code.
*/
h5_size_t
H5FedGetNumVerticesCnode (
	h5_file_t* const f,
	const h5_id_t cnode
	) {
	SET_FNAME (f, __func__);
	return h5t_get_num_vertices (f, cnode);
}

/*!
  Returns the number of vertices used for defining the (sub-)mesh
  at current level overl all compute nodes.

  \param[in]	f	file handle

  \return	Total number of vertices or error code.
*/
h5_size_t
H5FedGetNumVerticesTotal (
	h5_file_t* const f
	) {
	SET_FNAME (f, __func__);
	return h5t_get_num_vertices (f, -1);
}

/*!
  Returns the number of elements present in the (sub-)mesh
  at current level on this compute node.

  \param[in]	f	file handle

  \return	Number of elements or error code.
*/
h5_size_t
H5FedGetNumElements (
	h5_file_t* const f
	) {
	SET_FNAME (f, __func__);
	return h5t_get_num_elems (f, f->myproc);
}

/*!
  Returns the number of elements present in the (sub-)mesh
  at current level on compute node \c cnode.

  \param[in]	f	file handle
  \param[in]	cnode	Compute node

  \return	Number of elements or error code.
*/
h5_size_t
H5FedGetNumElementsCnode (
	h5_file_t* const f,
	const h5_id_t cnode
	) {
	SET_FNAME (f, __func__);
	return h5t_get_num_elems (f, cnode);
}
/*!
  Returns the number of elements present in the mesh
  at current level over all compute nodes.

  \param[in]	f	File handle.

  \return	Number of elements or error code.
*/
h5_size_t
H5FedGetNumElementsTotal (
	h5_file_t* const f
	) {
	SET_FNAME (f, __func__);
	return h5t_get_num_elems (f, -1);
}
