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

#include <stdarg.h>
#include <hdf5.h>
#include "h5_core/h5_core.h"
#include "H5Fed.h"

h5_size_t
H5FedGetNumMeshes (
	h5_file_t * const f,
	const h5_oid_t mesh_type_id
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_num_meshes ( f, mesh_type_id );
}

/*!
  \ingroup h5fed_c_api

  Get the number of hierarchical mesh levels available in current step.

  \return Number of hierarchical mesh levels
  \return \c -1 on error
 */
h5_size_t
H5FedGetNumLevels (
	h5_file_t * const f		/*!< file handle		*/
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_num_levels ( f );
}

h5_id_t
H5FedGetLevel (
	h5_file_t * const f		/*!< file handle		*/
	) {
	SET_FNAME ( f, __func__ );
	return h5t_get_level ( f );
}

/*!
  Get number of local vertices on current level.
*/
h5_size_t
H5FedGetNumVertices (
	h5_file_t * const f		/*!< file handle		*/
	) {
	SET_FNAME ( f, __func__ );
	h5_id_t cur_level = h5t_get_level( f );
	return h5t_get_num_vertices ( f, f->myproc, cur_level );
}

/*!
  \ingroup h5fed_mesh_inquiry

  Get the total number of vertices used for defining a submesh
  at level \c level in current step, summed up over all compute nodes.

  \return number of vertices
  \return \c -1	on error.
*/
h5_size_t
H5FedGetNumVerticesTotal(
	h5_file_t * const f			/*!< file handle		*/
	) {
	SET_FNAME ( f, __func__ );
	h5_id_t cur_level = h5t_get_level( f );
	return h5t_get_num_vertices ( f, -1, cur_level );
}

/*!
  \ingroup h5fed_mesh_inquiry

  Returns the number of vertices used for defining a submesh
  at level \c level for compute node \c cnode.

  \return number of vertices
q  \return \c -1	on error.
*/
h5_size_t H5FedGetNumVerticesCnode (
	h5_file_t * const f,		/*!< file handle		*/
	const h5_id_t cnode		/*!< compute node		*/
	) {
	SET_FNAME ( f, __func__ );
	h5_id_t cur_level = h5t_get_level( f );
	return h5t_get_num_vertices ( f, cnode, cur_level );
}

/*!
  \ingroup h5fed_mesh_inquiry

  Returns the number of tetrahedral elements present in the mesh at 
  current \c level summed up over all compute nodes.

  \return number of tetrahedra
  \return \c -1 on error.
*/
h5_size_t
H5FedGetNumElements (
	h5_file_t * const f		/*!< file handle		*/
	) {
	SET_FNAME ( f, __func__ );
	h5_id_t cur_level = h5t_get_level( f );
	return h5t_get_num_elems ( f, f->myproc, cur_level );
}

h5_size_t
H5FedGetNumElementsTotal (
	h5_file_t * const f		/*!< file handle		*/
	) {
	SET_FNAME ( f, __func__ );
	h5_id_t cur_level = h5t_get_level( f );
	return h5t_get_num_elems ( f, -1, cur_level );
}

/*!
  \ingroup h5fed_mesh_inquiry

  Returns the number of tetrahedral elements present in the mesh at 
  level \c level in current step on compute nodes \c computenode.

  \return number of tetrahedra
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumElementsCnode (
	h5_file_t * const f,		/*!< file handle		*/
	const h5_id_t cnode		/*!< compute node to query	*/
	) {
	SET_FNAME ( f, __func__ );
	h5_id_t cur_level = h5t_get_level( f );
	return h5t_get_num_elems ( f, cnode, cur_level );
}
