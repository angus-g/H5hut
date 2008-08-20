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
#include "h5/h5_core.h"
#include "h5/h5_private.h"
#include "H5Fed.h"

h5_size_t
H5FedGetNumMeshes (
	h5_file * f,			/*!< file handle		*/
	const enum h5_mesh_types type
	) {
	SET_FNAME ( __func__ );
	return h5t_get_num_meshes ( f, type );
}

/*!
  \ingroup h5fed_c_api

  Get the number of hierarchical mesh levels available in current step.

  \return Number of hierarchical mesh levels
  \return \c -1 on error
 */
h5_size_t
H5FedGetNumLevels (
	h5_file * fh			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return h5t_get_num_levels ( fh );
}

h5_id_t
H5FedGetLevel (
	h5_file * fh			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return h5t_get_level ( fh );
}

h5_size_t
H5FedGetNumVertices (
	h5_file * fh			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return h5t_get_num_vertices_on_level ( fh );
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
	h5_file * fh			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return h5t_get_num_vertices_on_level ( fh );
}

/*!
  \ingroup h5fed_mesh_inquiry

  Returns the number of vertices used for defining a submesh
  at level \c level for compute node \c cnode.

  \return number of vertices
q  \return \c -1	on error.
*/
h5_size_t H5FedGetNumVerticesCnode (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t cnode		/*!< compute node		*/
	) {
	return -1;
}

/******	TRIANGLE statistics routines *****************************************/

h5_size_t
H5FedGetNumTriangles (
	h5_file * fh			/*!< file handle		*/
	) {
	return h5t_get_num_entities_on_level ( fh );
}

/*!
  \ingroup h5fed_mesh_inquiry

  Get the number of triangles present in the mesh at level \c level in 
  current time step summed up over all compute notes.

  \return Number of triangles
  \return \c -1 on error.
*/
h5_size_t
H5FedGetNumTrianglesTotal (
	h5_file * fh			/*!< file handle		*/
	) {
	return h5t_get_num_entities_on_level ( fh );
}


/*!
  \ingroup h5fed_mesh_inquiry

  Get the number of triangles present in the mesh at level \c level in 
  current time step on compute node \c cnode.

  \return Number of triangles
  \return \c -1 on error.
*/
h5_size_t
H5FedGetNumTrianglesCnode (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t cnode		/*!< compute node to query	*/
	) {
	return -1;
}

/******	TETRAHEDRON statistics routines **************************************/

/*!
  \ingroup h5fed_mesh_inquiry

  Returns the number of tetrahedral elements present in the mesh at 
  level \c level in current step summed up over all compute nodes.

  \return number of tetrahedra
  \return \c -1 on error.
*/
h5_size_t
H5FedGetNumTetrahedraTotal(
	h5_file * fh			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return h5t_get_num_entities_on_level ( fh );
}

/*!
  \ingroup h5fed_mesh_inquiry

  Returns the number of tetrahedral elements present in the mesh at 
  level \c level in current step on compute nodes \c computenode.

  \return number of tetrahedra
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumTetrahedraCnode (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	) {
	return -1;
}


/******	BOUNDARY statistics routines ****************************************/


/*!
  \ingroup h5fed_mesh_inquiry

  Get the number of boundary triangles present in the mesh at
  level \c level in current step summed up over all compute nodes.

  \return number of tetrahedras
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumBoundaryTrianglesTotal (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_mesh_inquiry

  Get the number of boundary triangles present in the mesh at
  level \c level in current step on compute node \c cnode.

  \return number of tetrahedras
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumBoundaryTrianglesCnode (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t cnode		/*!< compute node		*/
	) {
	return -1;
}
