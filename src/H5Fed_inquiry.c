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
#include "h5/h5.h"
#include "h5/h5_private.h"
#include "H5Fed.h"

/*!
  \ingroup h5fed_mesh_inquiry

  Check whether a tetrahedral mesh hierarchy exists on level \c level

  \return  \c 1		tetrahedral mesh hierarchy exists on given level. 
  \return  \c 0		tetrahedral mesh exists, but not on given level.
  \return  \c -1	tetrahedral mesh doesn't exist
 */
h5_int_t H5FedHasTetrahedralMesh (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to check	*/
	) {
	SET_FNAME ( __func__ );
	return -1;
}

/*!
  \ingroup h5fed_mesh_inquiry

  Check whether a triangle mesh hierarchy exists on level \c level

  \return \c 1		boundary mesh hierarchy exists on given level.
  \return \c 0		boundary mesh hierarchy exists, but not on given level.
  \return \c -1		boundary mesh doesn't exist.
*/
h5_int_t H5FedHasTriangleMesh(
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to check	*/
	) {
	SET_FNAME ( __func__ );
	return -1;
}

h5_size_t
H5FedGetNumMeshes (
	h5_file * f			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return H5t_get_num_meshes ( f );
}

/*!
  \ingroup h5fed_c_api

  Get the number of hierarchical mesh levels available in current step.

  \return Number of hierarchical mesh levels
  \return \c -1 on error
 */
h5_size_t
H5FedGetNumRefinementLevels (
	h5_file * f			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return H5t_get_num_levels ( f );
}

/*!
  \ingroup h5fed_mesh_inquiry

  Get the total number of vertices used for defining a submesh
  at level \c level in current step, summed up over all compute nodes.

  \return number of vertices
  \return \c -1	on error.
*/
h5_size_t H5FedGetNumVerticesTotal(
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	) {
	return -1;
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
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t cnode		/*!< compute node		*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Get the number of vertices used for defining a submesh
  at level \c level for this compute node in current step.

  \return number of vertices
  \return \c -1	on error.
*/
h5_size_t
H5FedGetNumVertices (
	h5_file * f			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return H5t_get_num_vertices ( f );
}

/******	EDGE statistics routines ********************************************/


/*!
  \ingroup h5fed_mesh_inquiry

  Returns the number of edges present in the mesh at level \c level,
  in current step, summed up over all compute nodes

  \return number of edges
  \return \c -1	on error.
*/
h5_size_t H5FedGetNumEdgesTotal (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_mesh_inquiry

  Returns the number of edges present in the mesh at level \c level
  on compute node \c cnode

  \return number of edges
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumEdgesCnode (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t cnode		/*!< compute node		*/
	) {
	return -1;
}

/******	TRIANGLE statistics routines *****************************************/

/*!
  \ingroup h5fed_mesh_inquiry

  Get the number of triangles present in the mesh at level \c level in 
  current time step summed up over all compute notes.

  \return Number of triangles
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumTrianglesTotal (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_mesh_inquiry

  Get the number of triangles present in the mesh at level \c level in 
  current time step on compute node \c cnode.

  \return Number of triangles
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumTrianglesCnode (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t cnode		/*!< compute node		*/
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
h5_size_t H5FedGetNumTetrahedraTotal(
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	) {
	return -1;
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
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t cnode		/*!< compute node		*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Returns the number of tetrahedral elements present in the mesh at 
  level \c level in current step on this compute node.

  \return number of tetrahedra
  \return \c -1 on error.
*/
h5_size_t
H5FedGetNumTetrahedra (
	h5_file * f			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return H5t_get_num_tets ( f );
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
