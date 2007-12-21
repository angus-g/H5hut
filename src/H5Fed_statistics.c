/******	VERTEX statistics routines *******************************************/
/*
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

#include <hdf5.h>
#include "h5/h5_types.h"
#include "H5Fed.h"

/*!
  \ingroup h5fed_c_api

  Get the number of vertices used for defining a submesh
  at level \c level for this compute node in current step.

  \return number of vertices
  \return \c -1	on error.
*/
h5_size_t H5FedGetNumVertices (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

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
  \ingroup h5fed_c_api

  Returns the number of vertices used for defining a submesh
  at level \c level for compute node \c cnode.

  \return number of vertices
  \return \c -1	on error.
*/
h5_size_t H5FedGetNumVerticesCnode (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t cnode		/*!< compute node		*/
	) {
	return -1;
}

/******	EDGE statistics routines ********************************************/

/*!
  \ingroup h5fed_c_api

  Returns the number of edges present in the mesh at level \c level in 
  current time step on this compute node.

  \return number of edges
  \return \c -1	on error.

  \note
  It is left to the API implementor how to make this information
  available; in general he can compute it from the definition of
  the tetrahedral mesh.

  \note
  After counting the number for the local cnode we call MPI_Gather()
  to collect the number from the other cnodes and store them into an
  array.  The inquired number will be provided from this array.
*/
h5_size_t H5FedGetNumEdgesInTetrahedralMesh (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Returns the number of edges present in the mesh at level \c level,
  in current step, summed up over all compute nodes

  \return number of edges
  \return \c -1	on error.
*/
h5_size_t H5FedGetNumEdgesInTetrahedralMeshTotal (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Returns the number of edges present in the mesh at level \c level
  on compute node \c cnode

  \return number of edges
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumEdgeInTetrahedralMeshCnode (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t cnode		/*!< compute node		*/
	) {
	return -1;
}

/******	TRIANGLE statistics routines *****************************************/

/*!
  \ingroup h5fed_c_api

  Get the number of triangles present in the mesh at level \c level in 
  current time step on this compute note.

  \return Number of triangles
  \return \c -1 on error.

  \note
  After counting the number for the local cnode we call MPI_Gather()
  to collect the number from the other cnodes and store them into an
  array.  The inquired number will be provided from this array.
*/
h5_size_t H5FedGetNumTrianglesInTetrahedralMesh (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Get the number of triangles present in the mesh at level \c level in 
  current time step summed up over all compute notes.

  \return Number of triangles
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumTrianglesInTetrahedralMeshTotal (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Get the number of triangles present in the mesh at level \c level in 
  current time step on compute node \c cnode.

  \return Number of triangles
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumTrianglesInTetrahedralMeshCnode (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t cnode		/*!< compute node		*/
	) {
	return -1;
}

/******	TETRAHEDRON statistics routines **************************************/

/*!
  \ingroup h5fed_c_api

  Returns the number of tetrahedral elements present in the mesh at 
  level \c level in current step on this compute node.

  \return number of tetrahedra
  \return \c -1 on error.

  \note
  After counting the number for the local cnode we call MPI_Gather()
  to collect the number from the other cnodes and store them into an
  array.  The inquired number will be provided from this array.
*/
h5_size_t H5FedGetNumTetrahedra (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

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
  \ingroup h5fed_c_api

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

/******	BOUNDARY statistics routines ****************************************/

/*!
  \ingroup h5fed_c_api

  Get the number of boundary triangles present in the mesh at
  level \c level in current step on this compute node.

  \return number of tetrahedras
  \return \c -1 on error.

  \note
  It is left to the API implementor how to make this information
  available. In general he can compute it from the definition
  of the tetrahedral mesh, using the adjacency relations, i.e.
  the fact that a boundary triangle has only one single adjacent
  tetrahedron.

  \note
  After counting the number for the local cnode we call MPI_Gather()
  to collect the number from the other cnodes and store them into an
  array.  The inquired number will be provided from this array.
*/
h5_size_t H5FedGetNumBoundaryTriangles (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

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
  \ingroup h5fed_c_api

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
