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

#include "H5.h"
#include "H5Fed_boundaries.h"
#include "H5Fed_map.h"

/******	General routines *****************************************************/

h5_size_t
H5FedGetNumMeshes (
	h5_file_t * f,
	const h5_oid_t type
	);

h5_err_t
H5FedOpenMesh (
	h5_file_t * f,
	const h5_id_t id,
	const h5_oid_t type
	);
h5_id_t
H5FedAddMesh (
	h5_file_t * f,
	const h5_oid_t type
	);

h5_size_t
H5FedGetNumLevels (
	h5_file_t * f
	);

h5_err_t
H5FedSetLevel (
	h5_file_t * f,
	const h5_id_t id
	);

h5_id_t
H5FedGetLevel (
	h5_file_t * f
	);

h5_id_t
H5FedAddLevel (
	h5_file_t * f
	);

h5_err_t
H5FedAddNumVertices (
	h5_file_t * f,
	const h5_size_t num
	);

h5_err_t
H5FedAddNumEntities (
	h5_file_t * f,
	const h5_size_t num
	);

/******	INQUIRY routines *****************************************************/

h5_size_t
H5GetNumNodes (
	h5_file_t * f
	);

/******	VERTEX statistics routines *******************************************/

h5_size_t
H5FedGetNumVertices (
	h5_file_t * f
	);

h5_size_t
H5FedGetNumVerticesTotal(
	h5_file_t * f
	);

h5_size_t
H5FedGetNumVerticesCnode (
	h5_file_t * f,
	const h5_id_t cnode
	);

/******	TRIANGLE statistics routines *****************************************/
h5_size_t
H5FedGetNumTriangles (
	h5_file_t * f
	);

h5_size_t
H5FedGetNumTrianglesTotal (
	h5_file_t * f
	);

h5_size_t
H5FedGetNumTrianglesCnode (
	h5_file_t * f,
	const h5_id_t cnode
	);

/******	TETRAHEDRON statistics routines **************************************/
h5_size_t
H5FedGetNumTetrahedra (
	h5_file_t * f
	);

h5_size_t
H5FedGetNumTetrahedraTotal (
	h5_file_t * f
	);

h5_size_t
H5FedGetNumTetrahedraCnode (
	h5_file_t * f,
	const h5_id_t cnode
	);

/******	STORE / RETRIEVAL routines ********************************************/


/* vertices */
h5_id_t
H5FedStoreVertex (
	h5_file_t * f,
	const h5_id_t id,
	const h5_float64_t P[3]
	);

h5_err_t
H5FedStartTraverseVertices (
	h5_file_t * f
	);

h5_id_t
H5FedTraverseVertices (
	h5_file_t * f,
	h5_id_t	* const	id,
	h5_float64_t P[3]
	);


h5_size_t
H5FedGetNumTriangles (
	h5_file_t * f
	);

h5_id_t
H5FedStoreTriangle (
	h5_file_t * f,
	const h5_id_t id,
	const h5_id_t parent_id,
	h5_id_t vertex_ids[3]
	);

h5_err_t
H5FedStartTraverseTriangles (
	h5_file_t * f
	);

h5_id_t
H5FedTraverseTriangles (
	h5_file_t * f,
	h5_id_t * const id,
	h5_id_t * const parent_id,
	h5_id_t vertex_ids[3]
	);

/* tetrahedra */
h5_id_t
H5FedStoreTetrahedron (
	h5_file_t * f,
	const h5_id_t id,
	const h5_id_t parent_id,
	const h5_id_t vertex_ids[4]
	);

h5_err_t
H5FedStartTraverseTetrahedra (
	h5_file_t * f
	);

h5_id_t
H5FedTraverseTetrahedra (
	h5_file_t * f,
	h5_id_t * const id,
	h5_id_t * const parent_id,
	h5_id_t vertex_ids[4]
	);

#endif












