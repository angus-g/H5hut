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
#include "H5Fed_store.h"

/******	General routines *****************************************************/

h5_size_t
H5FedGetNumMeshes (
	h5_file_t * const f,
	const h5_oid_t mesh_type_id
	);

h5_err_t
H5FedOpenMesh (
	h5_file_t * const f,
	const h5_id_t mesh_id,
	const h5_oid_t mesh_type_id
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

h5_size_t
H5GetNumNodes (
	h5_file_t * f
	);

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

h5_size_t
H5FedGetNumElements (
	h5_file_t * f
	);

h5_size_t
H5FedGetNumElementsTotal (
	h5_file_t * f
	);

h5_size_t
H5FedGetNumElementsCnode (
	h5_file_t * f,
	const h5_id_t cnode
	);

/******	STORE / RETRIEVAL routines ********************************************/

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

h5_err_t
H5FedEndTraverseVertices (
	h5_file_t * f
	);

h5_err_t
H5FedStartTraverseElements (
	h5_file_t * f
	);

h5_id_t
H5FedTraverseElements (
	h5_file_t * f,
	h5_id_t * const id,
	h5_id_t * const parent_id,
	h5_id_t * const vertex_ids
	);

h5_err_t
H5FedEndTraverseElements (
	h5_file_t * f
	);

#endif












