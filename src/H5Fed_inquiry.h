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
#ifndef __H5FED_INQUIRY_H
#define __H5FED_INQUIRY_H

h5_size_t
H5FedGetNumMeshes (
	h5_file_t * const f,
	const h5_oid_t mesh_type_id
	);

h5_size_t
H5FedGetNumLevels (
	h5_file_t * f
	);

h5_id_t
H5FedGetLevel (
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
#endif
