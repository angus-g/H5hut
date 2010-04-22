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

#ifndef __H5FED_MAP_H
#define __H5FED_MAP_H

h5_err_t
H5FedLMapEdgeID2VertexIDs (
	h5_file_t * const f,
	h5_id_t local_id,
	h5_id_t *localvids
	);

h5_err_t
H5FedLMapTriangleID2VertexIDs (
	h5_file_t * const f,
	h5_id_t local_id,
	h5_id_t *localvids
	);

h5_err_t
H5FedLMapTetID2VertexIDs (
	h5_file_t * const f,
	h5_id_t local_id,
	h5_id_t *localvids
	);

#endif

