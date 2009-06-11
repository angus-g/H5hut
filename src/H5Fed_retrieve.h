/*
  Copyright 2007-2009
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Benedikt Oswald;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Achim Gsell
  
  Warning
	This code is under development.
 
 */
#ifndef __H5FED_RETRIEVE_H
#define __H5FED_RETRIEVE_H

h5_err_t
H5FedBeginTraverseVertices (
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
H5FedBeginTraverseElements (
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
