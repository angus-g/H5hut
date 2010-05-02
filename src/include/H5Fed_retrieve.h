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

h5t_entity_iterator_t*
H5FedBeginTraverseEntities (
	h5_file_t* const f,
	int codim
	);
h5_id_t
H5FedTraverseEntities (
	h5_file_t* const f,
	h5t_entity_iterator_t* iter
	);
h5_err_t
H5FedEndTraverseEntities (
	h5_file_t* const f,
	h5t_entity_iterator_t* iter
	);
h5_err_t
H5FedGetVertexCoordByIndex (
	h5_file_t* const f,
	h5_id_t vertex_index,
	h5_float64_t P[3]
	);
h5_err_t
H5FedGetVertexCoordByID (
	h5_file_t* const f,
	h5_id_t vertex_id,
	h5_float64_t P[3]
	);
h5_err_t
H5FedGetVertexIndicesOfEntity (
	h5_file_t* const f,
	h5_id_t entity_id,
	h5_id_t* const vertex_indices
	);
#endif
