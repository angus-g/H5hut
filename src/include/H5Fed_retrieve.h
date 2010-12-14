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

h5t_iterator_t*
H5FedBeginTraverseEntities (
	h5_file_t* const f,
	int codim
	);
h5t_iterator_t*
H5FedBeginTraverseBoundaryFaces (
	h5_file_t* const f,
	int codim
	);
h5_loc_id_t
H5FedTraverseEntities (
	h5_file_t* const f,
	h5t_iterator_t* iter
	);
h5_err_t
H5FedEndTraverseEntities (
	h5_file_t* const f,
	h5t_iterator_t* iter
	);
h5_err_t
H5FedGetVertexCoordsByIndex (
	h5_file_t* const f,
	h5_loc_idx_t vertex_index,
	h5_float64_t P[3]
	);
h5_err_t
H5FedGetVertexCoordsByID (
	h5_file_t* const f,
	h5_loc_id_t vertex_id,
	h5_float64_t P[3]
	);

h5_err_t
H5FedGetVertexIndicesOfEdge (
	h5_file_t* const f,
	h5_loc_id_t entity_id,
	h5_loc_idx_t* const vertex_indices
	);

h5_err_t
H5FedGetVertexIndicesOfTriangle (
	h5_file_t* const f,
	h5_loc_id_t entity_id,
	h5_loc_idx_t* const vertex_indices
	);

h5_err_t
H5FedGetVertexIndicesOfTriangleCClockwise (
	h5_file_t* const f,
	h5_loc_id_t entity_id,
	h5_loc_idx_t* const vertex_indices
	);

h5_err_t
H5FedGetVertexIndicesOfTet (
	h5_file_t* const f,
	h5_loc_id_t entity_id,
	h5_loc_idx_t* const vertex_indices
	);

h5_err_t
H5FedGetVertexIndicesOfEntity (
	h5_file_t* const f,
	h5_loc_id_t entity_id,
	h5_loc_idx_t* const vertex_indices
	);
#endif
