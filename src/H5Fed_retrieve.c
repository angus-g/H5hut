/******	RETRIEVAL routines **************************************************/
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

#include <stdarg.h>
#include <hdf5.h>
#include "h5/h5_core.h"
#include "h5/h5_private.h"
#include "H5Fed.h"

h5_err_t
H5FedStartTraverseVertices (
	h5_file * f			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return h5t_start_traverse_vertices ( f );
}

/*!
  \ingroup h5fed_c_api

  Get coordinates of next vertex.

  \return local id
  \return error code (H5_ERR_NOENT means no more vertices on this level)
 
 */
h5_err_t
H5FedTraverseVertices (
	h5_file * f,			/*!< file handle		*/
	h5_id_t	* const id,		/*!< OUT: global id		*/
	h5_float64_t P[3]		/*!< OUT: coordinates		*/
	) {
	SET_FNAME ( __func__ );
	return h5t_traverse_vertices ( f, id, P );
}



h5_err_t
H5FedStartTraverseTriangles (
	h5_file * f			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return h5t_start_traverse_triangles ( f );
}

h5_err_t
H5FedTraverseTriangles (
	h5_file * f,			/*!< file handle		*/
	h5_id_t * const id,		/*!< OUT: global tetrahedron id	*/
	h5_id_t * const parent_id,	/*!< OUT: parent id if level
					     \c >0 else \c -1		*/
	h5_id_t vertex_ids[3]		/*!< OUT: vertex id's		*/
	) {
	SET_FNAME ( __func__ );
	return h5t_traverse_triangles ( f, id, parent_id, vertex_ids );
}


h5_err_t
H5FedStartTraverseTetrahedra (
	h5_file * f			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return h5t_start_traverse_tets ( f );
}


/*!
  \ingroup h5fed_c_api

  Get the definition of a specific tetrahedron \c tetra_id, i.e. 
  a 4-tuple containing the specific indices of the 3-dimensional vertex
  coordinates.

  \return pointer to 4-tuple of vertex id's defining the tetrahedron.
  \return NULL-pointer on error.
*/
h5_err_t
H5FedTraverseTetrahedra (
	h5_file * f,			/*!< file handle		*/
	h5_id_t * const id,		/*!< OUT: global tetrahedron id	*/
	h5_id_t * const parent_id,	/*!< OUT: parent id if level
					     \c >0 else \c -1		*/
	h5_id_t vertex_ids[4]		/*!< OUT: vertex id's		*/
	) {
	SET_FNAME ( __func__ );
	return h5t_traverse_tets ( f, id, parent_id, vertex_ids );
}


