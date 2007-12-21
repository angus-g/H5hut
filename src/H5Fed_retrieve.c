/******	RETRIEVAL routines **************************************************/
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

  Get coordinates of the vertex \c vertex_id.

  \return pointer to 3-dimensional coordinates.
  \return NULL-pointer on error.
 
  \note
  Not all vertices are needed on all compute nodes, i.e. in order to
  guarantee optimum memory usage, the implementor of the API must make sure
  that the access to a desired vertex is carried out efficiently.

  Effectively, this means that the application programmer on a particular
  compute node loops over the tetrahedra that have been assigned to this
  particular compute node, using the API routine \c H5FedGetTetrahedron.

  Then, a specific tetrahedron is defined by indices into the set of vertex
  coordinates.
 
  Therefore, the application programmer wants to access these coordinates,
  and he calls the API routine \c H5FedGetVertexCoordinate.
 
  To achieve this behavior, it is required that the mesh be partitioned
  before any access to vertex coordinates or topological entities are
  carried out.
 */
h5_vertex * H5FedGetVertexCoordinate (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level			*/
	const h5_id_t vertex_id		/*!< vertex id			*/
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Get the definition of a specific edge \edge_id, i.e. a 2-tuple
  containing the specific indices of the 3-dimensional vertex
  coordinates.

  \return pointer to 2-tuple of vertex id's defining the edge.
  \return NULL-pointer on error.
*/
h5_edge * H5FedGetEdge (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level			*/
	const h5_id_t edge_id,		/*!< global edge id		*/
	h5_id_t * const parent_id	/*!< OUT: parent id if level
					     \c >0 else \c -1		*/
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Get the definition of a specific triangle \triangle_id, i.e. 
  a 3-tuple containing the specific indices of the 3-dimensional vertex
  coordinates.

  \return pointer to 3-tuple of vertex id's defining the triangle.
  \return NULL-pointer on error.
*/
h5_triangle * H5FedGetTriangle (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level			*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	h5_id_t * parent_id		/*!< OUT: parent id if level
					     \c >0 else \c -1		*/
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Get the definition of a specific tetrahedron \c tetra_id, i.e. 
  a 4-tuple containing the specific indices of the 3-dimensional vertex
  coordinates.

  \return pointer to 4-tuple of vertex id's defining the tetrahedron.
  \return NULL-pointer on error.
*/
h5_tetrahedron * H5FedGetTetrahedron (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	h5_id_t * parent_id		/*!< OUT: parent id if level
					     \c >0 else \c -1		*/
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Get the definition of the boundary triangle \c triangle_id at level
  \c level, i.e. the indices of the 3-dimensional vertex coordinates.

  \return pointer to 3-tuple of vertex id's defining the boundary triangle.
  \return NULL-pointer on error.
*/
h5_triangle * H5FedGetBoundaryTriangle (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t triangle_id,	/*!< global btriangle_id	*/
	h5_id_t * parent_id		/*!< OUT: parent id if level
					     \c >0 else \c -1		*/
	) {
	return NULL;
}
