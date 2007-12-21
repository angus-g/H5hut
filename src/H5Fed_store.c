/******	STORE routines*****************************************************/
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

h5_int_t H5FedSetNumVertices (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level			*/
	const h5_size_t num_vertices	/*!< number of verices at level
					  \c level			*/
	) {
	return -1;
}

h5_int_t H5FedSetNumTetrahedra (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level			*/
	const h5_size_t num_tet		/*!< number of tetrahedra at
					  level \c level		*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Stores the the coordinates of a specific vertex at level \c level
  with id \c vertex_id of the tetrahedral mesh.

  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_int_t H5FedStoreVertexCoordinate (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level			*/
	const h5_id_t vertex_id,	/*!< global vertex id		*/
	const h5_vertex * const vertex	/*!< 3-tuple of coordinates	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Stores the 2-tuple, that contains the specific indices describing
  an edge with id \c edge_id at level \c level of the tetrahedral mesh.

  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_int_t H5FedStoreEdge (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level			*/
	const h5_id_t edge_id,		/*!< global edge id		*/
	const h5_id_t parent_id,	/*!< parent id if level \c >0
					  else \x -1			*/
	const h5_edge * const edge	/*!< 2-tuple with vertex id's	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Stores the 3-tuple, that contains the specific indices describing a
  triangle with id \c triangle_id at level \c level of the tetrahedral mesh.

  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_int_t H5FedStoreTriangle (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level			*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	const h5_id_t parent_id,	/*!< parent id if level \c >0
					     else \x -1			*/
	const h5_triangle * const triangle/*!< 3-tuple with vertex id's	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Stores the 4-tuple, that contains the specific indices describing
  a tetrahedron with id \c tet_id at level \c level of the tetrahedral
  mesh.

  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_int_t H5FedStoreTetrahedron (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level			*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	const h5_id_t parent_id,	/*!< parent id if level \c >0
					  else \x -1			*/
	const h5_tetrahedron * const tet/*!< 4-tuple with vertex id's	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Writes the vector, that contains the specific indices that describe
  a boundary triangle \c btriangle with id \c btriangle_id at level
  \c level of the tetrahedral mesh.

  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_int_t H5FedStoreBoundaryTriangle (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level			*/
	const h5_id_t btriangle_id,	/*!< global boundary triangle id*/
	const h5_id_t parent_id,	/*!< parent id if level \c >0
					     else \c -1			*/
	const h5_triangle * const btriangle/*!< 3-tuple with vertex id's*/
	) {
	return -1;
}
