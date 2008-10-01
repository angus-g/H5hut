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
#include "h5_core/h5_types.h"
#include "H5Fed.h"

/******	UPWARD ADJACENCY routines *********************************************/

/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the edges that are
  adjacent to the vertex with id \c vertex_id and in \c num_adj_edges
  the number of edges to which this vertex is adjacent, i.e. the size 
  of the returned vector.

  \return n-tuple of upward adjacent edges
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetEdgesUAdjacentToVertex (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t vertex_id,	/*!< global vertex id		*/
	h5_size_t * const num_adj_edges	/*!< OUT: size of returned vector */
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the triangles that are
  adjacent to the vertex with id \c vertexid and  \c num_adj_triangles
  the number of triangle to which this vertex is adjacent, i.e. the size 
  of the returned vector.

  \return n-tuple of upward adjacent triangles.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetTrianglesUAdjacentToVertex (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t vertex_id,	/*!< global vertex id		*/
	h5_size_t * const num_adj_triangles
					/*!< OUT: size of returned vector */
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the tetrahedra that are
  adjacent to the vertex with id \c vertex_id and in
  \c num_adj_tetrahedra the number of tetrahera to which this vertex is
  adjacent, i.e. the size of the returned vector.

  \return n-tuple of upward adjacent tetrahedra.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetTetrahedrasUAdjacentToVertex (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t vertex_id,	/*!< global vertex id		*/
	h5_size_t * const num_adj_tetrahedra
					/*!< OUT: size of returned vector */
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the triangles that are
  adjacent to the edge with id \c vertex_id and in
  \c num_adj_triangles the number of triangles adjacent to the edge, i.e.
  the size of the return vector.

  \return n-tuple of upward adjacent triangles.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetTrianglesUAdjacentToEdge (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t edge_id,		/*!< global edge id		*/
	h5_size_t * const num_adj_triangles
					/*!< OUT: size of returned vector */
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the tetrahedra that
  are adjacent to the edge with id \c edge_id and in
  \c num_adj_tetrahedra the number of tetrahedra that are adjacent to
  the edge, i.e. the size of the returned vector.

  \return n-tuple of upward adjacent tetrahedra.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetTetrahedraUAdjacentToEdge (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t edge_id,		/*!< global edge id		*/
	h5_size_t * const num_adj_tets	/*!< OUT: size of returned vector */
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the tetrahedra that
  are adjacent to the triangle with id \c triangle_id and in
  \c num_adj_tetrahedra the number of tetrahedra that are adjacent to
  this triangle.

  There are only  two different cases: either the triangle is an internal,
  including interprocessor boundaries, triangle or an external triangle,
  i.e. a part of the mesh boundary; in the first case, there are \c 2
  adjacent tetrahedra, in the second case the triangle has exactly one
  sinle adjacent tetrahedron.

  \return n-tuple of upward adjacent tetrahedra.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetTetrahedraUAdjacentToTriangle (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	h5_size_t * const num_adj_tets	/*!< OUT: size of returned vector */
	) {
	return NULL;
}

/******	DOWNWARD ADJACENCY routines *********************************************/

/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the vertices that are
  adjacent to the edge with id \c vertex_id.

  \return n-tuple of downward adjacent vertices.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetVerticesDAdjacentToEdge (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query 	*/
	const h5_id_t edge_id,		/*!< global edge id		*/
	h5_size_t * const num_adj_vertices
					/*!< OUT: size of returned vector */
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the edges that are
  adjacent to the triangle with id \c triangle_id.

  \return n-tuple of downward adjacent edges.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetEdjesDAdjacentToTriangle (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query 	*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	h5_size_t * const num_adj_triangles
					/*!< OUT: size of returned vector */
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the vertices that are
  adjacent to the triangle with id \c triangle_id.

  \return n-tuple of downward adjacent vertices.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetVerticesDAdjacentToTriangle (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	h5_size_t * const num_adj_vertices
					/*!< OUT: size of returned vector */
	) {
	return NULL;
}
 
/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the triangles that are
  adjacent to the tetrahedron with id \c tet_id

  \return n-tuple of downward adjacent triangles.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetTrianglesDAdjacentToTetrahedron (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	h5_size_t * const num_adj_triangles
					/*!< OUT: size of returned vector */
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the edges that are
  adjacent to the tetrahedron with id \c tet_id.

  \return n-tuple of downward adjacent edges.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetEdjesDAdjacentToTetrahedron (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	h5_size_t * const num_adj_edges	/*!< OUT: size of returned vector */
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the vertices that are
  adjacent to the tetrahedron with id \c tet_id.

  \return n-tuple of downward adjacent vertices.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetVerticesDAdjacentToTetrahedron (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	h5_size_t * const num_adj_vertices
					/*!< OUT: size of returned vector */
	) {
	return NULL;
}
