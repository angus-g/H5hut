/*
  Header file for declaring the H5Fed application programming
  interface (API) in the C language.
  
  Copyright 2006-2007
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Benedikt Oswald;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Benedikt Oswald, Achim Gsell
  
  Warning
	This code is under development.
 
 */

/*!
  Some conventions:
	Functions:
		Name:
			ThisIsAFunction()
		Return values:
			-1 or NULL signals an error

  \note
  In function names we use the words \b get and \b store insteed of
  \b read and \b write, because no I/O is actually done in these
  functions.
*/


#ifndef __H5FED_H
#define __H5FED_H

typedef h5_float_t	h5_vertex[3];
typedef h5_id_t		h5_edge[2];
typedef h5_id_t		h5_triangle[3];
typedef h5_id_t		h5_tetrahedron[4];

/*!
  \defgroup h5fed_c_api H5Fed C API
*/

/******	General routines *****************************************************/

/*!
  \ingroup h5fed_c_api
  
  Open file with name \c filename. This function is available in the paralell
  and serial version. In the serial case \c comm may have any value.

  \return File handle.
  \return NULL on error.

  \note
  File is always opened in read/writer mode!

  \note
  Implement as wrapper of \c H5_open_file()!
*/
h5_file* H5FedOpenFile (
	const char * filename,		/*!< file name			*/
	const MPI_Comm comm		/*!< MPI communicator		*/
	);

/*!
  \ingroup h5fed_c_api

  Close file.

  \return value \c >=0 on success
  \return -1 on error
*/
h5_int_t H5FedCloseFile (
	h5_file * fh			/*!< file handle		*/
	);

/******	INQUIRY routines *****************************************************/

/*!
  \ingroup h5_c_api

  Get the number of compute nodes.

  \return Number of compute notes.
  \return \c -1 on error.
 */
h5_int_t H5GetNumNodes (
	h5_file * fh			/*!< file handle		*/
	);

/*!
  \ingroup h5fed_c_api

  Get the number of hierarchical mesh levels available in current step.

  \return Number of hierarchical mesh levels
  \return \c -1 on error
 */
h5_int_t H5FedGetNumMeshLevels (
	h5_file * fh			/*!< file handle		*/
	);

/*!
  \ingroup h5fed_c_api

  Check whether a tetrahedral mesh hierarchy exists on level \c level

  \return  \c 1		tetrahedral mesh hierarchy exists on given level. 
  \return  \c 0		tetrahedral mesh exists, but not on given level.
  \return  \c -1	tetrahedral mesh doesn't exist
 */
h5_int_t H5FedHasTetrahedralMesh (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to check	*/
	);

/*!
  \ingroup h5fed_c_api

  Check whether a boundary mesh hierarchy exists on level \c level

  \return \c 1		boundary mesh hierarchy exists on given level.
  \return \c 0		boundary mesh hierarchy exists, but not on given level.
  \return \c -1		boundary mesh doesn't exist.
*/
h5_int_t H5FedHasBoundaryMesh(
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to check	*/
);

/******	VERTEX statistics routines *******************************************/

/*!
  \ingroup h5fed_c_api

  Get the number of vertices used for defining a submesh
  at level \c level for this compute node in current step.

  \return number of vertices
  \return \c -1	on error.
*/
h5_size_t H5FedGetNumVerticesInMesh (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	);

/*!
  \ingroup h5fed_c_api

  Get the total number of vertices used for defining a submesh
  at level \c level in current step, summed up over all compute nodes.

  \return number of vertices
  \return \c -1	on error.
*/
h5_size_t H5FedGetNumVerticesInMeshTotal(
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	);

/*!
  \ingroup h5fed_c_api

  Returns the number of vertices used for defining a submesh
  at level \c level for compute node \c cnode.

  \return number of vertices
  \return \c -1	on error.
*/
h5_size_t H5FedGetNumVerticesInMeshOnNode (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t cnode		/*!< compute node		*/
	);

/******	EDGE statistics routines ********************************************/

/*!
  \ingroup h5fed_c_api

  Returns the number of edges present in the mesh at level \c level in 
  current time step on this compute node.

  \return number of edges
  \return \c -1	on error.

  \note
  It is left to the API implementor how to make this information
  available; in general he can compute it from the definition of
  the tetrahedral mesh.

  \note
  After counting the number for the local cnode we call MPI_Gather()
  to collect the number from the other cnodes and store them into an
  array.  The inquired number will be provided from this array.
*/
h5_size_t H5FedGetNumEdgesInTetrahedralMesh (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	);

/*!
  \ingroup h5fed_c_api

  Returns the number of edges present in the mesh at level \c level,
  in current step, summed up over all compute nodes

  \return number of edges
  \return \c -1	on error.
*/
h5_size_t H5FedGetNumEdgesInTetrahedralMeshTotal (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	);

/*!
  \ingroup h5fed_c_api

  Returns the number of edges present in the mesh at level \c level
  on compute node \c cnode

  \return number of edges
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumEdgeInTetrahedralMeshOfNode (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t cnode		/*!< compute node		*/
	);

/******	TRIANGLE statistics routines *****************************************/

/*!
  \ingroup h5fed_c_api

  Get the number of triangles present in the mesh at level \c level in 
  current time step on this compute note.

  \return Number of triangles
  \return \c -1 on error.

  \note
  After counting the number for the local cnode we call MPI_Gather()
  to collect the number from the other cnodes and store them into an
  array.  The inquired number will be provided from this array.
*/
h5_size_t H5FedGetNumTrianglesInTetrahedralMesh (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	);

/*!
  \ingroup h5fed_c_api

  Get the number of triangles present in the mesh at level \c level in 
  current time step summed up over all compute notes.

  \return Number of triangles
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumTrianglesInTetrahedralMeshTotal (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	);

/*!
  \ingroup h5fed_c_api

  Get the number of triangles present in the mesh at level \c level in 
  current time step on compute node \c cnode.

  \return Number of triangles
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumTrianglesInTetrahedralMeshOnNode (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t cnode		/*!< compute node		*/
	);

/******	TETRAHEDRON statistics routines **************************************/

/*!
  \ingroup h5fed_c_api

  Returns the number of tetrahedral elements present in the mesh at 
  level \c level in current step on this compute node.

  \return number of tetrahedra
  \return \c -1 on error.

  \note
  After counting the number for the local cnode we call MPI_Gather()
  to collect the number from the other cnodes and store them into an
  array.  The inquired number will be provided from this array.
*/
h5_size_t H5FedGetNumTetrahedraInMesh(
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	);

/*!
  \ingroup h5fed_c_api

  Returns the number of tetrahedral elements present in the mesh at 
  level \c level in current step summed up over all compute nodes.

  \return number of tetrahedra
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumTetrahedraInMeshTotal(
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	);

/*!
  \ingroup h5fed_c_api

  Returns the number of tetrahedral elements present in the mesh at 
  level \c level in current step on compute nodes \c computenode.

  \return number of tetrahedra
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumTetrahedraInMeshOnNode (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t cnode		/*!< compute node		*/
	);

/******	BOUNDARY statistics routines ****************************************/

/*!
  \ingroup h5fed_c_api

  Get the number of boundary triangles present in the mesh at
  level \c level in current step on this compute node.

  \return number of tetrahedras
  \return \c -1 on error.

  \note
  It is left to the API implementor how to make this information
  available. In general he can compute it from the definition
  of the tetrahedral mesh, using the adjacency relations, i.e.
  the fact that a boundary triangle has only one single adjacent
  tetrahedron.

  \note
  After counting the number for the local cnode we call MPI_Gather()
  to collect the number from the other cnodes and store them into an
  array.  The inquired number will be provided from this array.
*/
h5_size_t H5FedGetNumBoundaryTrianglesInMesh (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	);

/*!
  \ingroup h5fed_c_api

  Get the number of boundary triangles present in the mesh at
  level \c level in current step summed up over all compute nodes.

  \return number of tetrahedras
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumBoundaryTrianglesInMeshTotal (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to query	*/
	);

/*!
  \ingroup h5fed_c_api

  Get the number of boundary triangles present in the mesh at
  level \c level in current step on compute node \c cnode.

  \return number of tetrahedras
  \return \c -1 on error.
*/
h5_size_t H5FedGetNumBoundaryTrianglesInMeshOnNode (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t cnode		/*!< compute node		*/
	);

/******	RETRIEVAL routines **************************************************/

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
	);

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
	);

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
	);

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
	const h5_id_t tetra_id		/*!< global tetrahedron id	*/
	h5_id_t * parent_id		/*!< OUT: parent id if level
					     \c >0 else \c -1		*/
	);

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
	);

/******	STORE routines*****************************************************/

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
	);

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
	);

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
	);

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
	);

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
	);


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
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t vertex_id,	/*!< global vertex id		*/
	h5_size_t * const num_adj_edges	/*!< OUT: size of returned vector */
	);

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
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t vertex_id,	/*!< global vertex id		*/
	h5_size_t * const num_adj_triangles
					/*!< OUT: size of returned vector */
);

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
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t vertex_id,	/*!< global vertex id		*/
	h5_size_t * const num_adj_tetrahedra
					/*!< OUT: size of returned vector */
	);

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
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t edge_id,		/*!< global edge id		*/
	h5_size_t * const num_adj_triangles
					/*!< OUT: size of returned vector */
	);

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
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t edge_id,		/*!< global edge id		*/
	h5_size_t * const num_adj_tets	/*!< OUT: size of returned vector */
	);

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
	h5_file * fh,			/*!< file handle		*/
	const h5_int level,		/*!< mesh level to query	*/
	const h5_int triangle_id,	/*!< global triangle id		*/
	h5_size_t * const num_adj_tets	/*!< OUT: size of returned vector */
	);

/******	DOWNWARD ADJACENCY routines *********************************************/

/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the vertices that are
  adjacent to the edge with id \c vertex_id.

  \return n-tuple of downward adjacent vertices.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetVerticesDAdjacentToEdge (
	h5_file * fh,			/*!< file handle		*/
	const h5_int level,		/*!< mesh level to query 	*/
	const h5_int edge_id,		/*!< global edge id		*/
	h5_size_t * const num_adj_vertices
					/*!< OUT: size of returned vector */
	);

/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the edges that are
  adjacent to the triangle with id \c triangle_id.

  \return n-tuple of downward adjacent edges.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetEdjesDAdjacentToTriangle (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query 	*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	h5_size_t * const num_adj_triangles
					/*!< OUT: size of returned vector */
	);

/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the vertices that are
  adjacent to the triangle with id \c triangle_id.

  \return n-tuple of downward adjacent vertices.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetVerticesDAdjacentToTriangle (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	h5_size_t * const num_adj_vertices
					/*!< OUT: size of returned vector */
	);
 
/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the triangles that are
  adjacent to the tetrahedron with id \c tet_id

  \return n-tuple of downward adjacent triangles.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetTrianglesDAdjacentToTetrahedron (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	h5_size_t * const num_adj_triangles
					/*!< OUT: size of returned vector */
	); 

/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the edges that are
  adjacent to the tetrahedron with id \c tet_id.

  \return n-tuple of downward adjacent edges.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetEdjesDAdjacentToTetrahedron (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	h5_size_t * const num_adj_edges	/*!< OUT: size of returned vector */
	);

/*!
  \ingroup h5fed_c_api

  Returns a vector, that contains the id's of all the vertices that are
  adjacent to the tetrahedron with id \c tet_id.

  \return n-tuple of downward adjacent vertices.
  \return NULL-pointer on error.
*/
h5_id_t * H5FedGetVerticesDAdjacentToTetrahedron (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level,		/*!< mesh level to query	*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	h5_size_t * const num_adj_vertices
					/*!< OUT: size of returned vector */
	);

/******	routines for accessing degrees of freedom DoF *************************/

/* DoF acces for vertices */

/*!
  \ingroup h5fed_c_api

  Stores DoF of type \c h5_float for the vertex \c vertex_id in current
  step. The DoF are passed to the function as n-tuple with an arbitrary
  number of elements.

  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_int_t H5FedStoreDoFVertexFloat (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t vertex_id,	/*!< global vertex id		*/
	const h5_float * const dof,	/*!< DoF n-tuple		*/
	const h5_size_t dof_size	/*!< number of \c h5_float 
					      value in \c dof		*/
	);
 
/*!
  \ingroup h5fed_c_api

  Stores DoF of type \c h5_complex for the vertex \c vertex_id in current
  step. The DoF are passed to the function as n-tuple with an arbitrary
  number of elements.
  
  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_int H5FedStoreDoFVertexComplex (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t vertex_id,	/*!< global vertex id		*/
	const h5_complex * const dof,	/*!< DoF n-tuple		*/
	const h5_size_t dof_size	/*!< number of \c h5_complex
					     values in \c dof		*/
	);

/*!
  \ingroup h5fed_c_api

  Get DoF of type \c h5_float for vertex \c vertex_id in current time
  step.  The DoF are returned as n-tuple. The size of this tuple is
  returned in the parameter \c dof_size.

  \return Pointer to n-tuple of type \c h5_float.
  \return NULL-pointer on error.
*/
h5_float * H5FedGetDoFVertexFloat (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t vertex_id,	/*!< global vertex id		*/
	h5_size_t * const dof_size	/*!< OUT: number of \c h5_float
					     values in returned n-tuple	*/
	); 

/*!
  \ingroup h5fed_c_api

  Get DoF of type \c h5_complex for vertex \c vertex_id in current time
  step.  The DoF are returned as n-tuple. The size of this tuple is
  returned in the parameter \c dof_size.

  \return Pointer to n-tuple of type \c h5_float.
  \return NULL-pointer on error.
*/
h5_complex * H5FedGetDoFVertexComplex (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t vertex_id,	/*!< global vertex id		*/
	h5_size_t * const dof_size	/*!< OUT: number of \c h5_float
					     values in returned n-tuple	*/
	);

/*!
  \ingroup h5fed_c_api

  Retrieves the number of scalar components of type \c h5_float,
  of the DoF that is stored for vertex \c vertex_id.

  \return Number of stored degrees of freedom.
  \return \c -1 if vertex doesn't exist.

  \note
  Do we need this function????
*/
h5_size_t H5FedGetNumDoFVertexFloat  (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t vertex_id		/*!< global vertex id		*/
	);

/*!
  \ingroup h5fed_c_api

  Retrieves the number of scalar components, of type \c h5_complex,
  of the DoF that is stored for vertex \c vertex_id.

  \return Number of stored degrees of freedom.
  \return \c -1 if vertex doesn't exist.

  \note
  Do we need this function????
*/
h5_size_t H5FedGetNumDoFVertexComplex (
	h5_file * fh,			/*!< file handle		*/
	const h5_size_t vertex_id	/*!< global vertex id		*/
	);

/* DoF access for edges */

/*!
  \ingroup h5fed_c_api

  Stores DoF of type \c h5_float for the edge \c edge_id in current
  step. The DoF are passed to the function as n-tuple with an arbitrary
  number of elements.

  \return something \c >=0 on success
  \return \c -1 on error
*/
h5_int_t H5FedStoreDoFEdgeFloat (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t edge_id,		/*!< global edge id		*/
	const h5_float * const dof,	/*!< DoF n-tuple		*/
	const h5_size_t dof_size	/*!< number of \c h5_float 
					     value in \c dof		*/
	);
 
/*!
  \ingroup h5fed_c_api

  Stores DoF of type \c h5_complex for the edge \c edge_id in current
  step. The DoF are passed to the function as n-tuple with an arbitrary
  number of elements.

  \return something \c >=0 on success
  \return \c -1 on error
*/
h5_int_t H5FedStoreDoFEdgeComplex (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t edge_id,		/*!< global edge id		*/
	const h5_complex * const dof,	/*!< DoF n-tuple		*/
	const h5_size_t dof_size	/*!< number of \c h5_complex 
					     value in \c dof		*/
	);
 
/*!
  \ingroup h5fed_c_api

  Get DoF of type \c h5_float for vertex \c vertex_id in current time
  step.  The DoF are returned as n-tuple. The size of this tuple is
  returned in the parameter \c dof_size.

  \return Pointer to n-tuple of type \c h5_float.
  \return NULL-pointer on error.
*/
h5_float_t * H5FedGetDoFEdgeFloat (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t edge_id,		/*!< global edge id		*/
	h5_siue_t * const dof_size	/*!< OUT: number of \c h5_float
					     values in returned n-tuple	*/
	);

/*!
  \ingroup h5fed_c_api

  Get DoF of type \c h5_complex for vertex \c vertex_id in current time
  step.  The DoF are returned as n-tuple. The size of this tuple is
  returned in the parameter \c dof_size.

  \return Pointer to n-tuple of type \c h5_float.
  \return NULL-pointer on error.
*/
h5_complex* H5FedGetDoFEdgeComplex (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t edge_id,		/*!< global edge id		*/
	h5_size_t * const dof_size	/*!< OUT: number of \c h5_float
					     values in returned n-tuple	*/
	);

/*!
  \ingroup h5fed_c_api

  Retrieves the number of scalar components, of type \c h5_float,
  of the DoF that is stored for edge \c edge_id.

  \return Number of stored degrees of freedom.
  \return \c -1 if edge doesn't exist.

  \note
  Do we need this function????
*/
h5_size_t H5FedGetNumDoFEdgeFloat (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t edge_id		/*!< global edge id		*/
	);

/*!
  \ingroup h5fed_c_api

  Retrieves the number of scalar components, of type \c h5_complex,
  of the DoF that is stored for edge \c edge_id.

  \return Number of stored degrees of freedom.
  \return \c -1 if edge doesn't exist.
*/
h5_size_t H5FedGetNumDoFEdgeComplex (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t edge_id		/*!< global edge id		*/
	);


/* DoF access for triangles */

/*!
  \ingroup h5fed_c_api

  Stores DoF of type \c h5_float for the triangle \c triangle_id in current
  step. The DoF are passed to the function as n-tuple with an arbitrary
  number of elements.

  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_int H5FedStoreDoFTriangleFloat (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	const h5_float_t * const dof,	/*!< DoF vector			*/
	const h5_size_t dof_size	/*!< number of \c h5_float_t
					     values in \c dof		*/
	);
 
/*!
  \ingroup h5fed_c_api

  Stores DoF of type \c h5_complex for the triangle \c triangle_id in current
  step. The DoF are passed to the function as n-tuple with an arbitrary
  number of elements.

  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_int H5FedStoreDoFTriangleComplex (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	const h5_complex * const dof,	/*!< DoF vector			*/
	const h5_size_t dof_size	/*!< number of \c h5_complex
					     values in \c dof		*/
	);
 
/*!
  \ingroup h5fed_c_api

  Get DoF of type \c h5_float for triangle \c triangle_id in current time
  step.  The DoF are returned as n-tuple. The size of this tuple is
  returned in the parameter \c dof_size.

  \return Pointer to n-tuple of type \c h5_float.
  \return NULL-pointer on error.
*/
h5_float_t * H5FedGetDoFTriangleFloat (
	h5_file* fh,			/*!< file handle		*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	h5_size_t * const dof_size	/*!< OUT: number of \c h5_float
					     values in returned n-tuple	*/
	); 

/*!
  \ingroup h5fed_c_api

  Get DoF of type \c h5_complex for triangle \c triangle_id in current time
  step.  The DoF are returned as n-tuple. The size of this tuple is
  returned in the parameter \c dof_size.

  \return Pointer to n-tuple of type \c h5_float.
  \return NULL-pointer on error.
*/
h5_complex * H5FedGetDoFTriangleComplex (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	h5_size_t * const dof_size	/*!< OUT: number of \c h5_float
					     values in returned n-tuple	*/
	);

/*!
  \ingroup h5fed_c_api

  Retrieves the number of scalar components of type \c h5_float,
  of the DoF that is stored for triangle \c triangle_id.

  \return Number of stored degrees of freedom.
  \retuen \c -1 if triangle doesn't exist.

  \note
  Do we need this function????
*/
h5_size_t H5FedGetNumDoFTriangleFloat (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t triangle_id	/*!< global triangle id		*/
	); 

/*!
  \ingroup h5fed_c_api

  Retrieves the number of scalar components of type \c h5_float,
  of the DoF that is stored for triangle \c triangle_id.

  \return Number of stored degrees of freedom.
  \retuen \c -1 if triangle doesn't exist.

  \note
  Do we need this function????
*/
h5_size_t H5FedGetNumDoFTriangleComplex (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	);


/* DoF access for tetrahedra */

/*!
  \ingroup h5fed_c_api

  Stores DoF of type \c h5_float for the tetrahedron \c tet_id in current
  step. The DoF are passed to the function as n-tuple with an arbitrary
  number of elements.

  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_int_t H5FedStoreDoFTetrahedronFloat (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	const h5_float_t * const dof,	/*!< DoF vector			*/
	const h5_size_t dof_size	/*!< number of \c h5_float 
					     value in \c dof		*/
	);

/*!
  \ingroup h5fed_c_api

  Stores DoF of type \c h5_complex for the tetrahedron \c tet_id in current
  step. The DoF are passed to the function as n-tuple with an arbitrary
  number of elements.

  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_int_t H5FedStoreDoFTetrahedronComplex (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	const h5_complex * const dof,	/*!< DoF vector			*/
	const h5_size_t dof_size	/*!< number of \c h5_complex
					     value in \c dof		*/
	);
 
/*!
  \ingroup h5fed_c_api

  Get DoF of type \c h5_float for tetrahedron \c tet_id in current time
  step.  The DoF are returned as n-tuple. The size of this tuple is
  returned in the parameter \c dof_size.

  \return Pointer to n-tuple of type \c h5_float.
  \return NULL-pointer on error.
*/
h5_float_t * H5FedGetDoFTetrahedronFloat (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	h5_size_t * const dof_size	/*!< OUT: number of \c h5_float
					     values in returned n-tuple	*/
	);

/*!
  \ingroup h5fed_c_api

  Get DoF of type \c h5_complex for tetrahedron \c tet_id in current time
  step.  The DoF are returned as n-tuple. The size of this tuple is
  returned in the parameter \c dof_size.

  \return Pointer to n-tuple of type \c h5_complex
  \return NULL-pointer on error.
*/
h5_complex * H5FedGetDoFTetrahedronComplex (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	h5_size_t * const dof_size	/*!< OUT: number of \c h5_complex
					     values in returned n-tuple	*/
	);

/*!
  \ingroup h5fed_c_api

  Retrieves the number of scalar components of type \c h5_float,
  of the DoF that is stored for tetrahedron \c tet_id.

  \return Number of stored degrees of freedom.
  \return \c -1 if vertex doesn't exist.

  \note
  Do we need this function????
*/
h5_size_t H5FedGetNumDoFTetrahedronFloat (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t tet_id		/*!< global tetrahedron id	*/
	);

/*!
  \ingroup h5fed_c_api

  Retrieves the number of scalar components of type \c h5_complex,
  of the DoF that is stored for tetrahedron \c tet_id.

  \return Number of stored degrees of freedom.
  \retuen \c -1 if tetrahedron doesn't exist.
*/
h5_size_t H5FedGetNumDoFTetrahedronComplex (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t tet_id		/*!< global tetrahedron id	*/
	);

#endif












