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


#ifndef __H5FED_H
#define __H5FED_H


/******	General routines *****************************************************/

h5_file* H5FedOpenFile (
	const char * filename,
	const MPI_Comm comm
	);

h5_int64_t H5FedCloseFile (
	h5_file * fh
	);

h5_size_t
H5FedGetNumMeshes (
	h5_file * f			/*!< file handle		*/
	);

h5_err_t
H5FedOpenMesh (
	h5_file * f,
	const h5_id_t id
	);
h5_id_t
H5FedAddMesh (
	h5_file * fh
	);

h5_size_t
H5FedGetNumLevels (
	h5_file * f			/*!< file handle		*/
	);

h5_err_t
H5FedSetLevel (
	h5_file * f,
	const h5_id_t id
	);

h5_id_t
H5FedAddLevel (
	h5_file * fh
	);

h5_size_t
H5FedSetAdditionalNumVerticesToStore (
	h5_file * f,			/*!< file handle		*/
	const h5_size_t num		/*!< number of additional vertices */
	);

/******	INQUIRY routines *****************************************************/

h5_int_t H5GetNumNodes (
	h5_file * fh
	);

h5_int_t H5FedGetNumMeshLevels (
	h5_file * fh
	);

h5_int_t H5FedHasTetrahedralMesh (
	h5_file * fh,
	const h5_id_t level
	);

h5_int_t H5FedHasBoundaryMesh(
	h5_file * fh,
	const h5_id_t level
);

/******	VERTEX statistics routines *******************************************/

h5_size_t H5FedGetNumVerticesTotal(
	h5_file * fh,
	const h5_id_t level
	);

h5_size_t H5FedGetNumVerticesCnode (
	h5_file * fh,
	const h5_id_t level,
	const h5_id_t cnode
	);

/******	EDGE statistics routines ********************************************/

h5_size_t H5FedGetNumEdgesTotal (
	h5_file * fh,
	const h5_id_t level
	);

h5_size_t H5FedGetNumEdgesCnode (
	h5_file * fh,
	const h5_id_t level,
	const h5_id_t cnode
	);

/******	TRIANGLE statistics routines *****************************************/

h5_size_t H5FedGetNumTrianglesTotal (
	h5_file * fh,
	const h5_id_t level
	);

h5_size_t H5FedGetNumTrianglesCnode (
	h5_file * fh,
	const h5_id_t level,
	const h5_id_t cnode
	);

/******	TETRAHEDRON statistics routines **************************************/

h5_size_t H5FedGetNumTetrahedraTotal(
	h5_file * fh,
	const h5_id_t level
	);

h5_size_t H5FedGetNumTetrahedraCnode (
	h5_file * fh,
	const h5_id_t level,
	const h5_id_t cnode
	);

/******	BOUNDARY statistics routines ****************************************/

h5_size_t H5FedGetNumBoundaryTrianglesTotal (
	h5_file * fh,
	const h5_id_t level
	);

h5_size_t H5FedGetNumBoundaryTrianglesCnode (
	h5_file * fh,
	const h5_id_t level,
	const h5_id_t cnode
	);

/******	STORE / RETRIEVAL routines ********************************************/

h5_err_t
H5FedSetStep (
	h5_file * f,
	const h5_id_t step
	);
	

/* vertices */
h5_size_t
H5FedSetNumVertices (
	h5_file * f,
	const h5_size_t num
	);

h5_size_t
H5FedGetNumVertices (
	h5_file * f
	);

h5_id_t
H5FedStoreVertex (
	h5_file * f,
	const h5_id_t id,
	const h5_float64_t P[3]
	);

h5_id_t
H5FedGetVertex (
	h5_file * f,
	h5_id_t	* const	id,
	h5_float64_t P[3]
	);

/* edges */
h5_size_t
H5FedSetNumEdges (
	h5_file * f,
	const h5_size_t num
	);

h5_size_t
H5FedGetNumEdges (
	h5_file * f
	);

h5_id_t
H5FedStoreEdge (
	h5_file * f,
	const h5_id_t id,
	const h5_id_t parent_id,
	const h5_id_t vertex_ids[2]
	);

h5_id_t
H5FedGetEdge (
	h5_file * f,
	h5_id_t * const id,
	h5_id_t * const parent_id,
	h5_id_t * const vertex_ids[2]
	);

/* triangles */
h5_size_t
H5FedSetNumTriangles (
	h5_file * f,
	const h5_size_t num
	);

h5_size_t
H5FedGetNumTriangles (
	h5_file * f
	);

h5_int_t
H5FedStoreTriangle (
	h5_file * f,
	const h5_id_t id,
	const h5_id_t parent_id,
	const h5_id_t vertex_ids[3]
	);

h5_id_t
H5FedGetTriangle (
	h5_file * f,
	h5_id_t * const id,
	h5_id_t * const parent_id,
	h5_id_t * const vertex_ids[2]
	);

/* boundary triangles */
h5_size_t
H5FedSetNumBoundaryTriangles (
	h5_file * f,
	const h5_size_t num
	);

h5_size_t
H5FedGetNumBoundaryTriangles (
	h5_file * f
	);


h5_id_t
H5FedStoreBoundaryTriangle (
	h5_file * fh,
	const h5_id_t id,
	const h5_id_t parent_id,
	const h5_id_t vertex_ids[3]
	);

h5_id_t
H5FedGetBoundaryTriangle (
	h5_file * f,
	h5_id_t * const id,
	h5_id_t * const parent_id,
	h5_id_t * const vertex_ids[3]
	);

/* tetrahedra */
h5_size_t
H5FedSetAdditionalNumTetrahedraToStore (
	h5_file * f,
	const h5_size_t num_tet
	);

h5_size_t
H5FedGetNumTetrahedra (
	h5_file * f
	);

h5_id_t
H5FedStoreTetrahedron (
	h5_file * f,
	const h5_id_t id,
	const h5_id_t parent_id,
	const h5_id_t vertex_ids[4]
	);

h5_id_t
H5FedGetTetrahedron (
	h5_file * f,
	h5_id_t * const id,
	h5_id_t * parent_id,
	h5_id_t vertex_ids[4]
	);

/******	UPWARD ADJACENCY routines *********************************************/

h5_id_t * H5FedGetEdgesUAdjacentToVertex (
	h5_file * fh,
	const h5_id_t level,
	const h5_id_t vertex_id,
	h5_size_t * const num_adj_edges
	);

h5_id_t * H5FedGetTrianglesUAdjacentToVertex (
	h5_file * fh,
	const h5_id_t level,
	const h5_id_t vertex_id,
	h5_size_t * const num_adj_triangles
);

h5_id_t * H5FedGetTetrahedrasUAdjacentToVertex (
	h5_file * fh,
	const h5_id_t level,
	const h5_id_t vertex_id,
	h5_size_t * const num_adj_tetrahedra
	);

h5_id_t * H5FedGetTrianglesUAdjacentToEdge (
	h5_file * fh,
	const h5_id_t level,
	const h5_id_t edge_id,
	h5_size_t * const num_adj_triangles
	);

h5_id_t * H5FedGetTetrahedraUAdjacentToEdge (
	h5_file * fh,
	const h5_id_t level,
	const h5_id_t edge_id,
	h5_size_t * const num_adj_tets
	);

h5_id_t * H5FedGetTetrahedraUAdjacentToTriangle (
	h5_file * fh,
	const h5_int_t level,
	const h5_int_t triangle_id,
	h5_size_t * const num_adj_tets
	);

/******	DOWNWARD ADJACENCY routines *********************************************/

h5_id_t * H5FedGetVerticesDAdjacentToEdge (
	h5_file * fh,
	const h5_int_t level,
	const h5_int_t edge_id,
	h5_size_t * const num_adj_vertices
	);

h5_id_t * H5FedGetEdjesDAdjacentToTriangle (
	h5_file * fh,
	const h5_id_t level,
	const h5_id_t triangle_id,
	h5_size_t * const num_adj_triangles
	);

h5_id_t * H5FedGetVerticesDAdjacentToTriangle (
	h5_file * fh,
	const h5_id_t level,
	const h5_id_t triangle_id,
	h5_size_t * const num_adj_vertices
	);
 
h5_id_t * H5FedGetTrianglesDAdjacentToTetrahedron (
	h5_file * fh,
	const h5_id_t level,
	const h5_id_t tet_id,
	h5_size_t * const num_adj_triangles
	); 

h5_id_t * H5FedGetEdjesDAdjacentToTetrahedron (
	h5_file * fh,
	const h5_id_t level,
	const h5_id_t tet_id,
	h5_size_t * const num_adj_edges
	);

h5_id_t * H5FedGetVerticesDAdjacentToTetrahedron (
	h5_file * fh,
	const h5_id_t level,
	const h5_id_t tet_id,
	h5_size_t * const num_adj_vertices
	);

/******	routines for accessing degrees of freedom DoF *************************/

/* DoF acces for vertices */

h5_int_t H5FedStoreDoFVertexFloat (
	h5_file * fh,
	const h5_id_t vertex_id,
	const h5_float_t * const dof,
	const h5_size_t dof_size
	);
 
h5_int_t H5FedStoreDoFVertexComplex (
	h5_file * fh,
	const h5_id_t vertex_id,
	const h5_complex * const dof,
	const h5_size_t dof_size
	);

h5_float_t * H5FedGetDoFVertexFloat (
	h5_file * fh,
	const h5_id_t vertex_id,
	h5_size_t * const dof_size
	); 

h5_complex * H5FedGetDoFVertexComplex (
	h5_file * fh,
	const h5_id_t vertex_id,
	h5_size_t * const dof_size
	);

h5_size_t H5FedGetNumDoFVertexFloat  (
	h5_file * fh,
	const h5_id_t vertex_id
	);

h5_size_t H5FedGetNumDoFVertexComplex (
	h5_file * fh,
	const h5_size_t vertex_id
	);

/* DoF access for edges */

h5_int_t H5FedStoreDoFEdgeFloat (
	h5_file * fh,
	const h5_id_t edge_id,
	const h5_float_t * const dof,
	const h5_size_t dof_size
	);
 
h5_int_t H5FedStoreDoFEdgeComplex (
	h5_file * fh,
	const h5_id_t edge_id,
	const h5_complex * const dof,
	const h5_size_t dof_size
	);
 
h5_float_t * H5FedGetDoFEdgeFloat (
	h5_file * fh,
	const h5_id_t edge_id,
	h5_size_t * const dof_size
	);

h5_complex* H5FedGetDoFEdgeComplex (
	h5_file * fh,
	const h5_id_t edge_id,
	h5_size_t * const dof_size
	);

h5_size_t H5FedGetNumDoFEdgeFloat (
	h5_file * fh,
	const h5_id_t edge_id
	);

h5_size_t H5FedGetNumDoFEdgeComplex (
	h5_file * fh,
	const h5_id_t edge_id
	);


/* DoF access for triangles */

h5_int_t H5FedStoreDoFTriangleFloat (
	h5_file * fh,
	const h5_id_t triangle_id,
	const h5_float_t * const dof,
	const h5_size_t dof_size
	);
 
h5_int_t H5FedStoreDoFTriangleComplex (
	h5_file * fh,
	const h5_id_t triangle_id,
	const h5_complex * const dof,
	const h5_size_t dof_size
	);
 
h5_float_t * H5FedGetDoFTriangleFloat (
	h5_file* fh,
	const h5_id_t triangle_id,
	h5_size_t * const dof_size
	); 

h5_complex * H5FedGetDoFTriangleComplex (
	h5_file * fh,
	const h5_id_t triangle_id,
	h5_size_t * const dof_size
	);

h5_size_t H5FedGetNumDoFTriangleFloat (
	h5_file * fh,
	const h5_id_t triangle_id
	); 

h5_size_t H5FedGetNumDoFTriangleComplex (
	h5_file * fh,
	const h5_id_t triangle_id
	);

/* DoF access for tetrahedra */

h5_int_t H5FedStoreDoFTetrahedronFloat (
	h5_file * fh,
	const h5_id_t tet_id,
	const h5_float_t * const dof,
	const h5_size_t dof_size
	);

h5_int_t H5FedStoreDoFTetrahedronComplex (
	h5_file * fh,
	const h5_id_t tet_id,
	const h5_complex * const dof,
	const h5_size_t dof_size
	);
 
h5_float_t * H5FedGetDoFTetrahedronFloat (
	h5_file * fh,
	const h5_id_t tet_id,
	h5_size_t * const dof_size
	);

h5_complex * H5FedGetDoFTetrahedronComplex (
	h5_file * fh,
	const h5_id_t tet_id,
	h5_size_t * const dof_size
	);

h5_size_t H5FedGetNumDoFTetrahedronFloat (
	h5_file * fh,
	const h5_id_t tet_id
	);

h5_size_t H5FedGetNumDoFTetrahedronComplex (
	h5_file * fh,
	const h5_id_t tet_id
	);

#endif












