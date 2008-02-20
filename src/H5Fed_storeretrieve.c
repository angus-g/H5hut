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
#include "h5/h5.h"
#include "h5/h5_private.h"
#include "H5Fed.h"


/*
  Retrieval for all objects:

  SetLevel()
  while ( getObj() >=0 ) {
	...
  }

  For the implementation of "getObj()" we need the information whether an
  object has been refined or not. If we are on level n and an object has been
  refined on level i<n, we have to skip this object. Since we have a consecutive
  enumberation of objects, we have a simple mapping from the parent id of the 
  object to 
  
*/

/*** V E R T I C E S *********************************************************/

/*!
  \ingroup h5fed_c_api

  Set number of additional vertices in current step and level

  ERRORS:

  H5_ERR_INVAL:	It is not possible to add vertices to an existing mesh due
  to limitation of the library

  H5_ERR_NOMEM: Couldn't allocate enough memory.

  \return number of vertices
  \return errno	on error
*/

h5_size_t
H5FedSetAdditionalNumVerticesToStore (
	h5_file * f,			/*!< file handle		*/
	const h5_size_t num		/*!< number of additional vertices */
	) {

	SET_FNAME ( __func__ );
	return H5t_add_num_vertices ( f, num );
}

/*!
  \ingroup h5fed_c_api

  Get the number of vertices used for defining a submesh
  at level \c level for this compute node in current step.

  \return number of vertices
  \return \c -1	on error.
*/
h5_size_t
H5FedGetNumVertices (
	h5_file * f			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return H5t_get_num_vertices ( f );
}

/*!
  \ingroup h5fed_c_api

  Stores the the coordinates of a specific vertex at level \c level
  with id \c vertex_id of the tetrahedral mesh.

  \return local vertex id on success
  \return errno on error
*/
h5_id_t
H5FedStoreVertex (
	h5_file * f,			/*!< file handle		*/
	const h5_id_t id,		/*!< global vertex id or -1	*/
	const h5_float64_t P[3]		/*!< coordinates		*/
	) {

	SET_FNAME ( __func__ );
	return H5t_store_vertex ( f, id, P );
}

/*!
  \ingroup h5fed_c_api

  Get coordinates of next vertex.

  \return local id
  \return error code (H5_ERR_NOENT means no more vertices on this level)
 
 */
h5_id_t
H5FedGetVertex (
	h5_file * f,			/*!< file handle		*/
	h5_id_t	* const id,		/*!< OUT: global id		*/
	h5_float64_t * const P[3]	/*!< OUT: coordinates		*/
	) {
	SET_FNAME ( __func__ );
	return H5t_get_vertex ( f, id, P );
}

/*** E D G E S ***************************************************************/

h5_size_t
H5FedSetNumEdges (
	h5_file * f,
	const h5_size_t num
	) {
	SET_FNAME ( __func__ );
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Returns the number of edges present in the mesh at level \c level in 
  current time step on this compute node.

  \return number of edges
  \return \c -1	on error.
*/
h5_size_t
H5FedGetNumEdges (
	h5_file * f			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Stores the 2-tuple, that contains the specific indices describing
  an edge with id \c edge_id at level \c level of the tetrahedral mesh.

  \return local edge id
  \return \c -1 on error
*/
h5_id_t
H5FedStoreEdge (
	h5_file * f,			/*!< file handle		*/
	const h5_id_t id,		/*!< global edge id or -1	*/
	const h5_id_t parent_id,	/*!< local parent id
					     if level \c >0 else \x -1	*/
	const h5_id_t vertex_ids[2]	/*!< tuple with vertex id's	*/
	) {
	SET_FNAME ( __func__ );
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Get the definition of a specific edge \edge_id, i.e. a 2-tuple
  containing the specific indices of the 3-dimensional vertex
  coordinates.

  \return local id
  \return error code (H5_ERR_NOENT means no more vertices on this level)
*/
h5_id_t
H5FedGetEdge (
	h5_file * f,			/*!< file handle		*/
	h5_id_t * const id,		/*!< OUT: global edge id	*/
	h5_id_t * const parent_id,	/*!< OUT: global parent id
					     if level \c >0 else \c -1	*/
	h5_id_t * const vertex_ids[2]	/*!< OUT: vertex id's		*/
	) {
	SET_FNAME ( __func__ );
	return -1;
}

/*** T R I A N G L E S *******************************************************/

h5_size_t
H5FedSetNumTriangles (
	h5_file * f,
	const h5_size_t num
	) {
	SET_FNAME ( __func__ );
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Get the number of triangles present in the mesh at level \c level in 
  current time step on this compute note.

  \return Number of triangles
  \return \c -1 on error.

*/
h5_size_t
H5FedGetNumTriangles (
	h5_file * f			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Stores the 3-tuple, that contains the specific indices describing a
  triangle with id \c triangle_id at level \c level of the tetrahedral mesh.

  \return local triangle id
  \return \c -1 on error
*/
h5_int_t
H5FedStoreTriangle (
	h5_file * f,			/*!< file handle		*/
	const h5_id_t id,		/*!< global triangle id	or -1	*/
	const h5_id_t parent_id,	/*!< local parent id
					     if level \c >0 else \x -1	*/
	const h5_id_t vertex_ids[3]	/*!< tuple with vertex id's	*/
	) {
	SET_FNAME ( __func__ );
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Get the definition of a specific triangle \triangle_id, i.e. 
  a 3-tuple containing the specific indices of the 3-dimensional vertex
  coordinates.

  \return local id
  \return error code (H5_ERR_NOENT means no more vertices on this level)
*/

#define HANDLE_H5_ERR_NOT_IMPLEMENTED \
	(*H5_get_errorhandler()) (		\
		H5_get_funcname(),		\
		H5_ERR_NOT_IMPLEMENTED,		\
		"Function not yet implemented!" );

h5_id_t
H5FedGetTriangle (
	h5_file * f,			/*!< file handle		*/
	h5_id_t * const id,		/*!< OUT: global triangle id	*/
	h5_id_t * const parent_id,	/*!< OUT: parent id if level
					     \c >0 else \c -1		*/
	h5_id_t * const vertex_ids[3]	/*!< OUT: vertex id's		*/
	) {
	SET_FNAME ( __func__ );
	return HANDLE_H5_ERR_NOT_IMPLEMENTED;
}

/*** B O U N D A R Y   T R I A N G L E S *************************************/
 
/*!
  \ingroup h5fed_c_api

  Get the number of boundary triangles present in the mesh at
  level \c level in current step on this compute node.

  \return number of tetrahedras
  \return \c -1 on error.
*/
h5_size_t
H5FedGetNumBoundaryTriangles (
	h5_file * f			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Writes the vector, that contains the specific indices that describe
  a boundary triangle \c btriangle with id \c btriangle_id at level
  \c level of the tetrahedral mesh.

  \return local boundary triangle id
  \return \c -1 on error
*/
h5_id_t
H5FedStoreBoundaryTriangle (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t btriangle_id,	/*!< global boundary triangle id*/
	const h5_id_t parent_id,	/*!< parent id if level \c >0
					     else \c -1			*/
	const h5_id_t btriangle[3]	/*!< tuple with vertex id's*/
	) {
	SET_FNAME ( __func__ );
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Get the definition of the boundary triangle \c triangle_id at level
  \c level, i.e. the indices of the 3-dimensional vertex coordinates.

  \return pointer to 3-tuple of vertex id's defining the boundary triangle.
  \return NULL-pointer on error.
*/
h5_id_t
H5FedGetBoundaryTriangle (
	h5_file * f,			/*!< file handle		*/
	h5_id_t * const id,		/*!< global btriangle_id	*/
	h5_id_t * const parent_id,	/*!< OUT: parent id if level
					     \c >0 else \c -1		*/
	h5_id_t * const vertex_ids[3]	/*!< OUT: vertex id's		*/
	) {
	SET_FNAME ( __func__ );
	return -1;
}

/*** T E T R A H E D R A ****************************************************/

h5_size_t
H5FedSetAdditionalNumTetrahedraToStore (
	h5_file * f,			/*!< file handle		*/
	const h5_size_t num		/*!< number of additional
					  tetrahedra at level \c level	*/
	) {
	SET_FNAME ( __func__ );

	return H5t_add_num_tets ( f, num );
}

/*!
  \ingroup h5fed_c_api

  Returns the number of tetrahedral elements present in the mesh at 
  level \c level in current step on this compute node.

  \return number of tetrahedra
  \return \c -1 on error.
*/
h5_size_t
H5FedGetNumTetrahedra (
	h5_file * f			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return H5t_get_num_tets ( f );
}

/*!
  \ingroup h5fed_c_api

  Stores the 4-tuple, that contains the specific indices describing
  a tetrahedron with id \c tet_id at level \c level of the tetrahedral
  mesh.

  Errors:
  * current level not yet defined
  * to many tets stored on level

  \return local tetrahedron id
  \return \c errno on error
*/
h5_id_t
H5FedStoreTetrahedron (
	h5_file * f,			/*!< file handle		*/
	const h5_id_t id,		/*!< global tetrahedron id	*/
	const h5_id_t parent_id,	/*!< global parent id
					     if level \c >0 else \c -1	*/
	const h5_id_t vertex_ids[4]	/*!< tuple with vertex id's	*/
	) {
	SET_FNAME ( __func__ );
	return H5t_store_tet ( f, id, parent_id, vertex_ids );
}

/*!
  \ingroup h5fed_c_api

  Get the definition of a specific tetrahedron \c tetra_id, i.e. 
  a 4-tuple containing the specific indices of the 3-dimensional vertex
  coordinates.

  \return pointer to 4-tuple of vertex id's defining the tetrahedron.
  \return NULL-pointer on error.
*/
h5_id_t
H5FedGetTetrahedron (
	h5_file * f,			/*!< file handle		*/
	h5_id_t * const id,		/*!< OUT: global tetrahedron id	*/
	h5_id_t * parent_id,		/*!< OUT: parent id if level
					     \c >0 else \c -1		*/
	h5_id_t * const vertex_ids[4]	/*!< OUT: vertex id's		*/
	) {
	SET_FNAME ( __func__ );
	return H5t_get_tet ( f, id, parent_id, vertex_ids );
}

h5_err_t
H5FedSetStep (
	h5_file *f,			/*!< Handle to open file */
	const h5_id_t step		/*!< Time-step to set. */
	) {
	SET_FNAME ( __func__ );
	return (h5_err_t) H5_set_step ( f, step );
}


h5_id_t
H5FedAddMesh (
	h5_file * f
	) {
	SET_FNAME ( __func__ );
	return H5t_add_mesh ( f );
}

/*****************************************************************************/
/*!
  Add a new level.

  \note
  values for f->t.num_levels:
  \c -1		unknown: after opening the file. This is equivalent to
		"topological data has not been initialized".
  \c 0		no levels: HDF5 group for meshes may already exist but must not!
  \c > 0	number of mesh levels
 
  \note
  write new level:
	H5FedAddLevel( f );
	H5FedSetNumVertices( f, nv );
	H5FedSetNumTetrahedra( f, nt );
*/
  
h5_id_t
H5FedAddLevel (
	h5_file * f			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return H5t_add_level ( f );
}
