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
#include "h5_core/h5_core.h"
#include "H5Fed.h"


/*!
  Begin traverse over all vertices on this compute node. 
  Initialize internal data structures.

  \remark
  Vertices can be on processor boundaries! Therefore the same vertex can be
  processed on several compute nodes.

  \param[in]	f	file handle

  \return	H5_SUCCESS or error code
*/
h5_err_t
H5FedBeginTraverseVertices (
	h5_file_t * const f
	) {
	SET_FNAME ( f, __func__ );
	return h5t_begin_traverse_vertices ( f );
}

/*!
  Get coordinates of next vertex.

  \param[in]	f	file handle
  \param[out]	P	3-dimensional coordinates

  \return	Local vertex ID
  \return	-1, if done
  \return	error code on error
  */
h5_id_t
H5FedTraverseVertices (
	h5_file_t * const f,
	h5_float64_t P[3]
	) {
	SET_FNAME ( f, __func__ );
	return h5t_traverse_vertices ( f, P );
}

/*!
  End of traversing. Release internal data structures.

  \param[in]	f	File handle

  \return	H5_SUCCESS or error code
 
 */
h5_err_t
H5FedEndTraverseVertices (
	h5_file_t * const f
	) {
	SET_FNAME ( f, __func__ );
	return h5t_end_traverse_vertices ( f );
}

/*!
  Begin traverse over all edges on this compute node. 
  Initialize internal data structures.

  \remark
  Edges can be on processor boundaries! Therefore the same edge can be
  processed on several compute nodes.

  \param[in]	f	file handle

  \return	H5_SUCCESS or error code
*/
h5_err_t
H5FedBeginTraverseEdges (
	h5_file_t * const f  		/*!< file handle		*/
	) {
	SET_FNAME ( f, __func__ );
	return h5t_begin_traverse_edges ( f );
}

/*!
  Get local edge id and vertices of next edge.

  \param[in]	f	file handle
  \param[out]	vids	local vertex IDs

  \return	Local edge ID
  \return	-1, if done
  \return	error code on error
  */
h5_id_t
H5FedTraverseEdges (
	h5_file_t * const f,
	h5_id_t * const vids
	) {
	SET_FNAME ( f, __func__ );
	return h5t_traverse_edges ( f, vids );
}

/*!
  End of traversing. Release internal data structures.

  \param[in]	f	File handle

  \return	H5_SUCCESS or error code
  */
h5_err_t
H5FedEndTraverseEdges (
	h5_file_t * const f		/*!< file handle		*/
	) {
	SET_FNAME ( f, __func__ );
	return h5t_end_traverse_edges ( f );
}

/*!
  Begin traverse over all triangles on this compute node. 
  Initialize internal data structures.

  \remark
  Triangles can be on processor boundaries! Therefore the same vertex can be
  processed on several compute nodes.

  \param[in]	f	file handle

  \return	H5_SUCCESS or error code
*/
h5_err_t
H5FedBeginTraverseTriangles (
	h5_file_t * const f
	) {
	SET_FNAME ( f, __func__ );
	return h5t_begin_traverse_triangles ( f );
}

/*!
  Get local triangle ID and vertices of next triangle.

  \param[in]	f	file handle
  \param[out]	vids	local vertex IDs

  \return	Local triangle ID
  \return	-1, if done
  \return	error code on error
  */
h5_id_t
H5FedTraverseTriangles (
	h5_file_t * const f,
	h5_id_t * const vids
	) {
	SET_FNAME ( f, __func__ );
	return h5t_traverse_triangles ( f, vids );
}

/*!
  End of traversing. Release internal data structures.

  \param[in]	f	File handle

  \return	H5_SUCCESS or error code
  */
h5_err_t
H5FedEndTraverseTriangles (
	h5_file_t * const f
	) {
	SET_FNAME ( f, __func__ );
	return h5t_end_traverse_triangles ( f );
}

/*!
  Begin traverse over all elements on this compute node. 
  Initialize internal data structures.

  \remark
  Elements are unique per compute node. Ghost elements are *not* return 
  while traversing.

  \param[in]	f	file handle

  \return	H5_SUCCESS or error code
*/
h5_err_t
H5FedBeginTraverseElements (
	h5_file_t * const f
	) {
	SET_FNAME ( f, __func__ );
	return h5t_begin_traverse_elems ( f );
}

/*!
  Get local element ID and vertices of next element.

  \param[in]	f	file handle
  \param[out]	vids	local vertex IDs

  \return	Local element ID
  \return	-1, if done
  \return	error code on error
  */
h5_id_t
H5FedTraverseElements (
	h5_file_t * const f,
	h5_id_t * const vids
	) {
	SET_FNAME ( f, __func__ );
	return h5t_traverse_elems ( f, vids );
}

/*!
  End of traversing. Release internal data structures.

  \param[in]	f	File handle

  \return	H5_SUCCESS or error code
  */
h5_id_t
H5FedEndTraverseElements (
	h5_file_t * const f
	) {
	return h5t_end_traverse_elems ( f );
}

