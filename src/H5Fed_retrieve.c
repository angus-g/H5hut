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

h5_err_t
H5FedBeginTraverseVertices (
	h5_file_t * const f		/*!< file handle		*/
	) {
	SET_FNAME ( f, __func__ );
	return h5t_start_traverse_vertices ( f );
}

/*!
  \ingroup h5fed_c_api

  Get coordinates of next vertex.

  \return local id
  \return error code (H5_ERR_NOENT means no more vertices on this level)
 
 */
h5_id_t
H5FedTraverseVertices (
	h5_file_t * const f,		/*!< file handle		*/
	h5_id_t	* const id,		/*!< OUT: global id		*/
	h5_float64_t P[3]		/*!< OUT: coordinates		*/
	) {
	SET_FNAME ( f, __func__ );
	return h5t_traverse_vertices ( f, id, P );
}

h5_err_t
H5FedEndTraverseVertices (
	h5_file_t * const f		/*!< file handle		*/
	) {
	SET_FNAME ( f, __func__ );
	return h5t_end_traverse_vertices ( f );
}


h5_err_t
H5FedBeginTraverseElements (
	h5_file_t * const f  		/*!< file handle		*/
	) {
	SET_FNAME ( f, __func__ );
	return h5t_start_traverse_elems ( f );
}

/*!
  \ingroup h5fed_c_api

  Get the definition of a specific tetrahedron \c tetra_id, i.e. 
  a 4-tuple containing the specific indices of the 3-dimensional vertex
  coordinates.

  \param[in]	f		File handle.
  \param[out]	global_eid	The global element id.
  \param[out]	local_parent_eid The local element id of the parent or \c -1
  \param[in]	local_vids	Local vertex id's.

  \return \c H5_SUCCESS or error code.
*/

h5_id_t
H5FedTraverseElements (
	h5_file_t * const f,
	h5_id_t * const global_eid,
	h5_id_t * const local_parent_eid,
	h5_id_t * const local_vids
	) {
	SET_FNAME ( f, __func__ );
	return h5t_traverse_elems (
		f,
		global_eid,
		local_parent_eid,
		local_vids );
}

h5_id_t
H5FedEndTraverseElements (
	h5_file_t * const f
	) {
	return h5t_end_traverse_elems ( f );
}

