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


h5_id_t
H5FedAddTetMesh (
	h5_file_t * const f,
	const h5_size_t num_elems
	) {
	SET_FNAME ( f, __func__ );
	return h5t_add_mesh ( f, num_elems, H5_OID_TETRAHEDRON );
}

h5_id_t
H5FedAddTriangleMesh (
	h5_file_t * const f,
	const h5_size_t num_elems
	) {
	SET_FNAME ( f, __func__ );
	return h5t_add_mesh ( f, num_elems, H5_OID_TRIANGLE );
}


/*!
  \ingroup h5fed_c_api

  Add a new level with \c num_elems elements. The number of elements must be the
  real number of elements to add the level. If you want to refine \c n tetrahedra
  \c n*8 elements must be added.

  \param[in]	f		file handle
  \param[in]	num_elems	Number of elements to add.

  \return ID of new level.

  \note
  values for f->t.num_levels:
  \c -1		unknown: after opening the file. This is equivalent to
		"topological data has not been initialized".
  \c 0		no levels: HDF5 group for meshes may already exist but must not!
  \c > 0	number of mesh levels
 
*/
h5_id_t
H5FedAddLevel (
	h5_file_t * const f,		/*!< file handle		*/
	const h5_size_t num_elems
	) {
	SET_FNAME ( f, __func__ );
	h5_id_t num_vertices = (num_elems>>2)*3; /* this is an upper limit */
	return h5t_add_level ( f, num_vertices, num_elems );
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
	h5_file_t * const f,		/*!< file handle		*/
	const h5_id_t vertex_id,	/*!< id from mesher or -1	*/
	const h5_float64_t P[3]		/*!< coordinates		*/
	) {

	SET_FNAME ( f, __func__ );
	if ( h5t_get_level ( f ) != 0 ) {
		return h5_error (
			f,
			H5_ERR_INVAL,
			"Vertices can be added to level 0 only!" );
	}
	return h5t_store_vertex ( f, vertex_id, P );
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
H5FedStoreElement (
	h5_file_t * const f,		/*!< file handle		*/
	const h5_id_t local_vids[]	/*!< tuple with vertex id's	*/
	) {
	SET_FNAME ( f, __func__ );
	if ( h5t_get_level ( f ) != 0 ) {
		return h5_error (
			f,
			H5_ERR_INVAL,
			"Tetrahedra can be added to level 0 only!" );
	}
	return h5t_store_elem ( f, -1, local_vids );
}

h5_id_t
H5FedRefineElement (
	h5_file_t * const f,		/*!< file handle		*/
	const h5_id_t local_eid		/*!< local element id		*/
	) {
	SET_FNAME ( f, __func__ );
	return h5t_refine_elem ( f, local_eid );
}
