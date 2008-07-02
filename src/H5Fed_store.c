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
	H5FedAddNumVertices( f, nv );
	H5FedAddNumEntities( f, ne );
*/
  
h5_id_t
H5FedAddLevel (
	h5_file * f			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return h5t_add_level ( f );
}

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
h5_err_t
H5FedAddNumVertices (
	h5_file * f,			/*!< file handle		*/
	const h5_size_t num		/*!< number of additional vertices */
	) {

	SET_FNAME ( __func__ );
	return h5t_add_num_vertices ( f, num );
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
	return h5t_store_vertex ( f, id, P );
}

/*** T E T R A H E D R A ****************************************************/

h5_err_t
H5FedAddNumEntities (
	h5_file * f,			/*!< file handle		*/
	const h5_size_t num		/*!< number of additional
					  entities on current level	*/
	) {
	SET_FNAME ( __func__ );

	return h5t_add_num_entities ( f, num );
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
	return h5t_store_tet ( f, id, parent_id, vertex_ids );
}

h5_id_t
H5FedStoreTriangle (
	h5_file * f,			/*!< file handle		*/
	const h5_id_t id,		/*!< global tetrahedron id	*/
	const h5_id_t parent_id,	/*!< global parent id
					     if level \c >0 else \c -1	*/
	h5_id_t vertex_ids[3]		/*!< tuple with vertex id's	*/
	) {
	SET_FNAME ( __func__ );
	return h5t_store_triangle ( f, id, parent_id, vertex_ids );
}
