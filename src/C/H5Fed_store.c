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
#include "h5core/h5_core.h"
#include "H5Fed.h"


h5_id_t
H5FedAddMesh (
	h5_file_t* const f,
	const h5_oid_t mesh_type_id
	) {
	SET_FNAME (f, __func__);
	return h5t_add_mesh (f, mesh_type_id);
}

/*!
  \ingroup h5fed_c_api

  Add a new level with \c num_elems elements. The number of elements must be the
  real number of elements to add the level. If you want to refine \c n tetrahedra
  \c n*8 elements must be added.

  \param[in]	f			File handle.
  \param[in]	num_elems_to_refine	Number of elements which will be refined.

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
	h5_file_t* const f
	) {
	SET_FNAME (f, __func__);
	return h5t_add_level (f);
}

h5_err_t
H5FedBeginStoreVertices (
	h5_file_t* const f,
	const h5_size_t num
	) {
	SET_FNAME (f, __func__);
	return h5t_begin_store_vertices (f, num);
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
	h5_file_t* const f,		/*!< file handle		*/
	const h5_id_t vertex_id,	/*!< id from mesher or -1	*/
	const h5_float64_t P[3]		/*!< coordinates		*/
	) {

	SET_FNAME (f, __func__);
	if (h5t_get_level (f) != 0) {
		return h5_error (
			f,
			H5_ERR_INVAL,
			"Vertices can be added to level 0 only!");
	}
	return h5t_store_vertex (f, vertex_id, P);
}

h5_err_t
H5FedEndStoreVertices (
	h5_file_t* const f
	) {
	SET_FNAME (f, __func__);
	return h5t_end_store_vertices (f);
}

h5_err_t
H5FedBeginStoreElements (
	h5_file_t* const f,
	const h5_size_t num
	) {
	SET_FNAME (f, __func__);
	return h5t_begin_store_elems (f, num);
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
	h5_file_t* const f,		/*!< file handle		*/
	const h5_id_t local_vids[]	/*!< tuple with vertex id's	*/
	) {
	SET_FNAME (f, __func__);
	if (h5t_get_level (f) != 0) {
		return h5_error (
			f,
			H5_ERR_INVAL,
			"Elements can be added to level 0 only!");
	}
	return h5t_store_elem ( f, -1, local_vids );
}

h5_err_t
H5FedEndStoreElements (
	h5_file_t* const f
	) {
	SET_FNAME (f, __func__);
	return h5t_end_store_elems (f);
}

h5_err_t
H5FedBeginRefineElements (
	h5_file_t* const f
	) {
	SET_FNAME (f, __func__);
	return h5t_begin_refine_elems (f);
}

h5_id_t
H5FedRefineElement (
	h5_file_t* const f,		/*!< file handle		*/
	const h5_id_t local_eid		/*!< local element id		*/
	) {
	SET_FNAME (f, __func__);
	return h5t_refine_elem (f, local_eid);
}

h5_err_t
H5FedEndRefineElements (
	h5_file_t* const f
	) {
	SET_FNAME (f, __func__);
	return h5t_end_refine_elems (f);
}
