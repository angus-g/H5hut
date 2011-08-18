/*
  Header file for declaring the H5Fed application programming
  interface (API) in the C language.
  
  Copyright 2006-2009
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Benedikt Oswald;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Achim Gsell
  
  Warning
	This code is under development.
 
 */

#ifndef __H5FED_STORE_H
#define __H5FED_STORE_H

#ifdef __cplusplus
extern "C" {
#endif

static inline h5_err_t
H5FedAddTetrahedralMesh (
	h5_file_t* const f,
	const char* name,
	h5t_mesh_t** mesh
	) {
	H5_API_ENTER (h5_err_t, "f=%p, name=%s, mesh=%p", f, name, mesh);
	H5_API_RETURN (h5t_add_tetrahedral_mesh (f, name, mesh));
}

static inline h5_err_t
H5FedAddTriangleMesh (
	h5_file_t* const f,
	const char* name,
	h5t_mesh_t** mesh
	) {
	H5_API_ENTER (h5_err_t, "f=%p, name=%s, mesh=%p", f, name, mesh);
	H5_API_RETURN (h5t_add_triangle_mesh (f, name, mesh));
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
static inline h5t_lvl_idx_t
H5FedAddLevel (
	h5t_mesh_t* const m
	) {
	H5_API_ENTER (h5t_lvl_idx_t, "m=%p", m);
	H5_API_RETURN (h5t_add_level (m));
}

static inline h5_err_t
H5FedBeginStoreVertices (
	h5t_mesh_t* const m,
	const h5_size_t num
	) {
	H5_API_ENTER (h5_err_t,
		      "m=%p, num=%llu",
		      m, (long long unsigned)num);
	H5_API_RETURN (h5t_begin_store_vertices (m, num));
}

/*!
  \ingroup h5fed_c_api

  Stores the the coordinates of a specific vertex at level \c level
  with id \c vertex_id of the tetrahedral mesh.

  \return local vertex id on success
  \return errno on error
*/
static inline h5_loc_idx_t
H5FedStoreVertex (
	h5t_mesh_t* const m,		/*!< file handle		*/
	const h5_glb_id_t vertex_id,	/*!< id from mesher or -1	*/
	const h5_float64_t P[3]		/*!< coordinates		*/
	) {
	H5_API_ENTER (h5_loc_idx_t,
		      "m=%p, vertex_id=%lld, P=%p",
		      m, (long long)vertex_id, P);
	if (h5t_get_level (m) != 0) {
		H5_API_LEAVE (
			h5_error (
				H5_ERR_INVAL,
				"Vertices can be added to level 0 only!"));
	}
	H5_API_RETURN (h5t_store_vertex (m, vertex_id, P));
}

static inline h5_err_t
H5FedEndStoreVertices (
	h5t_mesh_t* const m
	) {
	H5_API_ENTER (h5_err_t, "m=%p", m);
	H5_API_RETURN (h5t_end_store_vertices (m));
}

static inline h5_err_t
H5FedBeginStoreElements (
	h5t_mesh_t* const m,
	const h5_size_t num
	) {
	H5_API_ENTER (h5_err_t,
		      "m=%p, num=%llu",
		      m, (long long unsigned)num);
	H5_API_RETURN (h5t_begin_store_elems (m, num));
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
static inline h5_loc_idx_t
H5FedStoreElement (
	h5t_mesh_t* const m,		/*!< file handle		*/
	const h5_loc_idx_t local_vids[]	/*!< tuple with vertex id's	*/
	) {
	H5_API_ENTER (h5_loc_idx_t, "m=%p, local_vids=%p", m, local_vids);
	if (h5t_get_level (m) != 0) {
		H5_API_LEAVE (
			h5_error (
				H5_ERR_INVAL,
				"Elements can be added to level 0 only!"));
	}
	H5_API_RETURN (h5t_store_elem (m, -1, local_vids));
}

static inline h5_err_t
H5FedEndStoreElements (
	h5t_mesh_t* const m
	) {
	H5_API_ENTER (h5_err_t, "m=%p", m);
	H5_API_RETURN (h5t_end_store_elems (m));
}

static inline h5_err_t
H5FedBeginRefineElements (
	h5t_mesh_t* const m
	) {
	H5_API_ENTER (h5_err_t, "m=%p", m);
	H5_API_RETURN (h5t_begin_refine_elems (m));
}

static inline h5_loc_idx_t
H5FedRefineElement (
	h5t_mesh_t* const m,		/*!< file handle		*/
	const h5_loc_id_t local_eid	/*!< local element id		*/
	) {
	H5_API_ENTER (h5_loc_idx_t,
		      "m=%p, local_eid=%lld",
		      m, (long long)local_eid);
	H5_API_RETURN (h5t_mark_entity (m, local_eid));
}

static inline h5_err_t
H5FedEndRefineElements (
	h5t_mesh_t* const m
	) {
	H5_API_ENTER (h5_err_t, "m=%p", m);
	H5_API_RETURN (h5t_end_refine_elems (m));
}

#ifdef __cplusplus
}
#endif

#endif
