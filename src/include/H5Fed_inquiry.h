/*
  Header file for declaring the H5Fed application programming
  interface (API) in the C language.
  
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
#ifndef __H5FED_INQUIRY_H
#define __H5FED_INQUIRY_H

#ifdef __cplusplus
extern "C" {
#endif

/*!
  Get number of meshes of given type.

  \param[in]	f	File handle
  \param[in]	type_id	Type of mesh we want the number of.

  \return	Number of meshes of type \c type_id or error code.
 */
static inline h5_ssize_t
H5FedGetNumTetrahedralMeshes ( 
	h5_file_t* const f
	) {
	H5_API_ENTER1 (h5_err_t, "f=0x%p", f);
	H5_API_RETURN (h5t_get_num_tetmeshes (f));
}

static inline h5_ssize_t
H5FedGetNumTriangleMeshes ( 
	h5_file_t* const f
	) {
	H5_API_ENTER1 (h5_err_t, "f=0x%p", f);
	H5_API_RETURN (h5t_get_num_trimeshes (f));
}

h5_ssize_t H5FedGetNumLevels ( h5_file_t * const f );
h5t_lvl_idx_t H5FedGetLevel ( h5_file_t * const f );
h5_ssize_t H5FedGetNumVertices ( h5_file_t * const f );
h5_ssize_t H5FedGetNumVerticesCnode ( h5_file_t * const f, const int cnode );
h5_ssize_t H5FedGetNumVerticesTotal ( h5_file_t * const f );
h5_ssize_t H5FedGetNumElements ( h5_file_t * const f );
h5_ssize_t H5FedGetNumElementsCnode ( h5_file_t * const f, const int cnode );
h5_ssize_t H5FedGetNumElementsTotal ( h5_file_t * const f );

#ifdef __cplusplus
}
#endif

#endif
