/******	routines for accessing degrees of freedom *************************/
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
#include "h5_core/h5_types.h"
#include "H5Fed.h"

/* DoF acces for vertices */

/*!
  \ingroup h5fed_c_api

  Stores DoF of type \c h5_float for the vertex \c vertex_id in current
  step. The DoF are passed to the function as n-tuple with an arbitrary
  number of elements.

  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_err_t
H5FedStoreDoFVertexFloat (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t vertex_id,	/*!< global vertex id		*/
	const h5_float64_t * const dof,	/*!< DoF n-tuple		*/
	const h5_size_t dof_size	/*!< number of \c h5_float 
					      value in \c dof		*/
	) {
	return -1;
}
 
/*!
  \ingroup h5fed_c_api

  Stores DoF of type \c h5_complex for the vertex \c vertex_id in current
  step. The DoF are passed to the function as n-tuple with an arbitrary
  number of elements.
  
  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_err_t
H5FedStoreDoFVertexComplex (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t vertex_id,	/*!< global vertex id		*/
	const h5_complex_t * const dof,	/*!< DoF n-tuple		*/
	const h5_size_t dof_size	/*!< number of \c h5_complex
					     values in \c dof		*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Get DoF of type \c h5_float for vertex \c vertex_id in current time
  step.  The DoF are returned as n-tuple. The size of this tuple is
  returned in the parameter \c dof_size.

  \return Pointer to n-tuple of type \c h5_float.
  \return NULL-pointer on error.
*/
h5_float64_t *
H5FedGetDoFVertexFloat (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t vertex_id,	/*!< global vertex id		*/
	h5_size_t * const dof_size	/*!< OUT: number of \c h5_float
					     values in returned n-tuple	*/
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Get DoF of type \c h5_complex for vertex \c vertex_id in current time
  step.  The DoF are returned as n-tuple. The size of this tuple is
  returned in the parameter \c dof_size.

  \return Pointer to n-tuple of type \c h5_float.
  \return NULL-pointer on error.
*/
h5_complex_t * H5FedGetDoFVertexComplex (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t vertex_id,	/*!< global vertex id		*/
	h5_size_t * const dof_size	/*!< OUT: number of \c h5_float
					     values in returned n-tuple	*/
	) {
	return NULL;
}

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
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t vertex_id		/*!< global vertex id		*/
	) {
	return -1;
}

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
	h5_file_t * fh,			/*!< file handle		*/
	const h5_size_t vertex_id	/*!< global vertex id		*/
	) {
	return -1;
}

/* DoF access for edges */

/*!
  \ingroup h5fed_c_api

  Stores DoF of type \c h5_float for the edge \c edge_id in current
  step. The DoF are passed to the function as n-tuple with an arbitrary
  number of elements.

  \return something \c >=0 on success
  \return \c -1 on error
*/
h5_err_t
H5FedStoreDoFEdgeFloat (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t edge_id,		/*!< global edge id		*/
	const h5_float64_t * const dof,	/*!< DoF n-tuple		*/
	const h5_size_t dof_size	/*!< number of \c h5_float 
					     value in \c dof		*/
	) {
	return -1;
}
 
/*!
  \ingroup h5fed_c_api

  Stores DoF of type \c h5_complex for the edge \c edge_id in current
  step. The DoF are passed to the function as n-tuple with an arbitrary
  number of elements.

  \return something \c >=0 on success
  \return \c -1 on error
*/
h5_err_t
H5FedStoreDoFEdgeComplex (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t edge_id,		/*!< global edge id		*/
	const h5_complex_t * const dof,	/*!< DoF n-tuple		*/
	const h5_size_t dof_size	/*!< number of \c h5_complex 
					     value in \c dof		*/
	) {
	return -1;
}
 
/*!
  \ingroup h5fed_c_api

  Get DoF of type \c h5_float for vertex \c vertex_id in current time
  step.  The DoF are returned as n-tuple. The size of this tuple is
  returned in the parameter \c dof_size.

  \return Pointer to n-tuple of type \c h5_float.
  \return NULL-pointer on error.
*/
h5_float64_t *
H5FedGetDoFEdgeFloat (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t edge_id,		/*!< global edge id		*/
	h5_size_t * const dof_size	/*!< OUT: number of \c h5_float
					     values in returned n-tuple	*/
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Get DoF of type \c h5_complex for vertex \c vertex_id in current time
  step.  The DoF are returned as n-tuple. The size of this tuple is
  returned in the parameter \c dof_size.

  \return Pointer to n-tuple of type \c h5_float.
  \return NULL-pointer on error.
*/
h5_complex_t* H5FedGetDoFEdgeComplex (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t edge_id,		/*!< global edge id		*/
	h5_size_t * const dof_size	/*!< OUT: number of \c h5_float
					     values in returned n-tuple	*/
	) {
	return NULL;
}

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
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t edge_id		/*!< global edge id		*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Retrieves the number of scalar components, of type \c h5_complex,
  of the DoF that is stored for edge \c edge_id.

  \return Number of stored degrees of freedom.
  \return \c -1 if edge doesn't exist.
*/
h5_size_t H5FedGetNumDoFEdgeComplex (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t edge_id		/*!< global edge id		*/
	) {
	return -1;
}


/* DoF access for triangles */

/*!
  \ingroup h5fed_c_api

  Stores DoF of type \c h5_float for the triangle \c triangle_id in current
  step. The DoF are passed to the function as n-tuple with an arbitrary
  number of elements.

  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_err_t
H5FedStoreDoFTriangleFloat (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	const h5_float64_t * const dof,	/*!< DoF vector			*/
	const h5_size_t dof_size	/*!< number of \c h5_float_t
					     values in \c dof		*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Stores DoF of type \c h5_complex for the triangle \c triangle_id in current
  step. The DoF are passed to the function as n-tuple with an arbitrary
  number of elements.

  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_err_t
H5FedStoreDoFTriangleComplex (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	const h5_complex_t * const dof,	/*!< DoF vector			*/
	const h5_size_t dof_size	/*!< number of \c h5_complex
					     values in \c dof		*/
	) {
	return -1;
}
 
/*!
  \ingroup h5fed_c_api

  Get DoF of type \c h5_float for triangle \c triangle_id in current time
  step.  The DoF are returned as n-tuple. The size of this tuple is
  returned in the parameter \c dof_size.

  \return Pointer to n-tuple of type \c h5_float.
  \return NULL-pointer on error.
*/
h5_float64_t *
H5FedGetDoFTriangleFloat (
	h5_file_t * f,			/*!< file handle		*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	h5_size_t * const dof_size	/*!< OUT: number of \c h5_float
					     values in returned n-tuple	*/
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Get DoF of type \c h5_complex for triangle \c triangle_id in current time
  step.  The DoF are returned as n-tuple. The size of this tuple is
  returned in the parameter \c dof_size.

  \return Pointer to n-tuple of type \c h5_float.
  \return NULL-pointer on error.
*/
h5_complex_t * H5FedGetDoFTriangleComplex (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	h5_size_t * const dof_size	/*!< OUT: number of \c h5_float
					     values in returned n-tuple	*/
	) {
	return NULL;
}

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
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t triangle_id	/*!< global triangle id		*/
	) {
	return -1;
}

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
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t triangle_id	/*!< global triangle id		*/
	) {
	return -1;
}


/* DoF access for tetrahedra */

/*!
  \ingroup h5fed_c_api

  Stores DoF of type \c h5_float for the tetrahedron \c tet_id in current
  step. The DoF are passed to the function as n-tuple with an arbitrary
  number of elements.

  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_err_t
H5FedStoreDoFTetrahedronFloat (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	const h5_float64_t * const dof,	/*!< DoF vector			*/
	const h5_size_t dof_size	/*!< number of \c h5_float 
					     value in \c dof		*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Stores DoF of type \c h5_complex for the tetrahedron \c tet_id in current
  step. The DoF are passed to the function as n-tuple with an arbitrary
  number of elements.

  \return value \c >=0 on success
  \return \c -1 on error
*/
h5_err_t
H5FedStoreDoFTetrahedronComplex (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	const h5_complex_t * const dof,	/*!< DoF vector			*/
	const h5_size_t dof_size	/*!< number of \c h5_complex
					     value in \c dof		*/
	) {
	return -1;
};
 
/*!
  \ingroup h5fed_c_api

  Get DoF of type \c h5_float for tetrahedron \c tet_id in current time
  step.  The DoF are returned as n-tuple. The size of this tuple is
  returned in the parameter \c dof_size.

  \return Pointer to n-tuple of type \c h5_float.
  \return NULL-pointer on error.
*/
h5_float64_t *
H5FedGetDoFTetrahedronFloat (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	h5_size_t * const dof_size	/*!< OUT: number of \c h5_float
					     values in returned n-tuple	*/
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Get DoF of type \c h5_complex for tetrahedron \c tet_id in current time
  step.  The DoF are returned as n-tuple. The size of this tuple is
  returned in the parameter \c dof_size.

  \return Pointer to n-tuple of type \c h5_complex
  \return NULL-pointer on error.
*/
h5_complex_t * 
H5FedGetDoFTetrahedronComplex (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	h5_size_t * const dof_size	/*!< OUT: number of \c h5_complex
					     values in returned n-tuple	*/
	) {
	return NULL;
}

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
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t tet_id		/*!< global tetrahedron id	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Retrieves the number of scalar components of type \c h5_complex,
  of the DoF that is stored for tetrahedron \c tet_id.

  \return Number of stored degrees of freedom.
  \retuen \c -1 if tetrahedron doesn't exist.
*/
h5_size_t H5FedGetNumDoFTetrahedronComplex (
	h5_file_t * fh,			/*!< file handle		*/
	const h5_id_t tet_id		/*!< global tetrahedron id	*/
	) {
	return -1;
}
