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
#include "h5/h5_types.h"
#include "h5/h5.h"
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

  H5_ERR_INVAL:	It is not possible to add vertices due to limitation of
  the library

  H5_ERR_NOMEM: Couldn't allocate enough memory.

  \return number of vertices
  \return errno	on error
*/
h5_size_t
H5FedSetAdditionalNumVerticesToStore (
	h5_file * f,			/*!< file handle		*/
	const h5_size_t num		/*!< number of additional vertices */
	) {

	struct h5t_fdata *t = &f->t;

	ssize_t num_elems = (t->cur_level > 0 ? t->num_vertices[t->cur_level-1] : 0)
		+ num;

	t->num_vertices[t->cur_level] = num_elems;
	t->vertices = realloc ( t->vertices, num_elems*sizeof ( t->vertices[0] ) );

	if ( t->vertices == NULL ) {
		return H5_ERR_NOMEM;
	}

	return num;
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
	return -1;
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

	struct h5t_fdata *t = &f->t;

	if ( t->cur_level < 0 )
		return H5_ERR_INVAL;

	if ( t->last_stored_vertex_id+1 >= t->num_vertices[t->cur_level] )
		return H5_ERR_INVAL;

	h5_vertex *vertex = &t->vertices[++t->last_stored_vertex_id];
	vertex->id = id;
	memcpy ( &vertex->P, P, sizeof ( vertex->P ) );
	
	return t->last_stored_vertex_id;
}

/*!
  \ingroup h5fed_c_api

  Get coordinates of the vertex \c vertex_id.

  \return local id
  \return error code (H5_ERR_NOENT means no more vertices on this level)
 
 */
h5_id_t
H5FedGetVertex (
	h5_file * f,			/*!< file handle		*/
	h5_id_t	* const id,		/*!< OUT: global id		*/
	h5_float64_t * const P[3]	/*!< OUT: coordinates		*/
	) {
	return -1;
}

/*** E D G E S ***************************************************************/

h5_size_t
H5FedSetNumEdges (
	h5_file * f,
	const h5_size_t num
	) {
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
	return -1;
}

/*** T R I A N G L E S *******************************************************/

h5_size_t
H5FedSetNumTriangles (
	h5_file * f,
	const h5_size_t num
	) {
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
h5_id_t
H5FedGetTriangle (
	h5_file * f,			/*!< file handle		*/
	h5_id_t * const id,		/*!< OUT: global triangle id	*/
	h5_id_t * const parent_id,	/*!< OUT: parent id if level
					     \c >0 else \c -1		*/
	h5_id_t * const vertex_ids[3]	/*!< OUT: vertex id's		*/
	) {
	return -1;
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
h5_id_t H5FedStoreBoundaryTriangle (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t btriangle_id,	/*!< global boundary triangle id*/
	const h5_id_t parent_id,	/*!< parent id if level \c >0
					     else \c -1			*/
	const h5_id_t btriangle[3]	/*!< tuple with vertex id's*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Get the definition of the boundary triangle \c triangle_id at level
  \c level, i.e. the indices of the 3-dimensional vertex coordinates.

  \return pointer to 3-tuple of vertex id's defining the boundary triangle.
  \return NULL-pointer on error.
*/
h5_id_t H5FedGetBoundaryTriangle (
	h5_file * f,			/*!< file handle		*/
	h5_id_t * const id,		/*!< global btriangle_id	*/
	h5_id_t * const parent_id,	/*!< OUT: parent id if level
					     \c >0 else \c -1		*/
	h5_id_t * const vertex_ids[3]	/*!< OUT: vertex id's		*/
	) {
	return -1;
}

/*** T E T R A H E D R A ****************************************************/

h5_size_t
H5FedSetAdditionalNumTetrahedraToStore (
	h5_file * f,			/*!< file handle		*/
	const h5_size_t num		/*!< number of additional
					  tetrahedra at level \c level	*/
	) {

	struct h5t_fdata *t = &f->t;

	ssize_t num_elems = (t->cur_level > 0 ? t->num_tets[t->cur_level-1] : 0)
		+ num;

	t->num_tets[t->cur_level] = num_elems;
	t->tets = realloc ( t->tets, num_elems*sizeof ( t->tets[0] ) );
	if ( t->tets == NULL ) {
		return H5_ERR_NOMEM;
	}
	t->map_tets_g2l = realloc (
		t->map_tets_g2l,
		num_elems*sizeof ( t->map_tets_g2l[0] ) );

	return num;
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
	return -1;
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
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	const h5_id_t parent_id,	/*!< global parent id
					     if level \c >0 else \c -1	*/
	const h5_id_t vertex_ids[4]	/*!< tuple with vertex id's	*/
	) {

	struct h5t_fdata *t = &f->t;

	/*
	  more than allocated
	*/
	if ( t->last_stored_tet_id+1 >= t->num_tets[t->cur_level] ) 
		return H5_ERR_INVAL;

	/*
	  missing call to add the first level
	 */
	if ( t->cur_level < 0 )
		return H5_ERR_INVAL;

	/*
	  check parent id
	*/
	if ( (t->cur_level == 0) && (parent_id != -1) ) {
		return H5_ERR_INVAL;
	} 
	if ( (t->cur_level >  0) && (parent_id < 0) ) {
		return H5_ERR_INVAL;
	}
	if ( (t->cur_level >  0) && (parent_id >= t->num_tets[t->cur_level-1]) ) {
		return H5_ERR_INVAL;
	}
	/*
	  check tet_id
	*/
	if ( (t->cur_level == 0) && (
		     (tet_id < 0) || (tet_id >= t->num_tets[0]) ) ) {
		return H5_ERR_INVAL;
	}
	if ( (t->cur_level > 0) && (
		     (tet_id <  t->num_tets[t->cur_level-1]) ||
		     (tet_id >= t->num_tets[t->cur_level]) ) ) {
		return H5_ERR_INVAL;
	}
	

	h5_tetrahedron *tet = &t->tets[++t->last_stored_tet_id];
	tet->id = tet_id;
	tet->parent_id = parent_id;
	tet->refined_on_level = -1;
	tet->unused = 0;
	memcpy ( &tet->vertex_ids, vertex_ids, sizeof ( tet->vertex_ids ) );

	t->map_tets_g2l[tet_id] = t->last_stored_tet_id;
	if ( parent_id >= 0 ) {
		h5_id_t local_parent_id = t->map_tets_g2l[parent_id];
		t->tets[local_parent_id].refined_on_level = t->cur_level;
	}
	return t->last_stored_vertex_id;
}

/*!
  \ingroup h5fed_c_api

  Get the definition of a specific tetrahedron \c tetra_id, i.e. 
  a 4-tuple containing the specific indices of the 3-dimensional vertex
  coordinates.

  \return pointer to 4-tuple of vertex id's defining the tetrahedron.
  \return NULL-pointer on error.
*/
h5_id_t H5FedGetTetrahedron (
	h5_file * f,			/*!< file handle		*/
	h5_id_t * const id,		/*!< OUT: global tetrahedron id	*/
	h5_id_t * parent_id,		/*!< OUT: parent id if level
					     \c >0 else \c -1		*/
	h5_id_t * const vertex_ids[4]	/*!< OUT: vertex id's		*/
	) {
	return -1;
}

h5_err_t
H5FedSetStep (
	h5_file *f,			/*!< Handle to open file */
	const h5_id_t step		/*!< Time-step to set. */
	) {

	return (h5_err_t) H5_set_step ( f, step );
}


h5_id_t
H5FedAddMesh (
	h5_file * f
	) {
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
	struct h5t_fdata *t = &f->t;

	if ( f->mode == H5_O_RDONLY ) {
		return H5_ERR_INVAL;
	}

	/* t->num_levels will be set to zero on file creation(!) */
	if ( t->num_levels == -1 ) {	/* unknown number of levels	*/
		/* determine number of levels */
		return -1;		/* not implemented		*/
	}
	t->cur_level = t->num_levels++;

	ssize_t num_bytes = t->num_levels*sizeof ( h5_size_t );
	t->num_vertices = realloc ( t->num_vertices, num_bytes );
	t->num_vertices[t->cur_level] = -1;

	t->num_tets = realloc ( t->num_tets, num_bytes );
	t->num_tets[t->cur_level] = -1;

	t->new_level = t->cur_level;
	if ( t->cur_level == 0 ) {
		/* nothing stored yet */
		t->last_stored_vertex_id = -1;
		t->last_stored_tet_id = -1;
	}
	return t->cur_level;
}
