/******	STORE routines*****************************************************/
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
	H5FedSetNumVertices( f, nv );
	H5FedSetNumTetrahedra( f, nt );
*/
  
h5_id_t H5FedAddLevel (
	h5_file * f			/*!< file handle		*/
	) {
	struct h5t_fdata *t = &f->t;

	if ( f->mode == H5_O_RDONLY ) {
		return -1;
	}
	if ( t->num_levels == -1 ) {	/* unknown number of levels	*/
		/* determine number of levels */
		return -1;		/* not implemented		*/
	}
	t->cur_level = t->num_levels++;

	ssize_t num_bytes = t->num_levels*sizeof ( t->levels[0] );
	t->levels = realloc ( t->levels, num_bytes );

	struct h5t_fdata_level *level = t->levels[t->cur_level];

	memset ( level, 0, sizeof ( *level ) );

	level->new_level = 1;
	level->last_stored_vertex = -1;
	level->last_stored_tet = -1;

	return t->cur_level;
}

/*!
  Set number of vertices in current step and level
*/
h5_int_t
H5FedSetNumVertices (
	h5_file * f,			/*!< file handle		*/
	const h5_size_t num_vertices	/*!< number of verices at level	*/
	) {
	struct h5t_fdata_level *level = &f->t.levels[f->t.cur_level];
	ssize_t num_bytes;

	level->num_vertices = num_vertices;
	num_bytes = num_vertices * sizeof( level->vertices[0] );
	level->vertices = realloc ( level->vertices, num_bytes );

	return num_vertices;
}

h5_int_t
H5FedSetNumTetrahedra (
	h5_file * fh,			/*!< file handle		*/
	const h5_size_t num_tets	/*!< number of tetrahedra at
					  level \c level		*/
	) {
	struct h5t_fdata_level *level = &f->t.levels[f->t.cur_level];
	ssize_t num_bytes;

	level->num_tets = num_tets;
	num_bytes = num_tets * sizeof( level->tets[0] );
	level->tets = realloc ( level->tets, num_bytes );

	return num_tets;
}

/*!
  \ingroup h5fed_c_api

  Stores the the coordinates of a specific vertex at level \c level
  with id \c vertex_id of the tetrahedral mesh.

  \return local vertex id on success
  \return \c -1 on error
*/
h5_id_t
H5FedStoreVertex (
	h5_file * f,			/*!< file handle		*/
	const h5_id_t vertex_id,	/*!< global vertex id		*/
	const h5_float64_t P[3]		/*!< coordinates		*/
	) {

	struct h5t_fdata *t = &f->t

	if ( t->cur_level < 0 ) return -1;

	struct h5t_fdata_level *level = t->levels[t->cur_level];
	if ( level->last_stored_vertex+1 >= level->num_vertices ) 
		return -1;

	h5_vertex *vertex = level->vertices[++level->last_stored_vertex];
	vertex->id = vertex_id;
	vertex->level = t->cur_level;
	memcpy ( vertex->P, P, sizeof ( P ) );
	
	return level->last_stored_vertex;
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
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t edge_id,		/*!< global edge id		*/
	const h5_id_t parent_id,	/*!< parent id if level \c >0
					  else \x -1			*/
	const h5_id_t edge[2]		/*!< tuple with vertex id's	*/
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
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t triangle_id,	/*!< global triangle id		*/
	const h5_id_t parent_id,	/*!< parent id if level \c >0
					     else \x -1			*/
	const h5_id triangle[3]		/*!< tuple with vertex id's	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Stores the 4-tuple, that contains the specific indices describing
  a tetrahedron with id \c tet_id at level \c level of the tetrahedral
  mesh.

  \return local tetrahedron id
  \return \c -1 on error
*/
h5_int_t
H5FedStoreTetrahedron (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t tet_id,		/*!< global tetrahedron id	*/
	const h5_id_t parent_id,	/*!< parent id if level \c >0
					     else \x -1			*/
	const h5_id_t tet[4];		/*!< tuple with vertex id's	*/
	) {

	struct h5t_fdata *t = &f->t

	if ( t->cur_level < 0 ) return -1;

	struct h5t_fdata_level *level = t->levels[t->cur_level];
	if ( level->last_stored_tet+1 >= level->num_tets ) 
		return -1;

	h5_tetrahedron *tet = level->tets[++level->last_stored_tet];
	tet->id = tet_id;
	tet->level = t->cur_level;
	memcpy ( tet->vertex_ids, tet, sizeof ( tet ) );
	
	return level->last_stored_vertex;

}

/*!
  \ingroup h5fed_c_api

  Writes the vector, that contains the specific indices that describe
  a boundary triangle \c btriangle with id \c btriangle_id at level
  \c level of the tetrahedral mesh.

  \return local boundary triangle id
  \return \c -1 on error
*/
h5_int_t H5FedStoreBoundaryTriangle (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t btriangle_id,	/*!< global boundary triangle id*/
	const h5_id_t parent_id,	/*!< parent id if level \c >0
					     else \c -1			*/
	const h5_id_t btriangle[3]	/*!< tuple with vertex id's*/
	) {
	return -1;
}
