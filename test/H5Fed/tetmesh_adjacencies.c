#include <stdio.h>
#include <stdlib.h>
#include <hdf5.h>
#include "H5Part.h"
#include "H5Fed.h"

#ifndef PARALLEL_IO
#ifndef MPI_COMM_WORLD
#define MPI_COMM_WORLD 0
#endif
#endif

struct vertex {
	h5_float64_t P[3];
};

typedef struct vertex vertex_t; 

struct tet {
	h5_id_t global_id;
	h5_id_t parent_id;
	h5_id_t vids[4];
};
typedef struct tet tet_t;

h5_err_t
traverse_vertices (
	h5_file_t * f
	) {
	h5_id_t id, local_id;
	h5_float64_t P[3];
	h5_size_t real_num = 0;

	h5_size_t num = H5FedGetNumVerticesTotal ( f );
	printf ( "    Number of vertices on level: %lld\n", num );

	h5_err_t h5err = H5FedBeginTraverseVertices ( f );
	if ( h5err < 0 ) return h5err;
	while ( (real_num < num) &&
		((local_id = H5FedTraverseVertices ( f, &id, P )) >= 0) ) {
		printf ( "    Vertex[%lld]: local id: %lld, coords: %f %f %f \n",
			 id, local_id, P[0], P[1], P[2] );
		h5_idlist_t *list;
		h5_id_t i;
		H5FedGetEdgesUpAdjacentToVertex ( f, local_id, &list );
		for ( i = 0; i < list->num_items; i++ ) {
			h5_id_t local_vids[2];
			H5FedMapEntity2LocalVids ( f, list->items[i], local_vids );
			printf ( "      Edge ID: %llx = (%lld,%lld)\n",
				 list->items[i],
				 local_vids[0],
				 local_vids[1] );
		}
		H5FedReleaseListOfAdjacencies ( f, &list );
		H5FedGetTrianglesUpAdjacentToVertex ( f, local_id, &list );
		for ( i = 0; i < list->num_items; i++ ) {
			h5_id_t local_vids[2];
			H5FedMapEntity2LocalVids ( f, list->items[i], local_vids );
			printf ( "      Triangle ID: %llx = (%lld,%lld,%lld)\n",
				 list->items[i],
				 local_vids[0],
				 local_vids[1],
				 local_vids[2] );
		}
		H5FedReleaseListOfAdjacencies ( f, &list );
		H5FedGetTetsUpAdjacentToVertex ( f, local_id, &list );
		for ( i = 0; i < list->num_items; i++ ) {
			h5_id_t local_vids[2];
			H5FedMapEntity2LocalVids ( f, list->items[i], local_vids );
			printf ( "      Tet ID: %llx = (%lld,%lld,%lld,%lld)\n",
				 list->items[i],
				 local_vids[0],
				 local_vids[1],
				 local_vids[2],
				 local_vids[3] );
		}
		H5FedReleaseListOfAdjacencies ( f, &list );

		real_num++;
	}
	H5FedEndTraverseVertices ( f );

	if ( real_num != num ) {
		fprintf ( stderr, "!!! Got %lld vertices, but expected %lld.\n",
			  real_num, num );
		return -1;
	}
	return H5_SUCCESS;
}


h5_err_t
traverse_edges (
	h5_file_t * f
	) {
	h5_id_t local_id, vids[4];

	h5_err_t h5err = H5FedBeginTraverseEdges ( f );
	if ( h5err < 0 ) return h5err;
	while ( (local_id = H5FedTraverseEdges ( f, vids )) >= 0 ) {
		printf ( "    Edge[%lld]: (%lld,%lld)\n",
			 local_id, vids[0], vids[1] );
	}
	h5err = H5FedEndTraverseEdges ( f );
	return h5err;
}

h5_err_t
travers_tets (
	h5_file_t * f
	) {
	h5_id_t id, local_id, parent_id, vids[4];
	h5_size_t real_num = 0;

	h5_size_t num = H5FedGetNumElementsTotal ( f );
	printf ( "    Number of tetrahedra on level: %lld\n", num );

	h5_err_t h5err = H5FedBeginTraverseElements ( f );
	if ( h5err < 0 ) return h5err;

	while ( (real_num < num) &&
		((local_id = H5FedTraverseElements (
			  f, &id, &parent_id, vids )) >= 0) ) {
		printf ( "    Tet[%lld]: local id: %lld, parent id: %lld,"
			 " vids: %lld %lld %lld %lld\n",
			 id, local_id, parent_id,
			 vids[0], vids[1], vids[2], vids[3] );
		real_num++;
	}
	H5FedEndTraverseElements ( f );
	if ( real_num != num ) {
		fprintf ( stderr, "!!! Got %lld tets, but expected %lld.\n",
			  real_num, num );
		return -1;
	}

	return H5_SUCCESS;
}

h5_err_t
traverse_level (
	h5_file_t * f
	) {
	h5_err_t h5err = traverse_vertices ( f );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Oops ...\n" );
		return -1;
	}
	h5err = traverse_edges ( f );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Oops ...\n" );
		return -1;
	}
#if 0
	h5err = traverse_tets ( f );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Oops ...\n" );
		return -1;
	}
#endif
	return H5_SUCCESS;
}

h5_err_t
traverse_mesh (
	h5_file_t * f
	) {

	h5_id_t level_id;
	h5_size_t num_levels = H5FedGetNumLevels ( f );
	printf ( "    Number of levels in mesh: %lld\n", num_levels );
	for ( level_id = 0; level_id < num_levels; level_id++ ) {
		h5_err_t h5err = H5FedSetLevel ( f, level_id );
		if ( h5err < 0 ) {
			fprintf ( stderr, "!!! Can't set level %lld.\n", level_id );
			return -1;
		}
		h5err = traverse_level ( f );
		if ( h5err < 0 ) {
			fprintf ( stderr, "!!! Oops ...\n" );
			return -1;
		}
	}

	return H5_SUCCESS;
}

int
main (
	int argc,
	char *argv[]
	) {

	H5SetVerbosityLevel ( 4 );

	h5_file_t *f = H5OpenFile ( "simple_tet.h5", H5_O_RDONLY, 0 );
	if ( f == NULL ) {
		fprintf ( stderr, "!!! Can't open file.\n" );
		return -1;
	}

	h5_size_t num_meshes = H5FedGetNumMeshes ( f, H5_TETRAHEDRAL_MESH );
	printf ( "    Number of meshes: %lld\n", num_meshes );

	h5_id_t mesh_id;
	for ( mesh_id = 0; mesh_id < num_meshes; mesh_id++ ) {
		h5_err_t h5err = H5FedOpenMesh ( f, mesh_id, H5_TETRAHEDRAL_MESH );
		if ( h5err < 0 ) {
			fprintf ( stderr, "!!! Can't open mesh %lld\n", mesh_id );
			return -1;
		}
		h5err = traverse_mesh ( f );
		if ( h5err < 0 ) {
			fprintf ( stderr, "!!! Oops ...\n" );
			return 1;
		}
	}

	h5_err_t h5err = H5CloseFile ( f );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Can't close file.\n" );
		return -1;
	}
	return 0;
}
