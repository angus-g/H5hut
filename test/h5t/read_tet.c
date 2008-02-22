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
read_vertices (
	h5_file * f
	) {
	h5_id_t id, local_id;
	h5_float64_t P[3];
	h5_size_t real_num = 0;

	h5_size_t num = H5FedGetNumVertices ( f );
	printf ( "    Number of vertices on level: %d\n", num );
	while ( (local_id = H5FedGetVertex ( f, &id, P )) >= 0 ) {
		printf ( "    Vertex[%d]: local id: %d, coords: %f %f %f \n",
			 id, local_id, P[0], P[1], P[2] );
		real_num++;
	}
	if ( real_num != num ) {
		fprintf ( stderr, "!!! Got %d vertices, but expected %d.\n",
			  real_num, num );
		return -1;
	}
	return H5_SUCCESS;
}

h5_err_t
read_tets (
	h5_file * f
	) {
	h5_id_t id, local_id, parent_id, vids[4];
	h5_size_t real_num = 0;

	h5_size_t num = H5FedGetNumTetrahedra ( f );
	printf ( "    Number of tetrahedra on level: %d\n", num );
	while ( (local_id = H5FedGetTetrahedron ( f, &id, &parent_id, vids )) >= 0 ) {
		printf ( "    Tet[%d]: local id: %d, parent id: %d, vids: %d %d %d %d\n",
			 id, local_id, parent_id, vids[0], vids[1], vids[2], vids[3] );
		real_num++;
	}
	if ( real_num != num ) {
		fprintf ( stderr, "!!! Got %d tets, but expected %d.\n",
			  real_num, num );
		return -1;
	}

	return H5_SUCCESS;
}

h5_err_t
read_level (
	h5_file * f
	) {
	h5_err_t h5err = read_vertices ( f );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Oops ...\n" );
		return -1;
	}
	h5err = read_tets ( f );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Oops ...\n" );
		return -1;
	}
	return H5_SUCCESS;
}


h5_err_t
read_mesh (
	h5_file * f
	) {

	h5_id_t level_id;
	h5_size_t num_levels = H5FedGetNumLevels ( f );
	printf ( "    Number of levels in mesh: %d\n", num_levels );
	for ( level_id = 0; level_id < num_levels; level_id++ ) {
		h5_err_t h5err = H5FedSetLevel ( f, level_id );
		if ( h5err < 0 ) {
			fprintf ( stderr, "!!! Can't set level %d.\n", level_id );
			return -1;
		}
		h5err = read_level ( f );
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

	H5PartSetVerbosityLevel ( 4 );

	h5_file *f = H5FedOpenFile ( "simple_tet.h5", 0 );
	if ( f == NULL ) {
		fprintf ( stderr, "!!! Can't open file.\n" );
		return -1;
	}

	h5_size_t num_meshes = H5FedGetNumMeshes ( f );
	printf ( "    Number of meshes: %d\n", num_meshes );

	h5_id_t mesh_id;
	for ( mesh_id = 0; mesh_id < num_meshes; mesh_id++ ) {
		h5_err_t h5err = H5FedOpenMesh ( f, mesh_id );
		if ( h5err < 0 ) {
			fprintf ( stderr, "!!! Can't open mesh %d\n", mesh_id );
			return -1;
		}
		h5err = read_mesh ( f );
		if ( h5err < 0 ) {
			fprintf ( stderr, "!!! Oops ...\n" );
			return 1;
		}
	}

	h5_err_t h5err = H5FedCloseFile ( f );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Can't close file.\n" );
		return -1;
	}
	return 0;
}
