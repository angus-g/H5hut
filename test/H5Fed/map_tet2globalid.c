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

int
main (
	int argc,
	char *argv[]
	) {

	H5PartSetVerbosityLevel ( 4 );

	h5_file *f = H5OpenFile ( "simple_tet.h5", 0 );
	if ( f == NULL ) {
		fprintf ( stderr, "!!! Can't open file.\n" );
		return -1;
	}

	h5_size_t num_meshes = H5FedGetNumMeshes ( f, TETRAHEDRAL_MESH );
	printf ( "    Number of meshes: %d\n", num_meshes );

	h5_id_t mesh_id = 0;
	
	h5_err_t h5err = H5FedOpenMesh ( f, mesh_id, TETRAHEDRAL_MESH );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Can't open mesh %d\n", mesh_id );
		return -1;
	}
	h5_id_t global_vids[4] = { 0, 3, 4, 5 };
	h5_id_t global_tid = H5FedMapTet2GlobalID ( f, global_vids );
	if ( global_tid < 0 ) {
		fprintf ( stderr, "!!! Oops ...\n" );
		return 1;
	}
	printf ( "    Global entity ID: %d\n", global_tid );

	h5err = H5CloseFile ( f );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Can't close file.\n" );
		return -1;
	}
	return 0;
}
