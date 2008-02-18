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
	       

vertex_t V0[5] = {
	{ {-1.0,  0.0,  0.0} },
	{ { 1.0,  0.0,  0.0} },
	{ { 0.0,  1.0,  0.0} },
	{ { 0.0,  0.0,  1.0} },
	{ { 0.0, -1.0,  0.0} }
};

vertex_t V1[1] = {
	{{  0.0,  0.0,  0.0 }}
};

tet_t T0[2] = {
	{ 1, -1, { 0, 1, 2, 3 } },
	{ 0, -1, { 0, 1, 3, 4 } }
};

tet_t T1[2] = {
	{ 2, 0, { 0, 3, 4, 5 } },
	{ 3, 0, { 1, 3, 4, 5 } }
};

h5_err_t
add_level (
	h5_file *f,
	vertex_t V[],
	int num_verts,
	tet_t T[],
	int num_tets
	) {

	h5_err_t h5err = H5FedAddLevel ( f );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Can't add level.\n" );
		return -1;
	}
	h5err = H5FedSetAdditionalNumVerticesToStore ( f, num_verts );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Can't set number of vertices.\n" );
		return -1;
	}

	int i;
	for ( i = 0; i<num_verts; i++ ) {
		h5err = H5FedStoreVertex (
			f,
			i,
			V[i].P );
		if ( h5err < 0 ) {
			fprintf ( stderr, "!!! Can't store vertex.\n" );
			return -1;
		}
	}
	h5err = H5FedSetAdditionalNumTetrahedraToStore ( f, num_tets );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Can't set number of tets.\n" );
		return -1;
	}

	for ( i = 0; i<num_tets; i++ ) {
		h5err = H5FedStoreTetrahedron (
			f,
			T[i].global_id,
			T[i].parent_id,
			T[i].vids );
		
		if ( h5err < 0 ) {
			fprintf ( stderr, "!!! Can't store tet.\n" );
			return -1;
		}
	}
	return 0;
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

	h5_err_t h5err = H5FedAddMesh ( f );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Can't set step.\n" );
		return -1;
	}

	h5err = add_level ( f, V0, 5, T0, 2 );
	h5err = add_level ( f, V1, 1, T1, 2 );

	h5err = H5FedCloseFile ( f );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Can't close file.\n" );
		return -1;
	}
	return 0;
}
