#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "H5Part.h"
#include "H5Fed.h"

#ifndef PARALLEL_IO
#ifndef MPI_COMM_WORLD
#define MPI_COMM_WORLD 0
#endif
#endif

struct vertex {
	h5_id_t global_id;
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
	{ 0, {-1.0,  0.0,  0.0} },
	{ 1, { 1.0,  0.0,  0.0} },
	{ 2, { 0.0,  1.0,  0.0} },
	{ 3, { 0.0,  0.0,  1.0} },
	{ 4, { 0.0, -1.0,  0.0} }
};

// sorted vertices: 0, 4, 5, 3, 2, 1

tet_t T0[2] = {
	{ 1, -1, { 0, 1, 2, 3 } },	// 0, 3, 2, 1
	{ 0, -1, { 0, 1, 3, 4 } }	// 0, 4, 3, 1
};


// sorted 0th vertex tets: 2, 1, 0, 3
int
main (
	int argc,
	char *argv[]
	) {
	H5SetVerbosityLevel ( 2 );
	H5SetErrorHandler ( H5AbortErrorhandler );

	h5_file_t *f = H5OpenFile ( "simple_tet.h5", H5_O_WRONLY, 0 );
	H5FedAddMesh ( f, H5_TETRAHEDRAL_MESH );
	int i;
	H5FedBeginStoreVertices ( f, 5 );
	for ( i = 0; i<5; i++ ) {
		H5FedStoreVertex ( f, -1, V0[i].P );
	}
	H5FedEndStoreVertices ( f );

	H5FedBeginStoreElements ( f, 2 );
	for ( i = 0; i<2; i++ ) {
		H5FedStoreElement ( f, T0[i].vids );
	}
	H5FedEndStoreElements ( f );

	/* add 1. Level */
	h5_id_t level_id = H5FedAddLevel( f );
	H5FedBeginRefineElements ( f, 1 );
	h5_id_t elem_id = H5FedRefineElement ( f, 0 );
	H5FedEndRefineElements ( f );

	/* add 2. Level */
	level_id = H5FedAddLevel( f );
	H5FedBeginRefineElements ( f, 1 );
	elem_id = H5FedRefineElement ( f, 2 );
	H5FedEndRefineElements ( f );

	H5CloseFile ( f );
	return 0;
}
