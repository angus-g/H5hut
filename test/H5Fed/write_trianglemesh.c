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
	h5_id_t global_id;
	h5_float64_t P[3];
};

typedef struct vertex vertex_t; 

struct entity {
	h5_id_t global_id;
	h5_id_t parent_id;
	h5_id_t vids[3];
};
typedef struct entity entity_t;
	       

vertex_t V[4] = {
	{ 0, {-1.0,  0.0,  0.0} },
	{ 1, { 1.0,  0.0,  0.0} },
	{ 2, { 0.0,  1.0,  0.0} },
	{ 3, { 0.0, -1.0,  0.0} }
};

entity_t T[2] = {
	{ 1, -1, { 0, 1, 2 } },
	{ 0, -1, { 0, 1, 3 } }
};

int
main (
	int argc,
	char *argv[]
	) {

	H5SetVerbosityLevel ( 4 );

	h5_file_t *f = H5OpenFile ( "simple_triangle.h5", H5_O_WRONLY, 0 );
	if ( f == NULL ) {
		fprintf ( stderr, "!!! Can't open file.\n" );
		return -1;
	}

	h5_err_t h5err = H5FedAddMesh ( f, H5_TRIANGLE_MESH );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Can't set step.\n" );
		return -1;
	}

	H5FedBeginStoreVertices ( f, 4 );
	int i;
	for ( i = 0; i < 4; i++ ) {
		h5err = H5FedStoreVertex (
			f,
			-1,
			V[i].P );
		if ( h5err < 0 ) {
			fprintf ( stderr, "!!! Can't store vertex.\n" );
			return -1;
		}
	}
	H5FedEndStoreVertices ( f );

	H5FedBeginStoreElements ( f, 2 );
	for ( i = 0; i < 2; i++ ) {
		h5err = H5FedStoreElement (
			f,
			T[i].vids );
		
		if ( h5err < 0 ) {
			fprintf ( stderr, "!!! Can't store tet.\n" );
			return -1;
		}
	}
	H5FedEndStoreElements ( f );

	h5_id_t level_id = H5FedAddLevel( f );
	if ( level_id < 0 ) {
		fprintf ( stderr, "!!! Can't add level.\n" );
		return -1;
	}

	h5err = H5FedBeginRefineElements ( f, 1 );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Can't set number of elements torefine.\n" );
		return -1;
	}

	h5_id_t elem_id = H5FedRefineElement ( f, 0 );
	if ( elem_id < 0 ) {
		fprintf ( stderr, "!!! Can't refine tet.\n" );
		return -1;
	}
	H5FedEndRefineElements ( f );

	h5err = H5CloseFile ( f );
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Can't close file.\n" );
		return -1;
	}
	return 0;
}
