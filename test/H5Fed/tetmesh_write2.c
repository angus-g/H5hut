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
	h5_float64_t P[3];
};

typedef struct vertex vertex_t; 

struct tet {
	h5_id_t vids[4];
};
typedef struct tet tet_t;
	       

vertex_t V0[] = {
	{{ -1.0,  0.0,  0.0 }},
	{{  1.0,  0.0,  0.0 }},
	{{  0.0,  1.0,  0.0 }},
	{{  0.0,  0.0,  1.0 }}
};

// sorted vertices: 0, 4, 5, 3, 2, 1

tet_t T0[] = {
	{{ 0, 1, 2, 3 }}	// 0, 3, 2, 1
};

static h5_int32_t
power (
	h5_int32_t x,
	h5_int32_t y
	) {
	h5_int32_t p = 1;
	h5_int32_t b = y;
	// bits in b correspond to values of powerN
	// so start with p=1, and for each set bit in b,
	// multiply corresponding table entry
	h5_int32_t powerN = x;

        while ( b != 0 ) {
		if ( (b&1) != 0 ) p *= powerN;
		b >>= 1;
		powerN=powerN*powerN;
	}
	return p;
}

int
main (
	int argc,
	char *argv[]
	) {
	h5_int32_t num_levels = 11;
	H5SetVerbosityLevel ( 2 );
	H5SetErrorHandler ( H5AbortErrorhandler );
	h5_file_t *f = H5OpenFile ( "tetmesh.h5", H5_O_WRONLY, 0 );
	H5FedAddMesh ( f, H5_TETRAHEDRAL_MESH );

	h5_int32_t i;
	h5_int32_t num = sizeof(V0) / sizeof(V0[0]);
	H5FedBeginStoreVertices ( f, num );
	for ( i = 0; i<num; i++ ) {
		H5FedStoreVertex ( f, -1, V0[i].P );
	}
	H5FedEndStoreVertices ( f );

	num = sizeof(T0)/sizeof(T0[0]);
	H5FedBeginStoreElements ( f, num );
	for ( i = 0; i<num; i++ ) {
		H5FedStoreElement ( f, T0[i].vids );
	}
	H5FedEndStoreElements ( f );
	H5FedAddLevel( f );
	H5FedBeginRefineElements ( f, 1 );
	H5FedRefineElement ( f, 0 );
	H5FedEndRefineElements ( f );

	h5_int32_t num_tets_last_level = 1;
	h5_int32_t level_id;
	for ( level_id = 2; level_id < num_levels; level_id++ ) {
		h5_id_t level_id = H5FedAddLevel( f );
		h5_int32_t num_tets2refine = power ( 4, level_id-1 );
		H5FedBeginRefineElements ( f, num_tets2refine );
		for ( i = num_tets_last_level;
		      i < num_tets_last_level+num_tets2refine;
		      i++ ) {
			H5FedRefineElement ( f, i );
		}
		H5FedEndRefineElements ( f );
		num_tets_last_level += 2 * num_tets2refine;
	}
	H5CloseFile ( f );
	return 0;
}
