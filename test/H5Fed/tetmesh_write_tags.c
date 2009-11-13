#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
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

static h5_err_t
set_vertex_tags (
	h5_file_t * f
	) {
	h5_id_t local_id;
	h5_int64_t val[3];
	h5_float64_t P[3];

	printf ( "\nSet tags to vertices\n" );
	H5FedBeginTraverseVertices ( f );
	while ( (local_id = H5FedTraverseVertices ( f, P )) >= 0 ) {
		val[0] = local_id;
		val[1] = local_id+1;
		val[2] = local_id+2;
		H5FedSetMTag (
			f,
			"testtag",
			local_id,
			3,
			val
			);
		h5_int64_t retval[3];
		size_t dims;
		H5FedGetMTag (
			f,
			"testtag",
			local_id,
			&dims,
			retval );
		if ( memcmp ( val, retval, sizeof(val) ) ) {
			fprintf (  stderr, "Oops!\n" );
		}
	}
	return H5FedEndTraverseVertices ( f );
}

static h5_err_t
set_edge_tags (
	h5_file_t * f
	) {
	h5_id_t local_id, vids[4];
	h5_int64_t val[3];
	printf ( "\nSet tags to edges\n" );
	H5FedBeginTraverseEdges ( f );
	while ( (local_id = H5FedTraverseEdges ( f, vids )) >= 0 ) {
		val[0] = local_id;
		val[1] = local_id+1;
		val[2] = local_id+2;
		H5FedSetMTag (
			f,
			"testtag",
			local_id,
			3,
			val
			);
		h5_int64_t retval[3];
		size_t dims;
		H5FedGetMTag (
			f,
			"testtag",
			local_id,
			&dims,
			retval );
		if ( memcmp ( val, retval, sizeof(val) ) ) {
			fprintf (  stderr, "Oops!\n" );
		}
	}
	return H5FedEndTraverseEdges ( f );
}

static h5_err_t
set_tri_tags (
	h5_file_t * f
	) {
	h5_id_t local_id, vids[4];
	h5_int64_t val[3];
	printf ( "\nSet tags to triangle\n" );
	H5FedBeginTraverseTriangles ( f );
	while ( (local_id = H5FedTraverseTriangles ( f, vids )) >= 0 ) {
		val[0] = local_id;
		val[1] = local_id+1;
		val[2] = local_id+2;
		H5FedSetMTag (
			f,
			"testtag",
			local_id,
			3,
			val
			);
		h5_int64_t retval[3];
		size_t dims;
		H5FedGetMTag (
			f,
			"testtag",
			local_id,
			&dims,
			retval );
		if ( memcmp ( val, retval, sizeof(val) ) ) {
			fprintf (  stderr, "Oops!\n" );
		}
	}
	return H5FedEndTraverseTriangles ( f );
}

static h5_err_t
set_tet_tags (
	h5_file_t * f
	) {
	h5_id_t local_id, vids[4];
	h5_int64_t val[3];
	printf ( "\nSet tags to tetrahedra\n" );
	H5FedBeginTraverseElements ( f );
	while ( (local_id = H5FedTraverseElements (f, vids )) >= 0 ) {
		val[0] = local_id;
		val[1] = local_id+1;
		val[2] = local_id+2;
		H5FedSetMTag (
			f,
			"testtag",
			local_id,
			3,
			val
			);
		h5_int64_t retval[3];
		size_t dims;
		H5FedGetMTag (
			f,
			"testtag",
			local_id,
			&dims,
			retval );
		if ( memcmp ( val, retval, sizeof(val) ) ) {
			fprintf (  stderr, "Oops!\n" );
		}
	}
	return H5FedEndTraverseElements ( f );
}

int
main (
	int argc,
	char *argv[]
	) {
	H5SetVerbosityLevel ( 5 );
	H5SetErrorHandler ( H5AbortErrorhandler );

	h5_file_t *f = H5OpenFile ( "simple_tet.h5", H5_O_RDONLY, 0 );
	H5FedOpenMesh ( f, 0, H5_TETRAHEDRAL_MESH );
	h5_size_t num_levels = H5FedGetNumLevels ( f );
	H5FedSetLevel ( f, num_levels-1 );
	H5FedAddMTagset ( f, "testtag", H5_INT64_T );
	set_vertex_tags ( f );
	set_edge_tags ( f );
	set_tri_tags ( f );
	set_tet_tags ( f );
	H5CloseFile ( f );
	return 0;
}
