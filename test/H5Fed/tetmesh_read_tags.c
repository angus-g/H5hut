#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "H5hut.h"

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
traverse_vertices (
	h5_file_t * f
	) {
	h5_id_t local_id;
	h5_size_t real_num = 0;

	h5_size_t num = H5FedGetNumVerticesTotal ( f );
	h5t_iterator_t* iter = H5FedBeginTraverseEntities (f, 3 );
	while ( (real_num < num) &&
		((local_id = H5FedTraverseEntities (f, iter)) >= 0) ) {
		size_t size;
		h5_int64_t retval[3];
		H5FedGetMTag ( f, "testtag", local_id, &size, retval );
		if ( (retval[0] != local_id) ||
		     (retval[1] != local_id+1) ||
		     (retval[2] != local_id+2) ) {
			fprintf ( stderr, "!!! Wrong tag values for vertex %lld\n",
				  local_id );
			exit ( 1 );
		}
		real_num++;
	}
	H5FedEndTraverseEntities (f, iter);

	if ( real_num != num ) {
		fprintf ( stderr, "!!! Got %lld vertices, but expected %lld.\n",
			  (long long)real_num, (long long)num );
		exit ( 1 );
	}
	return H5_SUCCESS;
}

static h5_err_t
traverse_edges (
	h5_file_t * f
	) {
	h5_id_t local_id;

	h5t_iterator_t* iter = H5FedBeginTraverseEntities (f, 1);
	while ( (local_id = H5FedTraverseEntities (f, iter)) >= 0 ) {
		h5_id_t local_vids[4];
		H5FedGetVertexIndicesOfEntity( f, local_id, local_vids );
		size_t size;
		h5_int64_t retval[3];
		H5FedGetMTag ( f, "testtag", local_id, &size, retval );
		if ( (retval[0] != local_id) ||
		     (retval[1] != local_id+1) ||
		     (retval[1] != local_id+1) ) {
			fprintf ( stderr, "!!! Wrong tag values for edge %lld\n",
				  local_id );
			exit ( 1 );
		}
	}
	H5FedEndTraverseEntities (f, iter);
	return H5_SUCCESS;
}

static h5_err_t
traverse_triangles (
	h5_file_t * f
	) {
	h5_id_t local_id;

	h5t_iterator_t* iter = H5FedBeginTraverseEntities (f, 2);
	while ( (local_id = H5FedTraverseEntities (f, iter)) >= 0 ) {
		size_t size;
		h5_int64_t retval[3];
		H5FedGetMTag ( f, "testtag", local_id, &size, retval );
		if ( (retval[0] != local_id) ||
		     (retval[1] != local_id+1) ||
		     (retval[1] != local_id+1) ) {
			fprintf ( stderr, "!!! Wrong tag values for edge %lld\n",
				  local_id );
			exit ( 1 );
		}
	}
	H5FedEndTraverseEntities (f, iter);
	return H5_SUCCESS;
}

static h5_err_t
traverse_tets (
	h5_file_t * f
	) {
	h5_id_t local_id;
	h5_size_t real_num = 0;

	h5_size_t num = H5FedGetNumElementsTotal ( f );

	h5t_iterator_t* iter = H5FedBeginTraverseEntities (f, 0);
	while ( (real_num < num) &&
		((local_id = H5FedTraverseEntities (f, iter)) >= 0) ) {
		size_t size;
		h5_int64_t retval[3];
		H5FedGetMTag ( f, "testtag", local_id, &size, retval );
		if ( (retval[0] != local_id) ||
		     (retval[1] != local_id+1) ||
		     (retval[1] != local_id+1) ) {
			fprintf ( stderr, "!!! Wrong tag values for edge %lld\n",
				  local_id );
			exit ( 1 );
		}
		real_num++;
	}
	H5FedEndTraverseEntities (f, iter);
	if ( real_num != num ) {
		fprintf ( stderr, "!!! Got %lld tets, but expected %lld.\n",
			  (long long)real_num, (long long)num );
		exit(1);
	}

	return H5_SUCCESS;
}

static h5_err_t
read_level (
	h5_file_t * f
	) {
	traverse_vertices ( f );
	traverse_edges ( f );
	traverse_triangles ( f );
	traverse_tets ( f );
	return H5_SUCCESS;
}

static h5_err_t
traverse_mesh (
	h5_file_t * f
	) {

	h5_id_t level_id;
	h5_size_t num_levels = H5FedGetNumLevels ( f );
	printf ( "    Number of levels in mesh: %lld\n", (long long)num_levels );
	for ( level_id = 2; level_id < num_levels; level_id++ ) {
		h5_err_t h5err = H5FedSetLevel ( f, level_id );
		if ( h5err < 0 ) {
			fprintf ( stderr, "!!! Can't set level %lld.\n",
				  level_id );
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
	H5SetVerbosityLevel ( 2 );
	H5SetErrorHandler ( H5AbortErrorhandler );

	h5_file_t *f = H5OpenFile ( "simple_tet.h5", H5_O_RDONLY, 0 );
	h5_size_t num_meshes = H5FedGetNumMeshes ( f, H5_TETRAHEDRAL_MESH );
	printf ( "    Number of meshes: %lld\n", (long long)num_meshes );

	h5_id_t mesh_id;
	for ( mesh_id = 0; mesh_id < num_meshes; mesh_id++ ) {
		fprintf (
			stderr,
			"Time used: %f\n",
			(float)clock()/(float)CLOCKS_PER_SEC );
		H5FedOpenMesh ( f, mesh_id, H5_TETRAHEDRAL_MESH );
		fprintf (
			stderr,
			"Time used: %f\n",
			(float)clock()/(float)CLOCKS_PER_SEC );
		traverse_mesh ( f );
	}

	H5CloseFile ( f );
	return 0;
}
