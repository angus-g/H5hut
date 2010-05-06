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

struct entity {
	h5_id_t global_id;
	h5_id_t parent_id;
	h5_id_t vids[3];
};
typedef struct entity entity_t;

static h5_err_t
traverse_vertices (
	h5_file_t * f
	) {
	h5_id_t vertex_id;
	h5_float64_t P[3];
	h5_size_t real_num = 0;
	h5t_entity_iterator_t* iter;

	h5_size_t num = H5FedGetNumVerticesTotal (f);
	printf ("    Number of vertices on level: %lld\n", num);

	iter = H5FedBeginTraverseEntities (f, 2);
	while ((real_num < num) &&
	       ((vertex_id = H5FedTraverseEntities (f, iter)) >= 0)) {
		H5FedGetVertexCoordByID (f, vertex_id, P);
		char v[256];
		snprintf (v, sizeof(v), "=%llx=", vertex_id);
		printf ("| %-18s | (%f, %f, %f) |\n",
			 v, P[0], P[1], P[2]);
		real_num++;
	}
	H5FedEndTraverseEntities (f, iter);

	if (real_num != num) {
		fprintf (stderr, "!!! Got %lld vertices, but expected %lld.\n",
			 real_num, num);
		exit (1);
	}
	return H5_SUCCESS;
}

static h5_err_t
traverse_elems (
	h5_file_t * f
	) {
	h5_id_t local_id;
	h5_size_t real_num = 0;

	h5_size_t num = H5FedGetNumElementsTotal ( f );
	printf ( "    Number of tetrahedra on level: %lld\n", num );
	h5t_entity_iterator_t* iter = H5FedBeginTraverseEntities (f, 0);
	while ( (real_num < num) &&
		((local_id = H5FedTraverseEntities ( f, iter )) >= 0) ) {
		char v[256];
		char t[256];
		h5_id_t local_vids[4];
		snprintf ( t, sizeof(t), "=%llx=", local_id );
		H5FedGetVertexIndicesOfEntity ( f, local_id, local_vids );
		snprintf (v, sizeof(v), "=[%lld,%lld,%lld]=",
			   local_vids[0], local_vids[1], local_vids[2]);
		printf ( "| %-18s | %-18s |\n", t, v );
		real_num++;
	}
	H5FedEndTraverseEntities (f, iter);
	if ( real_num != num ) {
		fprintf ( stderr, "!!! Got %lld tets, but expected %lld.\n",
			  real_num, num );
		exit(1);
	}

	return H5_SUCCESS;
}

h5_err_t
traverse_level (
	h5_file_t* const f
	) {
	h5_err_t h5err = traverse_vertices (f);
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Oops ...\n" );
		return -1;
	}
	h5err = traverse_elems (f);
	if ( h5err < 0 ) {
		fprintf ( stderr, "!!! Oops ...\n" );
		return -1;
	}
	return H5_SUCCESS;
}

h5_err_t
traverse_mesh (
	h5_file_t * f
	) {

	h5_id_t level_id;
	h5_size_t num_levels = H5FedGetNumLevels ( f );
	if ( num_levels < 0 ) {
		fprintf ( stderr, "!!! Oops ...\n" );
		return -1;
	}
	printf ( "    Number of levels in mesh: %lld\n", num_levels );
	for ( level_id = 0; level_id < num_levels; level_id++ ) {
		printf ( "    Going to level %lld\n", level_id );
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

	h5_file_t *f = H5OpenFile ( "simple_triangle.h5", H5_O_RDONLY, 0 );
	if ( f == NULL ) {
		fprintf ( stderr, "!!! Can't open file.\n" );
		return -1;
	}

	h5_size_t num_meshes = H5FedGetNumMeshes ( f, H5_TRIANGLE_MESH );
	printf ( "    Number of meshes: %lld\n", num_meshes );

	h5_id_t mesh_id;
	for ( mesh_id = 0; mesh_id < num_meshes; mesh_id++ ) {
		h5_err_t h5err = H5FedOpenMesh ( f, mesh_id, H5_TRIANGLE_MESH );
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
