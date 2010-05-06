#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <hdf5.h>
#include "H5Part.h"
#include "H5Fed.h"

#ifndef PARALLEL_IO
#ifndef MPI_COMM_WORLD
#define MPI_COMM_WORLD 0
#endif
#endif

#define PRINT_UPADJACENCIES 1
#define PRINT_DOWNADJACENCIES 0

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
print_adjacencies_of_vertex (
	h5_file_t * const f,
	h5_id_t local_id,
	clock_t *t
	) {
	h5_idlist_t *uadj_edges;
	h5_idlist_t *uadj_triangles;
	h5_idlist_t *uadj_tets;
	clock_t t1 = clock();
	H5FedGetEdgesUpAdjacentToVertex ( f, local_id, &uadj_edges );
	H5FedGetTrianglesUpAdjacentToVertex ( f, local_id, &uadj_triangles );
	H5FedGetTetsUpAdjacentToVertex ( f, local_id, &uadj_tets );
	*t = clock() - t1;
	int n = uadj_tets->num_items;
	if ( uadj_triangles->num_items > n ) n = uadj_triangles->num_items;
	if ( uadj_edges->num_items > n ) n = uadj_edges->num_items;
	int i;
	for ( i = 0; i < n; i++ ) {
		char v[256];
		char k[256];
		char d[256];
		char t[256];
		h5_id_t local_vids[4];
		if ( i == 0 ) {
			snprintf ( v, sizeof(v), "=%llx=", local_id );
		} else {
			*v = '\0';
		}
		if ( i < uadj_edges->num_items ) {
			H5FedGetVertexIndicesOfEntity (
				f, uadj_edges->items[i], local_vids );
			snprintf ( k, sizeof(k), "=[%lld,%lld]=",
				   local_vids[0], local_vids[1] );
		} else {
			*k = '\0';
		}
		if ( i < uadj_triangles->num_items ) {
			H5FedGetVertexIndicesOfEntity (
				f, uadj_triangles->items[i], local_vids );
			snprintf ( d, sizeof(d), "=[%lld,%lld,%lld]=",
				   local_vids[0], local_vids[1], local_vids[2] );
		} else {
			*d = '\0';
		}
		if ( i < uadj_tets->num_items ) {
			H5FedGetVertexIndicesOfEntity (
				f, uadj_tets->items[i], local_vids );
			snprintf ( t, sizeof(t), "=[%lld,%lld,%lld,%lld]=",
				   local_vids[0], local_vids[1],
				   local_vids[2], local_vids[3] );
		} else {
			*t = '\0';
		}
		printf ( "| %-18s | %-18s | %-18s | %-18s |\n", v, k, d, t );
	}
	H5FedReleaseListOfAdjacencies ( f, &uadj_edges );
	H5FedReleaseListOfAdjacencies ( f, &uadj_triangles );
	H5FedReleaseListOfAdjacencies ( f, &uadj_tets );
	return H5_SUCCESS;
}

static h5_err_t
print_adjacencies_of_edge (
	h5_file_t * const f,
	h5_id_t local_id,
	clock_t *t
	) {
	h5_idlist_t *dadj_vertices;
	h5_idlist_t *uadj_triangles;
	h5_idlist_t *uadj_tets;
	clock_t t1 = clock();
	H5FedGetVerticesDownAdjacentToEdge ( f, local_id, &dadj_vertices );
	H5FedGetTrianglesUpAdjacentToEdge ( f, local_id, &uadj_triangles );
	H5FedGetTetsUpAdjacentToEdge ( f, local_id, &uadj_tets );
	*t = clock() - t1;
	int n = dadj_vertices->num_items;
	if ( uadj_triangles->num_items > n ) n = uadj_triangles->num_items;
	if ( uadj_tets->num_items > n ) n = uadj_tets->num_items;

	int i;
	for ( i = 0; i < n; i++ ) {
		char v[256];
		char k[256];
		char d[256];
		char t[256];
		h5_id_t local_vids[4];
		if ( i < dadj_vertices->num_items ) {
			snprintf ( v, sizeof(v), "=[%lld]=",
				   dadj_vertices->items[i] );
		} else {
			*v = '\0';
		}
		if ( i == 0 ) {
			snprintf ( k, sizeof(k), "=%llx=", local_id );
		} else {
			*k = '\0';
		}
		if ( i < uadj_triangles->num_items ) {
			H5FedGetVertexIndicesOfEntity (
				f, uadj_triangles->items[i], local_vids );
			snprintf ( d, sizeof(d), "=[%lld,%lld,%lld]=",
				   local_vids[0], local_vids[1], local_vids[2] );
		} else {
			*d = '\0';
		}
		if ( i < uadj_tets->num_items ) {
			H5FedGetVertexIndicesOfEntity (
				f, uadj_tets->items[i], local_vids );
			snprintf ( t, sizeof(t), "=[%lld,%lld,%lld,%lld]=",
				   local_vids[0], local_vids[1],
				   local_vids[2], local_vids[3] );
		} else {
			*t = '\0';
		}
		printf ( "| %-18s | %-18s | %-18s | %-18s |\n", v, k, d, t );
	}
	H5FedReleaseListOfAdjacencies ( f, &dadj_vertices );
	H5FedReleaseListOfAdjacencies ( f, &uadj_triangles );
	H5FedReleaseListOfAdjacencies ( f, &uadj_tets );
	return H5_SUCCESS;
}

static h5_err_t
print_adjacencies_of_triangle (
	h5_file_t * const f,
	h5_id_t local_id,
	clock_t *t
	) {
	h5_idlist_t *dadj_vertices;
	h5_idlist_t *dadj_edges;
	h5_idlist_t *uadj_tets;
	clock_t t1 = clock();
	H5FedGetVerticesDownAdjacentToTriangle ( f, local_id, &dadj_vertices );
	H5FedGetEdgesDownAdjacentToTriangle ( f, local_id, &dadj_edges );
	H5FedGetTetsUpAdjacentToTriangle ( f, local_id, &uadj_tets );
	*t = clock() - t1;
	int n = dadj_vertices->num_items;
	if ( dadj_edges->num_items > n ) n = dadj_edges->num_items;
	if ( uadj_tets->num_items > n ) n = uadj_tets->num_items;
	int i;
	for ( i = 0; i < n; i++ ) {
		char v[256];
		char k[256];
		char d[256];
		char t[256];
		h5_id_t local_vids[4];
		if ( i < dadj_vertices->num_items ) {
			snprintf ( v, sizeof(v), "=[%lld]=",
				   dadj_vertices->items[i] );
		} else {
			*v = '\0';
		}
		if ( i < dadj_edges->num_items ) {
			H5FedGetVertexIndicesOfEntity (
				f, dadj_edges->items[i], local_vids );
			snprintf ( k, sizeof(k), "=[%lld,%lld]=",
				   local_vids[0], local_vids[1] );
		} else {
			*k = '\0';
		}
		if ( i == 0 ) {
			snprintf ( d, sizeof(d), "=%llx=", local_id );
		} else {
			*d = '\0';
		}
		if ( i < uadj_tets->num_items ) {
			H5FedGetVertexIndicesOfEntity (
				f, uadj_tets->items[i], local_vids );
			snprintf ( t, sizeof(t), "=[%lld,%lld,%lld,%lld]=",
				   local_vids[0], local_vids[1],
				   local_vids[2], local_vids[3] );
		} else {
			*t = '\0';
		}
		printf ( "| %-18s | %-18s | %-18s | %-18s |\n", v, k, d, t );
	}
	H5FedReleaseListOfAdjacencies ( f, &dadj_vertices );
	H5FedReleaseListOfAdjacencies ( f, &dadj_edges );
	H5FedReleaseListOfAdjacencies ( f, &uadj_tets );
	return H5_SUCCESS;
}

static h5_err_t
print_adjacencies_of_tet (
	h5_file_t * const f,
	h5_id_t local_id,
	clock_t *t
	) {
	h5_idlist_t *dadj_vertices;
	h5_idlist_t *dadj_edges;
	h5_idlist_t *dadj_triangles;
	clock_t t1 = clock();
	H5FedGetVerticesDownAdjacentToTet ( f, local_id, &dadj_vertices );
	H5FedGetEdgesDownAdjacentToTet ( f, local_id, &dadj_edges );
	H5FedGetTrianglesDownAdjacentToTet ( f, local_id, &dadj_triangles );
	*t = clock() - t1;
	int n = dadj_vertices->num_items;
	if ( dadj_edges->num_items > n ) n = dadj_edges->num_items;
	if ( dadj_triangles->num_items > n ) n = dadj_triangles->num_items;
	int i;
	for ( i = 0; i < n; i++ ) {
		char v[256];
		char k[256];
		char d[256];
		char t[256];
		h5_id_t local_vids[4];
		if ( i < dadj_vertices->num_items ) {
			snprintf ( v, sizeof(v), "=[%lld]=",
				   dadj_vertices->items[i] );
		} else {
			*v = '\0';
		}
		if ( i < dadj_edges->num_items ) {
			H5FedGetVertexIndicesOfEntity (
				f, dadj_edges->items[i], local_vids );
			snprintf ( k, sizeof(k), "=[%lld,%lld]=",
				   local_vids[0], local_vids[1] );
		} else {
			*k = '\0';
		}
		if ( i < dadj_triangles->num_items ) {
			H5FedGetVertexIndicesOfEntity (
				f, dadj_triangles->items[i], local_vids );
			snprintf ( d, sizeof(d), "=[%lld,%lld,%lld]=",
				   local_vids[0], local_vids[1], local_vids[2] );
		} else {
			*d = '\0';
		}
		if ( i == 0 ) {
			snprintf ( t, sizeof(t), "=%llx=", local_id );
		} else {
			*t = '\0';
		}
		printf ( "| %-18s | %-18s | %-18s | %-18s |\n", v, k, d, t );
	}
	H5FedReleaseListOfAdjacencies ( f, &dadj_vertices );
	H5FedReleaseListOfAdjacencies ( f, &dadj_edges );
	H5FedReleaseListOfAdjacencies ( f, &dadj_triangles );
	return H5_SUCCESS;
}

static h5_err_t
traverse_vertices (
	h5_file_t * f
	) {
	h5_id_t local_id;
	h5_id_t num = 0;
	clock_t t_total = 0;
	clock_t t_min = CLOCKS_PER_SEC;
	clock_t t_max = 0;
	clock_t t = 0;
	printf ( "\nAdjacencies to vertices\n" );
	h5t_entity_iterator_t* iter = H5FedBeginTraverseEntities (f, 3);
	fprintf (
		stderr,
		"Computing all adjacencies of all vertices ... ");
	while ( (local_id = H5FedTraverseEntities (f, iter)) >= 0 ) {
		print_adjacencies_of_vertex ( f, local_id, &t );
		num++;
		t_total += t;
		if ( t < t_min ) t_min = t;
		if ( t > t_max ) t_max = t;
	}
	fprintf (
		stderr,
		"%lld\ttotal: %f\tmin: %f\tavg: %f\tmax: %f\n",
		num,
		(double)t_total / (double)CLOCKS_PER_SEC,
		(double)t_min / (double)CLOCKS_PER_SEC,
		(double)t_total / (double)CLOCKS_PER_SEC / (double)num,
		(double)t_max / (double)CLOCKS_PER_SEC );
	return H5FedEndTraverseEntities (f, iter);
}

static h5_err_t
traverse_edges (
	h5_file_t * f
	) {
	h5_id_t local_id;
	h5_id_t num = 0;
	clock_t t_total = 0;
	clock_t t_min = CLOCKS_PER_SEC;
	clock_t t_max = 0;
	clock_t t = 0;
	printf ( "\nAdjacencies to edges\n" );
	h5t_entity_iterator_t* iter = H5FedBeginTraverseEntities (f, 2);
	fprintf (
		stderr,
		"Computing all adjacencies of all edges ... ");
	while ( (local_id = H5FedTraverseEntities (f, iter)) >= 0 ) {
		print_adjacencies_of_edge ( f, local_id, &t );
		num++;
		t_total += t;
		if ( t < t_min ) t_min = t;
		if ( t > t_max ) t_max = t;
	}
	fprintf (
		stderr,
		"%lld\ttotal: %f\tmin: %f\tavg: %f\tmax: %f\n",
		num,
		(double)t_total / (double)CLOCKS_PER_SEC,
		(double)t_min / (double)CLOCKS_PER_SEC,
		(double)t_total / (double)CLOCKS_PER_SEC / (double)num,
		(double)t_max / (double)CLOCKS_PER_SEC );
	return H5FedEndTraverseEntities (f, iter);
}

static h5_err_t
traverse_triangles (
	h5_file_t * f
	) {
	h5_id_t local_id;
	h5_id_t num = 0;
	clock_t t_min = CLOCKS_PER_SEC;
	clock_t t_max = 0;
	clock_t t_total = 0;
	clock_t t = 0;
	printf ( "\nAdjacencies to triangle\n" );
	h5t_entity_iterator_t* iter = H5FedBeginTraverseEntities (f, 1);
	fprintf (
		stderr,
		"Computing all adjacencies of all triangles ... ");
	while ( (local_id = H5FedTraverseEntities (f, iter)) >= 0 ) {
		print_adjacencies_of_triangle ( f, local_id, &t );
		num++;
		t_total += t;
		if ( t < t_min ) t_min = t;
		if ( t > t_max ) t_max = t;
	}
	fprintf (
		stderr,
		"%lld\ttotal: %f\tmin: %f\tavg: %f\tmax: %f\n",
		num,
		(double)t_total / (double)CLOCKS_PER_SEC,
		(double)t_min / (double)CLOCKS_PER_SEC,
		(double)t_total / (double)CLOCKS_PER_SEC / (double)num,
		(double)t_max / (double)CLOCKS_PER_SEC );
	return H5FedEndTraverseEntities (f, iter);
}

static h5_err_t
traverse_tets (
	h5_file_t * f
	) {
	h5_id_t local_id;
	h5_id_t num = 0;
	clock_t t_total = 0;
	clock_t t_min = CLOCKS_PER_SEC;
	clock_t t_max = 0;
	clock_t t = 0;
	printf ( "\nAdjacencies to tetrahedra\n" );
	h5t_entity_iterator_t* iter = H5FedBeginTraverseEntities (f, 0);
	fprintf (
		stderr,
		"Computing all adjacencies of all tetrahedra ... ");
	while ( (local_id = H5FedTraverseEntities (f, iter)) >= 0 ) {
		print_adjacencies_of_tet ( f, local_id, &t );
		num++;
		t_total += t;
		if ( t < t_min ) t_min = t;
		if ( t > t_max ) t_max = t;
	}
	fprintf (
		stderr,
		"%lld\ttotal: %f\tmin: %f\tavg: %f\tmax: %f\n",
		num,
		(double)t_total / (double)CLOCKS_PER_SEC,
		(double)t_min / (double)CLOCKS_PER_SEC,
		(double)t_total / (double)CLOCKS_PER_SEC / (double)num,
		(double)t_max / (double)CLOCKS_PER_SEC );
	return H5FedEndTraverseEntities (f, iter);
}

static h5_err_t
traverse_level (
	h5_file_t * f
	) {
	fprintf ( stderr, "Level ID: %lld\n", H5FedGetLevel( f ) );
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
	printf ( "    Number of levels in mesh: %lld\n", num_levels );
	for ( level_id = 0; level_id < num_levels; level_id++ ) {
		H5FedSetLevel ( f, level_id );
		traverse_level ( f );
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
	printf ( "    Number of meshes: %lld\n", num_meshes );

	h5_id_t mesh_id;
	for ( mesh_id = 0; mesh_id < num_meshes; mesh_id++ ) {
		fprintf (
			stderr,
			"Opening mesh ... " );
		clock_t t1 = clock();
		H5FedOpenMesh ( f, mesh_id, H5_TETRAHEDRAL_MESH );
		clock_t t2 = clock();
		fprintf (
			stderr,
			"%f secs\n",
			(double)(t2-t1) / (double)CLOCKS_PER_SEC );
		traverse_mesh ( f );
	}

	H5CloseFile ( f );
	return 0;
}
