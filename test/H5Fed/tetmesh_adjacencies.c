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
print_adjacencies_to_vertex (
	h5_file_t * const f,
	h5_id_t local_id
	) {
	h5_idlist_t *uadj_edges;
	h5_idlist_t *uadj_triangles;
	h5_idlist_t *uadj_tets;
	H5FedGetEdgesUpAdjacentToVertex ( f, local_id, &uadj_edges );
	H5FedGetTrianglesUpAdjacentToVertex ( f, local_id, &uadj_triangles );
	H5FedGetTetsUpAdjacentToVertex ( f, local_id, &uadj_tets );
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
			H5FedMapEntity2LocalVids (
				f, uadj_edges->items[i], local_vids );
			snprintf ( k, sizeof(k), "=[%lld,%lld]=",
				   local_vids[0], local_vids[1] );
		} else {
			*k = '\0';
		}
		if ( i < uadj_triangles->num_items ) {
			H5FedMapEntity2LocalVids (
				f, uadj_triangles->items[i], local_vids );
			snprintf ( d, sizeof(d), "=[%lld,%lld,%lld]=",
				   local_vids[0], local_vids[1], local_vids[2] );
		} else {
			*d = '\0';
		}
		if ( i < uadj_tets->num_items ) {
			H5FedMapEntity2LocalVids (
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
print_adjacencies_to_edge (
	h5_file_t * const f,
	h5_id_t local_id
	) {
	h5_idlist_t *dadj_vertices;
	h5_idlist_t *uadj_triangles;
	h5_idlist_t *uadj_tets;
	H5FedGetVerticesDownAdjacentToEdge ( f, local_id, &dadj_vertices );
	H5FedGetTrianglesUpAdjacentToEdge ( f, local_id, &uadj_triangles );
	H5FedGetTetsUpAdjacentToEdge ( f, local_id, &uadj_tets );
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
			H5FedMapEntity2LocalVids (
				f, dadj_vertices->items[i], local_vids );
			snprintf ( v, sizeof(v), "=[%lld]=",
				   local_vids[0] );
		} else {
			*v = '\0';
		}
		if ( i == 0 ) {
			snprintf ( k, sizeof(k), "=%llx=", local_id );
		} else {
			*k = '\0';
		}
		if ( i < uadj_triangles->num_items ) {
			H5FedMapEntity2LocalVids (
				f, uadj_triangles->items[i], local_vids );
			snprintf ( d, sizeof(d), "=[%lld,%lld,%lld]=",
				   local_vids[0], local_vids[1], local_vids[2] );
		} else {
			*d = '\0';
		}
		if ( i < uadj_tets->num_items ) {
			H5FedMapEntity2LocalVids (
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
print_adjacencies_to_triangle (
	h5_file_t * const f,
	h5_id_t local_id
	) {
	h5_idlist_t *dadj_vertices;
	h5_idlist_t *dadj_edges;
	h5_idlist_t *uadj_tets;
	H5FedGetVerticesDownAdjacentToTriangle ( f, local_id, &dadj_vertices );
	H5FedGetEdgesDownAdjacentToTriangle ( f, local_id, &dadj_edges );
	H5FedGetTetsUpAdjacentToTriangle ( f, local_id, &uadj_tets );

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
			H5FedMapEntity2LocalVids (
				f, dadj_vertices->items[i], local_vids );
			snprintf ( v, sizeof(v), "=[%lld]=",
				   local_vids[0] );
		} else {
			*v = '\0';
		}
		if ( i < dadj_edges->num_items ) {
			H5FedMapEntity2LocalVids (
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
			H5FedMapEntity2LocalVids (
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
print_adjacencies_to_tet (
	h5_file_t * const f,
	h5_id_t local_id
	) {
	h5_idlist_t *dadj_vertices;
	h5_idlist_t *dadj_edges;
	h5_idlist_t *dadj_triangles;
	H5FedGetVerticesDownAdjacentToTet ( f, local_id, &dadj_vertices );
	H5FedGetEdgesDownAdjacentToTet ( f, local_id, &dadj_edges );
	H5FedGetTrianglesDownAdjacentToTet ( f, local_id, &dadj_triangles );

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
			H5FedMapEntity2LocalVids (
				f, dadj_edges->items[i], local_vids );
			snprintf ( k, sizeof(k), "=[%lld,%lld]=",
				   local_vids[0], local_vids[1] );
		} else {
			*k = '\0';
		}
		if ( i < dadj_triangles->num_items ) {
			H5FedMapEntity2LocalVids (
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
	h5_id_t id, local_id;
	h5_float64_t P[3];

	printf ( "\nAdjacencies to vertices\n" );
	H5FedBeginTraverseVertices ( f );
	while ( (local_id = H5FedTraverseVertices ( f, &id, P )) >= 0 ) {
		print_adjacencies_to_vertex ( f, local_id );
	}
	return H5FedEndTraverseVertices ( f );
}

static h5_err_t
traverse_edges (
	h5_file_t * f
	) {
	h5_id_t local_id, vids[4];

	printf ( "\nAdjacencies to edges\n" );
	H5FedBeginTraverseEdges ( f );
	while ( (local_id = H5FedTraverseEdges ( f, vids )) >= 0 ) {
		print_adjacencies_to_edge ( f, local_id );
	}
	return H5FedEndTraverseEdges ( f );
}

static h5_err_t
traverse_triangles (
	h5_file_t * f
	) {
	h5_id_t local_id, vids[4];

	printf ( "\nAdjacencies to triangle\n" );
	H5FedBeginTraverseTriangles ( f );
	while ( (local_id = H5FedTraverseTriangles ( f, vids )) >= 0 ) {
		print_adjacencies_to_triangle ( f, local_id );
	}
	return H5FedEndTraverseTriangles ( f );
}

static h5_err_t
traverse_tets (
	h5_file_t * f
	) {
	h5_id_t id, local_id, parent_id, vids[4];

	printf ( "\nAdjacencies to tetrahedra\n" );
	H5FedBeginTraverseElements ( f );
	while ( (local_id = H5FedTraverseElements (f, &id, &parent_id, vids )) >= 0 ) {
		print_adjacencies_to_tet ( f, local_id );
	}
	return H5FedEndTraverseElements ( f );
}

static h5_err_t
traverse_level (
	h5_file_t * f
	) {
	if ( traverse_vertices ( f ) < 0 ) return -1;
	if ( traverse_edges ( f ) < 0 ) return -1;
	if ( traverse_triangles ( f ) < 0 ) return -1;
	if ( traverse_tets ( f ) < 0 ) return -1;

	return H5_SUCCESS;
}

static h5_err_t
traverse_mesh (
	h5_file_t * f
	) {

	h5_id_t level_id;
	h5_size_t num_levels = H5FedGetNumLevels ( f );
	num_levels = 3;
	printf ( "    Number of levels in mesh: %lld\n", num_levels );
	for ( level_id = 2; level_id < num_levels; level_id++ ) {
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

	H5SetVerbosityLevel ( 3 );

	h5_file_t *f = H5OpenFile ( "simple_tet.h5", H5_O_RDONLY, 0 );
	if ( f == NULL ) {
		fprintf ( stderr, "!!! Can't open file.\n" );
		return -1;
	}

	h5_size_t num_meshes = H5FedGetNumMeshes ( f, H5_TETRAHEDRAL_MESH );
	printf ( "    Number of meshes: %lld\n", num_meshes );

	h5_id_t mesh_id;
	for ( mesh_id = 0; mesh_id < num_meshes; mesh_id++ ) {
		h5_err_t h5err = H5FedOpenMesh ( f, mesh_id, H5_TETRAHEDRAL_MESH );
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
