#include <stdio.h>
#include <stdlib.h>

#include "H5Part.h"
#include "H5Fed.h"

const h5_oid_t MESH_TYPE = H5_TRIANGLE_MESH;
const char* FNAME = "simple_triangle.h5";

typedef struct vertex {
h5_id_t global_id;
	h5_float64_t P[3];
} vertex_t; 

typedef struct elem {
h5_id_t global_id;
	h5_id_t parent_id;
	h5_id_t vids[3];
} elem_t;
	       
vertex_t Vertices[] = {
	{ 0, {-1.0,  0.0,  0.0} },
	{ 1, { 1.0,  0.0,  0.0} },
	{ 2, { 0.0,  1.0,  0.0} },
	{ 3, { 0.0, -1.0,  0.0} }
};

elem_t Elems[] = {
	{ 1, -1, { 0, 1, 2 } },
	{ 0, -1, { 0, 1, 3 } }
};

const int num_vertices = sizeof (Vertices) / sizeof (Vertices[0]);
const int num_elems = sizeof (Elems) / sizeof (Elems[0]);

int
main (
	int argc,
	char* argv[]
	) {
	/* abort program on errors in library */
	H5SetErrorHandler (H5AbortErrorhandler);
	H5SetVerbosityLevel (2);

	/* open file and add mesh */
	h5_file_t* const f = H5OpenFile (FNAME, H5_O_WRONLY, 0);
	H5FedAddMesh (f, MESH_TYPE);

	/* store vertices */
	H5FedBeginStoreVertices (f, num_vertices);
	int i;
	for (i = 0; i < num_vertices; i++) {
		H5FedStoreVertex (f, -1, Vertices[i].P);
	}
	H5FedEndStoreVertices (f);

	/* store elements */
	H5FedBeginStoreElements (f, num_elems);
	for (i = 0; i < num_elems; i++) {
		H5FedStoreElement (f, Elems[i].vids);
	}
	H5FedEndStoreElements (f);

	/* add 1. Level */
	H5FedAddLevel(f);
	H5FedBeginRefineElements (f, 1);
	H5FedRefineElement (f, 0);
	H5FedEndRefineElements (f);

	H5FedCloseMesh (f);
	H5CloseFile (f);
	return 0;
}
