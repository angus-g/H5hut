#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include "H5hut.h"

const h5_oid_t MESH_TYPE = H5_TETRAHEDRAL_MESH;
const char* FNAME = "simple_tet.h5";

static h5_err_t
set_vertex_tags (
	h5_file_t* const f
	) {
	h5_loc_id_t local_id;
	h5_int64_t val[3];

	printf ("\nSet tags to vertices\n");
	h5t_iterator_t* iter = H5FedBeginTraverseEntities (f, 3);
	while ((local_id = H5FedTraverseEntities (f, iter)) >= 0) {
		val[0] = local_id;
		val[1] = local_id+1;
		val[2] = local_id+2;
		H5FedSetMTag (f, "testtag", local_id, 3, val);
		h5_int64_t retval[3];
		size_t dim = 3;
		H5FedGetMTag (f, "testtag", local_id, &dim, retval);
		if (memcmp (val, retval, sizeof(val))) {
			fprintf ( stderr, "Oops!\n");
		}
		printf ("Set tag for entity %llx\n", (long long)local_id);
	}
	return H5FedEndTraverseEntities (f, iter);
}

static h5_err_t
set_edge_tags (
	h5_file_t* const f
	) {
	h5_loc_id_t local_id;
	h5_int64_t val[3];
	printf ("\nSet tags to edges\n");
	h5t_iterator_t* iter = H5FedBeginTraverseEntities (f, 2);
	while ((local_id = H5FedTraverseEntities (f, iter)) >= 0) {
		val[0] = local_id;
		val[1] = local_id+1;
		val[2] = local_id+2;
		H5FedSetMTag (f, "testtag", local_id, 3, val);
		h5_int64_t retval[3];
		size_t dims;
		H5FedGetMTag (f, "testtag", local_id, &dims, retval);
		if (memcmp ( val, retval, sizeof(val))) {
			fprintf ( stderr, "Oops!\n");
		}
	}
	return H5FedEndTraverseEntities (f, iter);
}

static h5_err_t
set_tri_tags (
	h5_file_t* const f
	) {
	h5_loc_id_t local_id;
	h5_int64_t val[3];
	printf ("\nSet tags to triangle\n");
	h5t_iterator_t* iter = H5FedBeginTraverseEntities (f, 1);
	while ((local_id = H5FedTraverseEntities (f, iter)) >= 0) {
		val[0] = local_id;
		val[1] = local_id+1;
		val[2] = local_id+2;
		H5FedSetMTag (f, "testtag", local_id, 3, val);
		h5_int64_t retval[3];
		size_t dims;
		H5FedGetMTag (f, "testtag", local_id, &dims, retval);
		if (memcmp (val, retval, sizeof(val))) {
			fprintf (stderr, "Oops!\n");
		}
	}
	return H5FedEndTraverseEntities (f, iter);
}

static h5_err_t
set_tet_tags (
	h5_file_t* const f
	) {
	h5_loc_id_t local_id;
	h5_int64_t val[3];
	printf ("\nSet tags to tetrahedra\n");
	h5t_iterator_t* iter = H5FedBeginTraverseEntities (f, 0);
	while ((local_id = H5FedTraverseEntities (f, iter)) >= 0) {
		val[0] = local_id;
		val[1] = local_id+1;
		val[2] = local_id+2;
		H5FedSetMTag (f, "testtag", local_id, 3, val);
		h5_int64_t retval[3];
		size_t dims;
		H5FedGetMTag (f, "testtag", local_id, &dims, retval);
		if (memcmp (val, retval, sizeof(val))) {
			fprintf (stderr, "Oops!\n");
		}
	}
	return H5FedEndTraverseEntities (f, iter);
}

int
main (
	int argc,
	char* argv[]
	) {

	/* abort program on error, so we don't have to handle them */
	H5SetErrorHandler (H5AbortErrorhandler);
	H5SetVerbosityLevel (4);

	/* open file and get number of meshes */
	h5_file_t* f = H5OpenFile (FNAME, H5_O_RDWR, 0);
	H5FedOpenTetrahedralMesh (f, 0);

	/* open last level */
	h5_size_t num_levels = H5FedGetNumLevels (f);
	H5FedSetLevel (f, num_levels-1);

	/* add new tagset and write some data to it */
	H5FedAddMTagset (f, "testtag", H5_INT64_T);
	set_vertex_tags (f);
	set_edge_tags (f);
	set_tri_tags (f);
	set_tet_tags (f);

	/* done */
	H5FedCloseMesh (f);
	H5CloseFile (f);
	return 0;
}
