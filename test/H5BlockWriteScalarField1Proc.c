#include <stdio.h>
#include <stdlib.h>
#include <hdf5.h>
#include "H5Part.h"
#include "H5Block.h"
#include "H5BlockTypes.h"

struct H5BlockPartition Layout[1] = {
	{ 0, 63, 0, 63, 0, 511 }
};

static h5part_int64_t
_calc_index_KJI (
	int myproc,
	h5part_int64_t i,
	h5part_int64_t j,
	h5part_int64_t k
	) {
	h5part_int64_t i_dims = Layout[myproc].i_end - Layout[myproc].i_start + 1;
	h5part_int64_t j_dims = Layout[myproc].j_end - Layout[myproc].j_start + 1;

	return i + j*i_dims + k*i_dims*j_dims;
}



static h5part_int64_t
_write_file (
	H5PartFile *f,
	int myproc,
	int nprocs
	){

	h5part_int64_t i, j, k, idx;
	h5part_int64_t herr;
	h5part_float64_t *data;
	h5part_int64_t i_dims = Layout[myproc].i_end - Layout[myproc].i_start + 1;
	h5part_int64_t j_dims = Layout[myproc].j_end - Layout[myproc].j_start + 1;
	h5part_int64_t k_dims = Layout[myproc].k_end - Layout[myproc].k_start + 1;


	printf ( "Writing Step #%lld\n", (long long)f->timestep );

	data = malloc ( i_dims * j_dims * k_dims * sizeof ( *data ) );
	for ( i = 0; i < i_dims; i++ ) {
		for ( j = 0; j < j_dims; j++ ) {
			for ( k = 0; k < k_dims; k++ ) {
				idx = _calc_index_KJI ( myproc, i, j, k );
				*(data + idx) = k
					+ 1000*j
					+ 100000*i
					+ 10000000*myproc;
			}
		}
	}

	herr = H5BlockDefine3DFieldLayout (
		f,
		Layout[myproc].i_start, Layout[myproc].i_end,
		Layout[myproc].j_start, Layout[myproc].j_end,
		Layout[myproc].k_start, Layout[myproc].k_end );
	if ( herr < 0 ) return herr;

	herr = H5Block3dWriteScalarField ( f, "TestField", data );
	if ( herr < 0 ) return herr;

	free ( data );
	return 1;
}

int
main (
	int argc,
	char **argv
	) {
	char *fname = "blockfile1.h5";
	int timestep = 0;
	int myproc;
	int nprocs;
	h5part_int64_t herr;
	H5PartFile *f;
	MPI_Comm comm = MPI_COMM_WORLD;

	MPI_Init( &argc, &argv );
	MPI_Comm_size ( comm, &nprocs );
	MPI_Comm_rank( comm, &myproc );

	if ( nprocs != 1 ) {
		printf ( "Run this test with 1 processor!" );
		return 1;
	}

	printf ("PROC[%d]: Open file \"%s\" for writing ...\n", myproc, fname );
  
	f = H5PartOpenFileParallel ( fname, H5PART_WRITE, comm );
	if ( f == NULL ) return -1;

	herr = H5PartSetStep ( f, timestep );
	if ( herr < 0 ) return herr;


	H5PartSetVerbosityLevel ( 40 );

	if ( _write_file ( f, myproc, nprocs ) < 0 ) {
		printf ("Failed to write file \"%s\"\n", fname );
		return 2;
	}

	herr = H5BlockWriteFieldAttribString (
		f,
		"TestField",
		"TestString",
		"42" );
	if ( herr < 0 ) return -1;

	h5part_int64_t ival[1] = { 42 };
	h5part_float64_t rval[1] = { 42.0 };
	herr = H5BlockWriteFieldAttrib (
		f,
		"TestField",
		"TestInt64",
		H5PART_INT64,
		ival, 1 );
	if ( herr < 0 ) return -1;

	herr = H5BlockWriteFieldAttrib (
		f,
		"TestField",
		"TestFloat64",
		H5PART_FLOAT64,
		rval, 1 );
	if ( herr < 0 ) return -1;


	herr = H5PartCloseFile ( f );
	if ( herr < 0 ) return -1;

	return 0;
}

