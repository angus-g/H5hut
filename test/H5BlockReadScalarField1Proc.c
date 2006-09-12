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
_read_file (
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


	printf ( "Reading Step #%lld\n", (long long)f->timestep );

	data = malloc ( i_dims * j_dims * k_dims * sizeof ( *data ) );

	herr = H5BlockDefine3DFieldLayout (
		f,
		Layout[myproc].i_start, Layout[myproc].i_end,
		Layout[myproc].j_start, Layout[myproc].j_end,
		Layout[myproc].k_start, Layout[myproc].k_end );
	if ( herr < 0 ) return herr;

	herr = H5Block3dReadScalarField ( f, "TestField", data );
	if ( herr < 0 ) return herr;

	for ( i = 0; i < i_dims; i++ ) {
		for ( j = 0; j < j_dims; j++ ) {
			for ( k = 0; k < k_dims; k++ ) {
				idx = _calc_index_KJI ( myproc, i, j, k );
				h5part_float64_t val = k
					+ 1000*j
					+ 100000*i
					+ 10000000*myproc;
				if ( *(data + idx) != val ) {
					printf ( "Error reading field at [%lld,%lld,%lld]\n",
						 (long long)i,
						 (long long)j,
						 (long long)k );
				}
			}
		}
	}



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

	printf ("PROC[%d]: Open file \"%s\" for reading ...\n", myproc, fname );
  
	f = H5PartOpenFileParallel ( fname, H5PART_READ, comm );
	if ( f == NULL ) return -1;

	herr = H5PartSetStep ( f, timestep );
	if ( herr < 0 ) return herr;


	H5PartSetVerbosityLevel ( 40 );

	if ( _read_file ( f, myproc, nprocs ) < 0 ) {
		printf ("Failed to read file \"%s\"\n", fname );
		return 2;
	}

	char sval[16];
	herr = H5BlockReadFieldAttrib (
		f,
		"TestField",
		"TestString",
		sval );
	if ( herr < 0 ) return -1;
	if ( strcmp ( sval, "42" ) != 0 ) {
		printf ( "Error reading string attribute: "
			 "Value is \"%s\" and should be \"42\"\n", sval );
	}

	h5part_int64_t ival[1];
	h5part_float64_t rval[1];
	herr = H5BlockReadFieldAttrib (
		f,
		"TestField",
		"TestInt64",
		ival );
	if ( herr < 0 ) return -1;
	if ( ival[0] != 42 ) {
		printf ( "Error reading int64 attribute: "
			 "Value is %lld and should be 42\n",
			 (long long) ival[0] );
	}

	herr = H5BlockReadFieldAttrib (
		f,
		"TestField",
		"TestFloat64",
		rval );
	if ( herr < 0 ) return -1;
	if ( rval[0] != 42.0 ) {
		printf ( "Error reading float64 attribute: "
			 "Value is %f and should be 42.0\n",
			 rval[0] );
	}


	herr = H5PartCloseFile ( f );
	if ( herr < 0 ) return -1;

	return 0;
}

