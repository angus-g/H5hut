#include <stdio.h>
#include <stdlib.h>
#include <hdf5.h>
#include "H5Part.h"
#include "H5Block.h"
#include "H5BlockTypes.h"
#ifdef PARALLEL_IO
#include <mpi.h>
#else
typedef int MPI_Comm;
#ifndef MPI_COMM_WORLD
#define MPI_COMM_WORLD 0
#endif
#endif

struct H5BlockPartition Layout1[1] = {
	{ 0, 63, 0, 63, 0, 511 }
};

#define _calc_index( i, i_dims, j, j_dims, k, k_dims ) \
		(i + j*i_dims + k*i_dims*j_dims)

static h5part_int64_t
_write_data (
	H5PartFile *f,
	int myproc,
	struct H5BlockPartition *layout
	) {

	h5part_int64_t i, j, k, idx;
	h5part_int64_t herr;
	h5part_float64_t *data;
	h5part_int64_t i_dims = layout->i_end - layout->i_start + 1;
	h5part_int64_t j_dims = layout->j_end - layout->j_start + 1;
	h5part_int64_t k_dims = layout->k_end - layout->k_start + 1;

	printf ( "Writing Step #%lld\n", (long long)f->timestep );

	data = malloc ( i_dims * j_dims * k_dims * sizeof ( *data ) );
	for ( i = 0; i < i_dims; i++ ) {
		for ( j = 0; j < j_dims; j++ ) {
			for ( k = 0; k < k_dims; k++ ) {
				idx = _calc_index (
					i, i_dims,
					j, j_dims,
					k, k_dims );
				*(data + idx) = k
					+ 1000*j
					+ 100000*i
					+ 10000000*myproc;
			}
		}
	}

	herr = H5BlockDefine3DFieldLayout (
		f,
		layout->i_start, layout->i_end,
		layout->j_start, layout->j_end,
		layout->k_start, layout->k_end );
	if ( herr < 0 ) return herr;

	herr = H5Block3dWriteScalarField ( f, "TestField", data );
	if ( herr < 0 ) return herr;

	free ( data );
	return 1;
}

static h5part_int64_t
_write_file (
	const char *fname,
	const int myproc,
	MPI_Comm comm,
	struct H5BlockPartition *layout
	) {
	
	H5PartFile *f;
	h5part_int64_t timestep = 0;
	h5part_int64_t herr;

	printf ("PROC[%d]: Open file \"%s\" for writing ...\n",
		myproc, fname );

#ifdef PARALLEL_IO
	f = H5PartOpenFileParallel (
		fname,
		H5PART_WRITE,
		comm
		);
#else
	f = H5PartOpenFile (
		fname,
		H5PART_WRITE
		);

#endif
	if ( f == NULL ) return -1;
	
	herr = H5PartSetStep ( f, timestep );
	if ( herr < 0 ) return herr;
	
	if ( _write_data ( f, myproc, layout ) < 0 ) {
		printf ("Failed to write file \"%s\"\n", fname );
		return 2;
	}
	
	herr = H5PartCloseFile ( f );
	if ( herr < 0 ) return -1;
	
	return 0;
}

static h5part_int64_t
_write_attributes (
	const char *fname,
	const int myproc,
	MPI_Comm comm
	) {
	h5part_int64_t timestep = 0;


	printf ("PROC[%d]: Open file \"%s\" for writing ...\n",
		myproc, fname );
  
#ifdef PARALLEL_IO
	H5PartFile *f = H5PartOpenFileParallel (
		fname,
		H5PART_APPEND,
		comm
		);
#else
	H5PartFile *f = H5PartOpenFile (
		fname,
		H5PART_APPEND
		);

#endif
	if ( f == NULL ) return -1;
	
	h5part_int64_t herr = H5PartSetStep ( f, timestep );
	if ( herr < 0 ) return herr;


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
	
	return H5PART_SUCCESS;
}

static h5part_int64_t
_read_data (
	H5PartFile *f,
	int myproc,
	struct H5BlockPartition *layout
	) {

	h5part_int64_t i, j, k, idx;
	h5part_int64_t herr;
	h5part_float64_t *data;
	h5part_int64_t i_dims = layout->i_end - layout->i_start + 1;
	h5part_int64_t j_dims = layout->j_end - layout->j_start + 1;
	h5part_int64_t k_dims = layout->k_end - layout->k_start + 1;

	printf ( "Reading Step #%lld\n", (long long)f->timestep );

	data = malloc ( i_dims * j_dims * k_dims * sizeof ( *data ) );

	herr = H5BlockDefine3DFieldLayout (
		f,
		layout->i_start, layout->i_end,
		layout->j_start, layout->j_end,
		layout->k_start, layout->k_end );
	if ( herr < 0 ) return herr;

	herr = H5Block3dReadScalarField ( f, "TestField", data );
	if ( herr < 0 ) return herr;

	for ( i = 0; i < i_dims; i++ ) {
		for ( j = 0; j < j_dims; j++ ) {
			for ( k = 0; k < k_dims; k++ ) {
				idx = _calc_index (
					i, i_dims,
					j, j_dims,
					k, k_dims );

				/*
				  what do I need to calculate the value?
				  proc which has the item written
				  i, j, k relative to proc
				*/

				h5part_int64_t ri = i + layout->i_start;
				h5part_int64_t rj = j + layout->j_start;
				h5part_int64_t rk = k + layout->k_start;
				int proc = (int) H5Block3dGetProcOf ( f, ri, rj, rk );

				h5part_int64_t i_start, i_end;
				h5part_int64_t j_start, j_end;
				h5part_int64_t k_start, k_end;

 				H5Block3dGetPartitionOfProc (
					f,
					proc,
					&i_start, &i_end,
					&j_start, &j_end,
					&k_start, &k_end );
				ri -= i_start;
				rj -= j_start;
				rk -= k_start;
				h5part_float64_t value = rk
					+ 1000*rj
					+ 100000*ri
					+ 10000000*proc;
				if ( *(data + idx) != value ) {
					printf (
						"PROC[%d]: "
						"value missmatch for (%lld,%lld,%lld); is: %f;"
						" should be: %f\n",
						myproc,
						(long long)i, (long long)j, (long long)k,
						*( data + idx ), value );
					printf (
						"PROC[%d]: "
						"My partition is: "
						"%lld:%lld, %lld:%lld, %lld:%lld\n",
						myproc,
						(long long)layout->i_start, (long long)layout->i_end,
						(long long)layout->j_start, (long long)layout->j_end,
						(long long)layout->k_start, (long long)layout->k_end );
					printf (
						"PROC[%d]: "
						"Value has been written by proc %d\n",
						myproc, proc );
					printf (
						"PROC[%d]: "
						"The partition for this proc was: "
						"%lld:%lld, %lld:%lld, %lld:%lld\n",
						myproc,
						(long long)i_start, (long long)i_end,
						(long long)j_start, (long long)j_end,
						(long long)k_start, (long long)k_end );
					return -1;
				}
			}
		}
	}

	free ( data );

	return 0;
}

static h5part_int64_t
_read_file (
	const char *fname,
	const int myproc,
	MPI_Comm comm,
	struct H5BlockPartition *layout
	) {
	
	H5PartFile *f;
	h5part_int64_t timestep = 0;
	h5part_int64_t herr;

	printf ("PROC[%d]: Open file \"%s\" for reading ...\n",
		myproc, fname );
#ifdef PARALLEL_IO
	f = H5PartOpenFileParallel (
		fname,
		H5PART_READ,
		comm
		);
#else
	f = H5PartOpenFile (
		fname,
		H5PART_READ
		);
#endif
	if ( f == NULL ) return -1;
	
	herr = H5PartSetStep ( f, timestep );
	if ( herr < 0 ) return herr;
	
	if ( _read_data ( f, myproc, layout ) < 0 ) {
		printf ("Failed to read file \"%s\"\n", fname );
		return 2;
	}
	
	herr = H5PartCloseFile ( f );
	if ( herr < 0 ) return -1;
	
	return 0;
}

static h5part_int64_t
_read_attributes (
	const char *fname,
	const int myproc,
	MPI_Comm comm
	) {
	h5part_int64_t timestep = 0;


	printf ("PROC[%d]: Open file \"%s\" for writing ...\n",
		myproc, fname );
  
#ifdef PARALLEL_IO
	H5PartFile *f = H5PartOpenFileParallel (
		fname,
		H5PART_WRITE,
		comm
		);
#else
	H5PartFile *f = H5PartOpenFile (
		fname,
		H5PART_WRITE
		);
#endif
	if ( f == NULL ) return -1;
	
	h5part_int64_t herr = H5PartSetStep ( f, timestep );
	if ( herr < 0 ) return herr;

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


int
main (
	int argc,
	char **argv
	) {
	char *fname = "blockfile0.h5";
	int myproc =0;
	int opt_read = 0;
	int opt_write = 0;
	      
	int ex = 0;

	MPI_Comm comm = MPI_COMM_WORLD;
#ifdef PARALLEL_IO
	int nprocs;

	MPI_Init( &argc, &argv );
	MPI_Comm_size ( comm, &nprocs );
	MPI_Comm_rank( comm, &myproc );
#endif

	while ( --argc ) {
		if ( strcmp ( argv[argc], "-r" ) == 0 )
			opt_read = 1;
		else if ( strcmp ( argv[argc], "-w" ) == 0 )
			opt_write = 1;
		else {
			fprintf ( stderr,
				  "Illegal option %s\n\n"
				  "Usage: %s -w -r\n",
				  argv[argc], argv[0] );
			return 1;
		}
	}

	H5PartSetVerbosityLevel ( 4 );

	if ( opt_write ) {
		if ( _write_file ( fname, myproc, comm, Layout1 ) < 0 ) {
			printf ("Failed to write file \"%s\"\n", fname );
			ex = 1;
			goto cleanup;
		}
		if ( _write_attributes ( fname, myproc, comm ) < 0 ) {
			printf ("Failed to write attributes \"%s\"\n", fname );
			ex = 1;
			goto cleanup;
		}
	} else if ( opt_read ) {
		if ( _read_file ( fname, myproc, comm, Layout1 ) < 0 ) {
			printf ("Failed to read file \"%s\"\n", fname );
			ex = 1;
			goto cleanup;
		}
		if ( _read_attributes ( fname, myproc, comm ) < 0 ) {
			printf ("Failed to read attributes \"%s\"\n", fname );
			ex = 1;
			goto cleanup;
		}
	}

cleanup:
#ifdef PARALLEL_IO
	MPI_Finalize();
#endif
	return ex;
}
