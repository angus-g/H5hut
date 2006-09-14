#include <stdio.h>
#include <stdlib.h>
#include <hdf5.h>
#include "H5Part.h"
#include "H5Block.h"
#include "H5BlockTypes.h"

#define NPROCS  8

struct H5BlockPartition Layout1[1] = {
	{ 0, 63, 0, 63, 0, 511 }
};

struct H5BlockPartition Layout8[8] = {
		{  0,63,  0,63,   0, 63},
		{  0,63,  0,63,  64,127},
		{  0,63,  0,63, 128,191},
		{  0,63,  0,63, 192,255},
		{  0,63,  0,63, 256,319},
		{  0,63,  0,63, 320,383},
		{  0,63,  0,63, 384,447},
		{  0,63,  0,63, 448,511}
};

struct H5BlockPartition Layout8G[8] = {
		{  0,63,  0,63,   0, 64},
		{  0,63,  0,63,  63,128},
		{  0,63,  0,63, 127,192},
		{  0,63,  0,63, 191,256},
		{  0,63,  0,63, 255,320},
		{  0,63,  0,63, 319,384},
		{  0,63,  0,63, 383,448},
		{  0,63,  0,63, 447,511}
};

struct H5BlockPartition Layout16[16] = {
		{  0,63,  0,31,   0, 63},
		{  0,63, 32,63,   0, 63},
		{  0,63,  0,31,  64,127},
		{  0,63, 32,63,  64,127},
		{  0,63,  0,31, 128,191},
		{  0,63, 32,63, 128,191},
		{  0,63,  0,31, 192,255},
		{  0,63, 32,63, 192,255},
		{  0,63,  0,31, 256,319},
		{  0,63, 32,63, 256,319},
		{  0,63,  0,31, 320,383},
		{  0,63, 32,63, 320,383},
		{  0,63,  0,31, 384,447},
		{  0,63, 32,63, 384,447},
		{  0,63,  0,31, 448,511},
		{  0,63, 32,63, 448,511}
};

struct H5BlockPartition Layout16G[16] = {
		{  0,63,  0,32,   0, 64},
		{  0,63, 31,63,   0, 64},
		{  0,63,  0,32,  63,128},
		{  0,63, 31,63,  63,128},
		{  0,63,  0,32, 127,192},
		{  0,63, 31,63, 127,192},
		{  0,63,  0,32, 191,256},
		{  0,63, 31,63, 191,256},
		{  0,63,  0,32, 255,320},
		{  0,63, 31,63, 255,320},
		{  0,63,  0,32, 319,384},
		{  0,63, 31,63, 319,384},
		{  0,63,  0,32, 383,448},
		{  0,63, 31,63, 383,448},
		{  0,63,  0,32, 447,511},
		{  0,63, 31,63, 447,511}
};


struct H5BlockPartition Layout32[32] = {
		{  0,31,  0,31,   0, 63},
		{  0,31, 32,63,   0, 63},
		{ 32,63,  0,31,   0, 63},
		{ 32,63, 32,63,   0, 63},
		{  0,31,  0,31,  64,127},
		{  0,31, 32,63,  64,127},
		{ 32,63,  0,31,  64,127},
		{ 32,63, 32,63,  64,127},
		{  0,31,  0,31, 128,191},
		{  0,31, 32,63, 128,191},
		{ 32,63,  0,31, 128,191},
		{ 32,63, 32,63, 128,191},
		{  0,31,  0,31, 192,255},
		{  0,31, 32,63, 192,255},
		{ 32,63,  0,31, 192,255},
		{ 32,63, 32,63, 192,255},
		{  0,31,  0,31, 256,319},
		{  0,31, 32,63, 256,319},
		{ 32,63,  0,31, 256,319},
		{ 32,63, 32,63, 256,319},
		{  0,31,  0,31, 320,383},
		{  0,31, 32,63, 320,383},
		{ 32,63,  0,31, 320,383},
		{ 32,63, 32,63, 320,383},
		{  0,31,  0,31, 384,447},
		{  0,31, 32,63, 384,447},
		{ 32,63,  0,31, 384,447},
		{ 32,63, 32,63, 384,447},
		{  0,31,  0,31, 448,511},
		{  0,31, 32,63, 448,511},
		{ 32,63,  0,31, 448,511},
		{ 32,63, 32,63, 448,511}
};

struct H5BlockPartition Layout32G[32] = {
		{  0,32,  0,32,   0, 64},
		{  0,32, 31,63,   0, 64},
		{ 31,63,  0,32,   0, 64},
		{ 31,63, 31,63,   0, 64},
		{  0,32,  0,32,  63,128},
		{  0,32, 31,63,  63,128},
		{ 31,63,  0,32,  63,128},
		{ 31,63, 31,63,  63,128},
		{  0,32,  0,32, 127,192},
		{  0,32, 31,63, 127,192},
		{ 31,63,  0,32, 127,192},
		{ 31,63, 31,63, 127,192},
		{  0,32,  0,32, 191,256},
		{  0,32, 31,63, 191,256},
		{ 31,63,  0,32, 191,256},
		{ 31,63, 31,63, 191,256},
		{  0,32,  0,32, 255,320},
		{  0,32, 31,63, 255,320},
		{ 31,63,  0,32, 255,320},
		{ 31,63, 31,63, 255,320},
		{  0,32,  0,32, 319,384},
		{  0,32, 31,63, 319,384},
		{ 31,63,  0,32, 319,384},
		{ 31,63, 31,63, 319,384},
		{  0,31,  0,31, 383,448},
		{  0,31, 31,63, 383,448},
		{ 31,63,  0,31, 383,448},
		{ 31,63, 31,63, 383,448},
		{  0,32,  0,32, 447,511},
		{  0,32, 31,63, 447,511},
		{ 31,63,  0,32, 447,511},
		{ 31,63, 31,63, 447,511}
};

#if 0
static h5part_int64_t
_calc_index_KJI (
	int myproc,
	h5part_int64_t i,
	h5part_int64_t i_dims,
	h5part_int64_t j,
	h5part_int64_t j_dims,
	h5part_int64_t k,
	h5part_int64_t k_dims
	) {

	return i + j*i_dims + k*i_dims*j_dims;
}
#endif

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
  
	f = H5PartOpenFileParallel ( fname, H5PART_WRITE, comm );
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
						layout->i_start, layout->i_end,
						layout->j_start, layout->j_end,
						layout->k_start, layout->k_end );
					printf (
						"PROC[%d]: "
						"Value has been written by proc %d\n",
						myproc, proc );
					printf (
						"PROC[%d]: "
						"The partition for this proc was: "
						"%lld:%lld, %lld:%lld, %lld:%lld\n",
						myproc,
						i_start, i_end,
						j_start, j_end,
						k_start, k_end );
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
  
	f = H5PartOpenFileParallel ( fname, H5PART_READ, comm );
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

int
main (
	int argc,
	char **argv
	) {
	char *fname;
	int myproc;
	int nprocs;
	int opt_with_ghosts = 0;
	int opt_read = 0;
	int opt_write = 0;
	struct H5BlockPartition *layout;
	MPI_Comm comm = MPI_COMM_WORLD;

	MPI_Init( &argc, &argv );
	MPI_Comm_size ( comm, &nprocs );
	MPI_Comm_rank( comm, &myproc );

	while ( --argc ) {
		if ( strcmp ( argv[argc], "-r" ) == 0 )
			opt_read = 1;
		else if ( strcmp ( argv[argc], "-w" ) == 0 )
			opt_write = 1;
		else if ( strcmp ( argv[argc], "-g" ) == 0 )
			opt_with_ghosts = 1;
		else {
			fprintf ( stderr,
				  "Illegal option %s\n\n"
				  "Usage: %s -w -r -g\n",
				  argv[argc], argv[0] );
			return 1;
		}
	}
	switch ( nprocs ) {
	case 1:
		fname  = "blockfile1.h5";
		layout = &Layout1[myproc];
		break;
	case 8:
		if ( opt_with_ghosts ) {
			fname  = "blockfile8G.h5";
			layout = &Layout8G[myproc];
		} else {
			fname  = "blockfile8.h5";
			layout = &Layout8[myproc];
		}
		break;
	case 16:
		if ( opt_with_ghosts ) {
			fname  = "blockfile16G.h5";
			layout = &Layout16G[myproc];
		} else {
			fname  = "blockfile16.h5";
			layout = &Layout16[myproc];
		}
		break;
	case 32:
		if ( opt_with_ghosts ) {
			fname  = "blockfile32G.h5";
			layout = &Layout32G[myproc];
		} else {
			fname  = "blockfile32.h5";
			layout = &Layout32[myproc];
		}
		break;
	default:
		printf ( "Run this test on %d processor(s)!\n", NPROCS );
		return 1;
	}

	H5PartSetVerbosityLevel ( 4 );

	if ( opt_write ) {
		if ( _write_file ( fname, myproc, comm, layout ) < 0 ) {
			printf ("Failed to write file \"%s\"\n", fname );
			return 2;
		}
	} else if ( opt_read ) {
		if ( _read_file ( fname, myproc, comm, layout ) < 0 ) {
			printf ("Failed to read file \"%s\"\n", fname );
			return 2;
		}
	}

	return 0;
}
