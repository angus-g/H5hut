#include <stdio.h>
#include <stdlib.h>
#include <hdf5.h>
#include "H5Part.h"
#include "H5Block.h"
#include <mpi.h>


static h5part_int64_t
calc_index_KJI (
	h5part_int64_t i,
	h5part_int64_t x_extent,
	h5part_int64_t j,
	h5part_int64_t y_extent,
	h5part_int64_t k,
	h5part_int64_t z_extent
	) {
	return i + j*x_extent + k*x_extent*y_extent;
}

static int
_define_3dlayout (
	H5PartFile *f,
	h5part_int64_t *dims,
	int myproc
	) {

	h5part_int64_t i_start = 0;
	h5part_int64_t i_end = 63;
	h5part_int64_t j_start = 0;
	h5part_int64_t j_end = 63;
	h5part_int64_t k_start = 64 * myproc;
	h5part_int64_t k_end = k_start + 63;

	herr_t herr = H5BlockDefine3DFieldLayout ( f,
					    i_start, i_end,
					    j_start, j_end,
					    k_start, k_end );
	if ( herr < 0 ) return -1;

	dims[0] = i_end - i_start + 1;
	dims[1] = j_end - j_start + 1;
	dims[2] = k_end - k_start + 1;

	return 0;
}


static int
_read3d_scalar (
	H5PartFile *f,
	const char *name,
	h5part_int64_t *dims,
	int myproc

	) {
	h5part_float64_t *data;
	int i, j, k, idx;

	data = (h5part_float64_t*) malloc (
		dims[0] * dims[1] * dims[2] * sizeof(*data) );

	herr_t herr = H5Block3dReadScalarField ( f, name, data );
	if ( herr < 0 ) return -1;

	for ( i = 0; i < dims[0]; i++ ) {
		for ( j = 0; j < dims[1]; j++ ) {
			printf ("(%d,%d,0): ", i, j );
			for ( k = 0; k < dims[2]; k++ ) {
				idx = calc_index_KJI ( i, dims[0], j, dims[1], k, dims[2] );
				printf ( "%.0f  ", *(data+idx) );
			}
			printf ( "\n" );
		}
	}

	return 0;
}

static int
_read3d_3d_vector (
	H5PartFile *f,
	const char *name,
	h5part_int64_t *dims

	) {
	h5part_float64_t *x_data;
	h5part_float64_t *y_data;
	h5part_float64_t *z_data;

	x_data = (h5part_float64_t*) malloc (
		dims[0] * dims[1] * dims[2] * sizeof(*x_data) );
	y_data = (h5part_float64_t*) malloc (
		dims[0] * dims[1] * dims[2] * sizeof(*y_data) );
	z_data = (h5part_float64_t*) malloc (
		dims[0] * dims[1] * dims[2] * sizeof(*z_data) );

	herr_t herr = H5Block3dRead3dVectorField ( f, name,
						   x_data, y_data, z_data );
	if ( herr < 0 ) return -1;

	return 0;
}

static int
_read_field (
	H5PartFile *f,
	h5part_int64_t idx,
	int myproc
	) {

	char name[256];
	h5part_int64_t grid_rank;
	h5part_int64_t grid_dims[16];
	h5part_int64_t field_dims;

	herr_t herr = H5BlockGetFieldInfo (
		f,
		idx,
		name, sizeof(name),
		&grid_rank,
		grid_dims,
		&field_dims );
	if ( herr < 0 ) return -1;

	printf ( "Field #%lld has name \"%s\"\n", (long long) idx, name );

	switch ( grid_rank ) {
	case 3:
		printf ( "\tGrid dimension of \"%s\" is 3\n", name );
		printf ( "\tThe dimesion sizes are: (%lld, %lld, %lld)\n",
			 (long long)grid_dims[0], (long long)grid_dims[1], (long long)grid_dims[2] );
		if ( field_dims == 1 ) {
			printf ( "\tField data are scalar\n" );
		} else {
			printf ( "\tField data are %lld vectors\n",
				 (long long)field_dims );
		}
		_define_3dlayout ( f, grid_dims, myproc );

		if ( field_dims == 1 ) {
			_read3d_scalar ( f, name, grid_dims, myproc );
		} else if ( field_dims == 3 ) {
			_read3d_3d_vector ( f, name, grid_dims );
		}
		break;
	default:
		printf ( "unknown grid rank %lld!!!\n", (long long)grid_rank );
	}
	return 0;
}

static int
ReadFile (
	const char *fn,
	MPI_Comm comm,
	int myproc
	) {

	H5PartFile *f;
	h5part_int64_t herr;

	h5part_int64_t timestep;
	h5part_int64_t timesteps;

	h5part_int64_t i, n;

	printf ("Opening file %s for reading\n", fn );
  
	f = H5PartOpenFileParallel ( fn, H5PART_READ, comm );
	if ( f == NULL ) return -1;
  
	timesteps = H5PartGetNumSteps ( f );
  
	printf ( "Timesteps = %lld\n", (long long)timesteps );
	printf ( "\n===============================\n" );

	for ( timestep = 0; timestep < timesteps; timestep++) {
		herr = H5PartSetStep ( f, timestep );
		if ( herr < 0 ) return -1;

		n = H5BlockGetNumFields ( f );
		if ( n < 0 ) return -1;
		printf ( "Number of fields in time-step #%lld: %lld \n",
			 (long long)timestep,  (long long)n );

		for ( i = 0; i < n; i++ ) {
			_read_field ( f, i, myproc );

		}
	}
	return H5PartCloseFile ( f );
}


int
main ( 
	int argc,
	char **argv

	) {

	char *fn = "blockfile1.h5";
	int myproc;
	int nprocs;

	H5PartSetVerbosityLevel ( 10 );

	MPI_Comm comm=MPI_COMM_WORLD;

	MPI_Init(&argc,&argv);
	MPI_Comm_size(comm,&nprocs);
	MPI_Comm_rank(comm,&myproc);


	if ( ReadFile ( fn, comm, 1 ) < 0 ){
		printf ("Failed to read file %s\n", fn );
		exit ( 2 );
	}

	return 0;

}

