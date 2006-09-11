#include <stdio.h>
#include <stdlib.h>
#include <hdf5.h>
#include "H5Part.h"
#include "H5Block.h"


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
	h5part_int64_t *dims
	) {


	herr_t herr = H5BlockDefine3DFieldLayout ( f,
					    0, dims[0]-1,
					    0, dims[1]-1,
					    0, dims[2]-1 );
	if ( herr < 0 ) return -1;

	return 0;
}


static int
_read3d_scalar (
	H5PartFile *f,
	const char *name,
	h5part_int64_t *dims

	) {
	h5part_float64_t *data;
	int i, j, k, idx;

	data = (h5part_float64_t*) malloc (
		dims[0] * dims[1] * dims[2] * sizeof(*data) );

	herr_t herr = H5Block3dReadScalarField ( f, name, data );
	if ( herr < 0 ) return -1;

	herr = H5BlockDefine3DFieldLayout ( f,
					    1, 3,
					    1, 4,
					    0, 4 );
	if ( herr < 0 ) return -1;

	herr = H5Block3dReadScalarField ( f, name, data );
	if ( herr < 0 ) return -1;
	for ( i = 0; i < 3; i++ ) {
		for ( j = 0; j < 4; j++ ) {
			printf ("(%d,%d,0): ", i, j );
			for ( k = 0; k < 5; k++ ) {
				idx = calc_index_KJI ( i, 3, j, 4, k, 5 );
				printf ( "%.0f  ", *(data+idx) );
			}
			printf ( "\n" );
		}
	}

	herr = H5BlockDefine3DFieldLayout ( f,
					    0, dims[0]-1,
					    0, dims[1]-1,
					    0, dims[2]-1 );
	if ( herr < 0 ) return -1;
	herr = H5Block3dReadScalarField ( f, name, data );
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
	h5part_int64_t idx
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
		_define_3dlayout ( f, grid_dims );
		if ( field_dims == 1 ) {
			_read3d_scalar ( f, name, grid_dims );
		} else if ( field_dims == 3 ) {
			_read3d_3d_vector ( f, name, grid_dims );
		}
		break;
	default:
		printf ( "unknown grid rank %lld!!!\n", (long long)grid_rank );
	}
#if 0	
	for ( i = 0; i < i_dims; i++ ) {
		for ( j = 0; j < j_dims; j++ ) {
			for ( k = 0; k < k_dims; k++ ) {
				h5part_int64_t idx;
				idx = k + j*k_dims + i*k_dims*j_dims;
				
				if ( *(data+idx) != i + 10*j + 100*k ) {
					printf ( "Block data error!\n" );
					return -1;
				}
			}
		}
	}
	free ( data );
#endif
	return 0;
}

static int
ReadFile (
	const char *fn
	) {

	H5PartFile *f;
	h5part_int64_t herr;

	h5part_int64_t timestep;
	h5part_int64_t timesteps;

	h5part_int64_t i, n;


	printf ("Opening file %s\n", fn );
  
	f = H5PartOpenFile ( fn, H5PART_READ );
	if ( herr < 0 ) return -1;
  
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
			_read_field ( f, i );

		}
	}
	return H5PartCloseFile ( f );
}

int
WriteFile ( 
	const char *fn
	) {

	H5PartFile *f;
	h5part_int64_t i, j, k;
	int timestep;
	int timesteps = 5;
	h5part_int64_t herr;
	h5part_float64_t *data;
	h5part_int64_t i_dims, j_dims, k_dims;

	printf ("Opening file \"%s\" for writing\n", fn );

	i_dims = 4;
	j_dims = 6;
	k_dims = 8;

	data = (h5part_float64_t*) malloc (
		i_dims * j_dims * k_dims * sizeof(*data) );

  
	f = H5PartOpenFile ( fn, H5PART_WRITE );
	if ( herr < 0 ) return -1;

	for ( timestep = 0; timestep < timesteps; timestep++){
		herr = H5PartSetStep ( f, timestep );
		if ( herr < 0 ) return -1;

		printf ( "Write Step %d\n", timestep );

		for ( i = 0; i < i_dims; i++ ) {
			for ( j = 0; j < j_dims; j++ ) {
				for ( k = 0; k < k_dims; k++ ) {
					h5part_int64_t idx;
					idx = calc_index_KJI ( i, i_dims, j, j_dims, k, k_dims );
						
					*(data + idx) = k+ 10*j + 100*i;
				}
			}
		}

		herr = H5BlockDefine3DFieldLayout ( f,
						    0, i_dims-1,
						    0, j_dims-1,
						    0, k_dims-1 );
		if ( herr < 0 ) return -1;
		herr = H5Block3dWriteScalarField ( f, "scalar", data );
		if ( herr < 0 ) return -1;
		herr = H5Block3dWrite3dVectorField ( f, "3dVector",
						   data, data, data );
		if ( herr < 0 ) return -1;

	}
	free ( data );
	return H5PartCloseFile ( f );
}

int
main ( 
	int argc,
	char **argv

	) {

	char *fn;
	char dstr[]="testfile.h5";

	if ( argc > 1 )
		fn = argv[1];
	else
		fn = dstr;

	H5PartSetVerbosityLevel ( 10 );

	if ( WriteFile ( fn ) < 0 ){
		printf (" Failed to write file %s\n", fn );
		exit ( 2 );
	}

	if ( ReadFile ( fn ) < 0 ){
		printf ("Failed to read file %s\n", fn );
		exit ( 2 );
	}

	return 0;

}

