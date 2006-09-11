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

int
WriteFile ( 
	const char *fn,
	h5part_int64_t i_dims,
	h5part_int64_t j_dims,
	h5part_int64_t k_dims
	) {

	H5PartFile *f;
	h5part_int64_t i, j, k;
	int timestep;
	int timesteps = 1;
	h5part_int64_t herr;
	h5part_float64_t *data;

	printf ("Opening file \"%s\" for writing\n", fn );

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
						
					*(data + idx) = k+ 1000*j + 100000*i;
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

	}
	free ( data );
	return H5PartCloseFile ( f );
}

int
main ( 
	int argc,
	char **argv

	) {

	char *fn = "blockfile1.h5";

	H5PartSetVerbosityLevel ( 10 );

	if ( WriteFile ( fn, 64, 64, 512 ) < 0 ){
		printf (" Failed to write file %s\n", fn );
		exit ( 2 );
	}
	return 0;

}

