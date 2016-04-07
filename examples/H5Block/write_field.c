/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "H5hut.h"

// name of output file
const char* fname = "example_setnparticles.h5";

// H5hut verbosity level
const h5_int64_t h5_verbosity = H5_VERBOSE_DEFAULT;

// number of particles we are going to write per core
const ssize_t dim_x = 32;
const ssize_t dim_y = 8;
const ssize_t dim_z = 8;

#define idx(i, j, k)	(i + j*dim_y + k*dim_x*dim_y)

int
main (
        int argc,
        char* argv[]
        ){

        // initialize MPI & H5hut
        MPI_Init (&argc, &argv);
        MPI_Comm comm = MPI_COMM_WORLD;
        int comm_size = 1;
        MPI_Comm_size (comm, &comm_size);
        int comm_rank = 0;
        MPI_Comm_rank (comm, &comm_rank);
        H5AbortOnError ();
        H5SetVerbosityLevel (h5_verbosity);
	H5SetDebugMask (-1);

	// slice field in X direction

	ssize_t n_slices = dim_x / comm_size;
	ssize_t remaining_slices = dim_x % comm_size;
	ssize_t i_start = comm_rank * n_slices;
	if (comm_rank < remaining_slices) {
		n_slices++;
		i_start += comm_rank;
	} else {
		i_start += remaining_slices;
	}
	ssize_t i_end = i_start + n_slices;

	// create fake data
        h5_int64_t data[(i_end-i_start+1) * dim_y * dim_z];
	for (int k = 0; k < dim_z; k++) {
		for (int j = 0; j < dim_y; j++) {
			for (int i = i_start; i < i_end; i++) {
				data[idx(i-i_start,j,k)] = (h5_int64_t)idx(i,j,k);
			}
		}
	}
	

        // open file and create first step
        h5_file_t file = H5OpenFile (fname, H5_O_WRONLY, H5_PROP_DEFAULT);
        H5SetStep (file, 0); 

	H5Block3dSetView (file, i_start, i_end, 0, dim_y-1, 0, dim_z-1);
	
        // write data
        H5Block3dWriteScalarFieldInt64 (file, "data", data);

        // done
        H5CloseFile(file);
	MPI_Finalize ();
        return 0;
}
