/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "H5hut.h"

// name of output file
const char* fname = "example_setview.h5";

// H5hut verbosity level
const h5_int64_t h5_verbosity = H5_VERBOSE_DEFAULT;

// we are going to write multiple consecutive blocks
const h5_int64_t num_blocks = 4;
const h5_int64_t num_particles_per_block = 32;

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

        // open file and create first step
        h5_file_t file = H5OpenFile (fname, H5_O_WRONLY, H5_PROP_DEFAULT);
        H5SetStep (file, 0);

	/*
	  If we want to write consecutive blocks, the 'view' can be defined
	  with H5PartSetview(). Otherwise we have to define the total number
	  of particles with H5PartSetNumParticles().
	 */
        const h5_int64_t offset = comm_rank * num_blocks * num_particles_per_block;
	H5PartSetView (
		file,
		offset,
		offset + num_blocks*num_particles_per_block -1);

        // write multiple consecutive blocks
        for (int i = 0; i < num_blocks; i++) {
		// create fake data
		h5_int32_t data[num_particles_per_block];
		for (int j = 0; j < num_particles_per_block; j++) {
			data[j] = j + i*num_particles_per_block + offset;
		}
		
                // set the "view" to select a subset of the dataset
                H5PartSetView (
                        file,
                        offset + i*num_particles_per_block,
                        offset + (i+1)*num_particles_per_block - 1);
                // write data
                H5PartWriteDataInt32 (file, "data", data);
        }

        // done
        H5CloseFile(file);
	MPI_Finalize();
        return 0;
}
