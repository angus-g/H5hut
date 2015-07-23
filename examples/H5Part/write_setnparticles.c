/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include <stdlib.h>
#include "H5hut.h"

#define DEFAULT_VERBOSITY       H5_VERBOSE_DEFAULT

#define FNAME                   "example_setnparticles.h5"
#define NUM_PARTICLES           3

int
main (
        int argc,
        char* argv[]
        ){
        h5_int64_t verbosity = DEFAULT_VERBOSITY;

        // initialize MPI & H5hut
        MPI_Init (&argc, &argv);
        MPI_Comm comm = MPI_COMM_WORLD;
        int rank = 0;
        MPI_Comm_rank (comm, &rank);

        H5AbortOnError ();
        H5SetVerbosityLevel (verbosity);

        // create fake data
        h5_int32_t data[NUM_PARTICLES];
        h5_int64_t num_particles = NUM_PARTICLES;
        for (int i = 0; i < num_particles; i++) {
                data[i] = i + num_particles * rank;
        }

        // open file and create step #0
        h5_file_t file = H5OpenFile (FNAME, H5_O_WRONLY, H5_PROP_DEFAULT);
        H5SetStep (file, 0); 

        // define number of items this process will write
        H5PartSetNumParticles (file, num_particles);

        // write data
        H5PartWriteDataInt32 (file, "data", data);

        // done
        H5CloseFile(file);
        return MPI_Finalize ();
}
