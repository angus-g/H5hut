/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "H5hut.h"

#define DEFAULT_VERBOSITY       H5_VERBOSE_DEFAULT

#define FNAME                   "example_strided.h5"

int
main (
        int argc, char* argv[]
        ){
        h5_int64_t verbosity = DEFAULT_VERBOSITY;

        // initialize MPI & H5hut
        MPI_Init (&argc, &argv);
        MPI_Comm comm = MPI_COMM_WORLD;
        int rank = 0;
        MPI_Comm_rank (comm, &rank);

        H5AbortOnError ();
        H5SetVerbosityLevel (verbosity);

        h5_file_t file = H5OpenFile (FNAME, H5_O_RDONLY, H5_PROP_DEFAULT);

        H5SetStep (file, 0);

        // Get number of particles in datasets and allocate memory
        h5_int64_t num_particles = H5PartGetNumParticles (file);
        h5_float64_t* data = calloc (6*num_particles, sizeof (*data));

        // set number of particles and memory stride
        H5PartSetNumParticlesStrided (file, num_particles, 6);

        // read data
        H5PartReadDataFloat64 (file, "x",  data+0);
        H5PartReadDataFloat64 (file, "y",  data+1);
        H5PartReadDataFloat64 (file, "z",  data+2);
        H5PartReadDataFloat64 (file, "px", data+3);
        H5PartReadDataFloat64 (file, "py", data+4);
        H5PartReadDataFloat64 (file, "pz", data+5);

        H5CloseFile (file);
        return MPI_Finalize ();
}
