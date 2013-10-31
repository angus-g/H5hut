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
#define NPOINTS                 99

int
main (
	int argc,
	char** argv
	) {
        h5_int64_t verbosity = DEFAULT_VERBOSITY;

        // MPI & H5hut init
	MPI_Init (&argc, &argv);
        int rank;
        MPI_Comm comm = MPI_COMM_WORLD;
        MPI_Comm_rank (comm, &rank);

        H5AbortOnError ();
        H5SetVerbosityLevel (verbosity);

        // create fake data
        h5_float64_t particles[6*NPOINTS];
        h5_int64_t id[NPOINTS];
        for (int i = 0; i < NPOINTS; i++) {
                particles [6*i + 0] = 0.0 + i + NPOINTS * rank;
                particles [6*i + 1] = 0.1 + i + NPOINTS * rank;
                particles [6*i + 2] = 0.2 + i + NPOINTS * rank;
                particles [6*i + 3] = 0.3 + i + NPOINTS * rank;
                particles [6*i + 4] = 0.4 + i + NPOINTS * rank;
                particles [6*i + 5] = 0.5 + i + NPOINTS * rank;
                id [i] = i + NPOINTS * rank;
        }

        // open file with MPI_COMM_WORLD and create step #0
        h5_file_t file = H5OpenFile (FNAME, H5_O_WRONLY, H5_PROP_DEFAULT);
        H5SetStep (file, 0);

        // define number of items this processor will write and set the
        // in-memory striding
        H5PartSetNumParticlesStrided (file, NPOINTS, 6);

        // write strided data
        H5PartWriteDataFloat64 (file, "x",  particles+0);
        H5PartWriteDataFloat64 (file, "y",  particles+1);
        H5PartWriteDataFloat64 (file, "z",  particles+2);
        H5PartWriteDataFloat64 (file, "px", particles+3);
        H5PartWriteDataFloat64 (file, "py", particles+4);
        H5PartWriteDataFloat64 (file, "pz", particles+5);

        // disable striding to write the ID's
        H5PartSetNumParticles (file, NPOINTS);
        H5PartWriteDataInt64 (file, "id", id);

        // cleanup
	H5CloseFile (file);
	MPI_Finalize ();
	return 0;
}
