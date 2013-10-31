/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "H5hut.h"

#define DEFAULT_VERBOSITY       H5_VERBOSE_DEFAULT

#define FNAME                   "example_core_vfd"
#define DATASIZE                32

int
main (
        int argc, char* argv[]
        ){
        h5_int64_t verbosity = DEFAULT_VERBOSITY;

        // initialize MPI & H5hut
        int comm_rank = 0;
        int comm_size = 1;
        MPI_Init (&argc, &argv);
        MPI_Comm comm = MPI_COMM_WORLD;
        MPI_Comm_rank (comm, &comm_rank);
        MPI_Comm_size (comm, &comm_size);

        H5AbortOnError ();
        H5SetVerbosityLevel (verbosity);

        // open file and go to step#0
        char fname[64];
        sprintf (fname, "%s.%d.h5", FNAME, comm_rank);
        h5_prop_t prop = H5CreateFileProp ();
        H5SetPropFileCoreVFD (prop);
        h5_file_t file = H5OpenFile (fname, H5_O_RDONLY, prop);
        H5SetStep (file, 0);

        h5_int32_t data[DATASIZE];

        H5PartSetNumParticles(file, DATASIZE);

        // create fake data
        for (int i = 0; i < DATASIZE; i++) {
                data[i] = i + comm_rank * DATASIZE;
        }

        // write the data
        H5PartWriteDataInt32 (file, "data", data);
        
        H5CloseFile (file);

        MPI_Finalize ();
        return H5_SUCCESS;
}

