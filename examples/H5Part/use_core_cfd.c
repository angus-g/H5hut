/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "H5hut.h"
#include "examples.h"

#include <stdlib.h>
#include <assert.h>

#define FNAME           "example_core_vfd"
#define DATASIZE 32

int
main (
        int argc, char* argv[]
        ){
        H5AbortOnError ();
        H5SetVerbosityLevel (VERBOSITY);

        h5_int32_t data[DATASIZE];
        h5_int64_t stat;
        h5_file_t file;

        // initialize MPI
        MPI_Init (&argc, &argv);
        MPI_Comm_rank (MPI_COMM_WORLD, &rank);
        MPI_Comm_size (MPI_COMM_WORLD, &nprocs);

        char filename[32];
        sprintf (filename, "%s.%d.h5", FNAME, rank);
        file = H5OpenFile (filename, H5_O_WRONLY|H5_VFD_CORE, MPI_COMM_SELF);

        H5SetStep (file, 0);

        H5PartSetNumParticles(file, DATASIZE);

        // create fake data
        for (int i = 0; i < DATASIZE; i++) {
                data[i] = i + rank * DATASIZE;
        }

        // write the data
        H5PartWriteDataInt32 (file, "data", data);
        
        H5CloseFile (file);

        MPI_Finalize ();
        return H5_SUCCESS;
}

