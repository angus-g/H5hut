/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include <stdlib.h>
#include "H5hut.h"

#define FNAME           "example_setview.h5"
#define DATASIZE        32
#define ITERS           4

int
main (
        int argc, char** argv
        ) {

        // initialize MPI & H5hut
        MPI_Init (&argc, &argv);
        MPI_Comm comm = MPI_COMM_WORLD;
        int mpi_rank = 0;
        MPI_Comm_rank (comm, &mpi_rank);

        H5AbortOnError ();

        // create fake data
        h5_int64_t npoints = ITERS*DATASIZE;
        h5_int32_t data[ITERS*DATASIZE];
        for (int i = 0; i < npoints; i++) {
                data[i] = i + mpi_rank*npoints;
        }

        // open file and create step #0
        h5_file_t file = H5OpenFile (FNAME, H5_O_WRONLY, H5_PROP_DEFAULT);
        H5SetStep(file, 0);

        // before we can start writing, we have to define the number of
        // items this processor will write
        H5PartSetNumParticles(file, npoints);

        // write ITER consecutive blocks of size DATASIZE
        h5_int64_t offset = mpi_rank * npoints;
        for (int i = 0; i < ITERS; i++) {
                // set the "view" to select a subset of the dataset
                H5PartSetView (
                        file,
                        offset + i * DATASIZE,
                        offset + (i+1) * DATASIZE - 1);
                // write the data
                H5PartWriteDataInt32 (file, "data", data + i*DATASIZE);
        }

        // done
        H5CloseFile(file);
        MPI_Finalize();
        return H5_SUCCESS;
}

