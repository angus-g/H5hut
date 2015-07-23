/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "H5hut.h"

#define FNAME           "example_setnparticles.h5"

int
main (
        int argc, char* argv[]
        ){
        // initialize MPI & H5hut
        MPI_Init (&argc, &argv);
        MPI_Comm comm = MPI_COMM_WORLD;
        int comm_rank = 0;
        MPI_Comm_rank (comm, &comm_rank);
        int comm_size = 0;
        MPI_Comm_size (comm, &comm_size);
        H5AbortOnError ();
        H5SetVerbosityLevel (H5_VERBOSE_INFO);

        // open file and open step #0
        h5_file_t file = H5OpenFile (FNAME, H5_O_RDONLY, H5_PROP_DEFAULT);
        H5SetStep (file, 0);

        // compute number of particles this process has to read
        h5_ssize_t nparticels_total = H5PartGetNumParticles (file);
        
        h5_ssize_t nparticels = nparticels_total / comm_size;
        if (comm_rank+1 == comm_size)
                nparticels += nparticels_total % comm_size;

        h5_info ("Total number of particles: %lld", (long long unsigned)nparticels_total);
        h5_info ("Number of particles on this core: %lld", (long long unsigned)nparticels);

        // read data
        H5PartSetNumParticles (file, nparticels);
        h5_int32_t* data = calloc (nparticels, sizeof (*data));
        H5PartReadDataInt32 (file, "data", data);

        // cleanup
        H5CloseFile (file);
        return MPI_Finalize ();
}
