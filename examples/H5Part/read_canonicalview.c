/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "H5hut.h"

#define FNAME           "example_setview.h5"
#define VERBOSITY       H5_VERBOSE_DEFAULT
int
main (
        int argc, char* argv[]
        ){
        // initialize MPI & H5hut
        int mpi_rank = 0;
        int mpi_size = 1;
        MPI_Init (&argc, &argv);
        MPI_Comm comm = MPI_COMM_WORLD;
        MPI_Comm_rank (comm, &mpi_rank);
        MPI_Comm_size (comm, &mpi_size);

        // open file and go to step#0
        h5_file_t file = H5OpenFile (FNAME, H5_O_RDONLY, H5_PROP_DEFAULT);
        H5SetStep (file, 0);
  
        H5PartSetCanonicalView (file);
        h5_int64_t num_particles = H5PartGetNumParticles (file);
        printf ("[proc %d]: particles in view: %lld\n", myproc, num_particles);

        H5CloseFile (file);
        return MPI_Finalize ();
}
