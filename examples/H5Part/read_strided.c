/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "H5hut.h"
#include "examples.h"

#define FNAME           "example_particles.h5"

int
main (
        int argc, char* argv[]
        ){
        H5AbortOnError ();
        H5SetVerbosityLevel (VERBOSITY);

        int myproc;
        MPI_Init (&argc, &argv);
        MPI_Comm_rank (MPI_COMM_WORLD, &myproc);

        h5_file_t file = H5OpenFile (FNAME, H5_O_RDONLY, H5_PROP_DEFAULT);

        H5SetStep (file, 0);

        // TODO
  
        H5CloseFile (file);
        return MPI_Finalize ();
}
