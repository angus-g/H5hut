/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "H5hut.h"

#define DEFAULT_VERBOSITY       H5_VERBOSE_DEFAULT

#define FNAME                   "example_setview.h5"

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
        h5_file_t file = H5OpenFile (FNAME, H5_O_RDONLY, H5_PROP_DEFAULT);
        H5SetStep (file, 0);

        // compute a "canonical" view: all cores get almost the same number of
        // particles
        h5_int64_t total_particles = H5PartGetNumParticles (file);
        h5_int64_t nparticles = total_particles / comm_size;
        h5_int64_t remainder = total_particles % comm_size;
        h5_int64_t start = comm_rank * nparticles;

        // adjust number of local particles
        if (comm_rank < remainder)
                nparticles++;

        // adjust start
        if (comm_rank < remainder) 
                start += comm_rank;
        else
                start += remainder;
        
        // Note: setting end = start - 1 forces the 
        // selection of zero particles!
        h5_int64_t end = start + nparticles - 1;
        
        printf ("[proc %d]: set view to [%lld..%lld]\n", comm_rank, start, end);
        H5PartSetView (file, start, end);
        h5_int32_t* data = calloc (nparticles, sizeof (*data));

        H5PartReadDataInt32 (file, "data", data);
        for (int i = 0; i < nparticles; i++) {
                printf ("[proc %d]: global index = %lld; local index = %d, value = %d\n",
                        comm_rank, start+i, i, data[i]);
        }
        H5CloseFile (file);
        return MPI_Finalize ();
}
