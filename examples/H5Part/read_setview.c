/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "H5hut.h"
#include "examples.h"

#define FNAME           "example_setview.h5"

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

        // compute a "canonical" view: all cores get almost the same number of
        // particles
        h5_int64_t total_particles = H5PartGetNumParticles (file);
        h5_int64_t nparticles = total_particles / mpi_size;
        h5_int64_t remainder = total_particles % mpi_size;
        h5_int64_t start = mpi_rank * nparticles;

        // adjust number of local particles
        if (mpi_rank < remainder)
                nparticles++;

        // adjust start
        if (mpi_rank < remainder) 
                start += mpi_rank;
        else
                start += remainder;
        
        // Note: if npartices is 0 end = start - 1
        // this forces the selection of zero particles!
        h5_int64_t end = start + nparticles - 1;
        
        printf ("[proc %d]: set view to [%lld..%lld]\n", mpi_rank, start, end);
        H5PartSetView (file, start, end);
        h5_int32_t* data = calloc (nparticles, sizeof (*data));

        H5PartReadDataInt32 (file, "data", data);
        for (int i = 0; i < nparticles; i++) {
                printf ("[proc %d]: global index = %lld; local index = %d, value = %d\n",
                        mpi_rank, start+i, i, data[i]);
        }
        H5CloseFile (file);
        return MPI_Finalize ();
}
