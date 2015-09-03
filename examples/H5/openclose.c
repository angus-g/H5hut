/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "H5hut.h"

#if !defined (PARALLEL_IO)
#define MPI_Init(argc, argv)
#define MPI_Comm_size(comm, nprocs) { *nprocs = 1; }
#define MPI_Comm_rank(comm, myproc) { *myproc = 0; }
#define MPI_Finalize()
#define MPI_COMM_WORLD (0)
#endif

int
main (
	int argc,
	char** argv
	) {
	MPI_Comm comm = MPI_COMM_WORLD;

	int myproc;
	int nprocs;
	MPI_Init (&argc, &argv);
	MPI_Comm_size (comm, &nprocs);
	MPI_Comm_rank (comm, &myproc);

        h5_prop_t prop = H5CreateFileProp ();
        H5SetPropFileMPIO (prop, &comm);
        h5_file_t f = H5OpenFile ("testfile.h5", H5_O_WRONLY, prop);
        H5CloseProp (prop);
	H5CloseFile (f);

	MPI_Finalize ();
	return 0;
}
