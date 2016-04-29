/*
  Copyright (c) 2006-2016, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5CORE_H5_H
#define __H5CORE_H5_H

#include "h5core/h5_types.h"

// dummy MPI calls for serial code
#if !defined (PARALLEL_IO)
typedef int MPI_Comm;
#define MPI_Init(argc, argv)
#define MPI_Comm_size(comm, nprocs) {(void)comm; *nprocs = 1; }
#define MPI_Comm_rank(comm, myproc) {(void)comm; *myproc = 0; }
#define MPI_Finalize()
#define MPI_COMM_WORLD (0)
#define MPI_COMM_SELF (1)
#endif

#ifdef __cplusplus
extern "C" {
#endif

extern int h5_initialized;

h5_err_t
h5_initialize (void);


#ifdef __cplusplus
}
#endif

#endif
