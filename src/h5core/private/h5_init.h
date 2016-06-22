/*
  Copyright (c) 2006-2016, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __PRIVATE_H5_INIT_H
#define __PRIVATE_H5_INIT_H

#include "h5core/h5_types.h"
#include "private/h5t_types.h"

#define UNUSED_ARGUMENT(x) (void)x

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

h5_err_t
h5priv_initialize (void);

extern int h5_initialized;
extern h5_dta_types_t h5_dta_types;
extern int h5_myproc;

#ifdef __cplusplus
}
#endif


#endif
