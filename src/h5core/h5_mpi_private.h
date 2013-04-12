/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5_MPI_PRIVATE_H
#define __H5_MPI_PRIVATE_H

#ifdef PARALLEL_IO

#include "h5core/h5_types.h"
#include "h5core/h5_errorhandling.h"
#include "h5_debug_private.h"

static inline h5_err_t
h5priv_mpi_alltoall (
        void* sendbuf,
        const int sendcount,
        const MPI_Datatype sendtype,
        void* recvbuf,
        const int recvcount,
        const MPI_Datatype recvtype,
        const MPI_Comm comm
        ) {
	MPI_WRAPPER_ENTER (h5_err_t,
	                   "sendbuf=%p, sendcount=%d, sendtype=?, recvbuf=%p, "
	                   "recvcount=%d, recvtype=?, comm=?",
	                   sendbuf, sendcount, recvbuf, recvcount);
	int err = MPI_Alltoall (
	        sendbuf,
	        sendcount,
	        sendtype,
	        recvbuf,
	        recvcount,
	        recvtype,
	        comm);
	if (err != MPI_SUCCESS)
		MPI_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_MPI,
		                "Cannot perform all to all communication"));
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_err_t
h5priv_mpi_alltoallv (
        void* sendbuf,
        int* sendcounts,
        int* senddispls,
        const MPI_Datatype sendtype,
        void* recvbuf,
        int* recvcounts,
        int* recvdispls,
        const MPI_Datatype recvtype,
        const MPI_Comm comm
        ) {
	MPI_WRAPPER_ENTER (h5_err_t,
	                   "sendbuf=%p, sendcounts=%p, senddispls=%p, sendtype=?, "
	                   "recvbuf=%p, recvcounts=%p, recvdispls=%p, recvtype=?, "
	                   "comm=?",
	                   sendbuf, sendcounts, senddispls,
	                   recvbuf, recvcounts, recvdispls);
	int err = MPI_Alltoallv (
	        sendbuf,
	        sendcounts,
	        senddispls,
	        sendtype,
	        recvbuf,
	        recvcounts,
	        recvdispls,
	        recvtype,
	        comm);
	if (err != MPI_SUCCESS)
		MPI_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_MPI,
		                "Cannot perform all to all communication"));
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}


h5_err_t
h5priv_mpi_barrier (
        const MPI_Comm comm
        );


h5_err_t
h5priv_mpi_recv(
        void* buf,
        const int count,
        const MPI_Datatype type,
        const int from,
        const int tag,
        const MPI_Comm comm
        );

h5_err_t
h5priv_mpi_send(
        void* buf,
        const int count,
        const MPI_Datatype type,
        const int to,
        const int tag,
        const MPI_Comm comm
        );

h5_err_t
h5priv_mpi_bcast (
        void* buf,
        const int count,
        const MPI_Datatype type,
        const int root,
        const MPI_Comm comm
        );

h5_err_t
h5priv_mpi_sum (
        void* sendbuf,
        void* recvbuf,
        const int count,
        const MPI_Datatype type,
        const MPI_Comm comm
        );

h5_err_t
h5priv_mpi_prefix_sum (
        void* sendbuf,
        void* recvbuf,
        const int count,
        const MPI_Datatype type,
        const MPI_Comm comm
        );

#define h5priv_mpi_allgather mpi_allgather
static inline h5_err_t
mpi_allgather (
        void* sendbuf,
        const int sendcount,
        const MPI_Datatype sendtype,
        void* recvbuf,
        const int recvcount,
        const MPI_Datatype recvtype,
        const MPI_Comm comm
        ) {
	MPI_WRAPPER_ENTER (h5_err_t,
	                   "sendbuf=%p, sendcount=%d, sendtype=?, recvbuf=%p, "
	                   "recvcount=%d, recvtype=?, comm=?",
	                   sendbuf, sendcount, recvbuf, recvcount);
	int err = MPI_Allgather (
	        sendbuf,
	        sendcount,
	        sendtype,
	        recvbuf,
	        recvcount,
	        recvtype,
	        comm);
	if (err != MPI_SUCCESS)
		MPI_WRAPPER_LEAVE (h5_error (H5_ERR_MPI, "Cannot gather data"));
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}
#define h5priv_mpi_allgatherv mpi_allgatherv
static inline h5_err_t
mpi_allgatherv (
        void* sendbuf,
        const int sendcount,
        const MPI_Datatype sendtype,
        void* recvbuf,
        int* recvcounts,
        int* recvdispls,
        const MPI_Datatype recvtype,
        const MPI_Comm comm
        ) {
	MPI_WRAPPER_ENTER (h5_err_t,
	                   "sendbuf=%p, sendcount=%d, sendtype=?, "
	                   "recvbuf=%p, recvcounts=%p, recvtype=?, recvdispls=%p, "
	                   "&comm=%p",
	                   sendbuf, sendcount, recvbuf, recvcounts, recvdispls, &comm);
	int err = MPI_Allgatherv (
	        sendbuf,
	        sendcount,
	        sendtype,
	        recvbuf,
	        recvcounts,
	        recvdispls,
	        recvtype,
	        comm);
	if (err != MPI_SUCCESS)
		MPI_WRAPPER_LEAVE (h5_error (H5_ERR_MPI, "Cannot gather data"));
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}

///
h5_err_t
h5priv_mpi_comm_size (
        MPI_Comm comm,
        int* size
        );

h5_err_t
h5priv_mpi_comm_rank (
        MPI_Comm comm,
        int* rank
        );

// MPI type wrappers
h5_err_t
h5priv_mpi_type_contiguous (
        const size_t nelems,
        const MPI_Datatype oldtype,
        MPI_Datatype *newtype
        );

#define h5priv_mpi_get_address mpi_get_address
static inline h5_err_t
mpi_get_address (
        void* location,
        MPI_Aint* address
        ) {
	MPI_WRAPPER_ENTER (h5_err_t, "location=%p, address=%p", location, address);
	int err = MPI_Get_address (location, address);
	if (err != MPI_SUCCESS) {
		MPI_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_MPI,
		                "Cannot get MPI address of location=%p", location));
	}
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}

#define h5priv_mpi_create_type_struct  mpi_create_type_struct
static inline h5_err_t
mpi_create_type_struct (
        int count,
        int blocklens[],
        MPI_Aint indices[],
        MPI_Datatype old_types[],
        MPI_Datatype* new_type
        ) {
	MPI_WRAPPER_ENTER (h5_err_t,
	                   "count=%d, blocklens=%p, indices=%p, old_types=%p, new_type=%p",
	                   count, blocklens, indices, old_types, new_type);
	int err = MPI_Type_create_struct (count, blocklens, indices, old_types, new_type);
	if (err != MPI_SUCCESS) {
		MPI_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_MPI,
		                "Cannot create new MPI struct"));
	}
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_err_t
h5priv_mpi_type_commit (
		MPI_Datatype* type
		) {
	MPI_WRAPPER_ENTER (h5_err_t, "type=%p", type);
	int err = MPI_Type_commit (type);
		if (err != MPI_SUCCESS) {
			MPI_WRAPPER_LEAVE (
			        h5_error (
			                H5_ERR_MPI,
			                "Cannot commit MPI datatype"));
		}
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}

h5_err_t
h5priv_mpi_type_free (
        MPI_Datatype *type
        );


//
h5_err_t
h5priv_mpi_cart_create (
        MPI_Comm old_comm,
        int ndims,
        int *dims,
        int *period,
        int reorder,
        MPI_Comm *new_comm
        );

h5_err_t
h5priv_mpi_cart_coords (
        MPI_Comm comm,
        int rank,
        int maxdim,
        int *coords
        );


#endif
#endif
