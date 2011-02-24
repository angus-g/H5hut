#ifndef __H5_MPI_PRIVATE_H
#define __H5_MPI_PRIVATE_H

#ifdef PARALLEL_IO
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

h5_err_t
h5priv_mpi_allgather (
	void* sendbuf,
	const int sendcount,
	const MPI_Datatype sendtype,
	void* recvbuf,
	const int recvcount,
	const MPI_Datatype recvtype,
	const MPI_Comm comm
	);

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

h5_err_t
h5priv_mpi_type_contiguous (
	const size_t nelems,
	const MPI_Datatype oldtype,
	MPI_Datatype *newtype
	);

h5_err_t
h5priv_mpi_type_free (
	MPI_Datatype *type
	);

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
