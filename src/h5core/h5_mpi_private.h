#ifndef __H5_MPI_PRIVATE_H
#define __H5_MPI_PRIVATE_H

#ifdef PARALLEL_IO
h5_err_t
h5priv_mpi_recv(
	h5_file_t *f,
	void* buf,
	const int count,
	const MPI_Datatype type,
	const int from,
	const int tag,
	const MPI_Comm comm
	);

h5_err_t
h5priv_mpi_send(
	h5_file_t *f,
	void* buf,
	const int count,
	const MPI_Datatype type,
	const int to,
	const int tag,
	const MPI_Comm comm
	);

h5_err_t
h5priv_mpi_bcast (
	h5_file_t* const f,
	void* buf,
	const int count,
	const MPI_Datatype type,
	const int root,
	const MPI_Comm comm
	);

h5_err_t
h5priv_mpi_sum (
	h5_file_t* const f,
	void* sendbuf,
        void* recvbuf,
	const int count,
	const MPI_Datatype type,
	const MPI_Comm comm
	);

h5_err_t
h5priv_mpi_prefix_sum (
	h5_file_t* const f,
	void* sendbuf,
        void* recvbuf,
	const int count,
	const MPI_Datatype type,
	const MPI_Comm comm
	);

h5_err_t
h5priv_mpi_allgather (
	h5_file_t* const f,
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
	h5_file_t* const f,
	MPI_Comm comm,
	int* size
	);

h5_err_t
h5priv_mpi_comm_rank (
	h5_file_t* const f,
	MPI_Comm comm,
	int* rank
	);

h5_err_t
h5priv_mpi_type_contiguous (
	h5_file_t* const f,
	const size_t nelems,
	const MPI_Datatype oldtype,
	MPI_Datatype *newtype
	);

h5_err_t
h5priv_mpi_type_free (
	h5_file_t* const f,
	MPI_Datatype *type
	);

h5_err_t
h5priv_mpi_cart_create (
	h5_file_t* const f,
	MPI_Comm old_comm,
	int ndims,
	int *dims,
	int *period,
	int reorder,
	MPI_Comm *new_comm
	);

h5_err_t
h5priv_mpi_cart_coords (
	h5_file_t* const f,
	MPI_Comm comm,
	int rank,
	int maxdim,
	int *coords
	);

#endif
#endif
