#ifndef __H5_MPI_PRIVATE_H
#define __H5_MPI_PRIVATE_H

#ifdef PARALLEL_IO
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
#endif
#endif
