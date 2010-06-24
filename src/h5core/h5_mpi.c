#include "h5core/h5_core.h"
#include "h5_core_private.h"

#ifdef PARALLEL_IO

#define ERR_GATHER "Cannot gather data."
#define ERR_COMM_SIZE "Cannot get number of processes in my group."
#define ERR_COMM_RANK "Cannot get rank of the calling process."

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
	) {
	int err = MPI_Allgather (
		sendbuf,
		sendcount,
		sendtype,
		recvbuf,
		recvcount,
		recvtype,
		comm);
	if (err != MPI_SUCCESS)
		return h5_error (f, H5_ERR_MPI, ERR_GATHER);
	return H5_SUCCESS;
}

h5_err_t
h5priv_mpi_comm_size (
	h5_file_t* const f,
	MPI_Comm comm,
	int* size
	) {
	int err = MPI_Comm_size (comm, size);
	if (err != MPI_SUCCESS)
		return h5_error (f, H5_ERR_MPI, ERR_COMM_SIZE);
	return H5_SUCCESS;
}


h5_err_t
h5priv_mpi_comm_rank (
	h5_file_t* const f,
	MPI_Comm comm,
	int* rank
	) {
	int err = MPI_Comm_rank (comm, rank);
	if (err != MPI_SUCCESS)
		return h5_error (f, H5_ERR_MPI, ERR_COMM_RANK);
	return H5_SUCCESS;
}
#endif
