#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

#ifdef PARALLEL_IO
h5_err_t
h5priv_mpi_allgather (
	h5_file_t * const f,
	void * sendbuf,
	const int sendcount,
	const MPI_Datatype sendtype,
	void * recvbuf,
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
		comm );
	if ( err != MPI_SUCCESS )
		return h5_error (
			f,
			H5_ERR_MPI,
			"Cannot gather data." );
	return H5_SUCCESS;
}

h5_err_t
h5priv_mpi_comm_size (
	h5_file_t * const f,
	MPI_Comm comm,
	int *size
	) {
	int err = MPI_Comm_size ( comm, size );
	if ( err != MPI_SUCCESS )
		h5_error (
			f,
			H5_ERR_MPI,
			"Cannot get number of processes in my group." );

	return H5_SUCCESS;
}


h5_err_t
h5priv_mpi_comm_rank (
	h5_file_t * const f,
	MPI_Comm comm,
	int *rank
	) {
	int err = MPI_Comm_rank ( comm, rank );
	if ( err != MPI_SUCCESS )
		h5_error (
			f,
			H5_ERR_MPI,
			"Cannot get rank of the calling process in my group." );

	return H5_SUCCESS;
}
#endif
