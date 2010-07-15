#include "h5core/h5_core.h"
#include "h5_core_private.h"

#ifdef PARALLEL_IO

h5_err_t
h5priv_mpi_recv(
	h5_file_t *const f,
	void* buf,
	const int count,
	const MPI_Datatype type,
	const int from,
	const int tag,
	const MPI_Comm comm
	) {
	int err = MPI_Recv(
		buf,
		count,
		type,
		from,
		tag,
		comm,
		MPI_STATUS_IGNORE
		);
	if (err != MPI_SUCCESS)
		return h5_error (f, H5_ERR_MPI, "Cannot receive data");
	return H5_SUCCESS;
}

h5_err_t
h5priv_mpi_send(
	h5_file_t *f,
	void* buf,
	const int count,
	const MPI_Datatype type,
	const int to,
	const int tag,
	const MPI_Comm comm
	) {
	int err = MPI_Send(
		buf,
		count,
		type,
		to,
		tag,
		comm
		);
	if (err != MPI_SUCCESS)
		return h5_error (f, H5_ERR_MPI, "Cannot send data");
	return H5_SUCCESS;
}

h5_err_t
h5priv_mpi_bcast (
	h5_file_t* const f,
	void* buf,
	const int count,
	const MPI_Datatype type,
	const int root,
	const MPI_Comm comm
	) {
	int err = MPI_Bcast(
		buf,
		count,
		type,
		root,
		comm
		);
	if (err != MPI_SUCCESS)
		return h5_error (f, H5_ERR_MPI, "Cannot perform broadcast");
	return H5_SUCCESS;
}



h5_err_t
h5priv_mpi_sum (
	h5_file_t* const f,
	void* sendbuf,
	void* recvbuf,
	const int count,
	const MPI_Datatype type,
	const MPI_Comm comm
	) {
	int err = MPI_Allreduce(
		sendbuf,
		recvbuf,
		count,
		type,
		MPI_SUM,
		comm
		);
	if (err != MPI_SUCCESS)
		return h5_error (f, H5_ERR_MPI, "Cannot perform sum reduction");
	return H5_SUCCESS;
}

h5_err_t
h5priv_mpi_prefix_sum (
	h5_file_t* const f,
	void* sendbuf,
	void* recvbuf,
	const int count,
	const MPI_Datatype type,
	const MPI_Comm comm
	) {
	int err = MPI_Scan(
		sendbuf,
		recvbuf,
		count,
		type,
		MPI_SUM,
		comm
		);
	if (err != MPI_SUCCESS)
		return h5_error (f, H5_ERR_MPI, "Cannot perform prefix sum");
	return H5_SUCCESS;
}

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
		return h5_error (f, H5_ERR_MPI, "Cannot gather data");
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
		return h5_error (f, H5_ERR_MPI, "Cannot get communicator size");
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
		return h5_error (f, H5_ERR_MPI, "Cannot get this task's rank");
	return H5_SUCCESS;
}

h5_err_t
h5priv_mpi_type_contiguous (
	h5_file_t* const f,
	const size_t nelems,
	const MPI_Datatype oldtype,
	MPI_Datatype *const newtype
	) {
	int err;
	err = MPI_Type_contiguous ( nelems, oldtype, newtype );
	if (err != MPI_SUCCESS)
		return h5_error (f, H5_ERR_MPI, "Cannot create new MPI type");
	err = MPI_Type_commit ( newtype );
	if (err != MPI_SUCCESS)
		return h5_error (f, H5_ERR_MPI, "Cannot commit new MPI type");
	return H5_SUCCESS;
}

h5_err_t
h5priv_mpi_type_free (
	h5_file_t* const f,
	MPI_Datatype *type
	) {
	int err = MPI_Type_free ( type );
	if (err != MPI_SUCCESS)
		return h5_error (f, H5_ERR_MPI, "Cannot free MPI type");
	return H5_SUCCESS;
}

#endif // PARALLEL_IO

