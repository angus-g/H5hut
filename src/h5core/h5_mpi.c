#include "h5core/h5_core.h"
#include "h5_core_private.h"

#ifdef PARALLEL_IO

h5_err_t
h5priv_mpi_recv(
	void* buf,
	const int count,
	const MPI_Datatype type,
	const int from,
	const int tag,
	const MPI_Comm comm
	) {
	MPI_WRAPPER_ENTER (h5_err_t,
			   "buf=%p, count=%d, type=?, from=%d, tag=%d, comm=?",
			   buf, count, from, tag);
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
		MPI_WRAPPER_LEAVE (h5_error (H5_ERR_MPI, "Cannot receive data"));
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}

h5_err_t
h5priv_mpi_send(
	void* buf,
	const int count,
	const MPI_Datatype type,
	const int to,
	const int tag,
	const MPI_Comm comm
	) {
	MPI_WRAPPER_ENTER (h5_err_t,
			   "buf=%p, count=%d, type=?, to=%d, tag=%d, comm=?",
			   buf, count, to, tag);
	int err = MPI_Send(
		buf,
		count,
		type,
		to,
		tag,
		comm
		);
	if (err != MPI_SUCCESS)
		MPI_WRAPPER_LEAVE (h5_error (H5_ERR_MPI, "Cannot send data"));
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}

h5_err_t
h5priv_mpi_bcast (
	void* buf,
	const int count,
	const MPI_Datatype type,
	const int root,
	const MPI_Comm comm
	) {
	MPI_WRAPPER_ENTER (h5_err_t,
			   "buf=%p, count=%d, type=?, root=%d, comm=?",
			   buf, count, root);
	int err = MPI_Bcast(
		buf,
		count,
		type,
		root,
		comm
		);
	if (err != MPI_SUCCESS)
		MPI_WRAPPER_LEAVE (h5_error (H5_ERR_MPI, "Cannot perform broadcast"));
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}



h5_err_t
h5priv_mpi_sum (
	void* sendbuf,
	void* recvbuf,
	const int count,
	const MPI_Datatype type,
	const MPI_Comm comm
	) {
	MPI_WRAPPER_ENTER (h5_err_t,
			   "sendbuf=%p, recvbuf=%p, count=%d, type=?, comm=?",
			   sendbuf, recvbuf, count);
	int err = MPI_Allreduce(
		sendbuf,
		recvbuf,
		count,
		type,
		MPI_SUM,
		comm
		);
	if (err != MPI_SUCCESS)
		MPI_WRAPPER_LEAVE (h5_error (H5_ERR_MPI, "Cannot perform sum reduction"));
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}

h5_err_t
h5priv_mpi_prefix_sum (
	void* sendbuf,
	void* recvbuf,
	const int count,
	const MPI_Datatype type,
	const MPI_Comm comm
	) {
	MPI_WRAPPER_ENTER (h5_err_t,
			   "sendbuf=%p, recvbuf=%p, count=%d, type=?, comm=?",
			    sendbuf, recvbuf, count);
	int err = MPI_Scan(
		sendbuf,
		recvbuf,
		count,
		type,
		MPI_SUM,
		comm
		);
	if (err != MPI_SUCCESS)
		MPI_WRAPPER_LEAVE (h5_error (H5_ERR_MPI, "Cannot perform prefix sum"));
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}

h5_err_t
h5priv_mpi_allgather (
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

h5_err_t
h5priv_mpi_comm_size (
	MPI_Comm comm,
	int* size
	) {
	MPI_WRAPPER_ENTER (h5_err_t, "comm=?, size=%p", size);
	int err = MPI_Comm_size (comm, size);
	if (err != MPI_SUCCESS)
		MPI_WRAPPER_LEAVE (h5_error (H5_ERR_MPI, "Cannot get communicator size"));
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}


h5_err_t
h5priv_mpi_comm_rank (
	MPI_Comm comm,
	int* rank
	) {
	MPI_WRAPPER_ENTER (h5_err_t, "comm=?, rank=%p", rank);
	int err = MPI_Comm_rank (comm, rank);
	if (err != MPI_SUCCESS)
		MPI_WRAPPER_LEAVE (h5_error (H5_ERR_MPI, "Cannot get this task's rank"));
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}

h5_err_t
h5priv_mpi_type_contiguous (
	const size_t nelems,
	const MPI_Datatype oldtype,
	MPI_Datatype *const newtype
	) {
	MPI_WRAPPER_ENTER (h5_err_t, "nelems=%lu, oldtype=?, newtype=?", (long unsigned)nelems);
	int err;
	err = MPI_Type_contiguous ( nelems, oldtype, newtype );
	if (err != MPI_SUCCESS)
		MPI_WRAPPER_LEAVE (h5_error (H5_ERR_MPI, "Cannot create new MPI type"));
	err = MPI_Type_commit ( newtype );
	if (err != MPI_SUCCESS)
		MPI_WRAPPER_LEAVE (h5_error (H5_ERR_MPI, "Cannot commit new MPI type"));
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}

h5_err_t
h5priv_mpi_type_free (
	MPI_Datatype *type
	) {
	MPI_WRAPPER_ENTER (h5_err_t, "type=%p", type);
	int err = MPI_Type_free( type );
	if (err != MPI_SUCCESS)
		MPI_WRAPPER_LEAVE (h5_error(H5_ERR_MPI, "Cannot free MPI type"));
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}

h5_err_t
h5priv_mpi_cart_create (
	MPI_Comm old_comm,
	int ndims,
	int *dims,
	int *period,
	int reorder,
	MPI_Comm *new_comm
	) {
	MPI_WRAPPER_ENTER (h5_err_t, "old_comm=?, ndims=%d, dims=%p, period=%p, "
			   "reorder=%d, new_comm=%p",
			   ndims, dims, period, reorder, new_comm);
	int err = MPI_Cart_create(
		old_comm, ndims, dims, period, reorder, new_comm);
	if (err != MPI_SUCCESS)
		MPI_WRAPPER_LEAVE (h5_error(H5_ERR_MPI, "Cannot create cartesian grid"));
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}

h5_err_t
h5priv_mpi_cart_coords (
	MPI_Comm comm,
	int rank,
	int maxdim,
	int *coords
	) {
	MPI_WRAPPER_ENTER (h5_err_t, "comm=?, rank=%d, maxdim=%d, coords=%p",
			   rank, maxdim, coords);
	int err = MPI_Cart_coords( comm, rank, maxdim, coords );
	if (err != MPI_SUCCESS)
		MPI_WRAPPER_LEAVE (h5_error(H5_ERR_MPI, "Cannot create cartesian grid"));
	MPI_WRAPPER_RETURN (H5_SUCCESS);
}

#endif // PARALLEL_IO

