/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5_MODEL_PRIVATE_H
#define __H5_MODEL_PRIVATE_H


/* WARNING! Changing these values will alter the data model and introduce
 * file incompatibilities with previous versions. */

#define H5_DATANAME_LEN		H5_MAX_NAME_LEN
#define H5_STEPNAME_LEN		H5_MAX_NAME_LEN
#define H5_STEPNAME		"Step"
#define H5_STEPWIDTH		1
#define H5BLOCK_GROUPNAME_BLOCK	"Block"
#define H5_BLOCKNAME_X		"0"
#define H5_BLOCKNAME_Y		"1"
#define H5_BLOCKNAME_Z		"2"
#define H5_ATTACHMENT		"Attachment"

#include "h5core/h5_types.h"
#include "h5core/h5_model.h"
#include "h5_mpi_private.h"

#ifdef PARALLEL_IO
static inline h5_err_t
h5priv_start_throttle (
	const h5_file_p f
	) {
	H5_CORE_API_ENTER (h5_err_t, "f=%p", f);
	if (f->props->throttle > 0) {
		// throttle only if VFD is MPIO independent
		h5_int64_t mask = H5_VFD_MPIO_INDEPENDENT;
#if H5_VERSION_LE(1,8,12)
		//  or MPI POSIX - which has been removed in newer versions of hdf5
		mask |= H5_VFD_MPIO_POSIX;
#endif
		if (! (f->props->flags & mask)) {
			h5_warn (
				"Throttling is only permitted with the MPI-POSIX "
				"or MPI-IO Independent VFD." );
			H5_CORE_API_LEAVE (H5_SUCCESS);
		}

		int token = 1;
		h5_info (
			"Throttling with factor = %lld",
			(long long int)f->props->throttle);
		if (f->myproc / f->props->throttle > 0) {
			h5_debug (
				"throttle: waiting on token from %lld",
				(long long int)(f->myproc - f->props->throttle));
			// wait to receive token before continuing with read
			TRY( h5priv_mpi_recv(
				     &token, 1, MPI_INT,
				     f->myproc - f->props->throttle, // receive from previous proc
				     f->myproc, // use this proc id as message tag
				     f->props->comm
				     ) );
		}
		h5_debug ("throttle: received token");
	}
	H5_CORE_API_RETURN (H5_SUCCESS);
}

static inline h5_err_t
h5priv_end_throttle (
	const h5_file_p f
	) {
	H5_PRIV_API_ENTER (h5_err_t, "f=%p", f);
	if (f->props->throttle > 0) {
		int token;
		if (f->myproc + f->props->throttle < f->nprocs) {
			// pass token to next proc 
			h5_debug (
				"throttle: passing token to %lld",
				(long long int)(f->myproc + f->props->throttle));
			TRY (h5priv_mpi_send(
				     &token, 1, MPI_INT,
				     f->myproc + f->props->throttle, // send to next proc
				     f->myproc + f->props->throttle, // use the id of the target as tag
				     f->props->comm
				     ));
		}
	}
	H5_PRIV_API_RETURN (H5_SUCCESS);
}
#else // PARALLEL_IO
static inline h5_err_t
h5priv_start_throttle (const h5_file_p f) {
	UNUSED_ARGUMENT (f);
	return H5_SUCCESS;
}
static inline
h5priv_end_throttle (const h5_file_p f)
	UNUSED_ARGUMENT (f);
	return H5_SUCCESS;
}

#endif // PARALLEL_IO


h5_err_t
h5priv_close_step (
	const h5_file_p f
	);

#endif
