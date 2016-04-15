/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "h5core/h5.h"
#include "h5_types_private.h"
#include "h5_hdf5_private.h"
#include "h5_model_private.h"
#include "h5_readwrite_private.h"
#include "h5u_types_private.h"

#include "h5core/h5.h"
#include "h5core/h5_model.h"
#include "h5core/h5_syscall.h"

/*!
  \ingroup h5_private

  \internal

  Initialize unstructured data internal data structure.

  TODO: Move to file "h5u_openclose.c"

  \return	H5_SUCCESS or error code
*/
h5_err_t
h5upriv_open_file (
	const h5_file_p f		/*!< IN: file handle */
	) {
	H5_PRIV_API_ENTER (h5_err_t, "f=%p", f);
	TRY (f->u = (h5u_fdata_t*)h5_calloc (1, sizeof (*f->u)));
	h5u_fdata_t *u = f->u;

        u->shape = -1;
	u->diskshape = H5S_ALL;
	u->memshape = H5S_ALL;
	u->viewstart = -1;
	u->viewend = -1;
	u->viewindexed = 0;

	TRY (u->dcreate_prop = hdf5_create_property (H5P_DATASET_CREATE));

	H5_PRIV_API_RETURN (H5_SUCCESS);
}

/*!
  \ingroup h5_private

  \internal

  De-initialize H5Block internal structure.  Open HDF5 objects are 
  closed and allocated memory freed.

  \return	H5_SUCCESS or error code
*/
h5_err_t
h5upriv_close_file (
	const h5_file_p f	/*!< file handle */
	) {
	H5_PRIV_API_ENTER (h5_err_t, "f=%p", f);
	struct h5u_fdata* u = f->u;

	h5_errno = H5_SUCCESS;
	TRY (hdf5_close_dataspace (u->shape));
	TRY (hdf5_close_dataspace (u->diskshape));
	TRY (hdf5_close_dataspace (u->memshape));
	TRY (hdf5_close_property (u->dcreate_prop));
	TRY (h5_free (f->u));
	f->u = NULL;

	H5_PRIV_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5u_read_data (
	const h5_file_t fh,	/*!< [in] Handle to open file */
	const char* name,	/*!< [in] Name to associate dataset with */
	void* data,		/*!< [out] Array of data */
	const hid_t type
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t,
			   "f=%p, name='%s', data=%p, type=%lld",
	                   f, name, data, (long long int)type);
	CHECK_TIMEGROUP (f);

	struct h5u_fdata *u = f->u;
	hid_t dataset_id;
	hid_t space_id;
	hid_t memspace_id;
	hsize_t ndisk, nread, nmem;

	if ( f->step_gid < 0 ) {
		TRY (h5_set_step ((h5_file_t)f, f->step_idx));
	}

	char name2[H5_DATANAME_LEN];
	TRY (h5priv_normalize_dataset_name (name, name2));

	TRY (dataset_id = hdf5_open_dataset (f->step_gid, name2));

	/* default spaces, if not using a view selection */
	memspace_id = H5S_ALL;
	TRY (space_id = hdf5_get_dataset_space (dataset_id));

	/* get the number of elements on disk for the datset */
	TRY (ndisk = hdf5_get_npoints_of_dataspace (space_id));

	if (u->diskshape != H5S_ALL) {
		TRY (nread = hdf5_get_selected_npoints_of_dataspace (u->diskshape));

		/* make sure the disk space selected by the view doesn't
		 * exceed the size of the dataset */
		if (nread <= ndisk) {
			/* we no longer need the dataset space... */
			TRY (hdf5_close_dataspace(space_id));
			/* ...because it's safe to use the view selection */
			space_id = f->u->diskshape;
		} else {
			/* the view selection is too big?
			 * fall back to using the dataset space */
			h5_warn (
			        "Ignoring view: dataset[%s] has fewer "
			        "elements on disk (%lld) than are selected "
			        "(%lld).",
			        name2, (long long)ndisk, (long long)nread );
			nread = ndisk;
		}
	} else {
		/* since the view selection is H5S_ALL, we will
		 * read all available elements in the dataset space */
		nread = ndisk;
	}

	if (u->memshape != H5S_ALL) {
		TRY (nmem = hdf5_get_npoints_of_dataspace (u->memshape));

		/* make sure the memory space selected by the view has
		 * enough capacity for the read */
		if (nmem >= nread) {
			memspace_id = f->u->memshape;
		} else {
			/* the view selection is too small?
			 * fall back to using H5S_ALL */
			h5_warn (
			        "Ignoring view: dataset[%s] has more "
			        "elements selected (%lld) than are available "
			        "in memory (%lld).",
			        name2, (long long)nread, (long long)nmem );
			memspace_id = H5S_ALL;
		}
	}
	TRY (h5priv_start_throttle (f));
	TRY (hdf5_read_dataset (
	             dataset_id,
	             type,
	             memspace_id,
	             space_id,
	             f->props->xfer_prop,
	             data ));
	TRY (h5priv_end_throttle (f));
	if (space_id != f->u->diskshape) {
		TRY (hdf5_close_dataspace (space_id));
	}

	TRY (hdf5_close_dataset (dataset_id));

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5u_write_data (
	const h5_file_t fh,	/*!< IN: Handle to open file */
	const char *name,	/*!< IN: Name to associate array with */
	const void *data,	/*!< IN: Array to commit to disk */
	const hid_t type	/*!< IN: Type of data */
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t,
			   "f=%p, name='%s', data=%p, type=%lld",
	                   f, name, data, (long long int)type);
	CHECK_TIMEGROUP( f );
	CHECK_WRITABLE_MODE( f );

	struct h5u_fdata *u = f->u;
	hid_t dset_id;

	char name2[H5_DATANAME_LEN];
	TRY (h5priv_normalize_dataset_name (name, name2));

	if ( u->shape == H5S_ALL )
		h5_warn("The view is unset or invalid.");

	/* test for existing dataset */
	H5E_BEGIN_TRY
	        dset_id = H5Dopen(f->step_gid, name2, H5P_DEFAULT);
	H5E_END_TRY

	if (dset_id > 0) {
		h5_warn("Dataset %s/%s already exists",
		        hdf5_get_objname(f->step_gid), name2);
	} else {
		TRY (dset_id = hdf5_create_dataset (
		             f->step_gid,
		             name2,
		             type,
		             u->shape,
		             H5P_DEFAULT));
	}

	TRY (h5priv_start_throttle (f));
	h5_info ("Writing dataset %s/%s.",
	         hdf5_get_objname(f->step_gid), name2);
	TRY (hdf5_write_dataset (
	             dset_id,
	             type,
	             u->memshape,
	             u->diskshape,
	             f->props->xfer_prop,
	             data));
	TRY (h5priv_end_throttle (f));
	if (f->props->flags & H5_FLUSH_STEP)
		TRY (hdf5_flush (f->step_gid, H5F_SCOPE_LOCAL));

	TRY (hdf5_close_dataset (dset_id));

	f->empty = 0;
	TRY (hdf5_flush (f->step_gid, H5F_SCOPE_LOCAL));

	H5_CORE_API_RETURN (H5_SUCCESS);
}

