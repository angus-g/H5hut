#include "h5core/h5_core.h"
#include "h5_core_private.h"

h5_err_t
h5u_read_data (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate dataset with */
	void *data,		/*!< [out] Array of data */
	const hid_t type
	) {
	H5_CORE_API_ENTER (h5_err_t);
	CHECK_TIMEGROUP (f);

	struct h5u_fdata *u = f->u;
	hid_t dataset_id;
	hid_t space_id;
	hid_t memspace_id;
	hsize_t ndisk, nread, nmem;

	if ( f->step_gid < 0 ) {
		TRY (h5_set_step (f, f->step_idx));
	}

	char name2[H5_DATANAME_LEN];
	TRY (h5_normalize_dataset_name (name, name2));

	TRY (dataset_id = hdf5_open_dataset (f->step_gid, name2));

	/* default spaces, if not using a view selection */
	memspace_id = H5S_ALL;
	TRY (space_id = hdf5_get_dataset_space (dataset_id));

	/* get the number of elements on disk for the datset */
	TRY (ndisk = hdf5_get_npoints_of_dataspace (space_id));

	if (u->diskshape != H5S_ALL) {
		TRY (nread = hdf5_get_npoints_of_dataspace(u->diskshape));

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
#ifdef PARALLEL_IO
	TRY (h5_start_throttle (f));
#endif
	TRY (hdf5_read_dataset (
		     dataset_id,
		     type,
		     memspace_id,
		     space_id,
		     f->xfer_prop,
		     data ));
#ifdef PARALLEL_IO
	TRY (h5_end_throttle (f));
#endif
	if (space_id != f->u->diskshape) {
		TRY (hdf5_close_dataspace (space_id));
	}

	TRY (hdf5_close_dataset (dataset_id));
	
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5u_write_data (
	h5_file_t *f,		/*!< IN: Handle to open file */
	const char *name,	/*!< IN: Name to associate array with */
	const void *data,	/*!< IN: Array to commit to disk */
	const hid_t type	/*!< IN: Type of data */
	) {
	H5_CORE_API_ENTER (h5_err_t);
	CHECK_TIMEGROUP( f );
	CHECK_WRITABLE_MODE( f );

	struct h5u_fdata *u = f->u;
	hid_t dset_id;

	char name2[H5_DATANAME_LEN];
	TRY (h5_normalize_dataset_name (name, name2));

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

#ifdef PARALLEL_IO
	TRY (h5_start_throttle (f));
#endif
	h5_info ("Writing dataset %s/%s.",
		hdf5_get_objname(f->step_gid), name2); 
	TRY (hdf5_write_dataset (
		     dset_id,
		     type,
		     u->memshape,
		     u->diskshape,
		     f->xfer_prop,
		     data));
#ifdef PARALLEL_IO
	TRY (h5_end_throttle (f));
#endif
	TRY (hdf5_close_dataset (dset_id));

	f->empty = 0;

	H5_CORE_API_RETURN (H5_SUCCESS);
}

