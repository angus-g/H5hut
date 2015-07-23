/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "h5core/h5.h"
#include "h5_private.h"
#include "h5_hdf5_private.h"

#include "h5_model_private.h"
#include "h5_mpi_private.h"
#include "h5_readwrite_private.h"
#include "h5u_types_private.h"
#include "h5core/h5u_model.h"

h5_ssize_t
h5u_get_num_particles (
	const h5_file_t fh      /*!< [in]  Handle to open file */
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_ssize_t, "f=%p", f);
	h5_ssize_t nparticles;

	if (h5u_has_view ((h5_file_t)f)) {
                /* if a view exists, use its size as the number of particles */
		TRY (nparticles = h5u_get_num_particles_in_view (fh));
	} else {
		/* otherwise, report all particles on disk in the first dataset
                   for this timestep */
                TRY (nparticles = h5u_get_totalnum_particles_by_idx (fh, 0));
	}

	H5_CORE_API_RETURN (nparticles);
}

h5_ssize_t
h5u_get_num_particles_in_view (
	const h5_file_t fh      /*!< [in]  Handle to open file */
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_ssize_t, "f=%p", f);
	h5_ssize_t nparticles;

	if (!h5u_has_view (fh)) {
                H5_CORE_API_LEAVE (
                        h5_error (
                                H5_ERR_H5PART,
                                "No view has been set."));
        }
        TRY (nparticles = hdf5_get_selected_npoints_of_dataspace(f->u->diskshape));
        h5_debug ("Found %lld particles in view.", (long long)nparticles );
	H5_CORE_API_RETURN (nparticles);
}

h5_ssize_t
h5u_get_totalnum_particles_by_name (
	const h5_file_t fh,     ///< [in] Handle to open file
        const char* const name  ///< [in] Index of dataset to query
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_ssize_t, "f=%p, name=%s", f, name);
	h5_ssize_t nparticles;

        TRY (nparticles = hdf5_get_npoints_of_dataset_by_name (f->step_gid, name));
        h5_debug ("Found %lld particles in dataset %s.", (long long)nparticles, name);
	H5_CORE_API_RETURN (nparticles);
}

h5_ssize_t
h5u_get_totalnum_particles_by_idx (
	const h5_file_t fh,     ///< [in] Handle to open file
        h5_id_t idx             ///< [in] Index of dataset to query
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_ssize_t, "f=%p, idx=%lld", f, (long long)idx);
        char dataset_name[H5_DATANAME_LEN];
        dataset_name[0] = '\0';
        TRY (hdf5_get_name_of_dataset_by_idx (
                    f->step_gid,
                    idx,
                    dataset_name,
                    H5_DATANAME_LEN));
        H5_CORE_API_RETURN (h5u_get_totalnum_particles_by_name (fh, dataset_name));
}

h5_err_t
h5u_set_num_particles (
	const h5_file_t fh,		/*!< [in] Handle to open file */
	const h5_size_t nparticles,	/*!< [in] Number of particles */
	const h5_size_t stride		/*!< [in] Stride of particles in memory */
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_ssize_t,
	                   "f=%p, nparticles=%llu, stride=%llu",
	                   f, (long long unsigned)nparticles,
	                   (long long unsigned)stride);
	struct h5u_fdata *u = f->u;
	hsize_t start;
	hsize_t dmax = H5S_UNLIMITED;

	if (nparticles < 0)
		H5_CORE_API_LEAVE (
		        h5_error(
		                H5_ERR_INVAL,
		                "Invalid number particles: %lld!\n",
		                (long long)nparticles));

#ifndef PARALLEL_IO
	/*
	   if we are not using parallel-IO, there is enough information
	   to know that we can short circuit this routine.  However,
	   for parallel IO, this is going to cause problems because
	   we don't know if things have changed globally
	 */
	if ( u->nparticles == nparticles && stride == 1 ) {
		H5_CORE_API_LEAVE (H5_SUCCESS);
	}
#endif

	TRY (h5u_reset_view(fh));

	TRY (hdf5_close_dataspace (u->shape));
	u->shape = H5S_ALL;

	u->nparticles = (hsize_t)nparticles;

	/* declare local memory datasize with striding */
	hsize_t count = u->nparticles * stride;
	TRY (u->memshape = hdf5_create_dataspace (1, &count, &dmax));

	/* we need a hyperslab selection if there is striding
	 * (otherwise, the default H5S_ALL selection is ok)
	 */
	if (stride > 1) {
		h5_debug ("Striding by %lld elements.", (long long)stride);
		start = 0;
                hsize_t hstride = (hsize_t)stride;
		count = u->nparticles;
		TRY (hdf5_select_hyperslab_of_dataspace (
                             u->memshape,
                             H5S_SELECT_SET,
                             &start, &hstride, &count,
                             NULL));
	}

#ifndef PARALLEL_IO
	count = u->nparticles;
	TRY( u->shape = hdf5_create_dataspace (1, &count, NULL));
	u->viewstart = 0;
	u->viewend   = nparticles - 1; // view range is *inclusive*
#else /* PARALLEL_IO */
	/*
	 The Gameplan here is to declare the overall size of the on-disk
	 data structure the same way we do for the serial case.  But
	 then we must have additional "DataSpace" structures to define
	 our in-memory layout of our domain-decomposed portion of the particle
	 list as well as a "selection" of a subset of the on-disk
	 data layout that will be written in parallel to mutually exclusive
	 regions by all of the processors during a parallel I/O operation.
	 These are f->shape, f->memshape and f->diskshape respectively.
	 */

	/*
	   acquire the number of particles to be written from each MPI process
	 */
	hsize_t total;
	TRY( h5priv_mpi_sum(
	             &(u->nparticles), &total, 1, MPI_LONG_LONG, f->props->comm ) );
	TRY( h5priv_mpi_prefix_sum(
	             &(u->nparticles), &start, 1, MPI_LONG_LONG, f->props->comm ) );
	start -= u->nparticles;

	h5_debug("Total particles across all processors: %lld.", (long long)total);

	u->viewstart = start;
	u->viewend   = start + u->nparticles - 1; // view range is *inclusive*

	/* declare overall datasize */
	count = total;
	TRY( u->shape = hdf5_create_dataspace(1, &count, NULL) );

	/* declare overall data size  but then will select a subset */
	TRY( u->diskshape = hdf5_create_dataspace(1, &count, NULL) );

	count = nparticles;
	if (count > 0) {
                hsize_t hstride = 1;
		TRY( hdf5_select_hyperslab_of_dataspace(
		             u->diskshape,
		             H5S_SELECT_SET,
		             &start, &hstride, &count,
		             NULL) );
	} else {
		TRY (hdf5_select_none (u->diskshape));
	}
#endif
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5u_has_view (
	const const h5_file_t fh
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_ssize_t, "f=%p", f);
	H5_CORE_API_RETURN (f->u->viewindexed || f->u->viewstart >= 0);
}

h5_err_t
h5u_reset_view (
	const h5_file_t fh
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_ssize_t, "f=%p", f);
	struct h5u_fdata *u = f->u;

	u->viewstart = -1;
	u->viewend = -1;
	u->viewindexed = 0;
	TRY (hdf5_close_dataspace (u->diskshape));
	u->diskshape = H5S_ALL;
	TRY (hdf5_close_dataspace (u->memshape));
	u->memshape = H5S_ALL;

	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*
  if start == -1 && end == -1 -> reset view
  elif end == -1 -> select to end

 */
h5_err_t
h5u_set_view (
	const h5_file_t fh,		///!< [in]  Handle to open file
	h5_int64_t start,		///!< [in]  Start particle
	h5_int64_t end	        	///!< [in]  End particle
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t,
	                   "f=%p, start=%lld, end=%lld",
	                   f, (long long)start, (long long)end);
	hsize_t total = 0;
	hsize_t dmax = H5S_UNLIMITED;
	struct h5u_fdata *u = f->u;

	TRY (h5u_reset_view (fh));

	if (start == -1 && end == -1)   // we are already done
		H5_CORE_API_LEAVE (H5_SUCCESS);

	if (f->u->shape > 0) {
		TRY (total = hdf5_get_npoints_of_dataspace (f->u->shape) );
        } else {
                TRY (total = (hsize_t)h5u_get_totalnum_particles_by_idx (fh,0));
        }
	if (total == 0) {
		/* No datasets have been created yet and no views are set.
		 * We have to leave the view empty because we don't know how
		 * many particles there should be! */
		H5_CORE_API_LEAVE (H5_SUCCESS);
	}
	if (end < 0) {
		end = total+end;
	}
	
        if (start < 0 || start >= total) {
		H5_CORE_API_LEAVE (
		        h5_error(
		                H5_ERR_INVAL,
		                "Start of selection out of range: %lld not in [0..%lld]",
		                (long long)start, (long long)total-1));
        } else if (end < 0 || end >= total) {
		H5_CORE_API_LEAVE (
		        h5_error(
		                H5_ERR_INVAL,
		                "End of selection out of range: %lld not in [0..%lld]",
		                (long long)end, (long long)total-1));
        } else if (end+1 < start) {
		H5_CORE_API_LEAVE (
		        h5_error(
		                H5_ERR_INVAL,
		                "Invalid selection: start=%lld > end=%lld!\n",
		                (long long)start, (long long)end));
        }
	/* setting up the new view */
	u->viewstart =  start;
	u->viewend =    end;
        if (end == -1)
                u->nparticles = 0;
        else
                u->nparticles = end - start + 1;

	h5_debug (
	        "This view includes %lld particles.",
	        (long long)u->nparticles );

	/* declare overall data size  but then will select a subset */
	TRY (u->diskshape = hdf5_create_dataspace ( 1, &total, NULL ));

	hsize_t hstart = (hsize_t)start;
	hsize_t hstride = 1;
	hsize_t hcount = (hsize_t)u->nparticles;

	TRY (hdf5_select_hyperslab_of_dataspace (
	             u->diskshape,
	             H5S_SELECT_SET,
	             &hstart, &hstride, &hcount,
	             NULL));

	/* declare local memory datasize */
	TRY (u->memshape = hdf5_create_dataspace (1, &hcount, &dmax));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5u_set_view_length (
	const h5_file_t fh,		///!< [in]  Handle to open file
	h5_int64_t start,		///!< [in]  Start particle
	h5_int64_t length	       	///!< [in]  number of particle in view
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t,
	                   "f=%p, start=%lld, length=%lld",
	                   f, (long long)start, (long long)length);
	struct h5u_fdata *u = f->u;

	TRY (h5u_reset_view (fh));

	if (start == -1 && length == -1)
		H5_CORE_API_LEAVE (H5_SUCCESS);

	hsize_t total = 0;
	if (u->shape > 0) {
		TRY (total = hdf5_get_npoints_of_dataspace (u->shape) );
		h5_debug(
			"Found %lld particles from previous H5PartSetNumParticles call.",
			(long long)total);
        } else {
                TRY (total = (hsize_t)h5u_get_totalnum_particles_by_idx (fh,0));
        }
	if (total == 0) {
		/* No datasets have been created yet and no views are set.
		 * We have to leave the view empty because we don't know how
		 * many particles there should be! */
		H5_CORE_API_LEAVE (H5_SUCCESS);
	}

	if (start < 0 || length < 0 || start+length > total) 
                H5_CORE_API_LEAVE (
		        h5_error(
		                H5_ERR_INVAL,
		                "Invalid view: start=%lld, length=%lld, total=%lld",
		                (long long)start, (long long)length, (long long)total));

	/* setting up the new view */
	u->viewstart =  start;
	u->viewend =    start + length - 1;
	u->nparticles = length;

	h5_debug (
	        "This view has %lld particles.",
	        (long long)u->nparticles );

	/* declare overall data size  but then will select a subset */
	TRY (u->diskshape = hdf5_create_dataspace (1, &total, NULL));

	hsize_t hstart = (hsize_t)start;
	hsize_t hstride = 1;
	hsize_t hcount = (hsize_t)u->nparticles;

	TRY (hdf5_select_hyperslab_of_dataspace (
	             u->diskshape,
	             H5S_SELECT_SET,
	             &hstart, &hstride, &hcount,
	             NULL));

	/* declare local memory datasize */
	hsize_t dmax = H5S_UNLIMITED;
	TRY (u->memshape = hdf5_create_dataspace (1, &hcount, &dmax));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5u_set_view_indices (
	const h5_file_t fh,	        	/*!< [in]  Handle to open file */
	const h5_size_t *const indices,		/*!< [in]  List of indices */
	h5_size_t nelems		        /*!< [in]  Size of list */
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t,
	                   "f=%p, indices=%p, nelems=%llu",
	                   f, indices, (long long unsigned)nelems);
        CHECK_FILEHANDLE (f);
        if (f->step_gid < 0) {
                TRY (h5_set_step (fh, 0));
        }

	hsize_t total = 0;
	hsize_t dmax = H5S_UNLIMITED;
	struct h5u_fdata *u = f->u;

	TRY (h5u_reset_view (fh));

	if ( indices == NULL ) {
		h5_warn ("View indices array is null: reseting view.");
		H5_CORE_API_LEAVE (H5_SUCCESS);
	}
	if (f->u->shape > 0) {
		TRY (total = hdf5_get_npoints_of_dataspace (f->u->shape) );
		h5_debug(
			"Found %lld particles from previous H5PartSetNumParticles call.",
			(long long)total);
        } else {
                TRY (total = h5u_get_totalnum_particles_by_idx (fh,0));
        }
	if (total == 0) {
		/* No datasets have been created yet and no views are set.
		 * We have to leave the view empty because we don't know how
		 * many particles there should be! */
		H5_CORE_API_LEAVE (H5_SUCCESS);
	}

	u->nparticles = nelems;

	/* declare overall data size  but then we select a subset */
	TRY (u->diskshape = hdf5_create_dataspace (1, &total, NULL));

	/* declare local memory datasize */
	total = u->nparticles;
	TRY (u->memshape = hdf5_create_dataspace (1, &total, &dmax));
	if (nelems > 0) {
		TRY (hdf5_select_elements_of_dataspace (
		             u->diskshape,
		             H5S_SELECT_SET,
		             (hsize_t)nelems, (hsize_t*)indices ) );
	} else {
		TRY (hdf5_select_none (u->diskshape));
	}
	u->viewindexed = 1;

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_int64_t
h5u_get_view (
	const h5_file_t fh,
	h5_int64_t *start,
	h5_int64_t *end
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t,
	                   "f=%p, start=%p, end=%p",
	                   f, start, end);
	struct h5u_fdata *u = f->u;

	if ( u->viewindexed ) {
		H5_CORE_API_LEAVE (
		        h5_error (
		                H5_ERR_INVAL,
		                "The current view has an index selection, but "
		                "this function only works for ranged views." ));
	}

	h5_int64_t viewstart = 0;
	h5_int64_t viewend = 0;

	if ( u->viewstart >= 0 )
		viewstart = u->viewstart;

	if ( u->viewend >= 0 ) {
		viewend = u->viewend;
	}
	else {
		TRY (viewend = h5u_get_num_particles (fh));
	}

	if ( start ) *start = viewstart;
	if ( end ) *end = viewend;

	H5_CORE_API_RETURN (viewend - viewstart + 1); // view range is *inclusive*
}

h5_int64_t
h5u_set_canonical_view (
	const h5_file_t fh
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_int64_t, "f=%p", f);
	h5u_fdata_t* u = f->u;
	TRY( h5u_reset_view (fh) );

	h5_int64_t start = 0;
	h5_int64_t total = 0;

	TRY (total = h5u_get_num_particles (fh));

	u->nparticles = total / f->nprocs;

#ifdef PARALLEL_IO
	h5_int64_t remainder = 0;
	remainder = total % f->nprocs;
	start = f->myproc * u->nparticles;

	/* distribute the remainder */
	if ( f->myproc < remainder ) u->nparticles++;

	/* adjust the offset */
	if ( f->myproc < remainder )
		start += f->myproc;
	else
		start += remainder;
#endif // PARALLEL_IO

	h5_int64_t length = u->nparticles;
	H5_CORE_API_RETURN (h5u_set_view_length (fh, start, length));
}

h5_ssize_t
h5u_get_num_datasets (
	const h5_file_t fh		/*!< [in]  Handle to open file */
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_int64_t, "f=%p", f);
	H5_CORE_API_RETURN (hdf5_get_num_datasets (f->step_gid));
}

/*!
   Get information about dataset in current index given by its index
 */
h5_err_t
h5u_get_dataset_info (
	const h5_file_t fh,	/*!< [in] Handle to open file */
	const h5_id_t idx,	/*!< [in] Index of the dataset */
	char *dataset_name,	/*!< [out] Name of dataset */
	const h5_size_t len_dataset_name,
				/*!< [in] Size of buffer \c dataset_name */
 	h5_int64_t *type,	/*!< [out] Type of data in dataset */
	h5_size_t *nelem	/*!< [out] Number of elements. */
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t, 
			   "f=%p, "
			   "idx=%lld, "
			   "dataset_name='%s', len_dataset_name=%llu, "
			   "type=%p, nelem=%p",
			   f,
			   (long long)idx,
			   dataset_name,
			   (long long unsigned)len_dataset_name,
			   type, nelem);
	TRY (hdf5_get_name_of_dataset_by_idx (
	             f->step_gid,
	             idx,
	             dataset_name, len_dataset_name) );

	H5_CORE_API_RETURN (
	        h5u_get_dataset_info_by_name(f, dataset_name, type, nelem));
}

/*!
   Get information about dataset in current index given by its index
 */
h5_err_t
h5u_get_dataset_info_by_name (
        const h5_file_p f,            /*!< [in] Handle to open file */
        const char* const dataset_name, /*!< [in] Name of dataset */
        h5_int64_t* const type,         /*!< [out] Type of data in dataset */
        h5_size_t* const nelem          /*!< [out] Number of elements. */
        ) {
	H5_CORE_API_ENTER (h5_err_t,
	                   "f=%p, "
	                   "dataset_name='%s', "
	                   "type=%p, nelem=%p",
	                   f,
	                   dataset_name,
	                   type, nelem);

	if (nelem) {
		h5_ssize_t nelem_;
		TRY (nelem_ = hdf5_get_npoints_of_dataset_by_name (
		             f->step_gid,
		             dataset_name) );
		if ( nelem_ < 0 ) H5_CORE_API_LEAVE (nelem_);
		*nelem = nelem_;
	}

	if (type) {
		*type = h5priv_get_dataset_type (f->step_gid, dataset_name);
		if (*type < 0) H5_CORE_API_LEAVE (*type);
	}

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5u_set_chunk (
        const h5_file_t fh,
        const h5_size_t size
        ) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_int64_t, "f=%p, size=%llu", f, (long long unsigned)size);
	if ( size == 0 )
	{
		h5_info ("Disabling chunking" );
		TRY (hdf5_set_layout_property (
		             f->u->dcreate_prop, H5D_CONTIGUOUS));
	} else
	{
		h5_info ("Setting chunk size to %lld particles", (long long)size);
		TRY (hdf5_set_chunk_property(
		             f->u->dcreate_prop, 1, (hsize_t*)&size));
	}
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5u_get_chunk (
	const h5_file_t fh,		/*!< IN: File handle */
	const char *name, 		/*!< IN: name of dataset */
	h5_size_t *size			/*!< OUT: chunk size in particles */
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_int64_t, "f=%p, name='%s', size=%p", f,name,size);
	hid_t dataset_id;
	hid_t plist_id;
	hsize_t hsize;

	TRY (dataset_id = hdf5_open_dataset (f->step_gid, name) );
	TRY (plist_id = hdf5_get_dataset_create_plist (dataset_id) );
	TRY (hdf5_get_chunk_property (plist_id, 1, &hsize) );
	TRY (hdf5_close_property ( plist_id) );
	TRY (hdf5_close_dataset (dataset_id) );

	*size = (h5_size_t)hsize;

	h5_info ("Found chunk size of %lld particles", (long long)*size);
	H5_CORE_API_RETURN (H5_SUCCESS);
}

