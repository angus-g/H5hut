/*
  Copyright (c) 2006-2016, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "h5core/h5_log.h"
#include "h5core/h5u_model.h"
#include "h5core/h5u_io.h"

#include "private/h5_file.h"
#include "private/h5_hdf5.h"

#include "private/h5_model.h"
#include "private/h5_mpi.h"
#include "private/h5_io.h"
#include "private/h5u_types.h"

#include <string.h>

h5_ssize_t
h5u_get_num_points (
	const h5_file_t fh      /*!< [in]  Handle to open file */
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_ssize_t, "f=%p", f);
	h5_ssize_t nparticles;

	if (h5u_has_view ((h5_file_t)f)) {
                /* if a view exists, use its size as the number of particles */
		TRY (nparticles = h5u_get_num_points_in_view (fh));
	} else {
		/* otherwise, report all particles on disk in the first dataset
                   for this timestep */
                TRY (nparticles = h5u_get_totalnum_particles_by_idx (fh, 0));
	}

	H5_RETURN (nparticles);
}

h5_ssize_t
h5u_get_num_points_in_view (
	const h5_file_t fh      /*!< [in]  Handle to open file */
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_ssize_t, "f=%p", f);
	h5_ssize_t nparticles;

	if (!h5u_has_view (fh)) {
                H5_LEAVE (
                        h5_error (
                                H5_ERR_H5PART,
                                "No view has been set."));
        }
        TRY (nparticles = hdf5_get_selected_npoints_of_dataspace(f->u->diskshape));
        h5_debug ("Found %lld particles in view.", (long long)nparticles );
	H5_RETURN (nparticles);
}

h5_ssize_t
h5u_get_totalnum_particles_by_name (
	const h5_file_t fh,		///< [in] Handle to open file
        const char* const dataset_name	///< [in] dataset to query
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_ssize_t,
			   "f=%p, dataset_name=%s",
			   f, dataset_name);
	h5_ssize_t nparticles;
        TRY (nparticles = hdf5_get_npoints_of_dataset_by_name (
		     f->step_gid, dataset_name));
        h5_debug ("Found %lld particles in dataset %s.",
		  (long long)nparticles, dataset_name);
	H5_RETURN (nparticles);
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
	h5_err_t h5err;
        TRY (h5err = hdf5_get_name_of_dataset_by_idx (
                    f->step_gid,
                    idx,
                    dataset_name,
                    H5_DATANAME_LEN));
	if (h5err == H5_NOK)
		H5_LEAVE (H5_NOK);
	h5_ssize_t nparticles;
        TRY (nparticles = hdf5_get_npoints_of_dataset_by_name (
		     f->step_gid, dataset_name));
        h5_debug ("Found %lld particles in dataset %s.",
		  (long long)nparticles, dataset_name);
	H5_RETURN (nparticles);
}

h5_err_t
h5u_set_num_points (
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
		H5_LEAVE (
		        h5_error(
		                H5_ERR_INVAL,
		                "Invalid number of particles: %lld!\n",
		                (long long)nparticles));

#ifndef PARALLEL_IO
	/*
	   if we are not using parallel-IO, there is enough information
	   to know that we can short circuit this routine.  However,
	   for parallel IO, this is going to cause problems because
	   we don't know if things have changed globally
	 */
	if ( u->nparticles == nparticles && stride == 1 ) {
		H5_LEAVE (H5_SUCCESS);
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
	h5_debug("Start index on this processor: %lld.", (long long)start);

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
	H5_RETURN (H5_SUCCESS);
}

h5_err_t
h5u_has_view (
	const const h5_file_t fh
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_ssize_t, "f=%p", f);
	H5_RETURN (f->u->viewindexed || f->u->viewstart >= 0);
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

	H5_RETURN (H5_SUCCESS);
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
		H5_LEAVE (H5_SUCCESS);

	if (f->u->shape > 0) {
		TRY (total = hdf5_get_npoints_of_dataspace (f->u->shape) );
        } else {
                TRY (total = (hsize_t)h5u_get_totalnum_particles_by_idx (fh, 0));
        }
	h5_debug ("Total = %lld", (long long) total);
	if ((long long)total <= 0) {
		/*
		  No datasets have been created yet and no views are set.
		  We have to leave the view empty because we don't know how
		  many particles there should be!

		  :FIXME: Should 'total == 0' be considered valid or not?
		  :FIXME: why not gather total size?
		*/
		if (start < 0) {
			H5_LEAVE (
				h5_error(
					H5_ERR_INVAL,
					"Start of selection '%lld' out of range: "
					"must be >= 0",
					(long long)start)
				);
		}
		if (end < start) {
			H5_LEAVE (
				h5_error(
					H5_ERR_INVAL,
					"End of selection '%lld' out of range: "
					"must be >= %lld",
					(long long)end,
					(long long)start)
				);
		}
#if PARALLEL_IO
		TRY (
			h5priv_mpi_allreduce_max (
			     &end, &total, 1, MPI_LONG_LONG, f->props->comm)
			);
#else
		total = end - start;
#endif
		total++;
		TRY (h5u_reset_view(fh));

		TRY (hdf5_close_dataspace (u->shape));
		TRY (u->shape = hdf5_create_dataspace(1, &total, NULL) );
	} else {
		if (end < 0) {
			end = total+end;
		}
	
		if (start < 0 || start >= total) {
			H5_LEAVE (
				h5_error(
					H5_ERR_INVAL,
					"Start of selection '%lld' out of range: "
					"must be in [0..%lld]",
					(long long)start, (long long)total-1));
		} else if (end < 0 || end >= total) {
			H5_LEAVE (
				h5_error(
					H5_ERR_INVAL,
					"End of selection '%lld' out of range: "
					"must be in [0..%lld]",
					(long long)end, (long long)total-1));
		} else if (end+1 < start) {
			H5_LEAVE (
				h5_error(
					H5_ERR_INVAL,
					"Invalid selection: start=%lld > end=%lld!\n",
					(long long)start, (long long)end));
		}
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
	H5_RETURN (H5_SUCCESS);
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
		H5_LEAVE (H5_SUCCESS);

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
		H5_LEAVE (H5_SUCCESS);
	}

	if (start < 0 || length < 0 || start+length > total) 
                H5_LEAVE (
		        h5_error(
		                H5_ERR_INVAL,
		                "Invalid view: start=%lld, length=%lld, total=%lld",
		                (long long)start, (long long)length,
				(long long)total));

	/* setting up the new view */
	u->viewstart =  start;
	u->viewend =    start + length - 1;
	u->nparticles = length;

	h5_debug (
	        "This view includes %lld particles.",
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
	H5_RETURN (H5_SUCCESS);
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
		H5_LEAVE (H5_SUCCESS);
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
		H5_LEAVE (H5_SUCCESS);
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

	H5_RETURN (H5_SUCCESS);
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
		H5_LEAVE (
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
		TRY (viewend = h5u_get_num_points (fh));
	}

	if ( start ) *start = viewstart;
	if ( end ) *end = viewend;

	H5_RETURN (viewend - viewstart + 1); // view range is *inclusive*
}

h5_err_t
h5u_set_canonical_view (
	const h5_file_t fh
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_int64_t, "f=%p", f);
	h5u_fdata_t* u = f->u;
	TRY( h5u_reset_view (fh) );

	h5_int64_t start = 0;
	h5_int64_t total = 0;

	TRY (total = h5u_get_num_points (fh));

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
	TRY (h5u_set_view_length (fh, start, length));
	H5_RETURN (H5_SUCCESS);
}

h5_ssize_t
h5u_get_num_datasets (
	const h5_file_t fh		/*!< [in]  Handle to open file */
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_ssize_t, "f=%p", f);
	TRY (ret_value = hdf5_get_num_datasets (f->step_gid));
	H5_RETURN (ret_value);
}

h5_err_t
h5u_has_dataset (
	const h5_file_t fh,
	const char* const name
	) {
	h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t, 
			   "f=%p, name='%s'",
			   f, name);
        TRY (ret_value = hdf5_link_exists (f->step_gid, name));
	H5_RETURN (ret_value);
}

static inline h5_err_t
get_dataset_info (
	hid_t dataset_id,
	h5_int64_t* dataset_type,
	h5_size_t* dataset_nelem
	) {
        H5_INLINE_FUNC_ENTER (h5_err_t);
	if (dataset_type) {
		h5_int64_t type_;
		TRY (type_ = h5priv_get_normalized_dataset_type (dataset_id));
		TRY (*dataset_type = h5priv_map_hdf5_type_to_enum (type_));
	}
	if (dataset_nelem) {
		h5_ssize_t nelem_;
		TRY (nelem_ = hdf5_get_npoints_of_dataset (dataset_id));
		*dataset_nelem = nelem_;
	}
	H5_RETURN (H5_SUCCESS);
}

h5_err_t
h5priv_get_dataset_info_by_idx (
	const hid_t id,			/*!< [in] group ID */
	const h5_id_t dataset_idx,	/*!< [in] Index of the dataset */
	char* dataset_name,		/*!< [out] Name of dataset */
	const h5_size_t len_dataset_name,/*!<[in] Size of buffer */
 	h5_int64_t* dataset_type,	/*!< [out] Type of data in dataset */
	h5_size_t* dataset_nelem	/*!< [out] Number of elements. */
	) {
	H5_PRIV_API_ENTER (h5_err_t, 
			   "id=%lld, "
			   "dataset_idx=%lld, "
			   "dataset_name='%s', len_dataset_name=%llu, "
			   "dataset_type=%p, dataset_nelem=%p",
			   (long long)id,
			   (long long)dataset_idx,
			   dataset_name,
			   (long long unsigned)len_dataset_name,
			   dataset_type, dataset_nelem);
	char dataset_name_[H5_DATANAME_LEN];
	TRY (hdf5_get_name_of_dataset_by_idx (
	             id,
	             dataset_idx,
	             dataset_name_, sizeof(dataset_name_)));
	hid_t dataset_id;
	TRY (dataset_id = hdf5_open_dataset_by_name (id, dataset_name_));
	if (dataset_name) {
		strncpy (dataset_name, dataset_name_, len_dataset_name);
	}
	TRY (get_dataset_info (dataset_id, dataset_type, dataset_nelem));
	TRY (hdf5_close_dataset (dataset_id));
	H5_RETURN (H5_SUCCESS);
}

/*!
   Get information about dataset in current index given by its index
 */
h5_err_t
h5u_get_dataset_info_by_idx (
	const h5_file_t fh,		/*!< [in] Handle to open file */
	const h5_id_t idx,		/*!< [in] Index of the dataset */
	char *dataset_name,		/*!< [out] Name of dataset */
	const h5_size_t len_dataset_name,/*!< [in] Size of buffer */
 	h5_int64_t *dataset_type,	/*!< [out] Type of data in dataset */
	h5_size_t *dataset_nelem	/*!< [out] Number of elements. */
	) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t, 
			   "f=%p, "
			   "idx=%lld, "
			   "dataset_name='%s', len_dataset_name=%llu, "
			   "dataset_type=%p, dataset_nelem=%p",
			   f,
			   (long long)idx,
			   dataset_name,
			   (long long unsigned)len_dataset_name,
			   dataset_type, dataset_nelem);
	CHECK_FILEHANDLE (f);
	TRY (h5priv_get_dataset_info_by_idx (
	             f->step_gid,
	             idx,
	             dataset_name, len_dataset_name,
		     dataset_type, dataset_nelem));
	H5_RETURN (H5_SUCCESS);
}

h5_err_t
h5priv_get_dataset_info_by_name (
	const hid_t id,			/*!< [in] group ID */
	const char* const dataset_name,	/*!< [out] Name of dataset */
 	h5_int64_t* dataset_type,	/*!< [out] Type of data in dataset */
	h5_size_t* dataset_nelem	/*!< [out] Number of elements. */
	) {
	H5_PRIV_API_ENTER (h5_err_t, 
			   "id=%lld, "
			   "dataset_name='%s' "
			   "dataset_type=%p, dataset_nelem=%p",
			   (long long)id,
			   dataset_name,
			   dataset_type, dataset_nelem);
	hid_t dataset_id;
	TRY (dataset_id = hdf5_open_dataset_by_name (id, dataset_name));
	TRY (get_dataset_info (dataset_id, dataset_type, dataset_nelem));
	TRY (hdf5_close_dataset (dataset_id));
	H5_RETURN (H5_SUCCESS);
}

/*!
   Get information about dataset in current index given by its index
 */
h5_err_t
h5u_get_dataset_info_by_name (
        const h5_file_t fh,             /*!< [in] Handle to open file */
        const char* const dataset_name, /*!< [in] Name of dataset */
        h5_int64_t* const dataset_type, /*!< [out] Type of data in dataset */
        h5_size_t* const dataset_nelem  /*!< [out] Number of elements. */
        ) {
	h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (h5_err_t,
	                   "f=%p, "
	                   "dataset_name='%s', "
	                   "dataset_type=%p, dataset_nelem=%p",
	                   f,
	                   dataset_name,
	                   dataset_type, dataset_nelem);
	CHECK_FILEHANDLE (f);
	TRY (h5priv_get_dataset_info_by_name (
	             f->step_gid,
		     dataset_name,
		     dataset_type, dataset_nelem));
	H5_RETURN (H5_SUCCESS);
}

h5_err_t
h5u_set_chunk (
        const h5_file_t fh,
        const h5_size_t size
        ) {
        h5_file_p f = (h5_file_p)fh;
	H5_CORE_API_ENTER (
		h5_int64_t,
		"f=%p, size=%llu",
		f, (long long unsigned)size);
	if (size == 0) {
		h5_info ("Disabling chunking" );
		TRY (hdf5_set_layout_property (
		             f->u->dcreate_prop, H5D_CONTIGUOUS));
	} else {
		h5_info ("Setting chunk size to %lld particles", (long long)size);
		TRY (hdf5_set_chunk_property(
		             f->u->dcreate_prop, 1, (hsize_t*)&size));
	}
	H5_RETURN (H5_SUCCESS);
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

	TRY (dataset_id = hdf5_open_dataset_by_name (f->step_gid, name) );
	TRY (plist_id = hdf5_get_dataset_create_plist (dataset_id) );
	TRY (hdf5_get_chunk_property (plist_id, 1, &hsize) );
	TRY (hdf5_close_property ( plist_id) );
	TRY (hdf5_close_dataset (dataset_id) );

	*size = (h5_size_t)hsize;

	h5_info ("Found chunk size of %lld particles", (long long)*size);
	H5_RETURN (H5_SUCCESS);
}

