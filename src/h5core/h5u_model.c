#include "h5core/h5_core.h"
#include "h5_core_private.h"

h5_ssize_t
h5u_get_num_particles (
	h5_file_t *const f      /*!< [in]  Handle to open file */
	) {
	H5_CORE_API_ENTER (h5_ssize_t, "f=%p", f);
	h5_ssize_t nparticles;

	/* if a view exists, use its size as the number of particles */
	if (h5u_has_view (f)) {
		TRY (nparticles = hdf5_get_selected_npoints_of_dataspace(
			    f->u->diskshape));
		h5_debug(
			"Found %lld particles in existing view.",
			(long long)nparticles );
	}
	else if ( f->u->shape > 0 ) {
		TRY (nparticles = hdf5_get_npoints_of_dataspace (f->u->shape) );
		h5_debug(
			"Found %lld particles from previous H5PartSetNumParticles call.",
			(long long)nparticles );
	} else {
		/* otherwise, report all particles on disk in the first dataset
		 * for this timestep */
		char dataset_name[H5_DATANAME_LEN];
		dataset_name[0] = '\0';
		h5_err_t exists = hdf5_get_name_of_dataset_by_idx (
				f->step_gid,
				0,
				dataset_name,
				H5_DATANAME_LEN);
		if (exists < 0)
			H5_CORE_API_LEAVE (
				h5_error (
					H5_ERR_INVAL,
					"Cannot determine the number of particles: "
					"H5PartSetNumParticles has not been called, "
					"no view has been set, and there are no "
					"data sets for this time step!"));
		TRY (nparticles = hdf5_get_npoints_of_dataset_by_name(
			     f->step_gid,
			     dataset_name) );
		h5_debug (
			"Found %lld particles in the first data set of this time step.",
			(long long)nparticles);
	}

	H5_CORE_API_RETURN (nparticles);
}

h5_err_t
h5u_set_num_particles (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const h5_size_t nparticles,	/*!< [in] Number of particles */
	const h5_size_t stride		/*!< [in] Stride of particles in memory */
	) {
	H5_CORE_API_ENTER (h5_ssize_t,
			   "f=%p, nparticles=%llu, stride=%llu",
			   f, (long long unsigned)nparticles,
			   (long long unsigned)stride);
	struct h5u_fdata *u = f->u;
	hsize_t start;
	hsize_t total;
	hsize_t dmax = H5S_UNLIMITED;

	if (nparticles < 0)
		H5_CORE_API_LEAVE (
			h5_error(
				H5_ERR_INVAL,
				"Invalid number particles: %lld!\n",
				(long long)nparticles));

	hsize_t hstride = (hsize_t)stride;
	if (hstride > 1)
		h5_debug ("Striding by %lld elements.", (long long)hstride);

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

	TRY (h5u_reset_view(f));

	TRY (hdf5_close_dataspace (u->shape));
	u->shape = H5S_ALL;

	u->nparticles = (hsize_t)nparticles;

	/* declare local memory datasize with striding */
	hsize_t count = u->nparticles * stride;
	TRY (u->memshape = hdf5_create_dataspace (1, &count, &dmax));

	/* we need a hyperslab selection if there is striding
	 * (otherwise, the default H5S_ALL selection is ok)
	 */
	if (hstride > 1) {
		start = 0;
		count = u->nparticles;
		TRY (hdf5_select_hyperslab_of_dataspace(
			u->memshape,
			  H5S_SELECT_SET,
			  &start, &hstride, &count,
			  NULL));
	}

#ifndef PARALLEL_IO
        UNUSED_ARGUMENT (total);
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

	TRY( h5priv_mpi_sum(
                    &(u->nparticles), &total, 1, MPI_LONG_LONG, f->comm ) );
        TRY( h5priv_mpi_prefix_sum(
                    &(u->nparticles), &start, 1, MPI_LONG_LONG, f->comm ) );
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
	hstride = 1;
	if (count > 0) {
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
	const h5_file_t *const f
	) {
	H5_CORE_API_ENTER (h5_ssize_t, "f=%p", f);
	H5_CORE_API_RETURN (f->u->viewindexed || (f->u->viewstart >= 0 && f->u->viewend >= 0));
}

h5_err_t
h5u_reset_view (
	h5_file_t *const f
	) {
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

h5_err_t
h5u_set_view (
	h5_file_t *const f,		/*!< [in]  Handle to open file */
	h5_int64_t start,		/*!< [in]  Start particle */
	h5_int64_t end	        	/*!< [in]  End particle */
	) {
	H5_CORE_API_ENTER (h5_ssize_t,
			   "f=%p, start=%lld, end=%lld",
			   f, (long long)start, (long long)end);
	hsize_t total;
	hsize_t stride = 1;
	hsize_t dmax = H5S_UNLIMITED;
	struct h5u_fdata *u = f->u;

	h5_debug (
		"Set view (%lld,%lld).",
		(long long)start,(long long)end);

	TRY (h5u_reset_view (f));

	if (start == -1 && end == -1)
		H5_CORE_API_LEAVE (H5_SUCCESS);

	/*
	  View has been reset so H5PartGetNumParticles will tell
	  us the total number of particles.

	  For now, we interpret start=-1 to mean 0 and 
	  end==-1 to mean end of file
	*/
	TRY ( total = (hsize_t) h5u_get_num_particles ( f ) );
	if ( total == 0 ) {
		/* No datasets have been created yet and no veiws are set.
		 * We have to leave the view empty because we don't know how
		 * many particles there should be! */
		H5_CORE_API_LEAVE (H5_SUCCESS);
	}


	if ( start == -1 ) start = 0;
	if ( end == -1 )   end = total - 1; // range is *inclusive*

	/* so, is this selection inclusive or exclusive? 
	   it appears to be inclusive for both ends of the range.
	*/
	if ( end < start ) {
		h5_warn (
			"Nonfatal error. "
			"End of view (%lld) is less than start (%lld).",
			(long long)end, (long long)start );
		end = start; /* ensure that we don't have a range error */
	}
	/* setting up the new view */
	u->viewstart =  start;
	u->viewend =    end;
	u->nparticles = end - start + 1;
	
	h5_debug (
		"This view selected %lld particles.",
		(long long)u->nparticles );

	/* declare overall data size  but then will select a subset */
	TRY (u->diskshape = hdf5_create_dataspace ( 1, &total, NULL ));

	total = (hsize_t)u->nparticles;
	hsize_t hstart = (hsize_t)start;

	TRY (hdf5_select_hyperslab_of_dataspace ( 
		     u->diskshape,
		     H5S_SELECT_SET,
		     &hstart, &stride, &total,
		     NULL));

	/* declare local memory datasize */
	TRY (u->memshape = hdf5_create_dataspace (1, &total, &dmax));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5u_set_view_indices (
	h5_file_t *const f,	        	/*!< [in]  Handle to open file */
	const h5_size_t *const indices,		/*!< [in]  List of indices */
	h5_size_t nelems		        /*!< [in]  Size of list */
	) {
	H5_CORE_API_ENTER (h5_err_t,
			   "f=%p, indices=%p, nelems=%llu",
			   f, indices, (long long unsigned)nelems);
	hsize_t total;
	hsize_t dmax = H5S_UNLIMITED;
	struct h5u_fdata *u = f->u;

	TRY ( h5u_reset_view ( f ) );

	if ( indices == NULL ) {
		h5_warn ("View indices array is null: reseting view.");
		H5_CORE_API_LEAVE (H5_SUCCESS);
	}

	/*
	  View has been reset so H5PartGetNumParticles will tell
	  us the total number of particles.

	  For now, we interpret start=-1 to mean 0 and 
	  end==-1 to mean end of file
	*/
	TRY ( total = h5u_get_num_particles ( f ) );
	if ( total == 0 ) {
		/* No datasets have been created yet and no veiws are set.
		 * We have to leave the view empty because we don't know how
		 * many particles there should be! */
		H5_CORE_API_LEAVE (H5_SUCCESS);
	}

	if ( total == 0 ) H5_CORE_API_LEAVE (H5_SUCCESS);

	u->nparticles = nelems;
	h5_debug ("This view selected %lld particles.", (long long)u->nparticles );

	/* declare overall data size  but then will select a subset */
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

h5_err_t
h5u_get_view (
	h5_file_t *const f,
	h5_int64_t *start,
	h5_int64_t *end
	) {
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
		TRY ( viewend = h5u_get_num_particles ( f ) );
	}

	if ( start ) *start = viewstart;
	if ( end )   *end = viewend;

	H5_CORE_API_RETURN (viewend - viewstart + 1); // view range is *inclusive*
}

h5_int64_t
h5u_set_canonical_view (
	h5_file_t *const f
	) {
	H5_CORE_API_ENTER (h5_int64_t, "f=%p", f);
	TRY( h5u_reset_view ( f ) );

	h5_int64_t start = 0;
	h5_int64_t end = 0;
	h5_int64_t total = 0;

	TRY( total = h5u_get_num_particles ( f ) );

#ifdef PARALLEL_IO
	h5_int64_t remainder = 0;

	f->u->nparticles = total / f->nprocs;
	remainder = total % f->nprocs;
	start = f->myproc * f->u->nparticles;

	/* distribute the remainder */
	if ( f->myproc < remainder ) f->u->nparticles++;

	/* adjust the offset */
	if ( f->myproc < remainder ) start += f->myproc;
	else start += remainder;

	end = start + f->u->nparticles - 1;

#else
	f->u->nparticles = total;
	end = total - 1;
#endif // PARALLEL_IO

	TRY( h5u_set_view ( f, start, end ) );

	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_ssize_t
h5u_get_num_datasets (
	h5_file_t *const f		/*!< [in]  Handle to open file */
	) {
	H5_CORE_API_ENTER (h5_int64_t, "f=%p", f);
	H5_CORE_API_RETURN (hdf5_get_num_datasets (f->step_gid));
}

/*!
  Get information about dataset in current index given by its index
*/
h5_err_t
h5u_get_dataset_info (
	h5_file_t *const f,	/*!< [in] Handle to open file */
	const h5_id_t idx,	/*!< [in] Index of the dataset */
	char *dataset_name,	/*!< [out] Name of dataset */
	const h5_size_t len_dataset_name,
				/*!< [in] Size of buffer \c dataset_name */
 	h5_int64_t *type,	/*!< [out] Type of data in dataset */
	h5_size_t *nelem	/*!< [out] Number of elements. */
	) {
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

	if ( nelem ) {
		h5_ssize_t nelem_;
		TRY (nelem_ = hdf5_get_npoints_of_dataset_by_name (
			     f->step_gid,
			     dataset_name) );
		if ( nelem_ < 0 ) H5_CORE_API_LEAVE (nelem_);
		*nelem = nelem_;
	}

	if (type) {
		*type = h5_get_dataset_type (f->step_gid, dataset_name);
		if (*type < 0) H5_CORE_API_LEAVE (*type);
	}

	H5_CORE_API_RETURN (H5_SUCCESS);
} 

h5_err_t
h5u_set_chunk (
        h5_file_t *const f,
        const h5_size_t size
        ) {
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
	h5_file_t *const f,		/*!< IN: File handle */
	const char *name, 		/*!< IN: name of dataset */
	h5_size_t *size			/*!< OUT: chunk size in particles */
	) {
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

