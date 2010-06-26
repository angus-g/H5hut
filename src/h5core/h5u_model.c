#include "h5core/h5_core.h"
#include "h5_core_private.h"

h5_int64_t
h5u_get_num_particles (
	h5_file_t *f			/*!< [in]  Handle to open file */
	) {
	h5_int64_t nparticles;
	ssize_t exists;

	/* returns 0 if there are no datasets on disk */
	TRY ( exists = h5_get_num_hdf5_datasets(f, f->step_gid ) );
	if ( exists == 0 )
	{
		/* try to recover number of particles from a previous
		 * H5PartSetNumParticles call. */
#ifdef PARALLEL_IO
		hsize_t total;
		TRY( h5priv_mpi_sum(f,
			&(f->u->nparticles), &total,
			1, MPI_LONG_LONG, f->comm) );
		nparticles = (h5_int64_t)total;
#else
		nparticles = (h5_int64_t)f->u->nparticles;
#endif
		if ( nparticles > 0 ) {
			h5_debug (
					f,
					"Using existing view to report "
					"nparticles = %lld", (long long)nparticles );
			return nparticles;
		}
		else {
			h5_warn (
					f,
					"There are no datasets in the current timestep "
					"nor existing views: "
					"reporting 0 particles.");
			return 0;
		}

	}

	/* if a view exists, use its size as the number of particles */
	if ( h5u_has_view ( f ) )
	{
		TRY( nparticles = h5priv_get_selected_npoints_of_hdf5_dataspace(
			    f,
			    f->u->diskshape) );
		h5_debug(
				f,
				"Found %lld points with H5Sget_select_npoints",
				(long long)nparticles );
	}
	/* otherwise, report all particles on disk in the first dataset
	 * for this timestep */
	else
	{
		char dataset_name[H5_DATANAME_LEN];
		TRY( h5priv_get_hdf5_objname_by_idx(
				f,
				f->step_gid,
				0,
				dataset_name,
				H5_DATANAME_LEN) );
		TRY( nparticles = h5priv_get_npoints_of_hdf5_dataset_by_name (
			f,
			f->step_gid,
			dataset_name ) );
	}

	return nparticles;
}



h5_int64_t
h5u_set_num_particles (
	h5_file_t *f,			/*!< [in] Handle to open file */
	h5_int64_t nparticles,	/*!< [in] Number of particles */
	h5_int64_t stride		/*!< [in] Stride of particles in memory */
	) {
	CHECK_FILEHANDLE( f );
	struct h5u_fdata *u = f->u;

	hsize_t hstride;
	hsize_t count;
	hsize_t start;
	hsize_t total;
	hsize_t dmax = H5S_UNLIMITED;

	if ( nparticles <= 0 )
		return h5_error(
				f,
				H5_ERR_INVAL,
				"Invalid number particles: %lld!\n",
				(long long)nparticles);

	/* prevent invalid stride value */	
	if (stride < 1)
	{
		h5_warn (
				f,
				"Stride < 1 was specified: changing to 1." );
		hstride = 1;
	} else {
		hstride = (hsize_t)stride;
	}

#ifndef PARALLEL_IO
	/*
	  if we are not using parallel-IO, there is enough information
	   to know that we can short circuit this routine.  However,
	   for parallel IO, this is going to cause problems because
	   we don't know if things have changed globally
	*/
	if ( u->nparticles == nparticles && stride == 1 ) {
		return H5_SUCCESS;
	}
#endif

	TRY( h5u_reset_view(f) );

	TRY( h5priv_close_hdf5_dataspace( f, u->shape ) );
	u->shape = H5S_ALL;

	u->nparticles = (hsize_t)nparticles;

	/* declare local memory datasize with striding */
	count = u->nparticles * stride;
	TRY( u->memshape = h5priv_create_hdf5_dataspace(f, 1, &count, &dmax) );

	/* we need a hyperslab selection if there is striding
	 * (otherwise, the default H5S_ALL selection is ok)
	 */
	if ( hstride > 1 )
	{
		start = 0;
		count = u->nparticles;
		TRY( h5priv_select_hyperslab_of_hdf5_dataspace(
					f,
					u->memshape,
					H5S_SELECT_SET,
					&start,
					&hstride,
					&count, NULL ) );
	}

#ifndef PARALLEL_IO
	count = u->nparticles;
	TRY( u->shape = h5priv_create_hdf5_dataspace (
		     f,
		     1,
		     &count,
		     NULL ) );
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

	TRY( h5priv_mpi_sum(f,
                    &(u->nparticles), &total, 1, MPI_LONG_LONG, f->comm ) );
        TRY( h5priv_mpi_prefix_sum(f,
                    &(u->nparticles), &start, 1, MPI_LONG_LONG, f->comm ) );

	u->viewstart = start;
	u->viewend   = start + u->nparticles - 1; // view range is *inclusive*
	
	/* declare overall datasize */
	count = total;
	TRY ( u->shape = h5priv_create_hdf5_dataspace (f, 1, &count, NULL) );

	/* declare overall data size  but then will select a subset */
	TRY ( u->diskshape = h5priv_create_hdf5_dataspace (f, 1, &count, NULL) );

	count = nparticles;
	hstride = 1;
	TRY ( h5priv_select_hyperslab_of_hdf5_dataspace (
			f,
			u->diskshape,
			H5S_SELECT_SET,
			&start, &hstride, &count,
			NULL ) );
#endif
	return H5_SUCCESS;
}

h5_int64_t
h5u_has_view (
	h5_file_t *f
	) {
	return  ( f->u->viewindexed || ( f->u->viewstart >= 0 && f->u->viewend >= 0 ));
}

h5_err_t
h5u_reset_view (
	h5_file_t *f
	) {	     

	struct h5u_fdata *u = f->u;

	u->viewstart = -1;
	u->viewend = -1;
	u->viewindexed = 0;
	TRY( h5priv_close_hdf5_dataspace( f, u->diskshape ) );
	u->diskshape = H5S_ALL;
	TRY( h5priv_close_hdf5_dataspace( f, u->memshape ) );
	u->memshape = H5S_ALL;

	return H5_SUCCESS;
}

h5_err_t
h5u_set_view (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	h5_int64_t start,		/*!< [in]  Start particle */
	h5_int64_t end		/*!< [in]  End particle */
	) {

	h5_int64_t herr = 0;
	hsize_t total;
	hsize_t stride = 1;
	hsize_t hstart;
	hsize_t dmax = H5S_UNLIMITED;
	struct h5u_fdata *u = f->u;

	h5_debug (
		f,
		"Set view (%lld,%lld).",
		(long long)start,(long long)end);

	herr = h5u_reset_view ( f );
	if ( herr < 0 ) return herr;

	if ( start == -1 && end == -1 ) return H5_SUCCESS;

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
		return H5_SUCCESS;
	}


	if ( start == -1 ) start = 0;
	if ( end == -1 )   end = total - 1; // range is *inclusive*

	h5_debug ( f, "Total nparticles=%lld", (long long)total );

	/* so, is this selection inclusive or exclusive? 
	   it appears to be inclusive for both ends of the range.
	*/
	if ( end < start ) {
		h5_warn (
			f,
			"Nonfatal error. "
			"End of view (%lld) is less than start (%lld).",
			(long long)end, (long long)start );
		end = start; /* ensure that we don't have a range error */
	}
	/* setting up the new view */
	u->viewstart =  start;
	u->viewend =    end;
	u->nparticles = end - start + 1;
	
	h5_debug ( f, "nparticles=%lld", (long long)u->nparticles );

	/* declare overall data size  but then will select a subset */
	TRY ( u->diskshape = h5priv_create_hdf5_dataspace ( f, 1, &total, NULL ) );

	total = (hsize_t)u->nparticles;
	hstart = (size_t)start;

	TRY ( h5priv_select_hyperslab_of_hdf5_dataspace ( 
		      f,
		      u->diskshape,
		      H5S_SELECT_SET,
		      &hstart, &stride, &total,
		      NULL ) );

	/* declare local memory datasize */
	TRY ( u->memshape = h5priv_create_hdf5_dataspace (
		      f, 1, &total, &dmax ) );

	return H5_SUCCESS;
}

h5_int64_t
h5u_set_view_indices (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	const h5_int64_t *indices,	/*!< [in]  List of indices */
	h5_int64_t nelems		/*!< [in]  Size of list */
	) {

	hsize_t total;
	hsize_t dmax = H5S_UNLIMITED;
	struct h5u_fdata *u = f->u;

	TRY ( h5u_reset_view ( f ) );

	if ( indices == NULL ) {
		h5_warn ( f,
			"View indices array is null: reseting view." );
		return H5_SUCCESS;
	}

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
		return H5_SUCCESS;
	}

	h5_debug ( f, "Total nparticles=%lld", (long long)total );

	if ( total == 0 ) return H5_SUCCESS;

	/* check length of list */
	if ( nelems < 0 ) {
		h5_warn (f,
			"Array of view indices has length < 0: "
			"resetting view.");
		u->nparticles = 0;
	} else {
		u->nparticles = (hsize_t) nelems;
	}

	/* declare overall data size  but then will select a subset */
	TRY ( u->diskshape = h5priv_create_hdf5_dataspace ( f, 1, &total, NULL ) );

	/* declare local memory datasize */
	total = (size_t)u->nparticles;
	TRY ( u->memshape = h5priv_create_hdf5_dataspace (
		      f, 1, &total, &dmax ) );

	TRY ( h5priv_select_elements_of_hdf5_dataspace ( 
		      f,
		      u->diskshape,
		      H5S_SELECT_SET,
		      nelems, (hsize_t*)indices ) );

	u->viewindexed = 1;

	return H5_SUCCESS;
}

h5_int64_t 
h5u_get_view (
	h5_file_t *f,
	h5_int64_t *start,
	h5_int64_t *end
	) {
	struct h5u_fdata *u = f->u;

	if ( u->viewindexed ) {
		return h5_error (
                f,
                H5_ERR_INVAL,
			    "The current view has an index selection, but "
			    "this function only works for ranged views." );
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

	return viewend - viewstart + 1; // view range is *inclusive*
}

h5_int64_t
h5u_set_canonical_view (
	h5_file_t *f
	) {
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

	return H5_SUCCESS;
}

h5_int64_t
h5u_get_num_datasets (
	h5_file_t *f		/*!< [in]  Handle to open file */
	) {
	ssize_t n;
	TRY ( n = h5_get_num_hdf5_datasets(f, f->step_gid ) );
	return (h5_int64_t)n;
}

/*!
  Get information about dataset in current index given by its index
 */
h5_int64_t
h5u_get_dataset_info (
	h5_file_t *f,		/*!< [in]  Handle to open file */
	const h5_int64_t idx,/*!< [in]  Index of the dataset */
	char *dataset_name,	/*!< [out] Name of dataset */
	const h5_int64_t len_dataset_name,
				/*!< [in]  Size of buffer \c dataset_name */
	h5_int64_t *type,	/*!< [out] Type of data in dataset */
	h5_int64_t *nelem	/*!< [out] Number of elements. */
	) {

	TRY( h5_get_hdf5_datasetname_by_idx (
		     f,
		     f->step_gid,
		     idx,
		     dataset_name, len_dataset_name) );

	if ( nelem ) {
		TRY( *nelem = h5priv_get_npoints_of_hdf5_dataset_by_name (
			f,
			f->step_gid,
			dataset_name) );
		if ( *nelem < 0 ) return *nelem;
	}

	if ( type ) {
		*type = h5_get_dataset_type( f, f->step_gid, dataset_name );
		if ( *type < 0 ) return *type;
	}

	return H5_SUCCESS;
}

h5_err_t
h5u_set_chunk (
        h5_file_t *f,
        h5_size_t size
        ) {

	if ( f->u->dcreate_prop == H5P_DEFAULT ) {
		TRY( f->u->dcreate_prop = h5priv_create_hdf5_property(f,
						H5P_DATASET_CREATE) );
	}

	if ( size == 0 )
	{
		h5_info(f, "Disabling chunking" );
		TRY( h5priv_set_hdf5_layout_property(f,
					f->u->dcreate_prop, H5D_CONTIGUOUS) );
	} else 
	{
		h5_info(f, "Setting chunk size to %lld particles", (long long)size);
		TRY( h5priv_set_hdf5_chunk_property(f,
					f->u->dcreate_prop, 1, (hsize_t*)&size) );
	}

	return H5_SUCCESS;
}

