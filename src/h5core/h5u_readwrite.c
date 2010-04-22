#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

h5_int64_t
h5u_has_view (
	h5_file_t *f
	) {
	return  ( f->u->viewstart >= 0 ) && ( f->u->viewend >= 0 );
}

static hid_t
_get_diskshape_for_reading (
	h5_file_t *f,
	hid_t dataset
	) {
	struct h5u_fdata *u = f->u;
	hid_t space;

	TRY( space = h5priv_get_hdf5_dataset_space ( f, dataset ) );

	if ( h5u_has_view ( f ) ) {
		hsize_t stride;
		hsize_t count;
#ifdef HDF5V160
		hssize_t start;
#else
		hsize_t start;
#endif

		/* so, is this selection inclusive or exclusive? */
		start = u->viewstart;
		count = u->viewend - u->viewstart; /* to be inclusive */
		stride=1;

		/* now we select a subset */
		if ( u->diskshape > 0 ) {
			TRY ( h5priv_select_hyperslab_of_hdf5_dataspace (
				      f,
				      u->diskshape,
				      H5S_SELECT_SET,
				      &start, &stride, &count,
				      NULL ) );
		}
		TRY ( h5priv_select_hyperslab_of_hdf5_dataspace (
			      f,
			      space,
			      H5S_SELECT_SET,
			      &start, &stride, &count,
			      NULL ) );

		h5_debug (
			f,
			"Selection: range=%d:%d, npoints=%d s=%d",
			(int)u->viewstart,(int)u->viewend,
			(int)H5Sget_simple_extent_npoints(space),
			(int)H5Sget_select_npoints(space) );
	}
	return space;
}

static hid_t
_get_memshape_for_reading (
	h5_file_t *f,
	hid_t dataset
	) {
	struct h5u_fdata *u = f->u;

	if ( h5u_has_view ( f ) ) {
		hsize_t dmax=H5S_UNLIMITED;
		hsize_t len = u->viewend - u->viewstart;
		return h5priv_create_hdf5_dataspace ( f, 1, &len, &dmax );
	} else {
		return H5S_ALL;
	}
}

h5_int64_t
h5u_get_num_elems (
	h5_file_t *f			/*!< [in]  Handle to open file */
	) {
	hid_t space_id;
	hid_t dataset_id;
	char dataset_name[128];
	char step_name[128];
	hsize_t nparticles;

	/* Get first dataset in current time-step */
	sprintf (
		step_name,
		"%s#%0*lld",
		f->prefix_step_name, f->width_step_idx, (long long) f->step_idx );

	TRY( hdf5_get_object_name (
		f->file,
		step_name,
		H5G_DATASET,
		0,
		dataset_name, sizeof (dataset_name) ) );
	TRY( dataset_id = h5priv_open_hdf5_dataset ( f, f->step_gid, dataset_name ) );
	TRY( space_id = _get_diskshape_for_reading ( f, dataset_id ) );

	if ( h5u_has_view ( f ) ) {
		TRY ( nparticles = h5priv_get_selected_npoints_of_hdf5_dataspace (
			      f, space_id ) );
	}
	else {
		TRY ( nparticles = h5priv_get_npoints_of_hdf5_dataspace (
			      f, space_id ) );
	}
	TRY( h5priv_close_hdf5_dataspace( f, space_id ) );
	TRY( h5priv_close_hdf5_dataset( f, dataset_id ) );

	return (h5_int64_t) nparticles;
}

h5_int64_t
h5u_read_elems (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate dataset with */
	void *array,		/*!< [out] Array of data */
	const hid_t type
	) {

	hid_t dataset_id;
	hid_t space_id;
	hid_t memspace_id;

	if ( f->step_gid < 0 ) {
		TRY( h5_set_step ( f, f->step_idx ) );
	}
	TRY( (dataset_id = h5priv_open_hdf5_dataset ( f, f->step_gid, name ) ) );
	TRY( (space_id = _get_diskshape_for_reading ( f, dataset_id ) ) );
	TRY( (memspace_id = _get_memshape_for_reading ( f, dataset_id ) ) );
	TRY( h5priv_read_hdf5_dataset (
		     f,
		     dataset_id,
		     type,
		     memspace_id,
		     space_id,
		     f->xfer_prop,
		     array ) );
	TRY( h5priv_close_hdf5_dataspace( f, space_id ) );
	TRY( h5priv_close_hdf5_dataspace( f, memspace_id ) );
	TRY( h5priv_close_hdf5_dataset ( f, dataset_id ) );
	
	return H5_SUCCESS;
}

h5_int64_t
h5u_set_num_elements (
	h5_file_t *f,			/*!< [in] Handle to open file */
	h5_int64_t nparticles	/*!< [in] Number of particles */
	) {
	struct h5u_fdata *u = f->u;
	CHECK_FILEHANDLE( f );

#ifndef PARALLEL_IO
	/*
	  if we are not using parallel-IO, there is enough information
	   to know that we can short circuit this routine.  However,
	   for parallel IO, this is going to cause problems because
	   we don't know if things have changed globally
	*/
	if ( u->nparticles == nparticles ) {
		return H5_SUCCESS;
	}
#endif
	TRY( h5priv_close_hdf5_dataspace( f, u->diskshape ) );
	TRY( h5priv_close_hdf5_dataspace( f, u->memshape ) );
	TRY( h5priv_close_hdf5_dataspace( f, u->shape ) );
	u->diskshape = H5S_ALL;
	u->memshape = H5S_ALL;
	u->shape = H5S_ALL;
	u->nparticles =(hsize_t) nparticles;
#ifndef PARALLEL_IO
	TRY( u->shape = h5priv_create_hdf5_dataspace (
		     f,
		     1,
		     &(u->nparticles),
		     NULL ) );
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

	hsize_t start[1];

	hsize_t stride[1];
	hsize_t count[1];
	hsize_t total;
	hsize_t dmax = H5S_UNLIMITED;
	register int i;

	/*
	  acquire the number of particles to be written from each MPI process
	*/

	TRY ( h5priv_mpi_allgather (
		      f,
		      &nparticles, 1, MPI_LONG_LONG,
		      f->u->pnparticles, 1, MPI_LONG_LONG,
		      f->comm ) );

	if ( f->myproc == 0 ) {
		h5_debug ( f, "Particle offsets:" );
		for(i=0;i<f->nprocs;i++) 
			h5_debug ( f, 
				   "\tnp=%lld",
				   (long long) f->u->pnparticles[i] );
	}
	/* should I create a selection here? */

	/* compute start offsets */
	stride[0] = 1;
	start[0] = 0;
	for (i=0; i<f->myproc; i++) {
		start[0] += f->u->pnparticles[i];
	}
	
        /* compute total nparticles */
	total = 0;
	for (i=0; i < f->nprocs; i++) {
		total += f->u->pnparticles[i];
	}

	/* declare overall datasize */
	TRY ( f->u->shape = h5priv_create_hdf5_dataspace ( f, 1, &total, &total ) );

	/* declare overall data size  but then will select a subset */
	TRY ( f->u->diskshape = h5priv_create_hdf5_dataspace ( f, 1, &total, &total) );

	/* declare local memory datasize */
	TRY ( f->u->memshape = h5priv_create_hdf5_dataspace (
		      f, 1, &(f->u->nparticles), &dmax ) );

	count[0] = nparticles;
	TRY ( h5priv_select_hyperslab_of_hdf5_dataspace (
		      f,
		      f->u->diskshape,
		      H5S_SELECT_SET,
		      start, stride, count,
		      NULL ) );

	if ( f->step_gid < 0 ) {
		TRY ( h5_set_step ( f, 0 ) );
	}
#endif
	return H5_SUCCESS;
}

h5_int64_t
h5u_write_data (
	h5_file_t *f,		/*!< IN: Handle to open file */
	const char *name,	/*!< IN: Name to associate array with */
	const void *array,	/*!< IN: Array to commit to disk */
	const hid_t type	/*!< IN: Type of data */
	) {

	CHECK_FILEHANDLE ( f );
	CHECK_WRITABLE_MODE( f );
	CHECK_TIMEGROUP( f );
	struct h5u_fdata *u = f->u;

	return h5_write_data(
		f,
		name,
		array,
		type,
		f->step_gid,
		u->memshape,
		u->diskshape );
}

h5_int64_t
h5u_reset_view (
	h5_file_t *f
	) {	     

	struct h5u_fdata *u = f->u;

	u->viewstart = -1;
	u->viewend = -1;
	TRY( h5priv_close_hdf5_dataspace( f, u->shape ) );
	u->shape = H5S_ALL;
	TRY( h5priv_close_hdf5_dataspace( f, u->diskshape ) );
	u->diskshape = H5S_ALL;
	TRY( h5priv_close_hdf5_dataspace( f, u->memshape ) );
	u->memshape = H5S_ALL;

	return H5_SUCCESS;
}

h5_int64_t
h5u_set_view (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	h5_int64_t start,		/*!< [in]  Start particle */
	h5_int64_t end		/*!< [in]  End particle */
	) {
	h5_int64_t herr = 0;
	hsize_t total;
	hsize_t stride = 1;
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
	TRY ( total = (hsize_t) h5u_get_num_elems ( f ) );

	if ( start == -1 ) start = 0;
	if ( end == -1 )   end = total;

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
	
	/* declare overall datasize */
	TRY ( u->shape = h5priv_create_hdf5_dataspace ( f, 1, &total, &total ) );

	/* declare overall data size  but then will select a subset */
	TRY ( u->diskshape= h5priv_create_hdf5_dataspace ( f, 1, &total, &total ) );

	/* declare local memory datasize */
	TRY ( u->memshape = h5priv_create_hdf5_dataspace (
		      f, 1, &(u->nparticles), &dmax ) );

	TRY ( h5priv_select_hyperslab_of_hdf5_dataspace ( 
		      f,
		      u->diskshape,
		      H5S_SELECT_SET,
		      (hsize_t*)&start, &stride, &total,
		      NULL ) );

	return H5_SUCCESS;
}

h5_int64_t 
h5u_get_view (
	h5_file_t *f,
	h5_int64_t *start,
	h5_int64_t *end
	) {
	struct h5u_fdata *u = f->u;
	h5_int64_t viewstart = 0;
	h5_int64_t viewend = 0;

	if ( u->viewstart >= 0 )
		viewstart = u->viewstart;

	if ( u->viewend >= 0 ) {
		viewend = u->viewend;
	}
	else {
		viewend = h5u_get_num_elems ( f );
		if ( viewend < 0 )
			return HANDLE_H5_GET_NUM_PARTICLES_ERR ( f, viewend );
	}

	if ( start ) *start = viewstart;
	if ( end )   *end = viewend;

	return viewend - viewstart;
}

h5_int64_t
h5u_set_canonical_view (
	h5_file_t *f
	) {
	h5_int64_t herr = h5u_reset_view ( f );
	if ( herr < 0 ) return HANDLE_H5_SET_VIEW_ERR( f, herr, -1, -1 );

#ifdef PARALLEL_IO
	h5_int64_t start = 0;
	h5_int64_t end = 0;
	h5_int64_t n = 0;
	int i = 0;
	
	TRY ( n = h5u_get_num_elems ( f ) );
	/* 
	   now lets query the attributes for this group to see if there
	   is a 'pnparticles' group that contains the offsets for the
	   processors.
	*/
	if ( h5_read_attrib (
		     f,
		     f->step_gid,
		     "pnparticles", f->u->pnparticles ) < 0) {
		/*
		  Attribute "pnparticles" is not available.  So
		  subdivide the view into NP mostly equal pieces
		*/
		n /= f->nprocs;
		for ( i=0; i<f->nprocs; i++ ) {
			f->u->pnparticles[i] = n;
		}
	}

	for ( i = 0; i < f->myproc; i++ ){
		start += f->u->pnparticles[i];
	}
	end = start + f->u->pnparticles[f->myproc] - 1;
	TRY ( h5u_set_view ( f, start, end ) );

#endif

	return H5_SUCCESS;
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

	h5_int64_t herr = hdf5_get_object_name (
		f->file,
		f->step_name,
		H5G_DATASET,
		idx,
		dataset_name,
		len_dataset_name );
	if ( herr < 0 ) return herr;

	if ( nelem ) {
		*nelem = h5u_get_num_elems ( f );
		if ( *nelem < 0 ) return *nelem;
	}

	if ( type ) {
		*type = h5_get_dataset_type( f, f->step_gid, dataset_name );
		if ( *type < 0 ) return *type;
	}

	return H5_SUCCESS;
}
