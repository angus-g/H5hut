#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core.h"
#include "h5_private.h"
#include "H5Part.h"

extern h5part_error_handler	_err_handler;
extern h5part_int64_t		_h5part_errno;
extern unsigned			_debug;

static hid_t
_get_diskshape_for_reading (
	h5_file *f,
	hid_t dataset
	) {

	herr_t r;

	hid_t space = H5Dget_space(dataset);
	if ( space < 0 ) return (hid_t)HANDLE_H5D_GET_SPACE_ERR;

	if ( H5PartHasView(f) ){ 
		hsize_t stride;
		hsize_t count;
#ifdef HDF5V160
		hssize_t start;
#else
		hsize_t start;
#endif

		/* so, is this selection inclusive or exclusive? */
		start = f->viewstart;
		count = f->viewend - f->viewstart; /* to be inclusive */
		stride=1;

		/* now we select a subset */
		if ( f->diskshape > 0 ) {
			r = H5Sselect_hyperslab (
				f->diskshape, H5S_SELECT_SET,
				&start, &stride, &count, NULL);
			if ( r < 0 ) return (hid_t)HANDLE_H5S_SELECT_HYPERSLAB_ERR;
		}
		/* now we select a subset */
		r = H5Sselect_hyperslab (
			space,H5S_SELECT_SET,
			&start, &stride, &count, NULL );
		if ( r < 0 ) return (hid_t)HANDLE_H5S_SELECT_HYPERSLAB_ERR;

		h5_print_debug (
			"Selection: range=%d:%d, npoints=%d s=%d",
			(int)f->viewstart,(int)f->viewend,
			(int)H5Sget_simple_extent_npoints(space),
			(int)H5Sget_select_npoints(space) );
	}
	return space;
}

static hid_t
_get_memshape_for_reading (
	h5_file *f,
	hid_t dataset
	) {

	if(H5PartHasView(f)) {
		hsize_t dmax=H5S_UNLIMITED;
		hsize_t len = f->viewend - f->viewstart;
		hid_t r = H5Screate_simple(1,&len,&dmax);
		if ( r < 0 ) return (hid_t)HANDLE_H5S_CREATE_SIMPLE_ERR ( 1 );
		return r;
	}
	else {
		return H5S_ALL;
	}
}

h5part_int64_t
H5U_get_num_elems (
	h5_file *f			/*!< [in]  Handle to open file */
	) {

	h5part_int64_t herr;
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

	herr = h5_get_object_name (
		f->file,
		step_name,
		H5G_DATASET,
		0,
		dataset_name, sizeof (dataset_name) );
	if ( herr < 0 ) return herr;

	dataset_id = H5Dopen ( f->step_gid, dataset_name );
	if ( dataset_id < 0 ) 
		return HANDLE_H5D_OPEN_ERR ( dataset_name );

	space_id = _get_diskshape_for_reading ( f, dataset_id );
	if ( space_id < 0 ) return (h5part_int64_t)space_id;

	if ( H5PartHasView ( f ) ) {
		nparticles = H5Sget_select_npoints ( space_id );
		if ( nparticles < 0 ) return HANDLE_H5S_GET_SELECT_NPOINTS_ERR;
	}
	else {
		nparticles = H5Sget_simple_extent_npoints ( space_id );
		if ( nparticles < 0 )
			return HANDLE_H5S_GET_SIMPLE_EXTENT_NPOINTS_ERR;
	}
	if ( space_id != H5S_ALL ) {
		herr = H5Sclose ( space_id );
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;
	}
	herr = H5Dclose ( dataset_id );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR;

	return (h5part_int64_t) nparticles;
}

h5part_int64_t
H5U_read_elems (
	h5_file *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate dataset with */
	void *array,		/*!< [out] Array of data */
	const hid_t type
	) {

	herr_t herr;
	hid_t dataset_id;
	hid_t space_id;
	hid_t memspace_id;

	if ( f->step_gid < 0 ) {
		h5part_int64_t h5err = h5_set_step ( f, f->step_idx );
		if ( h5err < 0 ) return h5err;
	}
	dataset_id = H5Dopen ( f->step_gid, name );
	if ( dataset_id < 0 ) return HANDLE_H5D_OPEN_ERR ( name );

	space_id = _get_diskshape_for_reading ( f, dataset_id );
	if ( space_id < 0 ) return (h5part_int64_t)space_id;

	memspace_id = _get_memshape_for_reading ( f, dataset_id );
	if ( memspace_id < 0 ) return (h5part_int64_t)memspace_id;

	herr = H5Dread (
		dataset_id,
		type,
		memspace_id,		/* shape/size of data in memory (the
					   complement to disk hyperslab) */
		space_id,		/* shape/size of data on disk 
					   (get hyperslab if needed) */
		f->xfer_prop,		/* ignore... its for parallel reads */
		array );
	if ( herr < 0 ) return HANDLE_H5D_READ_ERR ( name );

	if ( space_id != H5S_ALL ) {
		herr = H5Sclose (space_id );
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;
	}

	if ( memspace_id != H5S_ALL )
		herr = H5Sclose ( memspace_id );
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;

	herr = H5Dclose ( dataset_id );
	if ( herr < 0 ) return HANDLE_H5D_CLOSE_ERR;
	
	return H5PART_SUCCESS;
}

h5part_int64_t
H5U_set_num_elements (
	h5_file *f,			/*!< [in] Handle to open file */
	h5part_int64_t nparticles	/*!< [in] Number of particles */
	) {

	CHECK_FILEHANDLE( f );

	herr_t r = 0;

#ifndef PARALLEL_IO
	/*
	  if we are not using parallel-IO, there is enough information
	   to know that we can short circuit this routine.  However,
	   for parallel IO, this is going to cause problems because
	   we don't know if things have changed globally
	*/
	if ( f->nparticles == nparticles ) {
		return H5PART_SUCCESS;
	}
#endif
	if ( f->diskshape != H5S_ALL ) {
		r = H5Sclose( f->diskshape );
		if ( r < 0 ) return HANDLE_H5S_CLOSE_ERR;
		f->diskshape = H5S_ALL;
	}
	if(f->memshape != H5S_ALL) {
		r = H5Sclose( f->memshape );
		if ( r < 0 ) return HANDLE_H5S_CLOSE_ERR;
		f->memshape = H5S_ALL;
	}
	if( f->shape ) {
		r = H5Sclose(f->shape);
		if ( r < 0 ) return HANDLE_H5S_CLOSE_ERR;
	}
	f->nparticles =(hsize_t) nparticles;
#ifndef PARALLEL_IO
	f->shape = H5Screate_simple (1,
				     &(f->nparticles),
				     NULL);
	if ( f->shape < 0 ) HANDLE_H5S_CREATE_SIMPLE_ERR ( 1 );

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

	r = MPI_Allgather (
		&nparticles, 1, MPI_LONG_LONG,
		f->pnparticles, 1, MPI_LONG_LONG,
		f->comm);
	if ( r != MPI_SUCCESS) {
		return HANDLE_MPI_ALLGATHER_ERR;
	}
	if ( f->myproc == 0 ) {
		h5_print_debug ( "Particle offsets:" );
		for(i=0;i<f->nprocs;i++) 
			h5_print_debug ( "\tnp=%lld",
					      (long long) f->pnparticles[i] );
	}
	/* should I create a selection here? */

	/* compute start offsets */
	stride[0] = 1;
	start[0] = 0;
	for (i=0; i<f->myproc; i++) {
		start[0] += f->pnparticles[i];
	}
	
        /* compute total nparticles */
	total = 0;
	for (i=0; i < f->nprocs; i++) {
		total += f->pnparticles[i];
	}

	/* declare overall datasize */
	f->shape = H5Screate_simple (1, &total, &total);
	if (f->shape < 0) return HANDLE_H5S_CREATE_SIMPLE_ERR ( 1 );


	/* declare overall data size  but then will select a subset */
	f->diskshape = H5Screate_simple (1, &total, &total);
	if (f->diskshape < 0) return HANDLE_H5S_CREATE_SIMPLE_ERR ( 1 );

	/* declare local memory datasize */
	f->memshape = H5Screate_simple (1, &(f->nparticles), &dmax);
	if (f->memshape < 0)
		return HANDLE_H5S_CREATE_SIMPLE_ERR ( 1 );

	count[0] = nparticles;
	r = H5Sselect_hyperslab (
		f->diskshape,
		H5S_SELECT_SET,
		start,
		stride,
		count, NULL );
	if ( r < 0 ) return HANDLE_H5S_SELECT_HYPERSLAB_ERR;

	if ( f->step_gid < 0 ) {
		r = h5_set_step ( f, 0 );
		if ( r < 0 ) return r;
		
	}
#endif
	return H5PART_SUCCESS;
}

h5part_int64_t
H5U_write_data (
	h5_file *f,		/*!< IN: Handle to open file */
	const char *name,	/*!< IN: Name to associate array with */
	const void *array,	/*!< IN: Array to commit to disk */
	const hid_t type	/*!< IN: Type of data */
	) {

	CHECK_FILEHANDLE ( f );
	CHECK_WRITABLE_MODE( f );
	CHECK_TIMEGROUP( f );

	return h5_write_data(
		f,
		name,
		array,
		type,
		f->step_gid,
		f->shape,
		f->memshape,
		f->diskshape );
}

h5part_int64_t
H5U_reset_view (
	h5_file *f
	) {	     

	herr_t herr = 0;

	f->viewstart = -1;
	f->viewend = -1;
	if ( f->shape != 0 ){
		herr = H5Sclose(f->shape);
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;
		f->shape=0;
	}
	if(f->diskshape!=0 && f->diskshape!=H5S_ALL){
		herr = H5Sclose(f->diskshape);
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;
		f->diskshape=H5S_ALL;
	}
	f->diskshape = H5S_ALL;
	if(f->memshape!=0 && f->memshape!=H5S_ALL){
		herr = H5Sclose ( f->memshape );
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;
		f->memshape=H5S_ALL;
	}
	return H5PART_SUCCESS;
}

h5part_int64_t
H5U_set_view (
	h5_file *f,			/*!< [in]  Handle to open file */
	h5part_int64_t start,		/*!< [in]  Start particle */
	h5part_int64_t end		/*!< [in]  End particle */
	) {
	h5part_int64_t herr = 0;
	hsize_t total;
	hsize_t stride = 1;
	hsize_t dmax = H5S_UNLIMITED;

	h5_print_debug (
		"Set view (%lld,%lld).",
		(long long)start,(long long)end);

	herr = H5U_reset_view ( f );
	if ( herr < 0 ) return herr;

	if ( start == -1 && end == -1 ) return H5PART_SUCCESS;

	/*
	  View has been reset so H5PartGetNumParticles will tell
	  us the total number of particles.

	  For now, we interpret start=-1 to mean 0 and 
	  end==-1 to mean end of file
	*/
	total = (hsize_t) H5U_get_num_elems ( f );
	if ( total < 0 ) return HANDLE_H5_GET_NUM_PARTICLES_ERR ( total );

	if ( start == -1 ) start = 0;
	if ( end == -1 )   end = total;

	h5_print_debug ( "Total nparticles=%lld", (long long)total );

	/* so, is this selection inclusive or exclusive? 
	   it appears to be inclusive for both ends of the range.
	*/
	if ( end < start ) {
		h5_print_warn (
			"Nonfatal error. "
			"End of view (%lld) is less than start (%lld).",
			(long long)end, (long long)start );
		end = start; /* ensure that we don't have a range error */
	}
	/* setting up the new view */
	f->viewstart =  start;
	f->viewend =    end;
	f->nparticles = end - start + 1;
	
	/* declare overall datasize */
	f->shape = H5Screate_simple ( 1, &total, &total );
	if ( f->shape < 0 )
		return HANDLE_H5S_CREATE_SIMPLE_ERR ( 1 );

	/* declare overall data size  but then will select a subset */
	f->diskshape= H5Screate_simple ( 1, &total, &total );
	if ( f->diskshape < 0 )
		return HANDLE_H5S_CREATE_SIMPLE_ERR ( 1 );

	/* declare local memory datasize */
	f->memshape = H5Screate_simple(1,&(f->nparticles),&dmax);
	if ( f->memshape < 0 )
		return HANDLE_H5S_CREATE_SIMPLE_ERR ( 1 );

	herr = H5Sselect_hyperslab ( 
		f->diskshape,
		H5S_SELECT_SET,
		(hsize_t*)&start,
		&stride,
		&total,
		NULL );
	if ( herr < 0 ) return HANDLE_H5S_SELECT_HYPERSLAB_ERR;

	return H5PART_SUCCESS;
}

h5part_int64_t 
H5U_get_view (
	h5_file *f,
	h5part_int64_t *start,
	h5part_int64_t *end
	) {

	h5part_int64_t viewstart = 0;
	h5part_int64_t viewend = 0;

	if ( f->viewstart >= 0 )
		viewstart = f->viewstart;

	if ( f->viewend >= 0 ) {
		viewend = f->viewend;
	}
	else {
		viewend = H5U_get_num_elems ( f );
		if ( viewend < 0 )
			return HANDLE_H5_GET_NUM_PARTICLES_ERR ( viewend );
	}

	if ( start ) *start = viewstart;
	if ( end )   *end = viewend;

	return viewend - viewstart;
}

h5part_int64_t
H5U_set_canonical_view (
	h5_file *f
	) {
	h5part_int64_t herr = H5U_reset_view ( f );
	if ( herr < 0 ) return HANDLE_H5_SET_VIEW_ERR( herr, -1, -1 );

#ifdef PARALLEL_IO
	h5part_int64_t start = 0;
	h5part_int64_t end = 0;
	h5part_int64_t n = 0;
	int i = 0;
	
	n = H5U_get_num_elems ( f );
	if ( n < 0 ) return HANDLE_H5_GET_NUM_PARTICLES_ERR ( n );
	/* 
	   now lets query the attributes for this group to see if there
	   is a 'pnparticles' group that contains the offsets for the
	   processors.
	*/
	if ( h5_read_attrib (
		     f->step_gid,
		     "pnparticles", f->pnparticles ) < 0) {
		/*
		  Attribute "pnparticles" is not available.  So
		  subdivide the view into NP mostly equal pieces
		*/
		n /= f->nprocs;
		for ( i=0; i<f->nprocs; i++ ) {
			f->pnparticles[i] = n;
		}
	}

	for ( i = 0; i < f->myproc; i++ ){
		start += f->pnparticles[i];
	}
	end = start + f->pnparticles[f->myproc] - 1;
	herr = H5U_set_view ( f, start, end );
	if ( herr < 0 ) return HANDLE_H5_SET_VIEW_ERR ( herr, start, end );

#endif

	return H5PART_SUCCESS;
}


/*!
  Get information about dataset in current index given by its index
 */
h5part_int64_t
H5U_get_dataset_info (
	h5_file *f,		/*!< [in]  Handle to open file */
	const h5part_int64_t idx,/*!< [in]  Index of the dataset */
	char *dataset_name,	/*!< [out] Name of dataset */
	const h5part_int64_t len_dataset_name,
				/*!< [in]  Size of buffer \c dataset_name */
	h5part_int64_t *type,	/*!< [out] Type of data in dataset */
	h5part_int64_t *nelem	/*!< [out] Number of elements. */
	) {

	h5part_int64_t herr = h5_get_object_name (
		f->file,
		f->step_name,
		H5G_DATASET,
		idx,
		dataset_name,
		len_dataset_name );
	if ( herr < 0 ) return herr;

	if ( nelem ) {
		*nelem = H5U_get_num_elems ( f );
		if ( *nelem < 0 ) return *nelem;
	}

	if ( type ) {
		*type = h5_get_dataset_type( f->step_gid, dataset_name );
		if ( *type < 0 ) return *type;
	}

	return H5PART_SUCCESS;
}
