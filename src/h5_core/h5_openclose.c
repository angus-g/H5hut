#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "h5_core.h"
#include "h5_core_private.h"

/*!
  \ingroup h5_core
  \defgroup h5_core_filehandling
*/

/*!
  \ingroup h5_core_filehandling

  Check whether \c f points to a valid file handle.

  \return	H5_SUCCESS or error code
*/
h5_err_t
h5_check_filehandle (
	h5_file_t * const f	/*!< filehandle  to check validity of */
	) {

	if ( f == NULL )
		return HANDLE_H5_BADFD_ERR( f );
	if ( f->file == 0 )
		return HANDLE_H5_BADFD_ERR( f );

	return H5_SUCCESS;
}


/*!
  Initialize H5Part
*/
static herr_t
_h5_error_handler (
	hid_t estack_id,
	void* __f
	) {
	if ( h5_get_debuglevel () >= 5 ) {
		H5Eprint (estack_id, stderr);
	}
	return 0;
}

/*!
  \ingroup h5_private

  \internal

  Initialize unstructured data internal data structure.

  \return	H5_SUCCESS or error code
*/
static h5_int64_t
_h5u_open_file (
	h5_file_t *f			/*!< IN: file handle */
	) {
	f->u = (struct h5u_fdata*) malloc( sizeof (*f->u) );
	if ( f->u == NULL ) {
		return HANDLE_H5_NOMEM_ERR( f );
	}
	struct h5u_fdata *u = f->u;

 	u->shape = 0;
	u->diskshape = H5S_ALL;
	u->memshape = H5S_ALL;
	u->viewstart = -1;
	u->viewend = -1;
	u->pnparticles =
		(h5_int64_t*) malloc (f->nprocs * sizeof (h5_int64_t));
	if (u->pnparticles == NULL) {
		return HANDLE_H5_NOMEM_ERR( f );
	}
	return H5_SUCCESS;
}

/*!
  \ingroup h5_private

  \internal

  Initialize H5Block internal structure.

  \return	H5_SUCCESS or error code
*/
static h5_int64_t
_h5b_open_file (
	h5_file_t *f			/*!< IN: file handle */
	) {
	struct h5b_fdata *b; 

	if ( (f == 0) || (f->file == 0) ) return HANDLE_H5_BADFD_ERR( f );
	if ( f->b ) return H5_SUCCESS;

	f->b = (struct h5b_fdata*) malloc( sizeof (*f->b) );
	if ( f->b == NULL ) {
		return HANDLE_H5_NOMEM_ERR( f );
	}
	b = f->b;
	memset ( b, 0, sizeof (*b) );
	b->user_layout = (struct h5b_partition*) malloc (
		f->nprocs * sizeof (b->user_layout[0]) );
	if ( b->user_layout == NULL ) {
		return HANDLE_H5_NOMEM_ERR( f );
	}
	b->write_layout = (struct h5b_partition*) malloc (
		f->nprocs * sizeof (b->write_layout[0]) );
	if ( b->write_layout == NULL ) {
		return HANDLE_H5_NOMEM_ERR( f );
	}
	b->step_idx = -1;
	b->blockgroup = -1;
	b->shape = -1;
	b->diskshape = -1;
	b->memshape = -1;
	b->field_group_id = -1;
	b->have_layout = 0;

	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_filehandling
  
  Open file with name \c filename. This function is available in the paralell
  and serial version. In the serial case \c comm may have any value.

  \return File handle.
  \return NULL on error.
*/

h5_err_t
_h5_open_file (
	h5_file_t * const f,
	const char *filename,	/*!< The name of the data file to open. */
	h5_int32_t flags,	/*!< The access mode for the file. */
	MPI_Comm comm		/*!< MPI communicator */
	) {
	h5_info ( f, "Opening file %s.", filename );

	TRY ( _h5_set_errorhandler ( f, H5E_DEFAULT, _h5_error_handler, NULL ) );

	f->prefix_step_name = strdup ( H5PART_GROUPNAME_STEP );
	if( f->prefix_step_name == NULL )
		return HANDLE_H5_NOMEM_ERR( f );

	f->width_step_idx = 0;

	f->xfer_prop = f->create_prop = f->access_prop = H5P_DEFAULT;

	f->comm = 0;		/* init values for serial case */
	f->nprocs = 1;
	f->myproc = 0;

#ifdef PARALLEL_IO
	f->comm = comm;
	if (MPI_Comm_size (comm, &f->nprocs) != MPI_SUCCESS) {
		return HANDLE_MPI_COMM_SIZE_ERR;
	}
	if (MPI_Comm_rank (comm, &f->myproc) != MPI_SUCCESS) {
		return HANDLE_MPI_COMM_RANK_ERR;
	}
	
	
	/* for the SP2... perhaps different for linux */
	MPI_Info info = MPI_INFO_NULL;
	
	/* ks: IBM_large_block_io */
	MPI_Info_create(&info);
	MPI_Info_set(info, "IBM_largeblock_io", "true" );
	if (H5Pset_fapl_mpio (f->access_prop, comm, info) < 0) {
		return HANDLE_H5P_SET_FAPL_MPIO_ERR;
	}
	MPI_Info_free(&info);
	
	TRY ( f->access_prop = _h5_create_property ( f, H5P_FILE_ACCESS ) );

	/*TRY ( f->create_prop = _h5_create_property ( f, H5P_FILE_CREATE) );*/
	f->create_prop = H5P_DEFAULT;

	/* xfer_prop:  also used for parallel I/O, during actual writes
	   rather than the access_prop which is for file creation. */
	TRY ( f->xfer_prop = _h5_create_property ( f, H5P_DATASET_XFER ) );
	
#ifdef COLLECTIVE_IO
	if (H5Pset_dxpl_mpio (f->xfer_prop,H5FD_MPIO_COLLECTIVE) < 0) {
		return HANDLE_H5P_SET_DXPL_MPIO_ERR;
		}
#endif /* COLLECTIVE_IO */

#endif /* PARALLEL_IO */

	if ( flags == H5_O_RDONLY ) {
		f->file = H5Fopen (filename, H5F_ACC_RDONLY, f->access_prop);
	}
	else if ( flags == H5_O_WRONLY ){
		f->file = H5Fcreate (filename, H5F_ACC_TRUNC, f->create_prop,
				     f->access_prop);
		f->empty = 1;
	}
	else if ( flags == H5_O_APPEND || flags == H5_O_RDWR ) {
		int fd = open (filename, O_RDONLY, 0);
		if ( (fd == -1) && (errno == ENOENT) ) {
			f->file = H5Fcreate(filename, H5F_ACC_TRUNC,
					    f->create_prop, f->access_prop);
			f->empty = 1;
		}
		else if (fd != -1) {
			close (fd);
			f->file = H5Fopen (filename, H5F_ACC_RDWR,
					   f->access_prop);
		}
	}
	else {
		return HANDLE_H5_FILE_ACCESS_TYPE_ERR ( f, flags );
	}
	
	if (f->file < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot open file \"%s\" with mode \"%d\"",
			filename, flags );
	TRY ( f->root_gid = _h5_open_group ( f,  f->file, "/" ) );
	f->mode = flags;
	f->step_gid = -1;

	sprintf (
		f->step_name,
		"%s#%0*lld",
		f->prefix_step_name, f->width_step_idx, (long long) f->step_idx );

	TRY ( _h5u_open_file ( f ) );
	TRY ( _h5b_open_file ( f ) );
	TRY ( _h5t_open_file ( f ) );
	return H5_SUCCESS;
}
 
h5_file_t *
h5_open_file (
	const char *filename,	/*!< The name of the data file to open. */
	h5_int32_t flags,	/*!< The access mode for the file. */
	MPI_Comm comm,		/*!< MPI communicator */
	const char *funcname	/*!< calling function name */
	) {


	h5_file_t *f = NULL;

	f = (h5_file_t*) malloc( sizeof (h5_file_t) );
	if( f == NULL ) {
		fprintf(
			stderr,
			"E: %s: Can't open file %s. Not enough memory!",
			funcname,
			filename );
		return NULL;
	}
	memset (f, 0, sizeof (h5_file_t));
	f->__funcname = funcname;
	if ( _h5_open_file( f, filename, flags, comm ) < 0 ) {
		if (f != NULL ) {
			if (f->prefix_step_name) {
				free (f->prefix_step_name);
			}
			if (f->u->pnparticles != NULL) {
				free (f->u->pnparticles);
			}
			free (f);
		}
		return NULL;
	}
	return f;
}

/*!
  \ingroup h5_private

  \internal

  De-initialize H5Block internal structure.  Open HDF5 objects are 
  closed and allocated memory freed.

  \return	H5_SUCCESS or error code
*/
static h5_int64_t
_h5u_close_file (
	h5_file_t *f		/*!< file handle */
	) {
	struct h5u_fdata *u = f->u;

	f->__errno = H5_SUCCESS;
	if( u->shape > 0 ) {
		TRY( _h5_close_dataspace( f, u->shape ) );
		u->shape = 0;
	}
	if( u->diskshape != H5S_ALL ) {
		TRY( _h5_close_dataspace( f, u->diskshape ) );
		u->diskshape = 0;
	}
	if( u->memshape != H5S_ALL ) {
		TRY( _h5_close_dataspace( f, u->memshape ) );
		u->memshape = 0;
	}
	if( u->pnparticles ) {
		free( u->pnparticles );
	}
	return f->__errno;
}

/*!
  \ingroup h5block_private

  \internal

  De-initialize H5Block internal structure.  Open HDF5 objects are 
  closed and allocated memory freed.

  \return	H5_SUCCESS or error code
*/
static h5_int64_t
_h5b_close_file (
	h5_file_t *f		/*!< IN: file handle */
	) {
	struct h5b_fdata *b = f->b;

	TRY ( _h5_close_group( f, b->blockgroup ) );
	TRY ( _h5_close_dataspace( f, b->shape ) );
	TRY ( _h5_close_dataspace( f, b->diskshape ) );
	TRY ( _h5_close_dataspace( f, b->memshape ) );
	free ( f->b );
	f->b = NULL;

	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_filehandling

  The h5_close_file() call writes all buffered data to disk, releases 
  all previously allocated memory and terminates access to the associated
  HDF5 file.

  \return	H5_SUCCESS or error code
*/
h5_err_t
h5_close_file (
	h5_file_t *f		/*!< file handle */
	) {
	f->__errno = H5_SUCCESS;

	CHECK_FILEHANDLE ( f );

	TRY( _h5_close_step ( f ) );
	TRY( _h5u_close_file ( f ) );
	TRY( _h5b_close_file ( f ) );
	TRY( _h5t_close_file ( f ) );
	TRY( _h5_close_group( f, f->step_gid ) );
	TRY( _h5_close_property ( f, f->xfer_prop ) );
	TRY( _h5_close_property ( f, f->access_prop ) );
	TRY( _h5_close_property ( f, f->create_prop ) );
	TRY( _h5_close_group ( f, f->root_gid ) );
	TRY( _h5_close_file ( f, f->file ) );

	if (f->prefix_step_name) {
		free (f->prefix_step_name);
	}
	free( f );

	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_filehandling

  Define format of the step names.

  Example: ==H5FedDefineStepNameFormat( f, "Step", 6 )== defines step names 
  like ==Step#000042==.

  \return \c H5_SUCCESS or error code
*/
h5_int64_t
h5_set_stepname_fmt (
	h5_file_t *f,
	const char *name,
	const h5_int64_t width
	) {
	f->prefix_step_name = strdup ( name );
	if( f->prefix_step_name == NULL ) {
		return HANDLE_H5_NOMEM_ERR( f );
	}
	f->width_step_idx = (int)width;
	
	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_filehandling

  Get format of the step names.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
h5_get_stepname_fmt (
	h5_file_t *f,			/*!< Handle to file		*/
	char *name,			/*!< OUT: Prefix		*/
	const h5_size_t l_name,		/*!< length of buffer name	*/
	h5_size_t *width		/*!< OUT: Width of the number	*/
	) {
	return -1;
}

/*!
  \ingroup h5_core_filehandling

  Get current step number.

  \return Current step number or error code
*/
h5_id_t
h5_get_step (
	h5_file_t * f			/*!< file handle		*/
	) {
	return -1;
}
	
/*!
  \ingroup h5_core_filehandling

  Check whether step with number \c stepno exists.

  \return True (value != 0) if step with \c stepno exists.
  \return False (0) otherwise
*/
h5_err_t
h5_has_step (
	h5_file_t * f,			/*!< file handle		*/
	h5_id_t stepno			/*!< step number to check	*/
	) {
	char name[128];
        sprintf ( name, "%s#%0*ld",
		  f->prefix_step_name, f->width_step_idx, (long) stepno );
	return ( H5Gget_info_by_name( f->file, name, NULL, H5P_DEFAULT ) >= 0 );
}

/*!
  \ingroup h5_core_filehandling

  Start traversing steps.

  \return \c H5_SUCCESS or error code 
*/
h5_err_t
h5_start_traverse_steps (
	h5_file_t * f			/*!< file handle		*/
	) {
	return -1;
}

/*!
  \ingroup h5_core_filehandling

  Go to next step.

  \return \c H5_SUCCESS or error code 
*/
h5_err_t
h5_traverse_steps (
	h5_file_t * f			/*!< file handle		*/
	) {
	return -1;
}
