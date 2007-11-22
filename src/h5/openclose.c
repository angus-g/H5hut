#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#include "H5PartTypes.h"
#include "H5BlockTypes.h"
#include "H5Part.h"
#include "H5Block.h"
#include "H5PartPrivate.h"
#include "H5BlockPrivate.h"
#include "H5PartErrors.h"
#include "H5BlockErrors.h"
#include "H5.h"

extern h5part_error_handler	_err_handler;
extern h5part_int64_t		_h5part_errno;
extern unsigned			_debug;

static h5part_int64_t
_close_block (
	H5PartFile *f
	);

/*!
  \ingroup h5block_private

  \internal

  Check whether \c f points to a valid file handle.

  \return	H5PART_SUCCESS or error code
*/
h5part_int64_t
H5_check_filehandle (
	const H5PartFile *f	/*!< filehandle  to check validity of */
	) {

	if ( f == NULL )
		return HANDLE_H5PART_BADFD_ERR;
	if ( f->file == 0 )
		return HANDLE_H5PART_BADFD_ERR;
	if ( f->block == NULL )
		return HANDLE_H5PART_BADFD_ERR;
	return H5PART_SUCCESS;
}


/*!
  Initialize H5Part
*/
static herr_t
_h5_error_handler ( void* unused ) {
	
	if ( _debug >= 5 ) {
		H5Eprint (stderr);
	}
	return 0;
}

static h5part_int64_t
_init ( void ) {
	static int __init = 0;

	herr_t r5;
	if ( ! __init ) {
		r5 = H5Eset_auto ( _h5_error_handler, NULL );
		if ( r5 < 0 ) return H5PART_ERR_INIT;
	}
	__init = 1;
	return H5PART_SUCCESS;
}

/*!
  \ingroup h5block_private

  \internal

  Initialize H5Block internal structure.

  \return	H5PART_SUCCESS or error code
*/
static h5part_int64_t
_init_block (
	H5PartFile *f			/*!< IN: file handle */
	) {
	h5part_int64_t herr;
	struct H5BlockStruct *b; 

	herr = _file_is_valid ( f );
	if ( herr == H5PART_SUCCESS ) return H5PART_SUCCESS;

	if ( (f == 0) || (f->file == 0) ) return HANDLE_H5PART_BADFD_ERR;

	f->block = (struct H5BlockStruct*) malloc( sizeof (*f->block) );
	if ( f->block == NULL ) {
		return HANDLE_H5PART_NOMEM_ERR;
	}
	b = f->block;
	memset ( b, 0, sizeof (*b) );
	b->user_layout = (struct H5BlockPartition*) malloc (
		f->nprocs * sizeof (b->user_layout[0]) );
	if ( b->user_layout == NULL ) {
		return HANDLE_H5PART_NOMEM_ERR;
	}
	b->write_layout = (struct H5BlockPartition*) malloc (
		f->nprocs * sizeof (b->write_layout[0]) );
	if ( b->write_layout == NULL ) {
		return HANDLE_H5PART_NOMEM_ERR;
	}
	b->timestep = -1;
	b->blockgroup = -1;
	b->shape = -1;
	b->diskshape = -1;
	b->memshape = -1;
	b->field_group_id = -1;
	b->have_layout = 0;

	f->close_block = _close_block;

	return H5PART_SUCCESS;
}

/*!
  \ingroup h5block_private

  \internal

  De-initialize H5Block internal structure.  Open HDF5 objects are 
  closed and allocated memory freed.

  \return	H5PART_SUCCESS or error code
*/
static h5part_int64_t
_close_block (
	H5PartFile *f		/*!< IN: file handle */
	) {

	herr_t herr;
	struct H5BlockStruct *b = f->block;

	if ( b->blockgroup >= 0 ) {
		herr = H5Gclose ( b->blockgroup );
		if ( herr < 0 ) return HANDLE_H5G_CLOSE_ERR;
		b->blockgroup = -1;
	}
	if ( b->shape >= 0 ) {
		herr = H5Sclose ( b->shape );
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;
		b->shape = -1;
	}
	if ( b->diskshape >= 0 ) {
		herr = H5Sclose ( b->diskshape );
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;
		b->diskshape = -1;
	}
	if ( b->memshape >= 0 ) {
		herr = H5Sclose ( b->memshape );
		if ( herr < 0 ) return HANDLE_H5S_CLOSE_ERR;
		b->memshape = -1;
	}
	free ( f->block );
	f->block = NULL;
	f->close_block = NULL;

	return H5PART_SUCCESS;
}


H5PartFile*
H5_open_file (
	const char *filename,	/*!< [in] The name of the data file to open. */
	unsigned flags,		/*!< [in] The access mode for the file. */
	MPI_Comm comm,		/*!< [in] MPI communicator */
	int f_parallel		/*!< [in] 0 for serial io otherwise parallel */
	) {

	h5part_int64_t rc = H5PART_SUCCESS;

	if ( _init() < 0 ) {
		HANDLE_H5PART_INIT_ERR;
		return NULL;
	}
	_h5part_errno = H5PART_SUCCESS;
	H5PartFile *f = NULL;

	f = (H5PartFile*) malloc( sizeof (H5PartFile) );
	if( f == NULL ) {
		HANDLE_H5PART_NOMEM_ERR;
		goto error_cleanup;
	}
	memset (f, 0, sizeof (H5PartFile));

	f->groupname_step = strdup ( H5PART_GROUPNAME_STEP );
	if( f->groupname_step == NULL ) {
		HANDLE_H5PART_NOMEM_ERR;
		goto error_cleanup;
	}
	f->stepno_width = 0;

	f->xfer_prop = f->create_prop = f->access_prop = H5P_DEFAULT;

	if ( f_parallel ) {
#ifdef PARALLEL_IO
		/* for the SP2... perhaps different for linux */
		MPI_Info info = MPI_INFO_NULL;

		/* ks: IBM_large_block_io */
		MPI_Info_create(&info);
		MPI_Info_set(info, "IBM_largeblock_io", "true" );

		if (MPI_Comm_size (comm, &f->nprocs) != MPI_SUCCESS) {
			HANDLE_MPI_COMM_SIZE_ERR;
			goto error_cleanup;
		}
		if (MPI_Comm_rank (comm, &f->myproc) != MPI_SUCCESS) {
			HANDLE_MPI_COMM_RANK_ERR;
			goto error_cleanup;
		}

		f->pnparticles =
		  (h5part_int64_t*) malloc (f->nprocs * sizeof (h5part_int64_t));
		if (f->pnparticles == NULL) {
			HANDLE_H5PART_NOMEM_ERR;
			goto error_cleanup;
		}
		
		f->access_prop = H5Pcreate (H5P_FILE_ACCESS);
		if (f->access_prop < 0) {
			HANDLE_H5P_CREATE_ERR;
			goto error_cleanup;
		}

		if (H5Pset_fapl_mpio (f->access_prop, comm, info) < 0) {
			HANDLE_H5P_SET_FAPL_MPIO_ERR;
			goto error_cleanup;
		}
		
		/* f->create_prop = H5Pcreate(H5P_FILE_CREATE); */
		f->create_prop = H5P_DEFAULT;

		/* xfer_prop:  also used for parallel I/O, during actual writes
		   rather than the access_prop which is for file creation. */
		f->xfer_prop = H5Pcreate (H5P_DATASET_XFER);
		if (f->xfer_prop < 0) {
			HANDLE_H5P_CREATE_ERR;
			goto error_cleanup;
		}

#ifdef COLLECTIVE_IO
		if (H5Pset_dxpl_mpio (f->xfer_prop,H5FD_MPIO_COLLECTIVE) < 0) {
			HANDLE_H5P_SET_DXPL_MPIO_ERR;
			goto error_cleanup;
		}
#endif
		f->comm = comm;

		MPI_Info_free(&info);
#else
		/* f_parallel is true, but compilation is for serial I/O */
		HANDLE_MPI_UNAVAILABLE_ERR;
		goto error_cleanup;

#endif /* PARALLEL_IO */
	} else {
		f->comm = 0;
		f->nprocs = 1;
		f->myproc = 0;
		f->pnparticles = 
			(h5part_int64_t*) malloc (f->nprocs * sizeof (h5part_int64_t));
	}
	if ( flags == H5PART_READ ) {
		f->file = H5Fopen (filename, H5F_ACC_RDONLY, f->access_prop);
	}
	else if ( flags == H5PART_WRITE ){
		f->file = H5Fcreate (filename, H5F_ACC_TRUNC, f->create_prop,
				     f->access_prop);
		f->empty = 1;
	}
	else if ( flags == H5PART_APPEND ) {
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
			/*
			  The following function call returns an error,
			  if f->file < 0. But we can safely ignore this.
			*/
			f->timestep = H5_get_num_objects_matching_pattern(
				f->file, "/", H5G_GROUP, f->groupname_step );
			if ( f->timestep < 0 ) goto error_cleanup;
		}
	}
	else {
		HANDLE_H5PART_FILE_ACCESS_TYPE_ERR ( flags );
		goto error_cleanup;
	}

	if (f->file < 0) {
		HANDLE_H5F_OPEN_ERR ( filename, flags );
		goto error_cleanup;
	}
	f->root_id = H5Gopen( f->file, "/" );
	if ( f->root_id < 0 ) {
		HANDLE_H5G_OPEN_ERR ( "/" );
		goto error_cleanup;
	}
	f->mode = flags;
	f->timegroup = -1;
	f->shape = 0;
	f->diskshape = H5S_ALL;
	f->memshape = H5S_ALL;
	f->viewstart = -1;
	f->viewend = -1;

	sprintf (
		f->index_name,
		"%s#%0*lld",
		f->groupname_step, f->stepno_width, (long long) f->timestep );

	rc = _init_block ( f );
	if ( rc != H5PART_SUCCESS ) {
		goto error_cleanup;
	}

	H5_print_debug (
		"Proc[%d]: Opened file \"%s\" val=%lld",
		f->myproc,
		filename,
		(long long)(size_t)f );

	return f;

 error_cleanup:
	if (f != NULL ) {
		if (f->groupname_step) {
			free (f->groupname_step);
		}
		if (f->pnparticles != NULL) {
			free (f->pnparticles);
		}
		free (f);
	}
	return NULL;
}


h5part_int64_t
H5_close_file (
	H5PartFile *f
	) {
	herr_t r = 0;
	_h5part_errno = H5PART_SUCCESS;

	CHECK_FILEHANDLE ( f );

	if ( f->block && f->close_block ) {
		(*f->close_block) ( f );
		f->block = NULL;
		f->close_block = NULL;
	}

	if( f->shape > 0 ) {
		r = H5Sclose( f->shape );
		if ( r < 0 ) HANDLE_H5S_CLOSE_ERR;
		f->shape = 0;
	}
	if( f->timegroup >= 0 ) {
		r = H5Gclose( f->timegroup );
		if ( r < 0 ) HANDLE_H5G_CLOSE_ERR;
		f->timegroup = -1;
	}
	if( f->diskshape != H5S_ALL ) {
		r = H5Sclose( f->diskshape );
		if ( r < 0 ) HANDLE_H5S_CLOSE_ERR;
		f->diskshape = 0;
	}
	if( f->xfer_prop != H5P_DEFAULT ) {
		r = H5Pclose( f->xfer_prop );
		if ( r < 0 ) HANDLE_H5P_CLOSE_ERR ( "f->xfer_prop" );
		f->xfer_prop = H5P_DEFAULT;
	}
	if( f->access_prop != H5P_DEFAULT ) {
		r = H5Pclose( f->access_prop );
		if ( r < 0 ) HANDLE_H5P_CLOSE_ERR ( "f->access_prop" );
		f->access_prop = H5P_DEFAULT;
	}  
	if( f->create_prop != H5P_DEFAULT ) {
		r = H5Pclose( f->create_prop );
		if ( r < 0 ) HANDLE_H5P_CLOSE_ERR ( "f->create_prop" );
		f->create_prop = H5P_DEFAULT;
	}
	if ( f->root_id >= 0 ) {
		r = H5Gclose ( f->root_id );
		if ( r < 0 ) HANDLE_H5G_CLOSE_ERR;
		f->root_id = 0;
	}
	if ( f->file ) {
		r = H5Fclose( f->file );
		if ( r < 0 ) HANDLE_H5F_CLOSE_ERR;
		f->file = 0;
	}
	if (f->groupname_step) {
		free (f->groupname_step);
	}
	if( f->pnparticles ) {
		free( f->pnparticles );
	}
	free( f );

	return _h5part_errno;
}

h5part_int64_t
H5_define_stepname (
	H5PartFile *f,
	const char *name,
	const h5part_int64_t width
	) {
	f->groupname_step = strdup ( name );
	if( f->groupname_step == NULL ) {
		return HANDLE_H5PART_NOMEM_ERR;
	}
	f->stepno_width = (int)width;
	
	return H5PART_SUCCESS;
}
