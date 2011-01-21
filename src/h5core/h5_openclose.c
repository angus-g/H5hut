#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>

#include "h5core/h5_core.h"
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
	h5_file_t* const f	/*!< filehandle  to check validity of */
	) {

	if (f == NULL || f->file < 0 || f->u == NULL || f->b == NULL
#ifndef PARALLEL_IO
		|| f->t == NULL
#endif
		) {
		return h5_error (
			f,
			H5_ERR_BADFD,
			"Called with bad filehandle.");
	}
	return H5_SUCCESS;
}

/*!
  Initialize H5Part
*/
static herr_t
h5priv_error_handler (
	hid_t estack_id,
	void* __f
	) {
	UNUSED_ARGUMENT (__f);
	if (h5_get_debuglevel() >= 5) {
		H5Eprint (estack_id, stderr);
	}
	return 0;
}

/*!
  \ingroup h5_private

  \internal

  Initialize unstructured data internal data structure.

  TODO: Move to file "h5u_openclose.c"

  \return	H5_SUCCESS or error code
*/
static h5_err_t
h5upriv_open_file (
	h5_file_t* const f		/*!< IN: file handle */
	) {
	TRY( f->u = (h5u_fdata_t*)h5_alloc (f, NULL, sizeof (*f->u)) );
	h5u_fdata_t *u = f->u;

        u->shape = -1;
	u->diskshape = H5S_ALL;
	u->memshape = H5S_ALL;
	u->viewstart = -1;
	u->viewend = -1;
	u->viewindexed = 0;

	TRY( u->dcreate_prop = h5priv_create_hdf5_property(f, H5P_DATASET_CREATE) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5_private

  \internal

  Initialize H5Block internal structure.

  TODO: Move to file "h5b_openclose.c"

  \return	H5_SUCCESS or error code
*/
static h5_err_t
h5bpriv_open_file (
	h5_file_t * const f		/*!< IN: file handle */
	) {
	h5b_fdata_t* b; 

	if ( f->b ) return H5_SUCCESS;

	TRY( f->b = (h5b_fdata_t*)h5_alloc( f, NULL, sizeof (*f->b)) );

	b = f->b;
	memset (b, 0, sizeof (*b));

#if defined(PARALLEL_IO)
	size_t n = sizeof (struct h5b_partition) / sizeof (h5_int64_t);
	TRY( h5priv_mpi_type_contiguous(f,
			n, MPI_LONG_LONG, &b->partition_mpi_t) );
#endif
	memset (b->user_layout, 0, sizeof(*b->user_layout));
	memset (b->write_layout, 0, sizeof(*b->write_layout));

	b->shape = -1;
	b->diskshape = -1;
	b->memshape = -1;
	b->block_gid = -1;
	b->field_gid = -1;
	b->have_layout = 0;

	TRY( b->dcreate_prop = h5priv_create_hdf5_property(f, H5P_DATASET_CREATE) );

	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_filehandling
  
  Open file with name \c filename. This function is available in the paralell
  and serial version. In the serial case \c comm may have any value.

  \return File handle.
  \return NULL on error.
*/

static h5_err_t
open_file (
	h5_file_t* const f,
	const char* filename,	/*!< The name of the data file to open. */
	h5_int32_t flags,	/*!< The access mode for the file. */
	MPI_Comm comm		/*!< MPI communicator */
	) {

#if !defined(PARALLEL_IO)
	UNUSED_ARGUMENT (f);
#endif

	h5_info (f, "Opening file %s.", filename);

	TRY( h5priv_set_hdf5_errorhandler (f, H5E_DEFAULT, h5priv_error_handler, NULL) );
	TRY( h5_set_stepname_fmt (f, H5_STEPNAME, H5_STEPWIDTH) );

	f->xfer_prop = f->create_prop = f->access_prop = H5P_DEFAULT;

	f->comm = 0;		/* init values for serial case */
	f->nprocs = 1;
	f->myproc = 0;

#ifdef PARALLEL_IO
	f->comm = comm;
	TRY( h5priv_mpi_comm_size (f, comm, &f->nprocs) );
	TRY( h5priv_mpi_comm_rank (f, comm, &f->myproc) );
	
	/* xfer_prop:  also used for parallel I/O, during actual writes
	   rather than the access_prop which is for file creation. */
	TRY( f->xfer_prop = h5priv_create_hdf5_property(f, H5P_DATASET_XFER) );
	TRY( f->access_prop = h5priv_create_hdf5_property(f, H5P_FILE_ACCESS) );
	TRY( f->create_prop = h5priv_create_hdf5_property(f, H5P_FILE_CREATE) );

	/* select the HDF5 VFD */
	if (flags & H5_VFD_MPIPOSIX) {
		h5_info(f, "Selecting MPI-POSIX VFD");
		hbool_t use_gpfs = 0; // TODO autodetect GPFS?
		TRY( h5priv_set_hdf5_fapl_mpiposix_property(f,
				f->access_prop, comm, use_gpfs) );
	} else {
		h5_info(f, "Selecting MPI-IO VFD");
		TRY( h5priv_set_hdf5_fapl_mpio_property(f,
				f->access_prop, comm, MPI_INFO_NULL) );
		if (flags & H5_VFD_INDEPENDENT) {
			h5_info(f, "MPI-IO: Using independent mode");
		} else {
			h5_info(f, "MPI-IO: Using collective mode");
			TRY( h5priv_set_hdf5_dxpl_mpio_property(f,
				f->xfer_prop, H5FD_MPIO_COLLECTIVE) );
		}
	}
#endif /* PARALLEL_IO */

#ifdef H5_USE_LUSTRE
        TRY( h5_optimize_for_lustre(f, filename) );
#endif

	if (flags & H5_O_RDONLY) {
		f->file = H5Fopen (filename, H5F_ACC_RDONLY, f->access_prop);
	}
	else if (flags & H5_O_WRONLY){
		f->file = H5Fcreate (filename, H5F_ACC_TRUNC, f->create_prop,
				     f->access_prop);
		f->empty = 1;
	}
	else if (flags & H5_O_APPEND || flags & H5_O_RDWR) {
		int fd = open (filename, O_RDONLY, 0);
		if ((fd == -1) && (errno == ENOENT)) {
			f->file = H5Fcreate (filename, H5F_ACC_TRUNC,
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
		return h5_error (
			f,
			H5_ERR_INVAL,
			"Invalid file access mode \"%d\".", flags);
	}
	
	if (f->file < 0)
		return h5_error (
			f,
			H5_ERR_HDF5,
			"Cannot open file \"%s\" with mode \"%d\"",
			filename, flags);
	TRY( f->root_gid = h5priv_open_hdf5_group (f,  f->file, "/" ) );
	f->mode = flags;
	f->step_gid = -1;
	f->throttle = 0;

	sprintf (
		f->step_name,
		"%s#%0*lld",
		f->prefix_step_name, f->width_step_idx, (long long)f->step_idx);

	TRY( h5upriv_open_file (f) );
	TRY( h5bpriv_open_file (f) );
#ifndef PARALLEL_IO
	TRY( h5tpriv_open_file (f) );
#endif
	return H5_SUCCESS;
}
 
h5_file_t*
h5_open_file (
	const char* filename,	/*!< The name of the data file to open. */
	h5_int32_t flags,	/*!< The access mode for the file. */
	MPI_Comm comm,		/*!< MPI communicator */
	const char* funcname	/*!< calling function name */
	) {
	h5_file_t* f = NULL;

	f = (h5_file_t*) malloc (sizeof (h5_file_t));
	if (f == NULL) {
		fprintf(
			stderr,
			"E: %s: Can't open file %s. Not enough memory!",
			funcname,
			filename);
		return NULL;
	}
	memset (f, 0, sizeof (h5_file_t));
	f->__funcname = funcname;
	if (open_file (f, filename, flags, comm) < 0) {
		if (f != NULL) {
			/* Oops, cannot open file. We release the memory allocated for
			   f only, there is most likely more allocated memory we do
			   *not* release.
			   We don't use the wrapper function because we don't know 
			   wheter it will work or not! */
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
static h5_err_t
h5upriv_close_file (
	h5_file_t* const f	/*!< file handle */
	) {
	struct h5u_fdata* u = f->u;
	h5_debug (f, "%s ()", __func__);

	f->__errno = H5_SUCCESS;
	TRY( h5priv_close_hdf5_dataspace (f, u->shape) );
	TRY( h5priv_close_hdf5_dataspace (f, u->diskshape) );
	TRY( h5priv_close_hdf5_dataspace (f, u->memshape) );
	TRY( h5priv_close_hdf5_property (f, u->dcreate_prop) );
	h5_free (f, f->u);
	f->u = NULL;

	return f->__errno;
}

/*!
  \ingroup h5block_private

  \internal

  De-initialize H5Block internal structure.  Open HDF5 objects are 
  closed and allocated memory freed.

  \return	H5_SUCCESS or error code
*/
static h5_err_t
h5bpriv_close_file (
	h5_file_t* const f	/*!< IN: file handle */
	) {
	struct h5b_fdata* b = f->b;
	h5_debug (f, "%s ()", __func__);
	TRY( h5priv_close_hdf5_group (f, b->block_gid) );
	TRY( h5priv_close_hdf5_group (f, b->field_gid) );
	TRY( h5priv_close_hdf5_dataspace (f, b->shape) );
	TRY( h5priv_close_hdf5_dataspace (f, b->diskshape) );
	TRY( h5priv_close_hdf5_dataspace (f, b->memshape) );
	TRY( h5priv_close_hdf5_property (f, b->dcreate_prop) );
#if defined(PARALLEL_IO)
	TRY( h5priv_mpi_type_free (f, &b->partition_mpi_t) );
#endif
	h5_free (f, f->b);
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
	h5_file_t* const f	/*!< file handle */
	) {
	h5_debug (f, "%s ()", __func__);
	f->__errno = H5_SUCCESS;

	CHECK_FILEHANDLE (f);

	TRY( h5priv_close_step (f) );
	TRY( h5upriv_close_file (f) );
	TRY( h5bpriv_close_file (f) );
#ifndef PARALLEL_IO
	TRY( h5tpriv_close_file (f) );
#endif
	TRY( h5priv_close_hdf5_property (f, f->xfer_prop) );
	TRY( h5priv_close_hdf5_property (f, f->access_prop) );
	TRY( h5priv_close_hdf5_property (f, f->create_prop) );
	TRY( h5priv_close_hdf5_group (f, f->root_gid) );
	TRY( h5priv_close_hdf5_file (f, f->file) );
	h5_debug (f, "%s (): done", __func__);

	h5_free (f, f);
	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_filehandling

  Define format of the step names.

  Example: ==H5FedDefineStepNameFormat( f, "Step", 6 )== defines step names 
  like ==Step#000042==.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
h5_set_stepname_fmt (
	h5_file_t* const f,
	const char* name,
	int width
	) {
	if (width < 0) width = 0;
	else if (width > H5_STEPNAME_LEN - 1) width = H5_STEPNAME_LEN - 1;
	strncpy (
		f->prefix_step_name,
		name,
		H5_STEPNAME_LEN - 1);
	f->width_step_idx = width;

	return H5_SUCCESS;
}

/*!
  \ingroup h5_core_filehandling

  Get format of the step names.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
h5_get_stepname_fmt (
	h5_file_t* const f,		/*!< Handle to file		*/
	char* name,			/*!< OUT: Prefix		*/
	int l_name,			/*!< length of buffer name	*/
	int* width			/*!< OUT: Width of the number	*/
	) {
	UNUSED_ARGUMENT (f);
	UNUSED_ARGUMENT (name);
	UNUSED_ARGUMENT (l_name);
	UNUSED_ARGUMENT (width);
	return h5_error_not_implemented (f, __FILE__, __func__, __LINE__);
}

/*!
  \ingroup h5_core_filehandling

  Get current step number.

  \return Current step number or error code
*/
h5_id_t
h5_get_step (
	h5_file_t* const f		/*!< file handle		*/
	) {
	return h5_error_not_implemented (f, __FILE__, __func__, __LINE__);
}

/*!
  \ingroup h5_core_filehandling

  Get number of processes.

  \return Number of processes or error code
*/
int
h5_get_num_procs (
	h5_file_t* const f		/*!< file handle		*/
	) {
	return f->nprocs;
}

/*!
  \ingroup h5_core_filehandling

  Provides access to the underlying HDF5 file handle.

  \return Number of steps or error code
*/
hid_t
h5_get_hdf5_file(
	h5_file_t* const f		/*!< file handle		*/
	) {
	return f->file;
}

/*!
  \ingroup h5_core_filehandling

  Get number of steps.

  \return Number of steps or error code
*/
h5_size_t
h5_get_num_steps(
	h5_file_t* const f		/*!< file handle		*/
	) {
	return h5_get_num_hdf5_groups_matching_prefix (
		f,
		f->step_gid,
		f->prefix_step_name);
}

/*!
  \ingroup h5_core_filehandling

  Start traversing steps.

  \return \c H5_SUCCESS or error code 
*/
h5_err_t
h5_start_traverse_steps (
	h5_file_t* const f		/*!< file handle		*/
	) {
	return h5_error_not_implemented (f, __FILE__, __func__, __LINE__);
}

/*!
  \ingroup h5_core_filehandling

  Go to next step.

  \return \c H5_SUCCESS or error code 
*/
h5_err_t
h5_traverse_steps (
	h5_file_t* const f		/*!< file handle		*/
	) {
	return h5_error_not_implemented (f, __FILE__, __func__, __LINE__);
}

char *
h5_strdupfor2c (
	const char *s,
	const ssize_t len
	) {

	char *dup = (char*)malloc ( len + 1 );
	strncpy ( dup, s, len );
	char *p = dup + len;
	do {
		*p-- = '\0';
	} while ( *p == ' ' );
	return dup;
}

char *
h5_strc2for (
	char * const str,
	const ssize_t l_str
	) {

	size_t len = strlen ( str );
	memset ( str+len, ' ', l_str-len );

	return str;
}

