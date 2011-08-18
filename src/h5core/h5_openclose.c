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

	if (f == NULL || f->file < 0 || f->u == NULL || f->b == NULL) {
		return h5_error (
			H5_ERR_BADFD,
			"Called with bad filehandle.");
	}
	return H5_SUCCESS;
}

/*!
  Initialize H5Part
*/
static herr_t
hdf5_error_handler (
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
	H5_PRIV_API_ENTER (h5_err_t, "f=%p", f);
	TRY (f->u = (h5u_fdata_t*)h5_alloc (NULL, sizeof (*f->u)));
	h5u_fdata_t *u = f->u;

        u->shape = -1;
	u->diskshape = H5S_ALL;
	u->memshape = H5S_ALL;
	u->viewstart = -1;
	u->viewend = -1;
	u->viewindexed = 0;

	TRY (u->dcreate_prop = hdf5_create_property (H5P_DATASET_CREATE));

	H5_PRIV_API_RETURN (H5_SUCCESS);
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
	H5_PRIV_API_ENTER (h5_err_t, "f=%p", f);
	h5b_fdata_t* b; 

	if (f->b)
		H5_PRIV_API_LEAVE (H5_SUCCESS);

	TRY (f->b = (h5b_fdata_t*)h5_alloc (NULL, sizeof (*f->b)));

	b = f->b;
	memset (b, 0, sizeof (*b));

#if defined(PARALLEL_IO)
	size_t n = sizeof (struct h5b_partition) / sizeof (h5_int64_t);
	TRY (h5priv_mpi_type_contiguous(n, MPI_LONG_LONG, &b->partition_mpi_t));
#endif
	memset (b->user_layout, 0, sizeof(*b->user_layout));
	memset (b->write_layout, 0, sizeof(*b->write_layout));

	b->shape = -1;
	b->diskshape = -1;
	b->memshape = -1;
	b->block_gid = -1;
	b->field_gid = -1;
	b->have_layout = 0;

	TRY (b->dcreate_prop = hdf5_create_property (H5P_DATASET_CREATE));

	H5_PRIV_API_RETURN (H5_SUCCESS);
}

static inline
h5_err_t mpi_init (
	h5_file_t* const f,
	MPI_Comm comm
	) {
	H5_INLINE_FUNC_ENTER (h5_err_t);
#ifdef PARALLEL_IO
	TRY2 (h5priv_mpi_comm_size (comm, &f->nprocs));
	TRY2 (h5priv_mpi_comm_rank (comm, &f->myproc));
	
	/* xfer_prop:  also used for parallel I/O, during actual writes
	   rather than the access_prop which is for file creation. */
	TRY2 (f->xfer_prop = hdf5_create_property(H5P_DATASET_XFER));
	TRY2 (f->access_prop = hdf5_create_property(H5P_FILE_ACCESS));

	/* select the HDF5 VFD */
	if (f->flags & H5_VFD_MPIPOSIX) {
		h5_info("Selecting MPI-POSIX VFD");
		hbool_t use_gpfs = 0; // TODO autodetect GPFS?
		TRY2 (hdf5_set_fapl_mpiposix_property(f->access_prop, comm, use_gpfs));
	} else {
		h5_info("Selecting MPI-IO VFD");
		TRY2 (hdf5_set_fapl_mpio_property(f->access_prop, comm, MPI_INFO_NULL));
		if (f->flags & H5_VFD_INDEPENDENT) {
			h5_info("MPI-IO: Using independent mode");
		} else {
			h5_info("MPI-IO: Using collective mode");
			TRY2 (hdf5_set_dxpl_mpio_property(f->xfer_prop, H5FD_MPIO_COLLECTIVE) );
		}
	}
#ifdef H5_USE_LUSTRE
	if (f->flags & H5_FS_LUSTRE) {
		TRY (h5_optimize_for_lustre(f, filename));
	}
#endif
#endif /* PARALLEL_IO */
	H5_INLINE_FUNC_RETURN (H5_SUCCESS);
}

static inline
h5_err_t set_alignment (
	h5_file_t* const f,
	h5_size_t align
	) {
	H5_INLINE_FUNC_ENTER (h5_err_t);
	if ( align != 0 ) {
		h5_info ("Setting HDF5 alignment to %llu bytes", align);
		TRY (hdf5_set_alignment_property (f->access_prop, 0, align));
		h5_info ("Setting HDF5 meta block to %llu bytes", align);
		TRY (H5Pset_meta_block_size (f->access_prop, align));
	}
	H5_INLINE_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
open_file (
	h5_file_t* const f,
	const char* const filename
	) {
	H5_INLINE_FUNC_ENTER (h5_err_t);
	if (f->mode & H5_O_RDONLY) {
		f->file = H5Fopen (filename, H5F_ACC_RDONLY, f->access_prop);
	}
	else if (f->mode & H5_O_WRONLY){
		f->file = H5Fcreate (filename, H5F_ACC_TRUNC, f->create_prop,
				     f->access_prop);
		f->empty = 1;
	}
	else if (f->mode & H5_O_APPEND || f->mode & H5_O_RDWR) {
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
		H5_PRIV_FUNC_LEAVE (
			h5_error (
				H5_ERR_INVAL,
				"Invalid file access mode '%d'.", f->mode));
	}
	
	if (f->file < 0)
		H5_PRIV_FUNC_LEAVE (
			h5_error (
				H5_ERR_HDF5,
				"Cannot open file '%s' with mode '%d'",
				filename, f->mode));
	H5_INLINE_FUNC_RETURN (H5_SUCCESS);
}

/*!
  \ingroup h5_core_filehandling
  
  Open file with name \c filename. This function is available in the paralell
  and serial version. In the serial case \c comm may have any value.

  \param[in]	filename	The name of the data file to open.
  \param[in]	flags		The access mode for the file.
  \param[in]	comm		MPI communicator
  \param[in]	align		Number of bytes for setting alignment,
				metadata block size, etc. Set to 0 to disable.

  \return File handle.
  \return H5_ERR  on error.
*/
h5_file_p
h5_open_file (
	const char* filename,
	h5_int32_t flags,
	MPI_Comm comm,
	h5_size_t align
	) {
	H5_CORE_API_ENTER (h5_file_p,
			   "filename='%s', flags=%d, comm=?, align=%llu",
			   filename, flags, align);
	h5_info ("Opening file %s.", filename);
	h5_file_p f = NULL;
	TRY2 (f = h5_calloc (1, sizeof (h5_file_t)));
	
	TRY2 (hdf5_set_errorhandler (H5E_DEFAULT, hdf5_error_handler, NULL));
	TRY2 (h5_set_stepname_fmt (f, H5_STEPNAME, H5_STEPWIDTH));

	/* init values  */
	f->comm = comm;
	f->nprocs = 1;		// queried later
	f->myproc = 0;		// queried later
	f->mode = flags;
	f->step_gid = -1;
	f->throttle = 0;
	sprintf (
		f->step_name,
		"%s#%0*lld",
		f->prefix_step_name, f->width_step_idx, (long long)f->step_idx);

	f->xfer_prop = f->access_prop = H5P_DEFAULT;
	TRY2 (f->create_prop = hdf5_create_property(H5P_FILE_CREATE));

	TRY2 (mpi_init (f, comm));
	TRY2 (set_alignment (f, align));
	TRY2 (open_file (f, filename));

	TRY2 (f->root_gid = hdf5_open_group (f->file, "/" ));

	TRY2 (h5upriv_open_file (f));
	TRY2 (h5bpriv_open_file (f));
	H5_CORE_API_RETURN (f);
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
	H5_PRIV_API_ENTER (h5_err_t, "f=%p", f);
	struct h5u_fdata* u = f->u;

	h5_errno = H5_SUCCESS;
	TRY (hdf5_close_dataspace (u->shape));
	TRY (hdf5_close_dataspace (u->diskshape));
	TRY (hdf5_close_dataspace (u->memshape));
	TRY (hdf5_close_property (u->dcreate_prop));
	TRY (h5_free (f->u));
	f->u = NULL;

	H5_PRIV_API_RETURN (H5_SUCCESS);
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
	H5_PRIV_API_ENTER (h5_err_t, "f=%p", f);
	struct h5b_fdata* b = f->b;
	TRY (hdf5_close_group (b->block_gid));
	TRY (hdf5_close_group (b->field_gid));
	TRY (hdf5_close_dataspace (b->shape));
	TRY (hdf5_close_dataspace (b->diskshape));
	TRY (hdf5_close_dataspace (b->memshape));
	TRY (hdf5_close_property (b->dcreate_prop));
#if defined(PARALLEL_IO)
	TRY (h5priv_mpi_type_free (&b->partition_mpi_t));
#endif
	TRY (h5_free (f->b));
	f->b = NULL;

	H5_PRIV_API_RETURN (H5_SUCCESS);
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
	H5_CORE_API_ENTER (h5_err_t, "f=%p", f);
	h5_errno = H5_SUCCESS;

	CHECK_FILEHANDLE (f);

	TRY (h5priv_close_step (f));
	TRY (h5upriv_close_file (f));
	TRY (h5bpriv_close_file (f));
	TRY (hdf5_close_property (f->xfer_prop));
	TRY (hdf5_close_property (f->access_prop));
	TRY (hdf5_close_property (f->create_prop));
	TRY (hdf5_close_group (f->root_gid));
	TRY (hdf5_close_file (f->file));
	h5_free (f);
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5_flush_step (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER (h5_err_t, "f=%p", f);
	H5_CORE_API_RETURN (hdf5_flush (f->step_gid, H5F_SCOPE_LOCAL));
}

h5_err_t
h5_flush_file (
	h5_file_t* const f
	) {
	H5_CORE_API_ENTER (h5_err_t, "f=%p", f);
	H5_CORE_API_RETURN (hdf5_flush (f->file, H5F_SCOPE_GLOBAL));
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
	H5_CORE_API_ENTER (h5_err_t, "f=%p, name='%s', width=%d", f, name, width);
	if (width < 0) width = 0;
	else if (width > H5_STEPNAME_LEN - 1) width = H5_STEPNAME_LEN - 1;
	strncpy (
		f->prefix_step_name,
		name,
		//H5_STEPNAME_LEN - 1);
		63);
	f->width_step_idx = width;

	H5_CORE_API_RETURN (H5_SUCCESS);
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
	return h5_error_not_implemented ();
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
	UNUSED_ARGUMENT (f);
	return h5_error_not_implemented ();
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
	H5_CORE_API_ENTER (int, "f=%p", f);
	H5_CORE_API_RETURN (f->nprocs);
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
	H5_CORE_API_ENTER (hid_t, "f=%p", f);
	H5_CORE_API_RETURN (f->file);
}

/*!
  \ingroup h5_core_filehandling

  Get number of steps.

  \return Number of steps or error code
*/
h5_ssize_t
h5_get_num_steps(
	h5_file_t* const f		/*!< file handle		*/
	) {
	return hdf5_get_num_groups_matching_prefix (
		f->root_gid,
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
	UNUSED_ARGUMENT (f);
	return h5_error_not_implemented ();
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
	UNUSED_ARGUMENT (f);
	return h5_error_not_implemented ();
}

char *
h5_strdupfor2c (
	const char *s,
	const ssize_t len
	) {

	char *dup = (char*)malloc ( len + 1 );
	strncpy ( dup, s, len );
	dup[len] = '\0';
	for (int i=len-1; i>=0; i--) {
		if (dup[i] == ' ') dup[i] = '\0';
		else break;
	}
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

h5_file_t *
h5_filehandlefor2c (
	const h5_int64_t *ptr
	) {
	return (h5_file_t*)(size_t)*ptr;
}

