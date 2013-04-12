/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

#include "h5core/h5.h"

#include "h5_private.h"
#include "h5_hdf5_private.h"

#include "h5_model_private.h"
#include "h5_mpi_private.h"
#include "h5u_readwrite_private.h"
#include "h5b_readwrite_private.h"

#include "h5core/h5_errorhandling.h"
#include "h5core/h5_syscall.h"

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
	const h5_file_t f_               /*!< filehandle  to check validity of */
	) {
        h5_file_p f = (h5_file_p)f_;
	if (f == NULL || f_ == H5_FAILURE || f->file < 0 || f->u == NULL || f->b == NULL) {
		return h5_error (
			H5_ERR_BADF,
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

static inline h5_err_t
mpi_init (
	const h5_file_p f
	) {
	H5_INLINE_FUNC_ENTER (h5_err_t);
#ifdef PARALLEL_IO
	TRY (h5priv_mpi_comm_size (f->props->comm, &f->nprocs));
	TRY (h5priv_mpi_comm_rank (f->props->comm, &f->myproc));
	
	/* xfer_prop:  also used for parallel I/O, during actual writes
	   rather than the access_prop which is for file creation. */
	TRY (f->props->xfer_prop = hdf5_create_property(H5P_DATASET_XFER));
	TRY (f->props->access_prop = hdf5_create_property(H5P_FILE_ACCESS));

	/* select the HDF5 VFD */
	if (f->props->mode & H5_VFD_MPIPOSIX) {
		h5_info("Selecting MPI-POSIX VFD");
		hbool_t use_gpfs = 0; // TODO autodetect GPFS?
		TRY (hdf5_set_fapl_mpiposix_property (f->props->access_prop,
                                                     f->props->comm, use_gpfs));

        } else if (f->props->mode & H5_VFD_CORE) {
		h5_info("Selecting CORE VFD");
                TRY (hdf5_set_fapl_core (f->props->access_prop,
                                         f->props->align, 1));
	} else {
		h5_info("Selecting MPI-IO VFD");
		TRY (hdf5_set_fapl_mpio_property (f->props->access_prop,
                                                  f->props->comm, MPI_INFO_NULL));
		if (f->props->mode & H5_VFD_MPIIO_IND) {
			h5_info("MPI-IO: Using independent mode");
		} else {
			h5_info("MPI-IO: Using collective mode");
			TRY (hdf5_set_dxpl_mpio_property (f->props->xfer_prop,
                                                          H5FD_MPIO_COLLECTIVE) );
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

static inline h5_err_t
set_alignment (
	const h5_file_p f
	) {
	H5_INLINE_FUNC_ENTER (h5_err_t);
	if ( f->props->align != 0 ) {
		h5_info ("Setting HDF5 alignment to %llu bytes "
                         "with threshold at half that many bytes.",
                         f->props->align);
		TRY (hdf5_set_alignment_property (
                             f->props->access_prop,
                             f->props->align / 2,
                             f->props->align));
		h5_info ("Setting HDF5 meta block to %llu bytes", f->props->align);
		TRY (H5Pset_meta_block_size (f->props->access_prop, f->props->align));
	}
	H5_INLINE_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
set_default_file_props (
        h5_prop_file_t* props
        ) {
        H5_INLINE_FUNC_ENTER (h5_err_t);
        h5_prop_file_t* file_props = (h5_prop_file_t*)props;
        bzero (file_props, sizeof (file_props));
        file_props->class = H5_PROP_FILE;
        TRY (file_props->prefix_step_name = h5_calloc (1, H5_STEPNAME_LEN));
        strncpy (
                file_props->prefix_step_name,
                H5_STEPNAME,
                H5_STEPNAME_LEN - 1);
        file_props->width_step_idx = H5_STEPWIDTH;
        H5_INLINE_FUNC_RETURN (H5_SUCCESS);
}

h5_err_t
h5_set_prop_file_mpio (
        h5_prop_t _prop,
        MPI_Comm* comm
        ) {
        h5_prop_p prop = (h5_prop_p)_prop;
        H5_CORE_API_ENTER (h5_err_t, "prop=%p, comm=%p", prop, comm);
        
        if (prop->class != H5_PROP_FILE) {
                H5_INLINE_FUNC_LEAVE (
                        h5_error (
                                H5_ERR_INVAL,
                                "Invalid property class: %lld", prop->class));
        }
        h5_prop_file_t* file_prop = (h5_prop_file_t*)prop;
        file_prop->comm = *comm;
        H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5_set_prop_file_align (
        h5_prop_t _prop,
        h5_int64_t align
        ) {
        h5_prop_p prop = (h5_prop_p)_prop;
        H5_CORE_API_ENTER (h5_err_t, "prop=%p, align=%lld", prop, align);
        if (prop->class != H5_PROP_FILE) {
                H5_INLINE_FUNC_LEAVE (
                        h5_error (
                                H5_ERR_INVAL,
                                "Invalid property class: %lld", prop->class));
        }
        h5_prop_file_t* file_prop = (h5_prop_file_t*)prop;
        file_prop->align = align;
        H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5_set_prop_file_throttle (
        h5_prop_t _prop,
        h5_int64_t throttle
        ) {
        h5_prop_p prop = (h5_prop_p)_prop;
        H5_CORE_API_ENTER (h5_err_t, "prop=%p, throttle=%lld", prop, throttle);
        if (prop->class != H5_PROP_FILE) {
                H5_INLINE_FUNC_LEAVE (
                        h5_error (
                                H5_ERR_INVAL,
                                "Invalid property class: %lld", prop->class));
        }
        h5_prop_file_t* file_prop = (h5_prop_file_t*)prop;
        file_prop->throttle = throttle;
        H5_CORE_API_RETURN (H5_SUCCESS);
}


h5_prop_t
h5_create_prop (
        h5_int64_t class
        ) {
        H5_CORE_API_ENTER (h5_prop_t,
                           "class=%lld", class);
        h5_prop_t* prop;
        switch (class) {
        case H5_PROP_FILE:
                TRY (prop = h5_calloc (1, sizeof (h5_prop_file_t)));
                set_default_file_props ((h5_prop_file_t*)prop);
                break;
        default:
                H5_CORE_API_LEAVE (
                        h5_error (
                                H5_ERR_INVAL,
                                "Invalid property class: %lld", class));
        }
        H5_CORE_API_RETURN ((h5_prop_t)prop);
}

h5_err_t
h5_close_prop (
        h5_prop_t _prop
        ) {
        h5_prop_p prop = (h5_prop_p)_prop;
        H5_CORE_API_ENTER (h5_err_t, "prop=%p", prop);
        switch (prop->class) {
        case H5_PROP_FILE: {
                h5_prop_file_t* file_prop = (h5_prop_file_t*)prop;
                TRY (h5_free (file_prop->prefix_step_name));
                break;
        }
        default:
                H5_CORE_API_LEAVE (
                        h5_error (
                                H5_ERR_INVAL,
                                "Invalid property class: %lld", prop->class));
        }
        H5_CORE_API_RETURN (h5_free (prop));
}

static inline h5_err_t
open_file (
	const h5_file_p f,
	const char* const filename,
	h5_int32_t mode
	) {
	H5_INLINE_FUNC_ENTER (h5_err_t);
        h5_info ("Opening file %s.", filename);

        f->props->mode = mode;

        f->nprocs = 1; // queried later
        f->myproc = 0; // queried later
        f->step_gid = -1;

        TRY (f->step_name = h5_calloc (2, H5_STEPNAME_LEN));
        sprintf (
                f->step_name,
                "%s#%0*lld",
                f->props->prefix_step_name,
                f->props->width_step_idx, (long long)f->step_idx);

        TRY (hdf5_set_errorhandler (H5E_DEFAULT, hdf5_error_handler, NULL));
        
        f->props->xfer_prop = f->props->access_prop = H5P_DEFAULT;
        TRY (f->props->create_prop = hdf5_create_property (H5P_FILE_CREATE));
	TRY (mpi_init (f));              // noop if serial
	TRY (set_alignment (f));

	if (f->props->mode & H5_O_RDONLY) {
		f->file = H5Fopen (filename, H5F_ACC_RDONLY, f->props->access_prop);
	}
	else if (f->props->mode & H5_O_WRONLY){
		f->file = H5Fcreate (filename, H5F_ACC_TRUNC, f->props->create_prop,
				     f->props->access_prop);
		f->empty = 1;
	}
	else if (f->props->mode & H5_O_APPEND || f->props->mode & H5_O_RDWR) {
		int fd = open (filename, O_RDONLY, 0);
		if ((fd == -1) && (errno == ENOENT)) {
			f->file = H5Fcreate (filename, H5F_ACC_TRUNC,
					     f->props->create_prop, f->props->access_prop);
			f->empty = 1;
		}
		else if (fd != -1) {
			close (fd);
			f->file = H5Fopen (filename, H5F_ACC_RDWR,
					   f->props->access_prop);
		}
	}
	else {
		H5_PRIV_FUNC_LEAVE (
			h5_error (
				H5_ERR_INVAL,
				"Invalid file access mode '%lld'.", f->props->mode));
	}
	
	if (f->file < 0)
		H5_PRIV_FUNC_LEAVE (
			h5_error (
				H5_ERR_HDF5,
				"Cannot open file '%s' with mode '%lld'",
				filename, f->props->mode));
	TRY (f->root_gid = hdf5_open_group (f->file, "/" ));

	TRY (h5upriv_open_file (f));
	TRY (h5bpriv_open_file (f));

	H5_INLINE_FUNC_RETURN (H5_SUCCESS);
}

h5_file_t
h5_open_file2 (
	const char* filename,
	h5_int32_t mode,
        h5_prop_t props_
	) {
        h5_prop_file_t* props = (h5_prop_file_t*)props_;
	H5_CORE_API_ENTER (h5_file_t,
			   "filename='%s', mode=%d, props=%p",
			   filename, mode, props);
                
	h5_file_p f = NULL;
	TRY (f = h5_calloc (1, sizeof (*f)));
	
        TRY (f->props = (h5_prop_file_t*)h5_create_prop (H5_PROP_FILE));
        TRY (set_default_file_props (f->props));
        TRY (h5_set_stepname_fmt ((uintptr_t)f, H5_STEPNAME, H5_STEPWIDTH));
                
        if (props != H5_PROP_DEFAULT) {
                if (props->class != H5_PROP_FILE) {
                        H5_CORE_API_LEAVE (
                                h5_error (
                                        H5_ERR_INVAL,
                                        "Invalid property class: %lld.",
                                        props->class));
                }
                f->props->comm = props->comm;
                f->props->mode |= props->mode;
                f->props->throttle = props->throttle;
                f->props->align = props->align;

                strncpy (
                        f->props->prefix_step_name,
                        props->prefix_step_name,
                        H5_STEPNAME_LEN - 1);
                f->props->width_step_idx = props->width_step_idx;
        }

	TRY (open_file (f, filename, mode));

	H5_CORE_API_RETURN ((h5_file_t)f);
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

h5_file_t
h5_open_file (
	const char* filename,
	h5_int32_t mode,
	MPI_Comm comm,
	h5_size_t align
	) {
	H5_CORE_API_ENTER (h5_file_t,
			   "filename='%s', mode=%d, comm=?, align=%llu",
			   filename, mode, align);
        h5_prop_file_t* props;
        h5_file_t f;
        TRY (props = (h5_prop_file_t*)h5_create_prop (H5_PROP_FILE));
        TRY (h5_set_prop_file_mpio ((h5_prop_t)props, &comm));
        TRY (h5_set_prop_file_align ((h5_prop_t)props, align));
        TRY (f = h5_open_file2 (filename, mode, (h5_prop_t)props));
        TRY (h5_close_prop ((h5_prop_t)props));
        H5_CORE_API_RETURN (f);

}
#if 0
h5_file_t
h5_open_file (
        const char* filename,
        h5_int32_t mode,
        MPI_Comm comm,
        h5_size_t align
        ) {
        H5_CORE_API_ENTER (h5_file_t,
                           "filename='%s', mode=%d, comm=?, align=%llu",
                           filename, mode, align);
        h5_prop_file_t* props;
        TRY (props = (h5_prop_file_t*)h5_create_prop (H5_PROP_FILE));
        TRY (set_default_file_props (props));

        TRY (h5_set_prop_file_mpio ((h5_prop_t)props, &comm));
        TRY (h5_set_prop_file_align ((h5_prop_t)props, align));
        
        h5_file_p f = NULL;
        TRY (f = h5_calloc (1, sizeof (*f)));
        f->props = props;
        TRY (open_file (f, filename, mode));

	H5_CORE_API_RETURN ((h5_file_t)f);
}
#endif

/*!
  \ingroup h5_core_filehandling

  The h5_close_file() call writes all buffered data to disk, releases 
  all previously allocated memory and terminates access to the associated
  HDF5 file.

  \return	H5_SUCCESS or error code
*/
h5_err_t
h5_close_file (
	const h5_file_t f_              /*!< file handle */
	) {
        h5_file_p f = (h5_file_p)f_;
	H5_CORE_API_ENTER (h5_err_t, "f=%p", f);
	h5_errno = H5_SUCCESS;

	CHECK_FILEHANDLE (f);

	TRY (h5priv_close_step (f));
	TRY (h5upriv_close_file (f));
	TRY (h5bpriv_close_file (f));
	TRY (hdf5_close_property (f->props->xfer_prop));
	TRY (hdf5_close_property (f->props->access_prop));
	TRY (hdf5_close_property (f->props->create_prop));
	TRY (hdf5_close_group (f->root_gid));
        TRY (hdf5_flush (f->file, H5F_SCOPE_GLOBAL));
        TRY (h5_close_prop ((h5_prop_t)f->props));
	TRY (hdf5_close_file (f->file));
        TRY (h5_free (f->step_name));
 	TRY (h5_free (f));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5_close_hdf5 (
        void
        ) {
	H5_CORE_API_ENTER (h5_err_t, "%s", "");
	H5_CORE_API_RETURN (hdf5_close ());
}

h5_err_t
h5_flush_step (
	const h5_file_t f_
	) {
        h5_file_p f = (h5_file_p)f_;
	H5_CORE_API_ENTER (h5_err_t, "f=%p", f);
	H5_CORE_API_RETURN (hdf5_flush (f->step_gid, H5F_SCOPE_LOCAL));
}

h5_err_t
h5_flush_file (
	const h5_file_t f_
	) {
        h5_file_p f = (h5_file_p)f_;
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
	const h5_file_t f_,
	const char* name,
	int width
	) {
        h5_file_p f = (h5_file_p)f_;
	H5_CORE_API_ENTER (h5_err_t,
                           "f=%p, name='%s', width=%d",
                           f, name, width);
	if (width < 0) width = 0;
	else if (width > H5_STEPNAME_LEN - 1) width = H5_STEPNAME_LEN - 1;
	strncpy (
		f->props->prefix_step_name,
		name,
		H5_STEPNAME_LEN - 1);
	f->props->width_step_idx = width;

	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*!
  \ingroup h5_core_filehandling

  Get format of the step names.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
h5_get_stepname_fmt (
	const h5_file_t f_,		/*!< Handle to file		*/
	char* name,			/*!< OUT: Prefix		*/
	int l_name,			/*!< length of buffer name	*/
	int* width			/*!< OUT: Width of the number	*/
	) {
        h5_file_p f = (h5_file_p)f_;
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
	const h5_file_t f_		/*!< file handle		*/
	) {
        h5_file_p f = (h5_file_p)f_;
	H5_CORE_API_ENTER (h5_id_t, "f=%p", f);
	H5_CORE_API_RETURN (f->step_idx);
}

/*!
  \ingroup h5_core_filehandling

  Get number of processes.

  \return Number of processes or error code
*/
int
h5_get_num_procs (
	const h5_file_t f_		/*!< file handle		*/
	) {
        h5_file_p f = (h5_file_p)f_;
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
	const h5_file_t f_		/*!< file handle		*/
	) {
        h5_file_p f = (h5_file_p)f_;
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
	const h5_file_t f_		/*!< file handle		*/
	) {
        h5_file_p f = (h5_file_p)f_;
	H5_CORE_API_ENTER (int, "f=%p", f);
	H5_CORE_API_RETURN (hdf5_get_num_groups_matching_prefix (
                                    f->root_gid,
                                    f->props->prefix_step_name));
}

/*!
  \ingroup h5_core_filehandling

  Start traversing steps.

  \return \c H5_SUCCESS or error code 
*/
h5_err_t
h5_start_traverse_steps (
	const h5_file_t f_		/*!< file handle		*/
	) {
        h5_file_p f = (h5_file_p)f_;
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
	const h5_file_t f_		/*!< file handle		*/
	) {
        h5_file_p f = (h5_file_p)f_;
	UNUSED_ARGUMENT (f);
	return h5_error_not_implemented ();
}
