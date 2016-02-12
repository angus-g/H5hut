/*
  Copyright (c) 2006-2016, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5_H
#define __H5_H

#include <string.h>

#include "H5_model.h"
#include "H5_attribs.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
  \ingroup h5hut_file

  Create a new, empty file property list. This list can be used in \ref H5OpenFile()
  to set various properties like the file I/O layer.

  \see H5SetPropFileMPIO()
  \see H5SetPropFileMPIOCollective()
  \see H5SetPropFileMPIOIndependent()
  \see H5SetPropFileMPIOPosix() (HDF5 <= 1.8.12 only)
  \see H5SetPropFileCoreVFD()
  \see H5SetPropFileAlign()
  \see H5SetPropFileThrottle()

  \return empty file property list
  \return \c H5_FAILURE on error
*/
static inline h5_prop_t
H5CreateFileProp (
        void
        ) {
        H5_API_ENTER (h5_prop_t, "%s", "");
        H5_API_RETURN (h5_create_prop (H5_PROP_FILE));
}

/**
  \ingroup h5hut_file

  Stores MPI IO communicator information to given file property list. If used in 
  \ref H5OpenFile(), MPI collective IO will be used.

  \note H5SetPropFileMPIO() is deprecated. Use H5SetPropFileMPIOCollective() instead.

  \see H5SetPropFileMPIOIndependent()
  \see H5SetPropFileMPIOPosix() (HDF5 <= 1.8.12 only)
  \see H5SetPropFileCoreVFD()

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5SetPropFileMPIO (
        h5_prop_t prop,	    ///< [in,out] identifier for file property list
        MPI_Comm* comm	    ///< [in] MPI communicator
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p, comm=%p", (void*)prop, comm);
        H5_API_RETURN (h5_set_prop_file_mpio_collective (prop, comm));
}

/**
  \ingroup h5hut_file

  Stores MPI IO communicator information to given file property list. If used in 
  \ref H5OpenFile(), MPI collective IO will be used.

  \note H5SetPropFileMPIOCollective() deprecates H5SetPropFileMPIO().

  \see H5SetPropFileMPIOIndependent()
  \see H5SetPropFileMPIOPosix() (HDF5 <= 1.8.12 only)
  \see H5SetPropFileCoreVFD()

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5SetPropFileMPIOCollective (
        h5_prop_t prop,	    ///> [in,out] identifier for file property list
	MPI_Comm* comm	    ///< [in] MPI communicator
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p, comm=%p", (void*)prop, comm);
        H5_API_RETURN (h5_set_prop_file_mpio_collective (prop, comm));
}

/**
  \ingroup h5hut_file

  Stores MPI IO communicator information to given file property list. If used in 
  \ref H5OpenFile(), MPI independent IO will be used.

  \see H5SetPropFileMPIOCollective()
  \see H5SetPropFileMPIOPosix() (HDF5 <= 1.8.12 only)
  \see H5SetPropFileCoreVFD()

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5SetPropFileMPIOIndependent (
        h5_prop_t prop,	    ///> [in,out] identifier for file property list
        MPI_Comm* comm	    ///> [in] MPI communicator
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p, comm=%p", (void*)prop, comm);
        H5_API_RETURN (h5_set_prop_file_mpio_independent (prop, comm));
}

#if H5_VERSION_LE(1,8,12)
/**
  \ingroup h5hut_file

  Stores MPI IO communicator information to given file property list. If used in 
  \ref H5OpenFile(), MPI POSIX IO will be used.

  \note This function is available only, if H5hut has been compiled with
  HDF5 1.8.12 or older. 

  \see H5SetPropFileMPIOCollective()
  \see H5SetPropFileMPIOIndependent()
  \see H5SetPropFileCoreVFD()

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5SetPropFileMPIOPosix (
        h5_prop_t prop,	    ///> [in,out] identifier for file property list
        MPI_Comm* comm	    ///> [in] MPI communicator
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p, comm=%p", (void*)prop, comm);
        H5_API_RETURN (h5_set_prop_file_mpio_posix (prop, comm));
}
#endif
	
/**
  \ingroup h5hut_file

  Modifies the file property list to use the \c H5FD_CORE driver.  The
  \c H5FD_CORE driver enables an application to work with a file in memory. 
  File contents are stored only in memory until the file is closed.

  The increment by which allocated memory is to be increased each time more
  memory is required, can be specified with \ref H5SetPropFileAlign(). 

  \see H5SetPropFileAlign()

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5SetPropFileCoreVFD (
        h5_prop_t prop	    ///> [in,out] identifier for file property list
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p", (void*)prop);
        H5_API_RETURN (h5_set_prop_file_core_vfd (prop));
}

/**
  \ingroup h5hut_file

  Sets alignment properties of a file property list so that any file
  object greater than or equal in size to threshold bytes will be
  aligned on an address which is a multiple of alignment. The
  addresses are relative to the end of the user block; the alignment
  is calculated by subtracting the user block size from the absolute
  file address and then adjusting the address to be a multiple of
  alignment.

  Default values for alignment is one, implying no
  alignment. Generally the default value result in the best
  performance for single-process access to the file. For MPI IO and
  other parallel systems, choose an alignment which is a multiple of
  the disk block size.

  \see H5SetPropFileCoreVFD()

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5SetPropFileAlign (
        h5_prop_t prop,	    ///> [in,out] identifier for file property list
        h5_int64_t align    ///> [in] alignment 
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p, align=%lld", (void*)prop, (long long int)align);
        H5_API_RETURN (h5_set_prop_file_align (prop, align));
}

/**
  \ingroup h5hut_file

  Set the `throttle` factor, which causes HDF5 write and read
  calls to be issued in that number of batches.

  This can prevent large concurrency parallel applications that
  use independent writes from overwhelming the underlying
  parallel file system.

  Throttling only works with the H5_VFD_MPIO_POSIX or
  H5_VFD_MPIO_INDEPENDENT drivers and is only available in
  the parallel library.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5SetPropFileThrottle (
        h5_prop_t prop,	    ///> [in,out] identifier for file property list
        h5_int64_t throttle ///> [in] throttle factor
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p, throttle=%lld", (void*)prop, (long long int)throttle);
        H5_API_RETURN (h5_set_prop_file_throttle (prop, throttle));
}

/**
  \ingroup h5hut_file

  Close file property list.
*/
static inline h5_err_t
H5CloseProp (
        h5_prop_t prop	    ///> [in] identifier for file property list
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p", (void*)prop);
        H5_API_RETURN (h5_close_prop (prop));
}

/**
  \ingroup h5hut_file

  Open file with name \c filename.

  File mode flags are:
  - \c H5_O_RDONLY: Only reading allowed
  - \c H5_O_WRONLY: create new file, dataset must not exist
  - \c H5_O_APPENDONLY: allows to append new data to an existing file
  - \c H5_O_RDWR:   dataset may exist
  - \c H5_FS_LUSTRE - enable optimizations for the Lustre file system
  - \c H5_VFD_MPIO_POSIX - use the HDF5 MPI-POSIX virtual file driver
  - \c H5_VFD_MPIO_INDEPENDENT - use MPI-IO in indepedent mode

  The file is opened with the properties set in the file property list
  \c prop.  This argument can also be set to \c H5_PROP_DEFAULT to use
  reasonable default values. In this case \c MPI_COMM_WORLD will be
  used as MPI communicator in a parallel execution environment.

  The typical file extension is \c .h5.

  \see H5CreateFileProp()

  \return File handle
  \return \c H5_FAILURE on error
*/
static inline h5_file_t
H5OpenFile (
	const char* filename,	///> [in] name of file
	h5_int64_t mode,	///> [in] file mode 
        h5_prop_t props		///> [in] identifier for file property list
	) {
	H5_API_ENTER (h5_file_t,
                      "filename='%s', mode=%lld, props=%p",
                      filename, (long long int)mode, (void*)props);
        H5_API_RETURN (h5_open_file2 (filename, mode, props));
}

/**
  \ingroup h5hut_file

  Close file and free all memory associated with the file handle.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5CloseFile (
	const h5_file_t f	///< [in] file handle.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5_close_file (f));
}

/**
  \ingroup h5hut_file

  Verify that the passed file handle is a valid H5hut file handle.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5CheckFile (
	const h5_file_t f	///< [in] file handle.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5_check_filehandle (f));
}

/**
  \ingroup h5hut_file

  Flush step data to disk.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
 */
static inline h5_err_t
H5FlushStep (
	const h5_file_t f	///< [in] file handle.
	) {
	H5_API_ENTER (h5_err_t, 
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5_flush_step (f));
}

/**
  \ingroup h5hut_file

  Flush all file data to disk.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
 */
static inline h5_err_t
H5FlushFile (
	const h5_file_t f	///< [in] file handle.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5_flush_file (f));
}

/**
  \ingroup h5hut_file

  Close H5hut library. This function should be called before program exit.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Finalize (
        void
        ) {
        H5_API_ENTER (h5_err_t, "%s", "");
	H5_API_RETURN (h5_close_hdf5 ());
}

/**
  \ingroup h5hut_error

  Set verbosity level to \c level.

  \return \c H5_SUCCESS
*/
static inline h5_err_t
H5SetVerbosityLevel (
	const h5_id_t level     ///< [in] verbosity/debug level.
	) {
	return h5_set_debuglevel (level);
}

/**
  \ingroup h5hut_error

  Set error handler to \c handler.

  \return \c H5_SUCCESS
*/
static inline h5_err_t
H5SetErrorHandler (
	h5_errorhandler_t handler ///< [in] error handler to set.
	) {
	H5_API_ENTER (h5_err_t, "handler=%p", handler);
	H5_API_RETURN (h5_set_errorhandler (handler));
}

/**
  \ingroup h5hut_error

  Set the abort error handler.

  \return \c H5_SUCCESS
*/
static inline h5_err_t
H5AbortOnError (
        void
        ) {
	H5_API_ENTER (h5_err_t, "%s", "");
	H5_API_RETURN (h5_set_errorhandler (h5_abort_errorhandler));
}
        
/**
  \ingroup h5hut_error

  Get current error handler.

  \return Pointer to error handler.
*/
static inline h5_errorhandler_t
H5GetErrorHandler (
	void
	) {
	H5_API_ENTER (h5_errorhandler_t, "%s", "void");
	H5_API_RETURN (h5_get_errorhandler());
}


/**
  \ingroup h5hut_error

  The report error handler writes a message to stderr, sets the error number
  and returns.

  \return       \c H5_FAILURE
 */
static inline h5_err_t
H5ReportErrorhandler (
	const char* fmt,        ///< [in] format string of error message.
	va_list ap              ///< [in] arguments to format string.
	) {
	return h5_report_errorhandler (fmt, ap);
}

/**
  \ingroup h5hut_error

  The abort error handler writes a message to stderr and exits the programm.

  \return       does not return.
 */
static inline h5_err_t
H5AbortErrorhandler (
	const char* fmt,        ///< [in] format string of error message.
	va_list ap              ///< [in] arguments to format string.
	) {
	return h5_abort_errorhandler (fmt, ap);
}

/**
  \ingroup h5hut_error

  Get last error code.

  \return error code
*/
static inline h5_err_t
H5GetErrno (
	void
	) {
	return h5_get_errno ();
}

#ifdef __cplusplus
}
#endif

#endif
