/*
  Copyright (c) 2006-2013, The Regents of the University of California,
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

static inline h5_prop_t
H5CreateFileProp (
        void
        ) {
        H5_API_ENTER (h5_prop_t, "%s", "");
        H5_API_RETURN (h5_create_prop (H5_PROP_FILE));
}

static inline h5_err_t
H5SetPropFileMPIO (
        h5_prop_t prop,
        MPI_Comm* comm
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p, comm=%p", (void*)prop, comm);
        H5_API_RETURN (h5_set_prop_file_mpio (prop, comm));
}

static inline h5_err_t
H5SetPropFileAlign (
        h5_prop_t prop,
        h5_int64_t align
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p, align=%lld", (void*)prop, align);
        H5_API_RETURN (h5_set_prop_file_align (prop, align));
}

static inline h5_err_t
H5SetPropFileThrottle (
        h5_prop_t prop,
        h5_int64_t throttle
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p, throttle=%lld", (void*)prop, throttle);
        H5_API_RETURN (h5_set_prop_file_throttle (prop, throttle));
}

static inline h5_err_t
H5CloseProp (
        h5_prop_t prop
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p", (void*)prop);
        H5_API_RETURN (h5_close_prop (prop));
}

/*!
  \ingroup h5hut_file
  <A NAME="H5OpenFile"></A>

  Open file with name \c filename. This function is available in the parallel
  and serial version. In the serial case \c comm may have any value.


  File mode flags are:
  - \c H5_O_RDONLY: only reading allowed
  - \c H5_O_WRONLY: create new file, dataset must not exist
  - \c H5_O_APPEND: allows to append a new datasets to an existing file
  - \c H5_O_RDWR:   dataset may exist
  - \c H5_FS_LUSTRE - enable optimizations for the Lustre file system
  - \c H5_VFD_MPIPOSIX - use the HDF5 MPI-POSIX virtual file driver
  - \c H5_VFD_MPIIO_IND - use MPI-IO in indepedent mode

  The typical file extension is \c .h5.
  
  \c h5_file_p should be treated as an essentially opaque
  datastructure.  It acts as the file handle, but internally
  it maintains several key state variables associated with 
  the file.

  \return	File handle or \c (void*)H5_FAILURE
*/
static inline h5_file_t
H5OpenFile (
	const char* filename,	///< [in] file name.
	h5_int32_t flags,	///< [in] file access mode flags.
	MPI_Comm comm		///< [in] MPI communicator.
	) {
	H5_API_ENTER (h5_file_t,
                      "filename='%s', flags=%d, ...",
                      filename, flags);
	H5_API_RETURN (h5_open_file (filename, flags, comm, 0));
}

static inline h5_file_t
H5OpenFile2 (
	const char* filename,
	h5_int64_t mode,
        h5_prop_t props
	) {
	H5_API_ENTER (h5_file_t,
                      "filename='%s', mode=%lld, props=%p",
                      filename, mode, (void*)props);
        H5_API_RETURN (h5_open_file2 (filename, mode, props));
}

/*!
  \ingroup h5hut_file
  <A NAME="H5OpenFileAlign"></A>

  Opens file with specified filename, and also specifices an alignment
  value used for HDF5 tuning parameters.  In the serial case \c comm may have
  any value.

  File modes and flags are bit values that can be combined with the bit operator \c |
  and include:

  - \c H5_O_RDONLY: only reading allowed
  - \c H5_O_WRONLY: create new file, dataset must not exist
  - \c H5_O_APPEND: allows to append a new datasets to an existing file
  - \c H5_O_RDWR:   dataset may exist
  - \c H5_FS_LUSTRE - enable optimizations for the Lustre file system
  - \c H5_VFD_MPIPOSIX - use the HDF5 MPI-POSIX virtual file driver
  - \c H5_VFD_MPIIO_IND - use MPI-IO in indepedent mode

  The typical file extension is \c .h5.
  
  \c h5_file_p should be treated as an essentially opaque
  datastructure.  It acts as the file handle, but internally
  it maintains several key state variables associated with 
  the file.

  \return	File handle or \c (void*)H5_FAILURE
 */
static inline h5_file_t
H5OpenFileAlign (
	const char* filename,	///< [in] name of the data file to open.
	const h5_int32_t flags,	///< [in] file access mode flags.
	MPI_Comm comm,		///< [in] MPI communicator.
	const h5_size_t align 	///< [in] alignment size in bytes.
	) {
	H5_API_ENTER (h5_file_t,
                      "filename='%s', flags=%d, ...",
                      filename, flags);
	H5_API_RETURN (h5_open_file (filename, flags, comm, align));
}

/*!
  \ingroup h5hut_file

  Close file and free all memory associated with the file handle.

  \return \c H5_SUCCESS or \c H5_FAILURE
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

/*!
  \ingroup h5hut_file

  Verify that the file handle points to a valid H5hut file structure.

  \return \c H5_SUCCESS or \c H5_FAILURE
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

/*!
  \ingroup h5hut_file

  Set the `throttle` factor, which causes HDF5 write and read
  calls to be issued in that number of batches.

  This can prevent large concurrency parallel applications that
  use independent writes from overwhelming the underlying
  parallel file system.

  Throttling only works with the H5_VFD_MPIPOSIX or
  H5_VFD_MPIIO_IND drivers and is only available in
  the parallel library.

  \return \c H5_SUCCESS or \c H5_FAILURE
*/
#ifdef PARALLEL_IO
h5_err_t
static inline H5SetThrottle (
	const h5_file_t f,              ///< [in] file handle.
	int factor                      ///< [in] throttle factor
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, factor=%d",
                      (h5_file_p)f, factor);
	H5_API_RETURN (h5_set_throttle(f, factor));
}
#endif // PARALLEL_IO
#ifdef __cplusplus
}
#endif


/*!
  \ingroup h5hut_file

  Flush step data to disk.

  \return \c H5_SUCCESS or \c H5_FAILURE
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

/*!
  \ingroup h5hut_file

  Flush file data to disk.

  \return \c H5_SUCCESS or \c H5_FAILURE
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

/*!
  \ingroup h5hut_file

  Close H5hut library. This function should be called before program exit.

  \return \c H5_SUCCESS or \c H5_FAILURE
*/
static inline h5_err_t
H5Finalize (
        void
        ) {
        H5_API_ENTER (h5_err_t, "%s", "");
	H5_API_RETURN (h5_close_hdf5 ());
}

/*!
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

/*!
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

/*!
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


/*!
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

/*!
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

/*!
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
