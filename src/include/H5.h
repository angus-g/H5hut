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

/**
   \ingroup h5_c_api
   \addtogroup h5_file
   @{
*/

#ifdef __cplusplus
extern "C" {
#endif

#if H5HUT_API_VERSION == 2
#define H5OpenFile2 H5OpenFile
#define H5SetVerbosityLevel2 H5SetVerbosityLevel
#elif H5HUT_API_VERSION == 1
#define H5OpenFile1 H5OpenFile
#define H5SetVerbosityLevel1 H5SetVerbosityLevel
#endif

/**
  Create a new, empty file property list.

  File property lists are used to control optional behavior like file
  creation, file access, dataset creation, dataset transfer. File
  property lists are attached to file handles while opened with \ref
  H5OpenFile().

  \return empty file property list
  \return \c H5_FAILURE on error

  \see H5SetPropFileMPIO()
  \see H5SetPropFileMPIOCollective()
  \see H5SetPropFileMPIOIndependent()
  \see H5SetPropFileMPIOPosix() (HDF5 <= 1.8.12 only)
  \see H5SetPropFileCoreVFD()
  \see H5SetPropFileAlign()
  \see H5SetPropFileThrottle()

  \note 
  | Release    | Change                               |
  | :------    | :-----				      |
  | \c 1.99.15 | Function introduced in this release. |
*/
static inline h5_prop_t
H5CreateFileProp (
        void
        ) {
        H5_API_ENTER (h5_prop_t, "%s", "");
        H5_API_RETURN (h5_create_prop (H5_PROP_FILE));
}

/**
  Stores MPI IO communicator information to given file property list. If used in 
  \ref H5OpenFile(), MPI collective IO will be used.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error

  \see H5SetPropFileMPIOIndependent()
  \see H5SetPropFileMPIOPosix() (HDF5 <= 1.8.12 only)
  \see H5SetPropFileCoreVFD()

  \note 
  | Release    | Change                               |
  | :------    | :-----				    |
  | \c 1.99.15 | Function introduced in this release. |
*/
static inline h5_err_t
H5SetPropFileMPIOCollective (
        h5_prop_t prop,	    ///< [in,out] identifier for file property list
	MPI_Comm* comm	    ///< [in] MPI communicator
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p, comm=%p", (void*)prop, comm);
        H5_API_RETURN (h5_set_prop_file_mpio_collective (prop, comm));
}

/**
  Stores MPI IO communicator information to given file property list. If used in 
  \ref H5OpenFile(), MPI independent IO will be used.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error

  \see H5SetPropFileMPIOCollective()
  \see H5SetPropFileMPIOPosix() (HDF5 <= 1.8.12 only)
  \see H5SetPropFileCoreVFD()

  \note 
  | Release    | Change                               |
  | :------    | :-----				      |
  | \c 1.99.15 | Function introduced in this release. |
*/
static inline h5_err_t
H5SetPropFileMPIOIndependent (
        h5_prop_t prop,	    ///< [in,out] identifier for file property list
        MPI_Comm* comm	    ///< [in] MPI communicator
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p, comm=%p", (void*)prop, comm);
        H5_API_RETURN (h5_set_prop_file_mpio_independent (prop, comm));
}

#if H5_VERSION_LE(1,8,12)
/**
  Stores MPI IO communicator information to given file property list. If used in 
  \ref H5OpenFile(), MPI POSIX IO will be used.


  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error

  \see H5SetPropFileMPIOCollective()
  \see H5SetPropFileMPIOIndependent()
  \see H5SetPropFileCoreVFD()

  \note This function is available only, if H5hut has been compiled with
  HDF5 1.8.12 or older. 

  \note 
  | Release    | Change                               |
  | :------    | :-----				      |
  | \c 1.99.15 | Function introduced in this release. |
*/
static inline h5_err_t
H5SetPropFileMPIOPosix (
        h5_prop_t prop,	    ///< [in,out] identifier for file property list
        MPI_Comm* comm	    ///< [in] MPI communicator
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p, comm=%p", (void*)prop, comm);
        H5_API_RETURN (h5_set_prop_file_mpio_posix (prop, comm));
}
#endif
	
/**
  Modifies the file property list to use the \c H5FD_CORE driver.  The
  \c H5FD_CORE driver enables an application to work with a file in memory. 
  File contents are stored only in memory until the file is closed.

  The increment by which allocated memory is to be increased each time more
  memory is required, must be specified with \c increment.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error

  \note 
  | Release    | Change                               |
  | :------    | :-----				    |
  | \c 1.99.15 | Function introduced in this release. |
*/
static inline h5_err_t
H5SetPropFileCoreVFD (
        h5_prop_t prop,		///< [in,out] identifier for file property list
	h5_int64_t increment	///< [in] size, in bytes, of memory increments.
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p", (void*)prop);
        H5_API_RETURN (h5_set_prop_file_core_vfd (prop, increment));
}

/**
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

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error

  \see H5SetPropFileCoreVFD()

  \note 
  | Release    | Change                               |
  | :------    | :-----				      |
  | \c 1.99.15 | Function introduced in this release. |
*/
static inline h5_err_t
H5SetPropFileAlign (
        h5_prop_t prop,	    ///< [in,out] identifier for file property list
        h5_int64_t align    ///< [in] alignment 
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p, align=%lld",
		      (void*)prop, (long long int)align);
        H5_API_RETURN (h5_set_prop_file_align (prop, align));
}

/**
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

  \note 
  | Release    | Change                               |
  | :------    | :-----				      |
  | \c 1.99.15 | Function introduced in this release. |
*/
static inline h5_err_t
H5SetPropFileThrottle (
        h5_prop_t prop,	    ///< [in,out] identifier for file property list
        h5_int64_t throttle ///< [in] throttle factor
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p, throttle=%lld",
		      (void*)prop, (long long int)throttle);
        H5_API_RETURN (h5_set_prop_file_throttle (prop, throttle));
}

/**
  Close file property list.

  \note 
  | Release    | Change                               |
  | :------    | :-----			  	      |
  | \c 1.99.15 | Function introduced in this release. |
*/
static inline h5_err_t
H5CloseProp (
        h5_prop_t prop	    ///< [in] identifier for file property list
        ) {
        H5_API_ENTER (h5_err_t, "prop=%p", (void*)prop);
        H5_API_RETURN (h5_close_prop (prop));
}

/**
  Open file with name \c filename.

  File mode flags are:
  - \c H5_O_RDONLY: Only reading allowed
  - \c H5_O_WRONLY: create new file, dataset must not exist
  - \c H5_O_APPENDONLY: allows to append new data to an existing file
  - \c H5_O_RDWR:   dataset may exist
  - \c H5_FS_LUSTRE: enable optimizations for the Lustre file system
  - \c H5_VFD_MPIO_POSIX: use the HDF5 MPI-POSIX virtual file driver
  - \c H5_VFD_MPIO_INDEPENDENT: use MPI-IO in indepedent mode

  The file is opened with the properties set in the file property list
  \c prop.  This argument can also be set to \c H5_PROP_DEFAULT to use
  reasonable default values. In this case \c MPI_COMM_WORLD will be
  used as MPI communicator in a parallel execution environment.

  The typical file extension is \c .h5.

  \return File handle
  \return \c H5_FAILURE on error

  \see H5CreateFileProp()

  | Release    | Change       |
  | :------    | :-----	      |
  | \c 1.99.15 | API changed, old implementation is available as \c H5OpenFile1()  |
*/
static inline h5_file_t
H5OpenFile2 (
	const char* filename,	///< [in] name of file
	h5_int64_t mode,	///< [in] file mode 
        h5_prop_t props		///< [in] identifier for file property list
	) {
	H5_API_ENTER (h5_file_t,
                      "filename='%s', mode=%lld, props=%p",
                      filename, (long long int)mode, (void*)props);
        H5_API_RETURN (h5_open_file2 (filename, mode, props));
}

/**
  Open file with name \c filename.

  File mode flags are:
  - \c H5_O_RDONLY: Only reading allowed
  - \c H5_O_WRONLY: create new file, dataset must not exist
  - \c H5_O_APPENDONLY: allows to append new data to an existing file
  - \c H5_O_RDWR:   dataset may exist
  - \c H5_FS_LUSTRE - enable optimizations for the Lustre file system
  - \c H5_VFD_MPIO_POSIX - use the HDF5 MPI-POSIX virtual file driver
       (hdf5 <= 1.8.12 only)
  - \c H5_VFD_MPIO_INDEPENDENT - use MPI-IO in indepedent mode

  In the serial version of H5hut, \c comm can be set to any value.

  \return File handle.
  \return (h5_file_p*)H5_FAILURE

  \note This function is deprecated!

  \note
  | Release    | Change       |
  | :------    | :-----	      |
  | \c 1.99.15 | Old implementation of \c H5OpenFile()  |
*/
static inline h5_file_p
H5OpenFile1 (
	const char* filename,	///< [in] file name
	h5_int32_t flags,	///< [in] file open flags
	MPI_Comm comm		///< [in] MPI communicator
	) {
	H5_API_ENTER (h5_file_p, "filename='%s', flags=%d, ...",filename,flags);
	H5_API_RETURN (h5_open_file (filename, flags, comm, 0));
}

/**
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


///< @}

/**
   \ingroup h5hut_c_api_error_handling_and_debugging
   \addtogroup  h5hut_verbosity
   @{
 */

/**
  Set verbosity level to \c level.

  Verbosity levels are:
  - \c H5_VERBOSE_NONE: be quiet
  - \c H5_VERBOSE_ERROR: output error messages
  - \c H5_VERBOSE_WARN: output error messages and warning
  - \c H5_VERBOSE_INFO: output error messages, warnings and informational messages

  The default verbosity level is \c H5_VERBOSE_ERROR.

  \return \c H5_SUCCESS

  \see H5GetVerbosityLevel()

  \note 
  | Release    | Change                               |
  | :------    | :-----			  	      |
  | \c 1.99.15 | Function cannot be used to set the debug level. |
*/
static inline h5_err_t
H5SetVerbosityLevel2 (
	const h5_id_t level     ///< [in] verbosity level.
	) {
	return h5_set_debuglevel (level & 0x03);
}

/**
  Set verbosity and debug level to \p level.

  Verbosity levels are:
  - \c H5_VERBOSE_NONE: be quiet
  - \c H5_VERBOSE_ERROR: output error messages
  - \c H5_VERBOSE_WARN: output error messages and warning
  - \c H5_VERBOSE_INFO: output error messages, warnings and informational messages

  The default verbosity level is \c H5_VERBOSE_ERROR.

  \return \c H5_SUCCESS

  \see H5GetVerbosityLevel()
*/
static inline h5_err_t
H5SetVerbosityLevel1 (
	const h5_id_t level     ///< [in] verbosity level.
	) {
	return h5_set_debuglevel (level);
}

/**
  Get verbosity level.

  \return   verbosity level

  \see H5SetVerbosityLevel()
*/
static inline h5_id_t
H5GetVerbosityLevel (
	void
	) {
	return h5_get_debuglevel (level) & 0x03;
}

///< @}

/**
   \addtogroup h5hut_debug
   @{
*/

/**
  Set debug mask. The debug mask is an or'ed value of

  - \c H5_DEBUG_API:	    C-API calls
  - \c H5_DEBUG_CORE_API:   core API calls.
  - \c H5_DEBUG_PRIV_API:   private API calls
  - \c H5_DEBUG_PRIV_FUNC:  static functions
  - \c H5_DEBUG_HDF5:	    HDF5 wrapper calls
  - \c H5_DEBUG_MPI:	    MPI wrapper calls
  - \c H5_DEBUG_MALLOC:	    memory allocation
  - \c H5_DEBUG_ALL:	    enable all

  \return \c H5_SUCCESS

  \see H5GetDebugMask()

  \note 
  | Release    | Change                               |
  | :------    | :-----			  	      |
  | \c 1.99.15 | Function introduced in this release. |
*/
static inline h5_err_t
H5SetDebugMask (
	const h5_id_t mask     ///< [in] debug mask
	) {
	return h5_set_debuglevel (mask & ~0x03);
}

/**
  Get debug mask.

  \return   debug mask

  \see H5SetDebugMask()

  \note 
  | Release    | Change                               |
  | :------    | :-----			  	      |
  | \c 1.99.15 | Function introduced in this release. |
*/
static inline h5_id_t
H5GetDebugMask (
	void
	) {
	return (h5_get_debuglevel () & ~0x03);
}
///< @}

/**
   \addtogroup h5hut_error
   @{
*/
       
/**
  Report error, do not abort program. The error must be handled in the programm.

  \return \c H5_SUCCESS

  \see H5SetErrorHandler()
  \see H5ReportErrorhandler()
*/
static inline h5_err_t
H5ReportOnError (
        void
        ) {
	H5_API_ENTER (h5_err_t, "%s", "");
	H5_API_RETURN (h5_set_errorhandler (h5_report_errorhandler));
}

/**
  Abort program on error.

  \return \c H5_SUCCESS

  \see H5SetErrorHandler()
  \see H5AbortErrorhandler()
*/
static inline h5_err_t
H5AbortOnError (
        void
        ) {
	H5_API_ENTER (h5_err_t, "%s", "");
	H5_API_RETURN (h5_set_errorhandler (h5_abort_errorhandler));
}

/**
  Set error handler to \c handler.

  \return \c H5_SUCCESS

  \see H5GetErrorHandler()
  \see H5ReportErrorhandler()
  \see H5AbortErrorhandler()
*/
static inline h5_err_t
H5SetErrorHandler (
	h5_errorhandler_t handler ///< [in] error handler to set.
	) {
	H5_API_ENTER (h5_err_t, "handler=%p", handler);
	H5_API_RETURN (h5_set_errorhandler (handler));
}

/**
  Get current error handler.

  \return Pointer to error handler.

  \see H5SetErrorHandler()
  \see H5ReportErrorhandler()
  \see H5AbortErrorhandler()
*/
static inline h5_errorhandler_t
H5GetErrorHandler (
	void
	) {
	H5_API_ENTER (h5_errorhandler_t, "%s", "void");
	H5_API_RETURN (h5_get_errorhandler());
}


/**
  The report error handler writes a message to stderr, sets the error number
  and returns.

  \return   \c H5_FAILURE
 */
static inline h5_err_t
H5ReportErrorhandler (
	const char* fmt,        ///< [in] format string of error message.
	va_list ap              ///< [in] arguments to format string.
	) {
	return h5_report_errorhandler (fmt, ap);
}

/**
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
  Get last error code.

  Error codes are:

  - \c H5_ERR_BADF:	Something is wrong with the file handle.
  - \c H5_ERR_NOMEM:	Out of memory.
  - \c H5_ERR_INVAL:	Invalid argument.
  
  - \c H5_ERR_VIEW:	Something is wrong with the view.
  - \c H5_ERR_NOENTRY:	A lookup failed.
  
  - \c H5_ERR_MPI:	A MPI error occured.
  - \c H5_ERR_HDF5:	A HDF5 error occured.
  - \c H5_ERR_H5:	Unspecified error in H5 module.
  - \c H5_ERR_H5PART:	Unspecified error in H5Part module.
  - \c H5_ERR_H5BLOCK:	Unspecified error in H5Block module.
  - \c H5_ERR_H5FED:	Unspecified error in H5Fed module.
  
  - \c H5_ERR_INTERNAL:	Internal error.
  - \c H5_ERR_NOT_IMPLEMENTED: Function not yet implemented.

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

///< @}
	
#endif
