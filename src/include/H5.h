/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5_H
#define __H5_H

#ifdef __cplusplus
extern "C" {
#endif

/*!
  \ingroup h5hut_file
  
  Open file with name \c filbename. This function is available in the parallel
  and serial version. In the serial case \c comm may have any value.

  File mode flags are:
  - H5_O_RDONLY: only reading allowed
  - H5_O_WRONLY: create new file, dataset must not exist
  - H5_O_APPEND: allows to append a new datasets to an existing file
  - H5_O_RDWR:   dataset may exist

  You can also select a "virtual file driver" in the HDF5 layer using:
  - H5_VFD_INDEPENDENT: MPI-IO in independent (asynchronous) mode
  - H5_VFD_MPIPOSIX: parallel I/O implemented directly by HDF5, bypassing MPI-IO

  \return File handle.
  \return NULL on error.
*/
static inline h5_file_p
H5OpenFile (
	const char* filename,		/*!< file name			*/
	h5_int32_t flags,		/*!< file open flags		*/
	MPI_Comm comm			/*!< MPI communicator		*/
	) {
	H5_API_ENTER (h5_file_p, "filename='%s', flags=%d, ...",filename,flags);
	H5_API_RETURN (h5_open_file (filename, flags, comm, 0));
}
/*!
  \ingroup h5hut_file

  Close file and free all memory associated with the file handle.

  \return \c H5_SUCCESS or error code
*/
static inline h5_err_t
H5CloseFile (
	h5_file_t* const f		/*!< file handle		*/
	) {
	H5_API_ENTER (h5_err_t, "f=%p", f);
	H5_API_RETURN (h5_close_file (f));
}

/*!
  \ingroup h5hut_file

  Verify that the file handle points to a valid H5hut file structure.

  \return \c H5_SUCCESS or error code
*/
static inline h5_err_t
H5CheckFile (
	h5_file_t* const f		/*!< file handle		*/
	) {
	H5_API_ENTER (h5_err_t, "f=%p", f);
	H5_API_RETURN (h5_check_filehandle (f));
}
/*!
  \ingroup h5hut_model

  Define format of the step names.

  Example: ==H5SetStepNameFormat( f, "Step", 6 )== defines step names 
  like ==Step#000042==.

  \return \c H5_SUCCESS or error code
*/
static inline h5_err_t
H5SetStepNameFormat (
	h5_file_t* const f,		/*!< Handle to file		*/
	const char* name,		/*!< Prefix			*/
	const h5_int64_t width		/*!< Width of the number	*/
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', width=%lld",
		      f, name, (long long) width);
	H5_API_RETURN (h5_set_stepname_fmt (f, name, width));
}

/*!
  \ingroup h5hut_model

  Get format of the step names.

  \return value \c >=0 on success
  \return -1 on error
*/
static inline h5_err_t
H5GetStepNameFormat (
	h5_file_t* const f,		/*!< Handle to file		*/
	char* name,			/*!< OUT: Prefix		*/
	const h5_size_t l_name,		/*!< length of buffer name	*/
	int* width			/*!< OUT: Width of the number	*/
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, name=%p, l_name=%llu, width=%p",
		      f, name, (unsigned long long)l_name, width);
	H5_API_RETURN (h5_get_stepname_fmt (f, name, l_name, width));
}

/*!
  \ingroup h5hut_model

  Set the current step.

  \return \c H5_SUCCESS or error code 
*/
static inline h5_err_t
H5SetStep (
	h5_file_t* const f,		/*!< [in]  Handle to open file */
	const h5_id_t step		/*!< [in]  Step to set. */
	) {
	H5_API_ENTER (h5_err_t, "f=%p, step=%lld", f, (long long)step);
	H5_API_RETURN (h5_set_step (f, step));
}

/*!
  \ingroup h5hut_model

  Get current step.

  \return \c H5_SUCCESS or error code 
*/
static inline h5_id_t
H5GetStep (
	h5_file_t* const f		/*!< Handle to open file */
	) {
	H5_API_ENTER (h5_err_t, "f=%p", f);
	H5_API_RETURN (h5_get_step (f));
}

/*!
  \ingroup h5hut_file

  Get the number of processors.

  \param[in]	f	File handle.

  \return Number of processors.
  \return \c -1 on error.
 */
static inline int
H5GetNumProcs (
	h5_file_t* const f
	) {
	H5_API_ENTER (h5_err_t, "f=%p", f);
	H5_API_RETURN (h5_get_num_procs(f));
}

/*!
  \ingroup h5hut_model

  Get the number of time-steps that are currently stored in the file
  \c f.

  It works for both reading and writing of files, but is probably
  only typically used when you are reading.

  \param[in]	f	File handle.

  \return	number of time-steps or error code
*/
static inline h5_ssize_t
H5GetNumSteps (
	h5_file_t* const f
	) {
	H5_API_ENTER (h5_err_t, "f=%p", f);
	H5_API_RETURN (h5_get_num_steps(f));
}

/*!
  \ingroup h5_inquiry

  Query whether a particular step already exists in the file.

  \param[in]	f	File handle.
  \param[in]	stepno	Step number to query for existence

  \return      true or false
*/
static inline h5_err_t
H5HasStep (
	h5_file_t* const f,
	h5_id_t stepno
	) {
	H5_API_ENTER (h5_err_t, 
		      "f=%p, stepno=%lld",
		      f, (long long)stepno);
	H5_API_RETURN (h5_has_step (f, stepno));
}


/*!
  \ingroup h5hut_error

  Set verbosity level to \c level.

  \return \c H5_SUCCESS
*/
static inline h5_err_t
H5SetVerbosityLevel (
	const h5_id_t level
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
	h5_errorhandler_t handler
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

static inline h5_err_t
H5ReportErrorhandler (
	const char* fmt,
	va_list ap
	) {
	return h5_report_errorhandler (fmt, ap);
}

static inline h5_err_t
H5AbortErrorhandler (
	const char* fmt,
	va_list ap
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


static inline h5_err_t
H5FlushStep (
	h5_file_t* const f		/*!< file handle		*/
	) {
	H5_API_ENTER (h5_err_t, "f=%p", f);
	H5_API_RETURN (h5_flush_step (f));
}

static inline h5_err_t
H5FlushFile (
	h5_file_t* const f		/*!< file handle		*/
	) {
	H5_API_ENTER (h5_err_t, "f=%p", f);
	H5_API_RETURN (h5_flush_file (f));
}

/*!
  \ingroup h5hut_file

  Set the `throttle` factor, which causes HDF5 write and read
  calls to be issued in that number of batches.

  This can prevent large concurrency parallel applications that
  use independent writes from overwhelming the underlying
  parallel file system.

  Throttling only works with the H5_VFD_MPIPOSIX or
  H5_VFD_INDEPENDENT drivers and is only available in
  the parallel library.

  \return \c H5_SUCCESS
*/
#ifdef PARALLEL_IO
static inline h5_err_t
H5SetThrottle (
	h5_file_t* f,
	int factor
	) {
	H5_API_ENTER (h5_err_t, "f=%p, factor=%d", f, factor);
	H5_API_RETURN (h5_set_throttle(f, factor));
}
#endif // PARALLEL_IO

#ifdef __cplusplus
}
#endif

#endif
