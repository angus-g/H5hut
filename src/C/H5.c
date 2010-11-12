/*
  Copyright 2007-2008
 	Paul Scherrer Institut, Villigen, Switzerland;
 	Benedikt Oswald;
 	Achim Gsell
 	All rights reserved.
 
  Authors
 	Achim Gsell
  
  Warning
	This code is under development.
 
 */

/*!
  Some conventions:
	Functions:
		Name:
			thisIsAFunction()
		Return values:
			negative value or NULL signals an error


	Macros:
		UPPERCASE_WITH_UNDERSCORE
  \note
  In function names we use the words \b get and \b store insteed of
  \b read and \b write, because no I/O is actually done in these
  functions.
*/

#include "h5core/h5_core.h"
#include "H5.h"

/******	General routines *****************************************************/

/*!
  \ingroup h5hut_file
  
  Open file with name \c filbename. This function is available in the parallel
  and serial version. In the serial case \c comm may have any value.

  \return File handle.
  \return NULL on error.
*/
h5_file_t*
H5OpenFile (
	const char* filename,		/*!< file name			*/
	h5_int32_t flags,		/*!< file open flags		*/
	MPI_Comm comm			/*!< MPI communicator		*/
	) {
	return h5_open_file (filename, flags, comm, __func__); 
}

/*!
  \ingroup h5hut_file

  Close file.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5CloseFile (
	h5_file_t* const f		/*!< file handle		*/
	) {
	H5_ENTER_API (f, __func__);
	return h5_close_file (f);
}

/*!
  \ingroup h5hut_file

  Close file.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5CheckFile (
	h5_file_t* const f		/*!< file handle		*/
	) {
	H5_ENTER_API (f, __func__);
	return h5_check_filehandle (f);
}

/*!
  \ingroup h5hut_model

  Define format of the step names.

  Example: ==H5SetStepNameFormat( f, "Step", 6 )== defines step names 
  like ==Step#000042==.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5SetStepNameFormat (
	h5_file_t* const f,		/*!< Handle to file		*/
	const char* name,		/*!< Prefix			*/
	const h5_int64_t width		/*!< Width of the number	*/
	) {
	H5_ENTER_API (f, __func__);

	return h5_set_stepname_fmt (f, name, width);
}

/*!
  \ingroup h5hut_model

  Get format of the step names.

  \return value \c >=0 on success
  \return -1 on error
*/
h5_err_t
H5GetStepNameFormat (
	h5_file_t* const f,		/*!< Handle to file		*/
	char* name,			/*!< OUT: Prefix		*/
	const h5_size_t l_name,		/*!< length of buffer name	*/
	int* width			/*!< OUT: Width of the number	*/
	) {
	H5_ENTER_API (f, __func__);

	return h5_get_stepname_fmt (f, name, l_name, width);
}

/*!
  \ingroup h5hut_model

  Set the current step.

  \return \c H5_SUCCESS or error code 
*/
h5_err_t
H5SetStep (
	h5_file_t* const f,		/*!< [in]  Handle to open file */
	const h5_id_t step		/*!< [in]  Step to set. */
	) {

	H5_ENTER_API (f, __func__);
	return h5_set_step (f, step);
}

/*!
  \ingroup h5hut_model

  Get current step.

  \return \c H5_SUCCESS or error code 
*/
h5_id_t
H5GetStep (
	h5_file_t* const f		/*!< Handle to open file */
	) {

	H5_ENTER_API (f, __func__);
	return h5_get_step (f);
}

/*!
  \ingroup h5hut_file

  Get the number of processors.

  \param[in]	f	File handle.

  \return Number of processors.
  \return \c -1 on error.
 */
int
H5GetNumProcs (
	h5_file_t* const f
	) {
	H5_ENTER_API (f, __func__);
	return h5_get_num_procs(f);
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
h5_ssize_t
H5GetNumSteps (
	h5_file_t* const f
	) {

	H5_ENTER_API (f, __func__);

	return h5_get_num_steps(f);
}

/*!
  \ingroup h5_inquiry

  Query whether a particular step already exists in the file.

  \param[in]	f	File handle.
  \param[in]	stepno	Step number to query for existence

  \return      true or false
*/
h5_err_t
H5HasStep (
	h5_file_t* const f,
	h5_id_t stepno
	) {
  
	H5_ENTER_API (f, __func__);

	return h5_has_step (f, stepno);
}

/*!
  \ingroup h5hut_model

  Start traversing steps.

  \return \c H5_SUCCESS or error code 
*/
h5_err_t
H5StartTraverseSteps (
	h5_file_t* const f		/*!< Handle to open file */
	) {

	H5_ENTER_API (f, __func__);

	return h5_start_traverse_steps (f);
}

/*!
  \ingroup h5hut_model

  Traverse steps.

  \return \c H5_SUCCESS or error code 
*/
h5_err_t
H5TraverseSteps (
	h5_file_t* const f		/*!< Handle to open file */
	) {

	H5_ENTER_API (f, __func__);

	return h5_traverse_steps (f);
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
h5_err_t
H5SetThrottle (
	h5_file_t* f,
	int factor
	) {

	H5_ENTER_API( f, __func__ );

	return h5_set_throttle( f, factor );
}
#endif // PARALLEL_IO

/*!
  \ingroup h5hut_error

  Set verbosity level to \c level.

  \return \c H5_SUCCESS
*/
h5_err_t
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
h5_err_t
H5SetErrorHandler (
	h5_errorhandler_t handler
	) {
	return h5_set_errorhandler (handler);
}

/*!
  \ingroup h5hut_error

  Get current error handler.

  \return Pointer to error handler.
*/
h5_errorhandler_t
H5GetErrorHandler (
	void
	) {
	return h5_get_errorhandler();
}

h5_err_t
H5ReportErrorhandler (
	const h5_file_t* const f,
	const char* fmt,
	va_list ap
	) {
	return h5_report_errorhandler (f, fmt, ap);
}

h5_err_t
H5AbortErrorhandler (
	const h5_file_t* const f,
	const char* fmt,
	va_list ap
	) {
	return h5_abort_errorhandler (f, fmt, ap);
}

/*!
  \ingroup h5hut_error

  Get last error code.

  \return error code
*/
h5_err_t
H5GetErrno (
	h5_file_t* const f
	) {
	return h5_get_errno (f);
}


