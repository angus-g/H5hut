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


/*!
  \ingroup h5_c_api
  \defgroup h5_c_api_general
*/

#include <stdarg.h>
#include <hdf5.h>
#include "h5core/h5_core.h"
#include "H5.h"

/******	General routines *****************************************************/

/*!
  \ingroup h5_c_api_general
  
  Open file with name \c filename. This function is available in the paralell
  and serial version. In the serial case \c comm may have any value.

  \return File handle.
  \return NULL on error.
*/
h5_file_t *
H5OpenFile (
	const char * filename,		/*!< file name			*/
	const h5_int32_t oflag,		/*!< file open flags		*/
	const MPI_Comm comm		/*!< MPI communicator		*/
	) {
	return h5_open_file( filename, H5_O_RDWR, comm, __func__ ); 
}

/*!
  \ingroup h5_c_api_general

  Close file.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5CloseFile (
	h5_file_t * const f		/*!< file handle		*/
	) {
	SET_FNAME ( f, __func__ );
	return h5_close_file ( f );
}

/*!
  \ingroup h5_c_api_general

  Define format of the step names.

  Example: ==H5FedDefineStepNameFormat( f, "Step", 6 )== defines step names 
  like ==Step#000042==.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5DefineStepNameFormat (
	h5_file_t *f,			/*!< Handle to file		*/
	const char *name,		/*!< Prefix			*/
	const h5_int64_t width		/*!< Width of the number	*/
	) {
	SET_FNAME ( f, __func__ );

	return h5_set_stepname_fmt( f, name, width );
}

/*!
  \ingroup h5_c_api_general

  Get format of the step names.

  \return value \c >=0 on success
  \return -1 on error
*/
h5_err_t
H5GetStepNameFormat (
	h5_file_t *f,			/*!< Handle to file		*/
	char *name,			/*!< OUT: Prefix		*/
	const h5_size_t l_name,		/*!< length of buffer name	*/
	h5_size_t *width		/*!< OUT: Width of the number	*/
	) {
	SET_FNAME ( f, __func__ );

	return h5_get_stepname_fmt( f, name, l_name, width );
}

/*!
  \ingroup h5_c_api_general

  Set the current step.

  \return \c H5_SUCCESS or error code 
*/
h5_err_t
H5SetStep (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	const h5_int64_t step		/*!< [in]  Step to set. */
	) {

	SET_FNAME ( f, __func__ );
	return h5_set_step ( f, step );
}

/*!
  \ingroup h5_c_api_general

  Get current step.

  \return \c H5_SUCCESS or error code 
*/
h5_id_t
H5GetStep (
	h5_file_t *f			/*!< Handle to open file */
	) {

	SET_FNAME ( f, __func__ );
	return h5_get_step ( f );
}

/*!
  \ingroup h5_c_api_general

  Start traversing steps.

  \return \c H5_SUCCESS or error code 
*/
h5_err_t
H5StartTraverseSteps (
	h5_file_t *f			/*!< Handle to open file */
	) {

	SET_FNAME ( f, __func__ );

	return h5_start_traverse_steps( f );
}

/*!
  \ingroup h5_c_api_general

  Traverse steps.

  \return \c H5_SUCCESS or error code 
*/
h5_err_t
H5TraverseSteps (
	h5_file_t * f			/*!< Handle to open file */
	) {

	SET_FNAME ( f, __func__ );

	return h5_traverse_steps( f );
}

/*!
  \ingroup h5part_c_api
  \defgroup h5part_c_api_errhandling	Error Handling
*/

/*!
  \ingroup h5part_c_api_errhandling

  Set verbosity level to \c level.

  \return \c H5_SUCCESS
*/
h5_err_t
H5SetVerbosityLevel (
	const h5_id_t level
	) {

	return h5_set_debuglevel ( level );
}

/*!
  \ingroup h5part_c_api_errhandling

  Set error handler to \c handler.

  \return \c H5_SUCCESS
*/
h5_err_t
H5SetErrorHandler (
	h5_errorhandler_t handler
	) {
  
	return h5_set_errorhandler( handler );
}

/*!
  \ingroup h5part_c_api_errhandling

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
	h5_file_t * const f,
	const char *fmt,
	va_list ap
	) {
	return h5_report_errorhandler ( f, fmt, ap );
}

h5_err_t
H5AbortErrorhandler (
	h5_file_t * const f,
	const char *fmt,
	va_list ap
	) {
	return h5_abort_errorhandler ( f, fmt, ap );
}

/*!
  \ingroup h5part_c_api_errhandling

  Get last error code.

  \return error code
*/
h5_err_t
H5GetErrno (
	h5_file_t * const f
	) {
	return h5_get_errno( f );
}
/*! @} */
