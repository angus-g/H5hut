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
  \defgroup h5_c_api H5Fed C API
*/
/*!
  \ingroup h5_c_api
  \defgroup h5_general
*/


#include <stdarg.h>
#include <hdf5.h>
#include "h5/h5.h"
#include "h5/h5_private.h"
#include "H5Fed.h"


/******	General routines *****************************************************/

/*!
  \ingroup h5_general
  
  Open file with name \c filename. This function is available in the paralell
  and serial version. In the serial case \c comm may have any value.

  \return File handle.
  \return NULL on error.

  \note
  File is always opened in read/writer mode!
*/
h5_file *
H5OpenFile (
	const char * filename,		/*!< file name			*/
	const MPI_Comm comm		/*!< MPI communicator		*/
	) {
	SET_FNAME ( __func__ );
	return h5_open_file( filename, H5_O_RDWR, comm ); 
}

/*!
  \ingroup h5_general

  Close file.

  \return value \c >=0 on success
  \return -1 on error
*/
h5_err_t
H5CloseFile (
	h5_file * fh			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return h5_close_file ( fh );
}

/*!
  \ingroup h5_general

  Define format of the step names.

  Example: ==H5FedDefineStepNameFormat( f, "Step", 6 )== defines step names 
  like ==Step#000042==.

  \return value \c >=0 on success
  \return -1 on error
*/
h5_err_t
H5DefineStepNameFormat (
	h5_file *f,			/*!< Handle to file		*/
	const char *name,		/*!< Prefix			*/
	const h5part_int64_t width	/*!< Width of the number	*/
	) {
	SET_FNAME ( "H5PartDefineStepNameFormat" );

	return h5_define_stepname_fmt( f, name, width );
}

/*!
  \ingroup h5_general

  Get format of the step names.

  \return value \c >=0 on success
  \return -1 on error
*/
h5_err_t
H5GetStepNameFormat (
	h5_file *f,			/*!< Handle to file		*/
	char *name,			/*!< OUT: Prefix		*/
	h5_int64_t *l_name,		/*!< length of buffer name	*/
	h5_int64_t *width		/*!< OUT: Width of the number	*/
	) {
	SET_FNAME ( "H5PartDefineStepNameFormat" );

	return h5_get_stepname_fmt( f, name, l_name, width );
}

/*!
  \ingroup h5_general

  Set the current step.

  \return \c H5_SUCCESS or error code 
*/
h5_err_t
H5SetStep (
	h5_file *f,			/*!< [in]  Handle to open file */
	const h5_int64_t step		/*!< [in]  Step to set. */
	) {

	SET_FNAME ( __func__ );
	return h5_set_step ( f, step );
}

/*!
  \ingroup h5_general

  Get current step.

  \return \c H5_SUCCESS or error code 
*/
h5_int64_t
H5GetStep (
	h5_file *f			/*!< Handle to open file */
	) {

	SET_FNAME ( __func__ );
	return h5_get_step ( f );
}

/*!
  \ingroup h5_general

  Start traversing steps.

  \return \c H5_SUCCESS or error code 
*/
h5_err_t
H5StartTraverseSteps (
	h5_file *f			/*!< Handle to open file */
	) {

	SET_FNAME ( "H5FedStartTraverseSteps" );

	return -1;
}

/*!
  \ingroup h5_general

  Traverse steps.

  \return \c H5_SUCCESS or error code 
*/
h5_err_t
H5TraverseSteps (
	h5_file *f			/*!< Handle to open file */
	) {

	SET_FNAME ( "H5FedStartTraverseSteps" );

	return -1;
}
