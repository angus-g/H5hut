/*
  Copyright 2006-2007
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
			ThisIsAFunction()
		Return values:
			-1 or NULL signals an error

  \note
  In function names we use the words \b get and \b store insteed of
  \b read and \b write, because no I/O is actually done in these
  functions.
*/


/*!
  \defgroup h5fed_c_api H5Fed C API
*/

#include <hdf5.h>
#include "h5/h5_types.h"
#include "H5Fed.h"


/******	General routines *****************************************************/

/*!
  \ingroup h5fed_c_api
  
  Open file with name \c filename. This function is available in the paralell
  and serial version. In the serial case \c comm may have any value.

  \return File handle.
  \return NULL on error.

  \note
  File is always opened in read/writer mode!

  \note
  Implement as wrapper of \c H5_open_file()!
*/
h5_file * H5FedOpenFile (
	const char * filename,		/*!< file name			*/
	const MPI_Comm comm		/*!< MPI communicator		*/
	) {
	return NULL;
}

/*!
  \ingroup h5fed_c_api

  Close file.

  \return value \c >=0 on success
  \return -1 on error
*/
h5_int_t H5FedCloseFile (
	h5_file * fh			/*!< file handle		*/
	) {
	return -1;
}

/******	INQUIRY routines *****************************************************/

/*!
  \ingroup h5_c_api

  Get the number of compute nodes.

  \return Number of compute notes.
  \return \c -1 on error.
 */
h5_int_t H5GetNumNodes (
	h5_file * fh			/*!< file handle		*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Get the number of hierarchical mesh levels available in current step.

  \return Number of hierarchical mesh levels
  \return \c -1 on error
 */
h5_int_t H5FedGetNumMeshLevels (
	h5_file * fh			/*!< file handle		*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Check whether a tetrahedral mesh hierarchy exists on level \c level

  \return  \c 1		tetrahedral mesh hierarchy exists on given level. 
  \return  \c 0		tetrahedral mesh exists, but not on given level.
  \return  \c -1	tetrahedral mesh doesn't exist
 */
h5_int_t H5FedHasTetrahedralMesh (
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to check	*/
	) {
	return -1;
}

/*!
  \ingroup h5fed_c_api

  Check whether a boundary mesh hierarchy exists on level \c level

  \return \c 1		boundary mesh hierarchy exists on given level.
  \return \c 0		boundary mesh hierarchy exists, but not on given level.
  \return \c -1		boundary mesh doesn't exist.
*/
h5_int_t H5FedHasBoundaryMesh(
	h5_file * fh,			/*!< file handle		*/
	const h5_id_t level		/*!< mesh level to check	*/
	) {
	return -1;
}

