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
  \ingroup h5f_c_api
  \defgroup h5_inquiry
*/

#include <stdarg.h>
#include <hdf5.h>
#include "h5/h5.h"
#include "h5/h5_private.h"
#include "H5Fed.h"

/*!
  \ingroup h5_inquiry

  Get the number of compute nodes.

  \return Number of compute notes.
  \return \c -1 on error.
 */
h5_size_t
H5GetNumNodes (
	h5_file * f			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return (h5_size_t)fh->nprocs;
}

/*`
  \ingroup h5_inquiry

  Get the number of steps that are currently stored in the file.

  \return	number of steps or error code
*/
h5_id_t
H5GetNumSteps (
	h5_file * fh			/*!< file handle		*/
	) {
	SET_FNAME ( __func__ );
	return h5_get_num_steps( fh );
}

/*!
  \ingroup h5_inquiry

  Query whether a particular step already exists in the file.

  \return      true or false
*/
h5_err_t
H5HasStep (
	h5_file *f,		/*!< [in]  Handle to open file */
	h5part_int64_t step	/*!< [in]  Step number to query */
	) {
  
	SET_FNAME ( __func__ );

	return h5_has_step( f, step );
}



