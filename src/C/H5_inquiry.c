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

#include "h5core/h5_core.h"
#include "H5.h"

/*!
  \ingroup h5_inquiry

  Get the number of compute nodes.

  \param[in]	f	File handle.

  \return Number of compute notes.
  \return \c -1 on error.
 */
int
H5GetNumNodes (
	h5_file_t* const f
	) {
	SET_FNAME (f, __func__);
	CHECK_FILEHANDLE (f);
	return h5_get_num_procs(f);
}

/*!
  \ingroup h5part_c_api_read

  Get the number of time-steps that are currently stored in the file
  \c f.

  It works for both reading and writing of files, but is probably
  only typically used when you are reading.

  \param[in]	f	File handle.

  \return	number of time-steps or error code
*/
h5_size_t
H5GetNumSteps (
	h5_file_t* const f
	) {

	SET_FNAME (f, __func__);
	CHECK_FILEHANDLE (f);

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
  
	SET_FNAME (f, __func__);
	CHECK_FILEHANDLE (f);

	return h5_has_step (f, stepno);
}



