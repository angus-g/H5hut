/*! \mainpage H5Part: A Portable High Performance Parallel Data Interface to HDF5

Particle based simulations of accelerator beam-lines, especially in
six dimensional phase space, generate vast amounts of data. Even
though a subset of statistical information regarding phase space or
analysis needs to be preserved, reading and writing such enormous
restart files on massively parallel supercomputing systems remains
challenging. 

H5Part consists of Particles and Block structured Fields.

Developed by:

<UL>
<LI> Andreas Adelmann (PSI) </LI>
<LI> Achim Gsell (PSI) </LI>
<LI> Benedikt Oswald (PSI) </LI>

<LI> Wes Bethel (NERSC/LBNL)</LI>
<LI> John Shalf (NERSC/LBNL)</LI>
<LI> Cristina Siegerist (NERSC/LBNL)</LI>
</UL>


Papers: 

<UL>
<LI> A. Adelmann, R.D. Ryne, C. Siegerist, J. Shalf,"From Visualization to Data Mining with Large Data Sets," <i>
<a href="http://www.sns.gov/pac05">Particle Accelerator Conference (PAC05)</a></i>, Knoxville TN., May 16-20, 2005. (LBNL-57603)
<a href="http://vis.lbl.gov/Publications/2005/FPAT082.pdf">FPAT082.pdf</a>
</LI>


<LI> A. Adelmann, R.D. Ryne, J. Shalf, C. Siegerist,"H5Part: A Portable High Performance Parallel Data Interface for Particle Simulations," <i>
<a href="http://www.sns.gov/pac05">Particle Accelerator Conference (PAC05)</a></i>, Knoxville TN., May 16-20, 2005.
<a href="http://vis.lbl.gov/Publications/2005/FPAT083.pdf">FPAT083.pdf</a>
</LI>
</UL>

For further information contact: <a href="mailto:h5part@lists.psi.ch">h5part</a>

Last modified on April 19, 2007.

*/


/*!
  \defgroup h5part_c_api H5Part C API

*/
/*!
  \ingroup h5part_c_api
  \defgroup h5part_openclose	File Opening and Closing
*/
/*!
  \ingroup h5part_c_api
  \defgroup h5part_write	File Writing
*/  
/*!
  \ingroup h5part_c_api
  \defgroup h5part_read		File Reading
*/  
/*!
  \ingroup h5part_c_api
  \defgroup h5part_attrib	Reading and Writing Attributes
*/
/*!
  \ingroup h5part_c_api
  \defgroup h5part_errhandle	Error Handling
*/
/*!
  \internal
  \defgroup h5partkernel H5Part private functions 
*/


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#ifndef WIN32
#include <unistd.h>
#else /* WIN32 */
#include <io.h>
#define open  _open
#define close _close
#endif /* WIN32 */

#include "h5/h5_core.h"
#include "h5/h5_private.h"
#include "H5Part.h"

extern h5part_error_handler	_err_handler;
extern h5part_int64_t		_h5part_errno;
extern unsigned			_debug;

/********* Private Variable Declarations *************/


/********** Declaration of private functions ******/


/*========== File Opening/Closing ===============*/


/*!
  \ingroup h5part_openclose

  Opens file with specified filename. 

  If you open with flag \c H5PART_WRITE, it will truncate any
  file with the specified filename and start writing to it. If 
  you open with \c H5PART_APPEND, then you can append new steps.
  If you open with \c H5PART_READ, then it will open the file
  readonly.

  The typical extension for these files is \c .h5.
  
  h5_file should be treated as an essentially opaque
  datastructure.  It acts as the file handle, but internally
  it maintains several key state variables associated with 
  the file.

  \return	File handle or \c NULL
 */
h5_file*
H5PartOpenFileParallel (
	const char *filename,	/*!< [in] The name of the data file to open. */
	unsigned flags,		/*!< [in] The access mode for the file. */
	MPI_Comm comm		/*!< [in] MPI communicator */
) {
	return h5_open_file ( filename, flags, comm );
}

/*!
  \ingroup  h5part_openclose

  Opens file with specified filename. 

  If you open with flag \c H5PART_WRITE, it will truncate any
  file with the specified filename and start writing to it. If 
  you open with \c H5PART_APPEND, then you can append new steps.
  If you open with \c H5PART_READ, then it will open the file
  readonly.

  The typical extension for these files is \c .h5.
  
  h5_file should be treated as an essentially opaque
  datastructure.  It acts as the file handle, but internally
  it maintains several key state variables associated with 
  the file.

  \return	File handle or \c NULL
 */

h5_file*
H5PartOpenFile (
	const char *filename,	/*!< [in] The name of the data file to open. */
	unsigned flags		/*!< [in] The access mode for the file. */
	) {

	SET_FNAME ( "H5PartOpenFile" );

	MPI_Comm comm = 0;	/* dummy */

	return h5_open_file ( filename, flags, comm );
}

/*!
  \ingroup h5part_openclose

  Closes an open file.

  \return	\c H5PART_SUCCESS or error code
*/
h5part_int64_t
H5PartCloseFile (
	h5_file *f		/*!< [in] filehandle of the file to close */
	) {

	SET_FNAME ( "H5PartCloseFile" );

	return h5_close_file( f );
}

/*============== File Writing Functions ==================== */

h5part_int64_t
H5PartDefineStepName (
	h5_file *f,
	const char *name,
	const h5part_int64_t width
	) {
	SET_FNAME ( "H5PartDefineStepName" );

	return h5_define_stepname_fmt( f, name, width );
}

/*!
  \ingroup h5part_write

  Set number of particles for current time-step.

  This function's sole purpose is to prevent 
  needless creation of new HDF5 DataSpace handles if the number of 
  particles is invariant throughout the simulation. That's its only reason 
  for existence. After you call this subroutine, all subsequent 
  operations will assume this number of particles will be written.


  \return	\c H5PART_SUCCESS or error code
 */
h5part_int64_t
H5PartSetNumParticles (
	h5_file *f,			/*!< [in] Handle to open file */
	h5part_int64_t nparticles	/*!< [in] Number of particles */
	) {

	SET_FNAME ( "H5PartSetNumParticles" );

	return H5U_set_num_elements( f, nparticles );
}

/*!
  \ingroup h5part_write

  Write array of 64 bit floating point data to file.

  After setting the number of particles with \c H5PartSetNumParticles() and
  the current step using \c H5PartSetStep(), you can start writing datasets
  into the file. Each dataset has a name associated with it (chosen by the
  user) in order to facilitate later retrieval. The name of the dataset is
  specified in the parameter \c name, which must be a null-terminated string.

  There are no restrictions on naming of datasets, but it is useful to arrive
  at some common naming convention when sharing data with other groups.

  The writing routines also implicitly store the datatype of the array so that
  the array can be reconstructed properly on other systems with incompatible
  type representations.

  All data that is written after setting the step is associated with that
  step. While the number of particles can change for each step, you
  cannot change the number of particles in the middle of a given step.

  The data is committed to disk before the routine returns.

  \return	\c H5PART_SUCCESS or error code
 */
h5part_int64_t
H5PartWriteDataFloat64 (
	h5_file *f,			/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name to associate array with */
	const h5part_float64_t *array	/*!< [in] Array to commit to disk */
	) {

	SET_FNAME ( "H5PartWriteDataFloat64" );

	return H5U_write_data ( f, name, (void*)array, H5T_NATIVE_DOUBLE );
}

/*!
  \ingroup h5part_write

  Write array of 64 bit integer data to file.

  After setting the number of particles with \c H5PartSetNumParticles() and
  the current step using \c H5PartSetStep(), you can start writing datasets
  into the file. Each dataset has a name associated with it (chosen by the
  user) in order to facilitate later retrieval. The name of the dataset is
  specified in the parameter \c name, which must be a null-terminated string.

  There are no restrictions on naming of datasets, but it is useful to arrive
  at some common naming convention when sharing data with other groups.

  The writing routines also implicitly store the datatype of the array so that
  the array can be reconstructed properly on other systems with incompatible
  type representations.

  All data that is written after setting the step is associated with that
  step. While the number of particles can change for each step, you
  cannot change the number of particles in the middle of a given step.

  The data is committed to disk before the routine returns.

  \return	\c H5PART_SUCCESS or error code
 */
h5part_int64_t
H5PartWriteDataInt64 (
	h5_file *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate array with */
	const h5part_int64_t *array	/*!< [in] Array to commit to disk */
	) {

	SET_FNAME ( "H5PartOpenWriteDataInt64" );

	return H5U_write_data ( f, name, (void*)array, H5T_NATIVE_INT64 );
}

/********************** reading and writing attribute ************************/

/********************** private functions to handle attributes ***************/


/********************** attribute API ****************************************/

/*!
  \ingroup h5part_attrib

  Writes a string attribute bound to a file.

  This function creates a new attribute \c name with the string \c value as
  content. The attribute is bound to the file associated with the file handle 
  \c f.

  If the attribute already exists an error will be returned. There
  is currently no way to change the content of an existing attribute.

  \return	\c H5PART_SUCCESS or error code   
*/
h5part_int64_t
H5PartWriteFileAttribString (
	h5_file *f,		/*!< [in] Handle to open file */
	const char *attrib_name,/*!< [in] Name of attribute to create */
	const char *attrib_value/*!< [in] Value of attribute */ 
	) {

	SET_FNAME ( "H5PartWriteFileAttribString" );

   	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	return h5_write_attrib (
		f->root_gid,
		attrib_name,
		H5T_NATIVE_CHAR,
		attrib_value,
		strlen ( attrib_value ) + 1 );
}

/*!
  \ingroup h5part_attrib

  Writes a string attribute bound to the current time-step.

  This function creates a new attribute \c name with the string \c value as
  content. The attribute is bound to the current time step in the file given
  by the file handle \c f.

  If the attribute already exists an error will be returned. There
  is currently no way to change the content of an existing attribute.

  \return	\c H5PART_SUCCESS or error code   
*/

h5part_int64_t
H5PartWriteStepAttribString (
	h5_file *f,		/*!< [in] Handle to open file */
	const char *attrib_name,/*!< [in] Name of attribute to create */
	const char *attrib_value/*!< [in] Value of attribute */ 
	) {

	SET_FNAME ( "H5PartWriteStepAttribString" );

   	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	return h5_write_attrib (
		f->step_gid,
		attrib_name,
		H5T_NATIVE_CHAR,
		attrib_value,
		strlen ( attrib_value ) + 1 );
}

/*!
  \ingroup h5part_attrib

  Writes a attribute bound to the current time-step.

  This function creates a new attribute \c name with the string \c value as
  content. The attribute is bound to the current time step in the file given
  by the file handle \c f.

  The value of the attribute is given the parameter \c type, which must be one
  of \c H5T_NATIVE_DOUBLE, \c H5T_NATIVE_INT64 of \c H5T_NATIVE_CHAR, the array
  \c value and the number of elements \c nelem in the array.

  If the attribute already exists an error will be returned. There
  is currently no way to change the content of an existing attribute.

  \return	\c H5PART_SUCCESS or error code   
*/

h5part_int64_t
H5PartWriteStepAttrib (
	h5_file *f,			/*!< [in] Handle to open file */
	const char *attrib_name,	/*!< [in] Name of attribute */
	const h5part_int64_t attrib_type,/*!< [in] Type of value. */
	const void *attrib_value,	/*!< [in] Value of attribute */ 
	const h5part_int64_t attrib_nelem/*!< [in] Number of elements */
	){

	SET_FNAME ( "H5PartWriteStepAttrib" );

   	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	return h5_write_attrib (
		f->step_gid,
		attrib_name,
		(const hid_t)attrib_type,
		attrib_value,
		attrib_nelem );
}

/*!
  \ingroup h5part_attrib

  Writes a attribute bound to a file.

  This function creates a new attribute \c name with the string \c value as
  content. The attribute is bound to the file file given by the file handle
  \c f.

  The value of the attribute is given the parameter \c type, which must be one
  of H5T_NATIVE_DOUBLE, H5T_NATIVE_INT64 of H5T_NATIVE_CHAR, the array \c value
  and the number of elements \c nelem in the array.

  If the attribute already exists an error will be returned. There
  is currently no way to change the content of an existing attribute.

  \return	\c H5PART_SUCCESS or error code   
*/

h5part_int64_t
H5PartWriteFileAttrib (
	h5_file *f,			/*!< [in] Handle to open file */
	const char *attrib_name,	/*!< [in] Name of attribute */
	const h5part_int64_t attrib_type,/*!< [in] Type of value. */
	const void *attrib_value,	/*!< [in] Value of attribute */ 
	const h5part_int64_t attrib_nelem/*!< [in] Number of elements */
	) {

	SET_FNAME ( "H5PartWriteFileAttrib" );

   	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	return h5_write_attrib (
		f->root_gid,
		attrib_name,
		(const hid_t)attrib_type,
		attrib_value,
		attrib_nelem );
}

/*!
  \ingroup h5part_attrib

  Gets the number of attributes bound to the current step.

  \return	Number of attributes bound to current time step or error code.
*/
h5part_int64_t
H5PartGetNumStepAttribs (
	h5_file *f			/*!< [in] Handle to open file */
	) {

	SET_FNAME ( "H5PartGetNumStepAttribs" );

   	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	return h5_get_num_attribs ( f, f->step_gid );
}

/*!
  \ingroup h5part_attrib

  Gets the number of attributes bound to the file.

  \return	Number of attributes bound to file \c f or error code.
*/
h5part_int64_t
H5PartGetNumFileAttribs (
	h5_file *f			/*!< [in] Handle to open file */
	) {

	SET_FNAME ( "H5PartGetNumFileAttribs" );

   	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	return h5_get_num_attribs ( f, f->root_gid );
}

/*!
  \ingroup h5part_attrib

  Gets the name, type and number of elements of the step attribute
  specified by its index.

  This function can be used to retrieve all attributes bound to the
  current time-step by looping from \c 0 to the number of attribute
  minus one.  The number of attributes bound to the current
  time-step can be queried by calling the function
  \c H5PartGetNumStepAttribs().

  \return	\c H5PART_SUCCESS or error code 
*/
h5part_int64_t
H5PartGetStepAttribInfo (
	h5_file *f,			/*!< [in]  Handle to open file */
	const h5part_int64_t attrib_idx,/*!< [in]  Index of attribute to
					           get infos about */
	char *attrib_name,		/*!< [out] Name of attribute */
	const h5part_int64_t len_of_attrib_name,
					/*!< [in]  length of buffer \c name */
	h5part_int64_t *attrib_type,	/*!< [out] Type of value. */
	h5part_int64_t *attrib_nelem	/*!< [out] Number of elements */
	) {
	
	SET_FNAME ( "H5PartGetStepAttribInfo" );

   	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	return h5_get_attrib_info (
		f->step_gid,
		attrib_idx,
		attrib_name,
		len_of_attrib_name,
		attrib_type,
		attrib_nelem );
}

/*!
  \ingroup h5part_attrib

  Gets the name, type and number of elements of the file attribute
  specified by its index.

  This function can be used to retrieve all attributes bound to the
  file \c f by looping from \c 0 to the number of attribute minus
  one.  The number of attributes bound to file \c f can be queried
  by calling the function \c H5PartGetNumFileAttribs().

  \return	\c H5PART_SUCCESS or error code 
*/

h5part_int64_t
H5PartGetFileAttribInfo (
	h5_file *f,			/*!< [in]  Handle to open file */
	const h5part_int64_t attrib_idx,/*!< [in]  Index of attribute to get
					           infos about */
	char *attrib_name,		/*!< [out] Name of attribute */
	const h5part_int64_t len_of_attrib_name,
					/*!< [in]  length of buffer \c name */
	h5part_int64_t *attrib_type,	/*!< [out] Type of value. */
	h5part_int64_t *attrib_nelem	/*!< [out] Number of elements */
	) {

	SET_FNAME ( "H5PartGetFileAttribInfo" );

	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	return h5_get_attrib_info (
		f->root_gid,
		attrib_idx,
		attrib_name,
		len_of_attrib_name,
		attrib_type,
		attrib_nelem );
}

/*!
  \ingroup h5part_attrib

  Reads an attribute bound to current time-step.

  \return \c H5PART_SUCCESS or error code 
*/
h5part_int64_t
H5PartReadStepAttrib (
	h5_file *f,			/*!< [in]  Handle to open file */
	const char *attrib_name,	/*!< [in] Name of attribute to read */
	void *attrib_value		/*!< [out] Value of attribute */
	) {

	SET_FNAME ( "H5PartReadStepAttrib" );

	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	return h5_read_attrib ( f->step_gid, attrib_name, attrib_value );
}

/*!
  \ingroup h5part_attrib

  Reads an attribute bound to file \c f.

  \return \c H5PART_SUCCESS or error code 
*/
h5part_int64_t
H5PartReadFileAttrib ( 
	h5_file *f,
	const char *attrib_name,
	void *attrib_value
	) {

	SET_FNAME ( "H5PartReadFileAttrib" );

	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	return h5_read_attrib ( f->root_gid, attrib_name, attrib_value );
}


/*================== File Reading Routines =================*/
/*
  H5PartSetStep:


  So you use this to random-access the file for a particular step.
  Failure to explicitly set the step on each read will leave you
  stuck on the same step for *all* of your reads.  That is to say
  the writes auto-advance the file pointer, but the reads do not
  (they require explicit advancing by selecting a particular step).
*/

/*!
  \ingroup h5part_read

  Set the current time-step.

  When writing data to a file the current time step must be set first
  (even if there is only one). In write-mode this function creates a new
  time-step! You are not allowed to step to an already existing time-step.
  This prevents you from overwriting existing data. Another consequence is,
  that you \b must write all data before going to the next time-step.

  In read-mode you can use this function to random-access the file for a
  particular step.

  \return \c H5PART_SUCCESS or error code 
*/
h5part_int64_t
H5PartSetStep (
	h5_file *f,			/*!< [in]  Handle to open file */
	const h5part_int64_t step	/*!< [in]  Time-step to set. */
	) {

	SET_FNAME ( "H5PartSetStep" );

	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	return h5_set_step ( f, step );
}

/********************** query file structure *********************************/


/*!
  \ingroup h5part_read

  Query whether a particular step already exists in the file
  \c f.

  It works for both reading and writing of files

  \return      true or false
*/
h5part_int64_t
H5PartHasStep (
	h5_file *f,		/*!< [in]  Handle to open file */
	h5part_int64_t step	/*!< [in]  Step number to query */
	) {
  
	SET_FNAME ( "H5PartHasStep" );

	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	return h5_has_step( f, step );
}

/*!
  \ingroup h5part_read

  Get the number of time-steps that are currently stored in the file
  \c f.

  It works for both reading and writing of files, but is probably
  only typically used when you are reading.

  \return	number of time-steps or error code
*/
h5part_int64_t
H5PartGetNumSteps (
	h5_file *f			/*!< [in]  Handle to open file */
	) {

	SET_FNAME ( "H5PartGetNumSteps" );

	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	return h5_get_num_objects_matching_pattern (
		f->file,
		"/",
		H5G_UNKNOWN,
		f->prefix_step_name );
}

/*!
  \ingroup h5part_read

  Get the number of datasets that are stored at the current time-step.

  \return	number of datasets in current step or error code
*/

h5part_int64_t
H5PartGetNumDatasets (
	h5_file *f			/*!< [in]  Handle to open file */
	) {

	SET_FNAME ( "H5PartGetNumDatasets" );

	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	return h5_get_num_objects ( f->file, f->step_name, H5G_DATASET );
}

/*!
  \ingroup h5part_read

  This reads the name of a dataset specified by it's index in the current
  time-step.

  If the number of datasets is \c n, the range of \c _index is \c 0 to \c n-1.

  \result	\c H5PART_SUCCESS
*/
h5part_int64_t
H5PartGetDatasetName (
	h5_file *f,			/*!< [in]  Handle to open file */
	const h5part_int64_t idx,	/*!< [in]  Index of the dataset */
	char *name,			/*!< [out] Name of dataset */
	const h5part_int64_t len_of_name/*!< [in]  Size of buffer \c name */
	) {

	SET_FNAME ( "H5PartGetDatasetName" );

	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	return h5_get_object_name (
		f->file,
		f->step_name,
		H5G_DATASET,
		idx,
		name,
		len_of_name );
}

/*!
  \ingroup h5part_read

  Gets the name, type and number of elements of a dataset specified by it's
  index in the current time-step.

  Type is one of \c H5T_NATIVE_DOUBLE or \c H5T_NATIVE_INT64.

  \return	\c H5PART_SUCCESS
*/
h5part_int64_t
H5PartGetDatasetInfo (
	h5_file *f,		/*!< [in]  Handle to open file */
	const h5part_int64_t idx,/*!< [in]  Index of the dataset */
	char *dataset_name,	/*!< [out] Name of dataset */
	const h5part_int64_t len_dataset_name,
				/*!< [in]  Size of buffer \c dataset_name */
	h5part_int64_t *type,	/*!< [out] Type of data in dataset */
	h5part_int64_t *nelem	/*!< [out] Number of elements. */
	) {

	SET_FNAME ( "H5PartGetDatasetInfo" );

	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	return H5U_get_dataset_info ( f, idx,
			      dataset_name, len_dataset_name, type, nelem );
}

/*!
  \ingroup h5part_read

  This gets the number of particles stored in the current step. 
  It will arbitrarily select a time-step if you haven't already set
  the step with \c H5PartSetStep().

  \return	number of particles in current step or an error
		code.
 */
h5part_int64_t
H5PartGetNumParticles (
	h5_file *f			/*!< [in]  Handle to open file */
	) {

	SET_FNAME ( "H5PartGetNumParticles" );

	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	if ( f->step_gid < 0 ) {
		h5part_int64_t herr = h5_set_step ( f, 0 );
		if ( herr < 0 ) return herr;
	}

	return H5U_get_num_elems ( f );
}

/*!
  \ingroup h5part_read
*/
h5part_int64_t
H5PartResetView (
 	h5_file *f			/*!< [in]  Handle to open file */
	) {
	SET_FNAME ( "H5PartResetView" );

	if ( h5_check_filehandle ( f ) != H5PART_SUCCESS )
		return _h5part_errno;

	CHECK_READONLY_MODE ( f );

	return H5U_reset_view ( f );
}

/*!
  \ingroup h5part_read
*/
h5part_int64_t
H5PartHasView (
 	h5_file *f			/*!< [in]  Handle to open file */
	) {
	SET_FNAME ( "H5PartResetView" );

	CHECK_FILEHANDLE( f );
	CHECK_READONLY_MODE ( f );

	return  ( f->viewstart >= 0 ) && ( f->viewend >= 0 );
}


/*!
  \ingroup h5part_read

  For parallel I/O or for subsetting operations on the datafile, the
  \c H5PartSetView() function allows you to define a subset of the total
  particle dataset to read.  The concept of "view" works for both serial
  and for parallel I/O.  The "view" will remain in effect until a new view
  is set, or the number of particles in a dataset changes, or the view is
  "unset" by calling \c H5PartSetView(file,-1,-1);

  Before you set a view, the \c H5PartGetNumParticles() will return the
  total number of particles in the current time-step (even for the parallel
  reads).  However, after you set a view, it will return the number of
  particles contained in the view.

  The range is inclusive (the start and the end index).

  \return	\c H5PART_SUCCESS or error code
*/
h5part_int64_t
H5PartSetView (
	h5_file *f,			/*!< [in]  Handle to open file */
	const h5part_int64_t start,	/*!< [in]  Start particle */
	const h5part_int64_t end	/*!< [in]  End particle */
	) {

	SET_FNAME ( "H5PartSetView" );

	CHECK_FILEHANDLE( f );
	CHECK_READONLY_MODE ( f );

	if ( f->step_gid < 0 ) {
		h5part_int64_t herr = h5_set_step ( f, 0 );
		if ( herr < 0 ) return herr;
	}

	return H5U_set_view ( f, start, end );
}

/*!
  \ingroup h5part_read

   Allows you to query the current view. Start and End
   will be \c -1 if there is no current view established.
   Use \c H5PartHasView() to see if the view is smaller than the
   total dataset.

   \return       the number of elements in the view 
*/
h5part_int64_t
H5PartGetView (
	h5_file *f,			/*!< [in]  Handle to open file */
	h5part_int64_t *start,		/*!< [out]  Start particle */
	h5part_int64_t *end		/*!< [out]  End particle */
	) {

	SET_FNAME ( "H5PartGetView" );

	CHECK_FILEHANDLE( f );

	if ( f->step_gid < 0 ) {
		h5part_int64_t herr = h5_set_step ( f, 0 );
		if ( herr < 0 ) return herr;
	}
	return H5U_get_view( f, start, end );
}

/*!
  \ingroup h5part_read

  If it is too tedious to manually set the start and end coordinates
  for a view, the \c H5SetCanonicalView() will automatically select an
  appropriate domain decomposition of the data arrays for the degree
  of parallelism and set the "view" accordingly.

  \return		H5PART_SUCCESS or error code
*/
/*
  \note
  There is a bug in this function:
  If (NumParticles % f->nprocs) != 0  then
  the last  (NumParticles % f->nprocs) particles are not handled!
*/

h5part_int64_t
H5PartSetCanonicalView (
	h5_file *f			/*!< [in]  Handle to open file */
	) {

	SET_FNAME ( "H5PartSetCanonicalView" );

	h5part_int64_t herr;

	CHECK_FILEHANDLE( f );
	CHECK_READONLY_MODE ( f )

	if ( f->step_gid < 0 ) {
		herr = h5_set_step ( f, 0 );
		if ( herr < 0 ) return herr;
	}

	return H5U_set_canonical_view ( f );
}

/*!
  \ingroup h5part_read

  Read array of 64 bit floating point data from file.

  When retrieving datasets from disk, you ask for them
  by name. There are no restrictions on naming of arrays,
  but it is useful to arrive at some common naming
  convention when sharing data with other groups.

  \return	\c H5PART_SUCCESS or error code
*/
h5part_int64_t
H5PartReadDataFloat64 (
	h5_file *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate dataset with */
	h5part_float64_t *array	/*!< [out] Array of data */
	) {

	SET_FNAME ( "H5PartReadDataFloat64" );

	CHECK_FILEHANDLE( f );

	return H5U_read_elems ( f, name, array, H5T_NATIVE_DOUBLE );
}

/*!
  \ingroup h5part_read

  Read array of 64 bit floating point data from file.

  When retrieving datasets from disk, you ask for them
  by name. There are no restrictions on naming of arrays,
  but it is useful to arrive at some common naming
  convention when sharing data with other groups.

  \return	\c H5PART_SUCCESS or error code
*/
h5part_int64_t
H5PartReadDataInt64 (
	h5_file *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate dataset with */
	h5part_int64_t *array	/*!< [out] Array of data */
	) {

	SET_FNAME ( "H5PartReadDataInt64" );

	CHECK_FILEHANDLE( f );

	return H5U_read_elems ( f, name, array, H5T_NATIVE_INT64 );
}

/*!
  \ingroup h5part_read

  This is the mongo read function that pulls in all of the data for a
  given step in one shot. It also takes the step as an argument
  and will call \c H5PartSetStep() internally so that you don't have to 
  make that call separately.

  \note
  See also \c H5PartReadDataInt64() and \c H5PartReadDataFloat64() if you want
  to just read in one of the many datasets.

  \return	\c H5PART_SUCCESS or error code
*/
h5part_int64_t
H5PartReadParticleStep (
	h5_file *f,		/*!< [in]  Handle to open file */
	h5part_int64_t step,	/*!< [in]  Step to read */
	h5part_float64_t *x,	/*!< [out] Buffer for dataset named "x" */
	h5part_float64_t *y,	/*!< [out] Buffer for dataset named "y" */
	h5part_float64_t *z,	/*!< [out] Buffer for dataset named "z" */
	h5part_float64_t *px,	/*!< [out] Buffer for dataset named "px" */
	h5part_float64_t *py,	/*!< [out] Buffer for dataset named "py" */
	h5part_float64_t *pz,	/*!< [out] Buffer for dataset named "pz" */
	h5part_int64_t *id	/*!< [out] Buffer for dataset named "id" */
	) {

	SET_FNAME ( "H5PartReadParticleStep" );
	h5part_int64_t herr;

	CHECK_FILEHANDLE( f );

	herr = h5_set_step ( f, step );
	if ( herr < 0 ) return herr;

	herr = H5U_read_elems ( f, "x", (void*)x, H5T_NATIVE_DOUBLE );
	if ( herr < 0 ) return herr;

	herr = H5U_read_elems ( f, "y", (void*)y, H5T_NATIVE_DOUBLE );
	if ( herr < 0 ) return herr;

	herr = H5U_read_elems ( f, "z", (void*)z, H5T_NATIVE_DOUBLE );
	if ( herr < 0 ) return herr;

	herr = H5U_read_elems ( f, "px", (void*)px, H5T_NATIVE_DOUBLE );
	if ( herr < 0 ) return herr;

	herr = H5U_read_elems ( f, "py", (void*)py, H5T_NATIVE_DOUBLE );
	if ( herr < 0 ) return herr;

	herr = H5U_read_elems ( f, "pz", (void*)pz, H5T_NATIVE_DOUBLE );
	if ( herr < 0 ) return herr;

	herr = H5U_read_elems ( f, "id", (void*)id, H5T_NATIVE_INT64 );
	if ( herr < 0 ) return herr;

	return H5PART_SUCCESS;
}

/****************** error handling ******************/

/*!
  \ingroup h5part_errhandle

  Set verbosity level to \c level.

  \return \c H5PART_SUCCESS
*/
h5part_int64_t
H5PartSetVerbosityLevel (
	const h5part_int64_t level
	) {

	return h5_set_debuglevel ( level );
}

/*!
  \ingroup h5part_errhandle

  Set error handler to \c handler.

  \return \c H5PART_SUCCESS
*/
h5part_int64_t
H5PartSetErrorHandler (
	h5part_error_handler handler
	) {
  
	return h5_set_errorhandler( handler );
}

/*!
  \ingroup h5part_errhandle

  Get current error handler.

  \return Pointer to error handler.
*/
h5part_error_handler
H5PartGetErrorHandler (
	void
	) {
	return h5_get_errorhandler();
}

/*!
  \ingroup h5part_errhandle

  Get last error code.

  \return error code
*/
h5part_int64_t
H5PartGetErrno (
	void
	) {
	return h5_get_errno();
}
/*! @} */

