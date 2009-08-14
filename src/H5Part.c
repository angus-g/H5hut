
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

#include "h5_core/h5_core.h"
#include "h5_core/h5_errorhandling_private.h"
#include "H5Part.h"

/********* Private Variable Declarations *************/


/********** Declaration of private functions ******/


/*========== File Opening/Closing ===============*/

/*!
  \ingroup h5part_c_api
  \defgroup h5part_c_api_openclose	File Opening and Closing
*/

/*!
  \ingroup h5part_c_api_openclose

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
h5_file_t *
H5PartOpenFileParallel (
	const char *filename,	/*!< [in] The name of the data file to open. */
	unsigned flags,		/*!< [in] The access mode for the file. */
	MPI_Comm comm		/*!< [in] MPI communicator */
) {
	return h5_open_file ( filename, flags, comm, __func__ );
}

/*!
  \ingroup  h5part_c_api_openclose

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

h5_file_t *
H5PartOpenFile (
	const char *filename,	/*!< [in] The name of the data file to open. */
	unsigned flags		/*!< [in] The access mode for the file. */
	) {

	MPI_Comm comm = 0;	/* dummy */

	return h5_open_file ( filename, flags, comm, __func__ );
}

/*!
  \ingroup h5part_c_api_openclose

  Closes an open file.

  \return	\c H5_SUCCESS or error code
*/
h5_int64_t
H5PartCloseFile (
	h5_file_t *f		/*!< [in] filehandle of the file to close */
	) {

	SET_FNAME ( f, __func__ );

	return h5_close_file( f );
}

/*============== File Writing Functions ==================== */

/*!
  \ingroup h5part_c_api
  \defgroup h5part_c_api_write	File Writing
*/  

h5_int64_t
H5PartDefineStepName (
	h5_file_t *f,
	const char *name,
	const h5_int64_t width
	) {
	SET_FNAME ( f, __func__ );

	return h5_set_stepname_fmt( f, name, width );
}

/*!
  \ingroup h5part_c_api_write

  Set number of particles for current time-step.

  This function's sole purpose is to prevent 
  needless creation of new HDF5 DataSpace handles if the number of 
  particles is invariant throughout the simulation. That's its only reason 
  for existence. After you call this subroutine, all subsequent 
  operations will assume this number of particles will be written.


  \return	\c H5_SUCCESS or error code
 */
h5_int64_t
H5PartSetNumParticles (
	h5_file_t *f,			/*!< [in] Handle to open file */
	h5_int64_t nparticles	/*!< [in] Number of particles */
	) {

	SET_FNAME ( f, __func__ );

	return h5u_set_num_elements( f, nparticles );
}

/*!
  \ingroup h5part_c_api_write

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

  \return	\c H5_SUCCESS or error code
 */
h5_int64_t
H5PartWriteDataFloat64 (
	h5_file_t *f,			/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name to associate array with */
	const h5_float64_t *array	/*!< [in] Array to commit to disk */
	) {

	SET_FNAME ( f, __func__ );

	return h5u_write_data ( f, name, (void*)array, H5T_NATIVE_DOUBLE );
}

/*!
  \ingroup h5part_c_api_write

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

  \return	\c H5_SUCCESS or error code
 */
h5_int64_t
H5PartWriteDataInt64 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate array with */
	const h5_int64_t *array	/*!< [in] Array to commit to disk */
	) {

	SET_FNAME ( f, __func__ );

	return h5u_write_data ( f, name, (void*)array, H5T_NATIVE_INT64 );
}

/********************** reading and writing attribute ************************/

/********************** private functions to handle attributes ***************/


/********************** attribute API ****************************************/

/*!
  \ingroup h5part_c_api
  \defgroup h5part_c_api_attrib	Reading and Writing Attributes
*/

/*!
  \ingroup h5part_c_api_attrib

  Writes a string attribute bound to a file.

  This function creates a new attribute \c name with the string \c value as
  content. The attribute is bound to the file associated with the file handle 
  \c f.

  If the attribute already exists an error will be returned. There
  is currently no way to change the content of an existing attribute.

  \return	\c H5_SUCCESS or error code   
*/
h5_int64_t
H5PartWriteFileAttribString (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *attrib_name,/*!< [in] Name of attribute to create */
	const char *attrib_value/*!< [in] Value of attribute */ 
	) {

	SET_FNAME ( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_write_attrib (
		f,
		f->root_gid,
		attrib_name,
		H5T_NATIVE_CHAR,
		attrib_value,
		strlen ( attrib_value ) + 1 );
}

/*!
  \ingroup h5part_c_api_attrib

  Writes a string attribute bound to the current time-step.

  This function creates a new attribute \c name with the string \c value as
  content. The attribute is bound to the current time step in the file given
  by the file handle \c f.

  If the attribute already exists an error will be returned. There
  is currently no way to change the content of an existing attribute.

  \return	\c H5_SUCCESS or error code   
*/

h5_int64_t
H5PartWriteStepAttribString (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *attrib_name,/*!< [in] Name of attribute to create */
	const char *attrib_value/*!< [in] Value of attribute */ 
	) {

	SET_FNAME ( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_write_attrib (
		f, 
		f->step_gid,
		attrib_name,
		H5T_NATIVE_CHAR,
		attrib_value,
		strlen ( attrib_value ) + 1 );
}

/*!
  \ingroup h5part_c_api_attrib

  Writes a attribute bound to the current time-step.

  This function creates a new attribute \c name with the string \c value as
  content. The attribute is bound to the current time step in the file given
  by the file handle \c f.

  The value of the attribute is given the parameter \c type, which must be one
  of \c H5T_NATIVE_DOUBLE, \c H5T_NATIVE_INT64 of \c H5T_NATIVE_CHAR, the array
  \c value and the number of elements \c nelem in the array.

  If the attribute already exists an error will be returned. There
  is currently no way to change the content of an existing attribute.

  \return	\c H5_SUCCESS or error code   
*/

h5_int64_t
H5PartWriteStepAttrib (
	h5_file_t *f,			/*!< [in] Handle to open file */
	const char *attrib_name,	/*!< [in] Name of attribute */
	const h5_int64_t attrib_type,/*!< [in] Type of value. */
	const void *attrib_value,	/*!< [in] Value of attribute */ 
	const h5_int64_t attrib_nelem/*!< [in] Number of elements */
	){

	SET_FNAME ( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_write_attrib (
		f,
		f->step_gid,
		attrib_name,
		(hid_t)attrib_type,
		attrib_value,
		attrib_nelem );
}

/*!
  \ingroup h5part_c_api_attrib

  Writes a attribute bound to a file.

  This function creates a new attribute \c name with the string \c value as
  content. The attribute is bound to the file file given by the file handle
  \c f.

  The value of the attribute is given the parameter \c type, which must be one
  of H5T_NATIVE_DOUBLE, H5T_NATIVE_INT64 of H5T_NATIVE_CHAR, the array \c value
  and the number of elements \c nelem in the array.

  If the attribute already exists an error will be returned. There
  is currently no way to change the content of an existing attribute.

  \return	\c H5_SUCCESS or error code   
*/

h5_int64_t
H5PartWriteFileAttrib (
	h5_file_t *f,			/*!< [in] Handle to open file */
	const char *attrib_name,	/*!< [in] Name of attribute */
	const h5_int64_t attrib_type,/*!< [in] Type of value. */
	const void *attrib_value,	/*!< [in] Value of attribute */ 
	const h5_int64_t attrib_nelem/*!< [in] Number of elements */
	) {

	SET_FNAME ( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_write_attrib (
		f,
		f->root_gid,
		attrib_name,
		(hid_t)attrib_type,
		attrib_value,
		attrib_nelem );
}

/*!
  \ingroup h5part_c_api_attrib

  Gets the number of attributes bound to the current step.

  \return	Number of attributes bound to current time step or error code.
*/
h5_int64_t
H5PartGetNumStepAttribs (
	h5_file_t *f			/*!< [in] Handle to open file */
	) {

	SET_FNAME ( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_get_num_attribs ( f, f->step_gid );
}

/*!
  \ingroup h5part_c_api_attrib

  Gets the number of attributes bound to the file.

  \return	Number of attributes bound to file \c f or error code.
*/
h5_int64_t
H5PartGetNumFileAttribs (
	h5_file_t *f			/*!< [in] Handle to open file */
	) {

	SET_FNAME ( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_get_num_attribs ( f, f->root_gid );
}

/*!
  \ingroup h5part_c_api_attrib

  Gets the name, type and number of elements of the step attribute
  specified by its index.

  This function can be used to retrieve all attributes bound to the
  current time-step by looping from \c 0 to the number of attribute
  minus one.  The number of attributes bound to the current
  time-step can be queried by calling the function
  \c H5PartGetNumStepAttribs().

  \return	\c H5_SUCCESS or error code 
*/
h5_int64_t
H5PartGetStepAttribInfo (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	const h5_int64_t attrib_idx,/*!< [in]  Index of attribute to
					           get infos about */
	char *attrib_name,		/*!< [out] Name of attribute */
	const h5_int64_t len_of_attrib_name,
					/*!< [in]  length of buffer \c name */
	h5_int64_t *attrib_type,	/*!< [out] Type of value. */
	h5_int64_t *attrib_nelem	/*!< [out] Number of elements */
	) {
	
	SET_FNAME ( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_get_attrib_info (
		f,
		f->step_gid,
		attrib_idx,
		attrib_name,
		len_of_attrib_name,
		attrib_type,
		attrib_nelem );
}

/*!
  \ingroup h5part_c_api_attrib

  Gets the name, type and number of elements of the file attribute
  specified by its index.

  This function can be used to retrieve all attributes bound to the
  file \c f by looping from \c 0 to the number of attribute minus
  one.  The number of attributes bound to file \c f can be queried
  by calling the function \c H5PartGetNumFileAttribs().

  \return	\c H5_SUCCESS or error code 
*/

h5_int64_t
H5PartGetFileAttribInfo (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	const h5_int64_t attrib_idx,/*!< [in]  Index of attribute to get
					           infos about */
	char *attrib_name,		/*!< [out] Name of attribute */
	const h5_int64_t len_of_attrib_name,
					/*!< [in]  length of buffer \c name */
	h5_int64_t *attrib_type,	/*!< [out] Type of value. */
	h5_int64_t *attrib_nelem	/*!< [out] Number of elements */
	) {

	SET_FNAME ( f, __func__ );

	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno ( f );

	return h5_get_attrib_info (
		f,
		f->root_gid,
		attrib_idx,
		attrib_name,
		len_of_attrib_name,
		attrib_type,
		attrib_nelem );
}

/*!
  \ingroup h5part_c_api_attrib

  Reads an attribute bound to current time-step.

  \return \c H5_SUCCESS or error code 
*/
h5_int64_t
H5PartReadStepAttrib (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	const char *attrib_name,	/*!< [in] Name of attribute to read */
	void *attrib_value		/*!< [out] Value of attribute */
	) {

	SET_FNAME ( f, __func__ );

	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno ( f );

	return h5_read_attrib ( f, f->step_gid, attrib_name, attrib_value );
}

/*!
  \ingroup h5part_c_api_attrib

  Reads an attribute bound to file \c f.

  \return \c H5_SUCCESS or error code 
*/
h5_int64_t
H5PartReadFileAttrib ( 
	h5_file_t *f,
	const char *attrib_name,
	void *attrib_value
	) {

	SET_FNAME ( f, __func__ );

	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno ( f );

	return h5_read_attrib ( f, f->root_gid, attrib_name, attrib_value );
}


/*================== File Reading Routines =================*/

/*!
  \ingroup h5part_c_api
  \defgroup h5part_c_api_read		File Reading
*/  

/*
  H5PartSetStep:


  So you use this to random-access the file for a particular step.
  Failure to explicitly set the step on each read will leave you
  stuck on the same step for *all* of your reads.  That is to say
  the writes auto-advance the file pointer, but the reads do not
  (they require explicit advancing by selecting a particular step).
*/

/*!
  \ingroup h5part_c_api_read

  Set the current time-step.

  When writing data to a file the current time step must be set first
  (even if there is only one). In write-mode this function creates a new
  time-step! You are not allowed to step to an already existing time-step.
  This prevents you from overwriting existing data. Another consequence is,
  that you \b must write all data before going to the next time-step.

  In read-mode you can use this function to random-access the file for a
  particular step.

  \return \c H5_SUCCESS or error code 
*/
h5_int64_t
H5PartSetStep (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	const h5_int64_t step	/*!< [in]  Time-step to set. */
	) {

	SET_FNAME ( f, __func__ );

	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno ( f );

	return h5_set_step ( f, step );
}

/********************** query file structure *********************************/

/*!
  \ingroup h5part_c_api_read

  Get the number of datasets that are stored at the current time-step.

  \return	number of datasets in current step or error code
*/

h5_int64_t
H5PartGetNumDatasets (
	h5_file_t *f			/*!< [in]  Handle to open file */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_FILEHANDLE ( f );

	return hdf5_get_num_objects ( f->file, f->step_name, H5G_DATASET );
}

/*!
  \ingroup h5part_c_api_read

  This reads the name of a dataset specified by it's index in the current
  time-step.

  If the number of datasets is \c n, the range of \c _index is \c 0 to \c n-1.

  \result	\c H5_SUCCESS
*/
h5_int64_t
H5PartGetDatasetName (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	const h5_int64_t idx,	/*!< [in]  Index of the dataset */
	char *name,			/*!< [out] Name of dataset */
	const h5_int64_t len_of_name/*!< [in]  Size of buffer \c name */
	) {

	SET_FNAME ( f, __func__ );

	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno ( f );

	return hdf5_get_object_name (
		f->file,
		f->step_name,
		H5G_DATASET,
		idx,
		name,
		len_of_name );
}

/*!
  \ingroup h5part_c_api_read

  Gets the name, type and number of elements of a dataset specified by it's
  index in the current time-step.

  Type is one of \c H5T_NATIVE_DOUBLE or \c H5T_NATIVE_INT64.

  \return	\c H5_SUCCESS
*/
h5_int64_t
H5PartGetDatasetInfo (
	h5_file_t *f,		/*!< [in]  Handle to open file */
	const h5_int64_t idx,/*!< [in]  Index of the dataset */
	char *dataset_name,	/*!< [out] Name of dataset */
	const h5_int64_t len_dataset_name,
				/*!< [in]  Size of buffer \c dataset_name */
	h5_int64_t *type,	/*!< [out] Type of data in dataset */
	h5_int64_t *nelem	/*!< [out] Number of elements. */
	) {

	SET_FNAME ( f, __func__ );

	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno ( f );

	return h5u_get_dataset_info (
		f, idx, dataset_name, len_dataset_name, type, nelem );
}

/*!
  \ingroup h5part_c_api_read

  This gets the number of particles stored in the current step. 
  It will arbitrarily select a time-step if you haven't already set
  the step with \c H5PartSetStep().

  \return	number of particles in current step or an error
		code.
 */
h5_int64_t
H5PartGetNumParticles (
	h5_file_t *f			/*!< [in]  Handle to open file */
	) {

	SET_FNAME ( f, __func__ );

	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno ( f );

	if ( f->step_gid < 0 ) {
		h5_int64_t herr = h5_set_step ( f, 0 );
		if ( herr < 0 ) return herr;
	}

	return h5u_get_num_elems ( f );
}

/*!
  \ingroup h5part_c_api_read
*/
h5_int64_t
H5PartResetView (
 	h5_file_t *f			/*!< [in]  Handle to open file */
	) {
	SET_FNAME ( f, __func__ );

	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno ( f );

	CHECK_READONLY_MODE ( f );

	return h5u_reset_view ( f );
}

/*!
  \ingroup h5part_c_api_read
*/
h5_int64_t
H5PartHasView (
 	h5_file_t *f			/*!< [in]  Handle to open file */
	) {
	SET_FNAME ( f, __func__ );

	CHECK_FILEHANDLE( f );
	CHECK_READONLY_MODE ( f );

	return  h5u_has_view ( f );
}


/*!
  \ingroup h5part_c_api_read

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

  \return	\c H5_SUCCESS or error code
*/
h5_int64_t
H5PartSetView (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	const h5_int64_t start,	/*!< [in]  Start particle */
	const h5_int64_t end	/*!< [in]  End particle */
	) {

	SET_FNAME ( f, __func__ );

	CHECK_FILEHANDLE( f );
	CHECK_READONLY_MODE ( f );

	if ( f->step_gid < 0 ) {
		h5_int64_t herr = h5_set_step ( f, 0 );
		if ( herr < 0 ) return herr;
	}

	return h5u_set_view ( f, start, end );
}

/*!
  \ingroup h5part_c_api_read

   Allows you to query the current view. Start and End
   will be \c -1 if there is no current view established.
   Use \c H5PartHasView() to see if the view is smaller than the
   total dataset.

   \return       the number of elements in the view 
*/
h5_int64_t
H5PartGetView (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	h5_int64_t *start,		/*!< [out]  Start particle */
	h5_int64_t *end		/*!< [out]  End particle */
	) {

	SET_FNAME ( f, __func__ );

	CHECK_FILEHANDLE( f );

	if ( f->step_gid < 0 ) {
		h5_int64_t herr = h5_set_step ( f, 0 );
		if ( herr < 0 ) return herr;
	}
	return h5u_get_view( f, start, end );
}

/*!
  \ingroup h5part_c_api_read

  If it is too tedious to manually set the start and end coordinates
  for a view, the \c H5SetCanonicalView() will automatically select an
  appropriate domain decomposition of the data arrays for the degree
  of parallelism and set the "view" accordingly.

  \return		H5_SUCCESS or error code
*/
/*
  \note
  There is a bug in this function:
  If (NumParticles % f->nprocs) != 0  then
  the last  (NumParticles % f->nprocs) particles are not handled!
*/

h5_int64_t
H5PartSetCanonicalView (
	h5_file_t *f			/*!< [in]  Handle to open file */
	) {

	SET_FNAME ( f, __func__ );

	h5_int64_t herr;

	CHECK_FILEHANDLE( f );
	CHECK_READONLY_MODE ( f )

	if ( f->step_gid < 0 ) {
		herr = h5_set_step ( f, 0 );
		if ( herr < 0 ) return herr;
	}

	return h5u_set_canonical_view ( f );
}

/*!
  \ingroup h5part_c_api_read

  Read array of 64 bit floating point data from file.

  When retrieving datasets from disk, you ask for them
  by name. There are no restrictions on naming of arrays,
  but it is useful to arrive at some common naming
  convention when sharing data with other groups.

  \return	\c H5_SUCCESS or error code
*/
h5_int64_t
H5PartReadDataFloat64 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate dataset with */
	h5_float64_t *array	/*!< [out] Array of data */
	) {

	SET_FNAME ( f, __func__ );

	CHECK_FILEHANDLE( f );

	return h5u_read_elems ( f, name, array, H5T_NATIVE_DOUBLE );
}

/*!
  \ingroup h5part_c_api_read

  Read array of 64 bit floating point data from file.

  When retrieving datasets from disk, you ask for them
  by name. There are no restrictions on naming of arrays,
  but it is useful to arrive at some common naming
  convention when sharing data with other groups.

  \return	\c H5_SUCCESS or error code
*/
h5_int64_t
H5PartReadDataInt64 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate dataset with */
	h5_int64_t *array	/*!< [out] Array of data */
	) {

	SET_FNAME ( f, __func__ );
	CHECK_FILEHANDLE( f );

	return h5u_read_elems ( f, name, array, H5T_NATIVE_INT64 );
}

/*!
  \ingroup h5part_c_api_read

  This is the mongo read function that pulls in all of the data for a
  given step in one shot. It also takes the step as an argument
  and will call \c H5PartSetStep() internally so that you don't have to 
  make that call separately.

  \note
  See also \c H5PartReadDataInt64() and \c H5PartReadDataFloat64() if you want
  to just read in one of the many datasets.

  \return	\c H5_SUCCESS or error code
*/
h5_int64_t
H5PartReadParticleStep (
	h5_file_t *f,		/*!< [in]  Handle to open file */
	h5_int64_t step,	/*!< [in]  Step to read */
	h5_float64_t *x,	/*!< [out] Buffer for dataset named "x" */
	h5_float64_t *y,	/*!< [out] Buffer for dataset named "y" */
	h5_float64_t *z,	/*!< [out] Buffer for dataset named "z" */
	h5_float64_t *px,	/*!< [out] Buffer for dataset named "px" */
	h5_float64_t *py,	/*!< [out] Buffer for dataset named "py" */
	h5_float64_t *pz,	/*!< [out] Buffer for dataset named "pz" */
	h5_int64_t *id	/*!< [out] Buffer for dataset named "id" */
	) {

	SET_FNAME ( f, __func__ );
	h5_int64_t herr;

	CHECK_FILEHANDLE( f );

	herr = h5_set_step ( f, step );
	if ( herr < 0 ) return herr;

	herr = h5u_read_elems ( f, "x", (void*)x, H5T_NATIVE_DOUBLE );
	if ( herr < 0 ) return herr;

	herr = h5u_read_elems ( f, "y", (void*)y, H5T_NATIVE_DOUBLE );
	if ( herr < 0 ) return herr;

	herr = h5u_read_elems ( f, "z", (void*)z, H5T_NATIVE_DOUBLE );
	if ( herr < 0 ) return herr;

	herr = h5u_read_elems ( f, "px", (void*)px, H5T_NATIVE_DOUBLE );
	if ( herr < 0 ) return herr;

	herr = h5u_read_elems ( f, "py", (void*)py, H5T_NATIVE_DOUBLE );
	if ( herr < 0 ) return herr;

	herr = h5u_read_elems ( f, "pz", (void*)pz, H5T_NATIVE_DOUBLE );
	if ( herr < 0 ) return herr;

	herr = h5u_read_elems ( f, "id", (void*)id, H5T_NATIVE_INT64 );
	if ( herr < 0 ) return herr;

	return H5_SUCCESS;
}

/****************** error handling ******************/


