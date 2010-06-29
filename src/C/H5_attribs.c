#include <string.h>

#include "h5core/h5_core.h"
#include "H5.h"

/********************** attribute API ****************************************/

/*!
  \ingroup h5_attrib

  Writes an attribute \c name with the string \c value to
  the file root ("/").

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5WriteFileAttribString (
	h5_file_t *const f,	/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name of attribute to create */
	const char *value	/*!< [in] Value of attribute */ 
	) {

	SET_FNAME( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_write_attrib (
		f,
		H5_ATTRIB_FILE,
		name,
		H5T_NATIVE_CHAR,
		value,
		strlen(value) + 1 );
}

/*!
  \ingroup h5_attrib

  Writes an attribute \c name with the string \c value to
  the current timestep.

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5WriteStepAttribString (
	h5_file_t *const f,	/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name of attribute to create */
	const char *value	/*!< [in] Value of attribute */ 
	) {

	SET_FNAME ( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_write_attrib (
		f, 
		H5_ATTRIB_STEP,
		name,
		H5T_NATIVE_CHAR,
		value,
		strlen(value) + 1 );
}

/*!
  \ingroup h5_attrib

  Gets the number of attributes in the file's root ("/").

  \return	Number of attributes or error code.
*/
h5_int64_t
H5GetNumFileAttribs (
	h5_file_t *const f	/*!< [in] Handle to open file */
	) {
	SET_FNAME( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_get_num_attribs ( f, H5_ATTRIB_FILE );
}

/*!
  \ingroup h5_attrib

  Gets the number of attributes bound to the current step.

  \return	Number of attributes or error code.
*/
h5_int64_t
H5GetNumStepAttribs (
	h5_file_t *const f 	/*!< [in] Handle to open file */
	) {
	SET_FNAME( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_get_num_attribs ( f, H5_ATTRIB_STEP );
}


/*!
  \ingroup h5_attrib

  Gets the name, type and number of elements of the file attribute
  specified by its index.

  This function can be used to retrieve all attributes bound to the
  file \c f by looping from \c 0 to the number of attribute minus
  one.  The number of attributes bound to file \c f can be queried
  by calling the function \c H5GetNumFileAttribs().

  \return	\c H5_SUCCESS or error code 
*/

h5_int64_t
H5GetFileAttribInfo (
	h5_file_t *const f,		/*!< [in]  Handle to open file */
	const h5_int64_t attrib_idx,	/*!< [in]  Index of attribute to get
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
		H5_ATTRIB_FILE,
		attrib_idx,
		attrib_name,
		len_of_attrib_name,
		attrib_type,
		attrib_nelem );
}

/*!
  \ingroup h5_attrib

  Gets the name, type and number of elements of the step attribute
  specified by its index.

  This function can be used to retrieve all attributes bound to the
  current time-step by looping from \c 0 to the number of attribute
  minus one.  The number of attributes bound to the current
  time-step can be queried by calling the function
  \c H5GetNumStepAttribs().

  \return	\c H5_SUCCESS or error code 
*/
h5_int64_t
H5GetStepAttribInfo (
	h5_file_t *const f,		/*!< [in]  Handle to open file */
	const h5_int64_t attrib_idx,	/*!< [in]  Index of attribute to
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
		H5_ATTRIB_STEP,
		attrib_idx,
		attrib_name,
		len_of_attrib_name,
		attrib_type,
		attrib_nelem );
}

