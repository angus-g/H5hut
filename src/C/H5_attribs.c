#include <string.h>

#include "h5core/h5_core.h"
#include "H5hut.h"


/*** WRITE ***/

/*!
  \ingroup h5hut_attrib

  Write an attribute \c name with the string \c value to
  the file root ("/").

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5WriteFileAttribString (
	h5_file_t *const f,	/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name of attribute to create */
	const char *value	/*!< [in] Value of attribute */ 
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_write_attrib (
			       f,
			       H5_ATTRIB_FILE,
			       name,
			       H5T_NATIVE_CHAR,
			       value,
			       strlen(value) + 1 ));
}

/*!
  \ingroup h5hut_attrib

  Write an attribute \c name with the string \c value to
  the current timestep.

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5WriteStepAttribString (
	h5_file_t *const f,	/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name of attribute to create */
	const char *value	/*!< [in] Value of attribute */ 
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_write_attrib (
			       f, 
			       H5_ATTRIB_STEP,
			       name,
			       H5T_NATIVE_CHAR,
			       value,
			       strlen(value) + 1 ));
}

/*!
  \ingroup h5hut_attrib

  Write an attribute \c name with float32 \c values to
  the file root ("/").

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5WriteFileAttribFloat32 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	const h5_float32_t *values,	/*!< [in] Values of attribute */
	const h5_size_t nelems		/*!< [in] Number of values */
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_write_attrib (
			       f,
			       H5_ATTRIB_FILE,
			       name,
			       H5T_NATIVE_FLOAT,
			       values,
			       nelems ));
}

/*!
  \ingroup h5hut_attrib

  Write an attribute \c name with float32 \c values to
  the current time step.

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5WriteStepAttribFloat32 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	const h5_float32_t *values,	/*!< [in] Values of attribute */
	const h5_size_t nelems		/*!< [in] Number of values */
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_write_attrib (
			       f,
			       H5_ATTRIB_STEP,
			       name,
			       H5T_NATIVE_FLOAT,
			       values,
			       nelems ));
}

/*!
  \ingroup h5hut_attrib

  Write an attribute \c name with float64 \c values to
  the file root ("/").

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5WriteFileAttribFloat64 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	const h5_float64_t *values,	/*!< [in] Values of attribute */
	const h5_size_t nelems		/*!< [in] Number of values */
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_write_attrib (
			       f,
			       H5_ATTRIB_FILE,
			       name,
			       H5T_NATIVE_DOUBLE,
			       values,
			       nelems));
}

/*!
  \ingroup h5hut_attrib

  Write an attribute \c name with float64 \c values to
  the current time step.

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5WriteStepAttribFloat64 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	const h5_float64_t *values,	/*!< [in] Values of attribute */
	const h5_size_t nelems		/*!< [in] Number of values */
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_write_attrib (
			       f,
			       H5_ATTRIB_STEP,
			       name,
			       H5T_NATIVE_DOUBLE,
			       values,
			       nelems));
}

/*!
  \ingroup h5hut_attrib

  Write an attribute \c name with int32 \c values to
  the file root ("/").

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5WriteFileAttribInt32 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	const h5_int32_t *values,	/*!< [in] Values of attribute */
	const h5_size_t nelems		/*!< [in] Number of values */
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_write_attrib (
			       f,
			       H5_ATTRIB_FILE,
			       name,
			       H5T_NATIVE_INT32,
			       values,
			       nelems));
}

/*!
  \ingroup h5hut_attrib

  Write an attribute \c name with int32 \c values to
  the current time step.

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5WriteStepAttribInt32 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	const h5_int32_t *values,	/*!< [in] Values of attribute */
	const h5_size_t nelems		/*!< [in] Number of values */
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_write_attrib (
			       f,
			       H5_ATTRIB_STEP,
			       name,
			       H5T_NATIVE_INT32,
			       values,
			       nelems));
}

/*!
  \ingroup h5hut_attrib

  Write an attribute \c name with int64 \c values to
  the file root ("/").

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5WriteFileAttribInt64 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	const h5_int64_t *values,	/*!< [in] Values of attribute */
	const h5_size_t nelems		/*!< [in] Number of values */
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_write_attrib (
			       f,
			       H5_ATTRIB_FILE,
			       name,
			       H5T_NATIVE_INT64,
			       values,
			       nelems));
}

/*!
  \ingroup h5hut_attrib

  Write an attribute \c name with int64 \c values to
  the current time step.

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5WriteStepAttribInt64 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	const h5_int64_t *values,	/*!< [in] Values of attribute */
	const h5_size_t nelems		/*!< [in] Number of values */
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_write_attrib (
			       f,
			       H5_ATTRIB_STEP,
			       name,
			       H5T_NATIVE_INT64,
			       values,
			       nelems));
}

/*** READ ***/

/*!
  \ingroup h5hut_attrib

  Read a string into a \c buffer from an attribute \c name
  in the file root ("/").

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5ReadFileAttribString (
	h5_file_t *const f,	/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name of attribute to create */
	char *buffer		/*!< [out] Value of attribute */ 
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_read_attrib (
			       f,
			       H5_ATTRIB_FILE,
			       name,
			       H5_STRING_T,
			       (void*)buffer));
}

/*!
  \ingroup h5hut_attrib

  Read a string into a \c buffer from an attribute \c name
  in the current timestep.

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5ReadStepAttribString (
	h5_file_t *const f,	/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name of attribute to create */
	char *buffer		/*!< [out] Value of attribute */ 
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_read_attrib (
			       f, 
			       H5_ATTRIB_STEP,
			       name,
			       H5_STRING_T,
			       (void*)buffer));
}

/*!
  \ingroup h5hut_attrib

  Read int32 values into a \c buffer from an attribute \c name
  in the file root ("/").

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5ReadFileAttribInt32 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	h5_int32_t *buffer		/*!< [out] Values of attribute */
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_read_attrib (
			       f,
			       H5_ATTRIB_FILE,
			       name,
			       H5_INT32_T,
			       (void*)buffer));
}

/*!
  \ingroup h5hut_attrib

  Read int32 values into a \c buffer from an attribute \c name
  in the current time step.

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5ReadStepAttribInt32 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	h5_int32_t *buffer		/*!< [out] Values of attribute */
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_read_attrib (
			       f,
			       H5_ATTRIB_STEP,
			       name,
			       H5_INT32_T,
			       (void*)buffer));
}

/*!
  \ingroup h5hut_attrib

  Read int64 values into a \c buffer from an attribute \c name
  in the file root ("/").

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5ReadFileAttribInt64 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	h5_int64_t *buffer		/*!< [out] Values of attribute */
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_read_attrib (
			       f,
			       H5_ATTRIB_FILE,
			       name,
			       H5_INT64_T,
			       (void*)buffer));
}

/*!
  \ingroup h5hut_attrib

  Read int64 values into a \c buffer from an attribute \c name
  in the current time step.

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5ReadStepAttribInt64 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	h5_int64_t *buffer		/*!< [out] Values of attribute */
	) {
	H5_API_ENTER (h5_err_t);
	h5_err_t h5err = h5_read_attrib (
		f,
		H5_ATTRIB_STEP,
		name,
		H5_INT64_T,
		(void*)buffer);
	H5_API_RETURN (h5err);
}

/*!
  \ingroup h5hut_attrib

  Read float32 values into a \c buffer from an attribute \c name
  in the file root ("/").

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5ReadFileAttribFloat32 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	h5_float32_t *buffer		/*!< [out] Values of attribute */
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_read_attrib (
			       f,
			       H5_ATTRIB_FILE,
			       name,
			       H5_FLOAT32_T,
			       (void*)buffer));
}

/*!
  \ingroup h5hut_attrib

  Read float32 values into a \c buffer from an attribute \c name
  in the current time step.

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5ReadStepAttribFloat32 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	h5_float32_t *buffer		/*!< [out] Values of attribute */
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_read_attrib (
			       f,
			       H5_ATTRIB_STEP,
			       name,
			       H5_FLOAT32_T,
			       (void*)buffer));
}

/*!
  \ingroup h5hut_attrib

  Read float64 values into a \c buffer from an attribute \c name
  in the file root ("/").

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5ReadFileAttribFloat64 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	h5_float64_t *buffer		/*!< [out] Values of attribute */
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_read_attrib (
			       f,
			       H5_ATTRIB_FILE,
			       name,
			       H5_FLOAT64_T,
			       (void*)buffer));
}

/*!
  \ingroup h5hut_attrib

  Read float64 values into a \c buffer from an attribute \c name
  in the current time step.

  \return	\c H5_SUCCESS or error code   
*/
h5_err_t
H5ReadStepAttribFloat64 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	h5_float64_t *buffer		/*!< [out] Values of attribute */
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_read_attrib (
			       f,
			       H5_ATTRIB_STEP,
			       name,
			       H5_FLOAT64_T,
			       (void*)buffer));
}

/*** QUERY ***/

/*!
  \ingroup h5hut_attrib

  Gets the number of attributes in the file's root ("/").

  \return	Number of attributes or error code.
*/
h5_int64_t
H5GetNumFileAttribs (
	h5_file_t *const f	/*!< [in] Handle to open file */
	) {
	H5_API_ENTER (h5_int64_t);
	H5_API_RETURN (h5_get_num_attribs (f, H5_ATTRIB_FILE));
}

/*!
  \ingroup h5hut_attrib

  Gets the number of attributes bound to the current step.

  \h5_err_t h5err =	Number of attributes or error code.
*/
h5_int64_t
H5GetNumStepAttribs (
	h5_file_t *const f 	/*!< [in] Handle to open file */
	) {
	H5_API_ENTER (h5_int64_t);
	H5_API_RETURN (h5_get_num_attribs (f, H5_ATTRIB_STEP));
}


/*!
  \ingroup h5hut_attrib

  Gets the name, type and number of elements of the file attribute
  specified by its index.

  This function can be used to retrieve all attributes bound to the
  file \c f by looping from \c 0 to the number of attribute minus
  one.  The number of attributes bound to file \c f can be queried
  by calling \ref H5GetNumFileAttribs.

  \return	\c H5_SUCCESS or error code 
*/

h5_err_t
H5GetFileAttribInfo (
	h5_file_t *const f,		/*!< [in]  Handle to open file */
	const h5_size_t attrib_idx,	/*!< [in]  Index of attribute to get
					           infos about */
	char *attrib_name,		/*!< [out] Name of attribute */
	const h5_size_t len_of_attrib_name,
					/*!< [in]  length of buffer \c name */
	h5_int64_t *attrib_type,	/*!< [out] Type of value. */
	h5_size_t *attrib_nelem         /*!< [out] Number of elements */
	) {
	H5_API_ENTER (h5_int64_t);
	H5_API_RETURN (h5_get_attrib_info (
			       f,
			       H5_ATTRIB_FILE,
			       attrib_idx,
			       attrib_name,
			       len_of_attrib_name,
			       attrib_type,
			       attrib_nelem));
}

/*!
  \ingroup h5hut_attrib

  Gets the name, type and number of elements of the step attribute
  specified by its index.

  This function can be used to retrieve all attributes bound to the
  current time-step by looping from \c 0 to the number of attribute
  minus one.  The number of attributes bound to the current
  time-step can be queried by calling \ref H5GetNumStepAttribs.

  \return	\c H5_SUCCESS or error code 
*/
h5_err_t
H5GetStepAttribInfo (
	h5_file_t *const f,		/*!< [in]  Handle to open file */
	const h5_size_t attrib_idx,	/*!< [in]  Index of attribute to
					           get infos about */
	char *attrib_name,		/*!< [out] Name of attribute */
	const h5_size_t len_of_attrib_name,
					/*!< [in]  length of buffer \c name */
	h5_int64_t *attrib_type,	/*!< [out] Type of value. */
	h5_size_t *attrib_nelem         /*!< [out] Number of elements */
	) {
	H5_API_ENTER (h5_err_t);
	H5_API_RETURN (h5_get_attrib_info (
			       f,
			       H5_ATTRIB_STEP,
			       attrib_idx,
			       attrib_name,
			       len_of_attrib_name,
			       attrib_type,
			       attrib_nelem));
}

