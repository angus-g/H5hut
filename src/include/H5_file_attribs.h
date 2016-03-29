/*
  Copyright (c) 2006-2016, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5_FILE_ATTRIBS_H
#define __H5_FILE_ATTRIBS_H

#include <string.h>

#include "h5core/h5_types.h"
#include "h5core/h5.h"
#include "h5core/h5_debug.h"
#include "h5core/h5_attribs.h"

/**
   \addtogroup h5_file_attribs
   @{
*/

#ifdef __cplusplus
extern "C" {
#endif

/*
   __ _ _              _   _        _ _           _            
  / _(_) | ___    __ _| |_| |_ _ __(_) |__  _   _| |_ ___  ___ 
 | |_| | |/ _ \  / _` | __| __| '__| | '_ \| | | | __/ _ \/ __|
 |  _| | |  __/ | (_| | |_| |_| |  | | |_) | |_| | ||  __/\__ \
 |_| |_|_|\___|  \__,_|\__|\__|_|  |_|_.__/ \__,_|\__\___||___/
 
   __ _ _   _  ___ _ __ _   _ 
  / _` | | | |/ _ \ '__| | | |
 | (_| | |_| |  __/ |  | |_| |
  \__, |\__,_|\___|_|   \__, |
     |_|                |___/
*/

/**
   Determines whether a file attribute with a given name exists.

   \return      true (value \c >0) if atrribute exists
   \return      false (\c 0) if attribute does not exist
   \return      \c H5_FAILURE on error
 */
static inline h5_err_t
H5HasFileAttrib (
	const h5_file_t f,	    ///< [in]  file handle
	const char* const name	    ///< [in]  name of attribute to query
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, "
		      "name=%p",
		      (h5_file_p)f,
		      name);
	H5_API_RETURN (
		h5_has_file_attrib (
			f,
			name));
}

/**
  Get the type and number of elements of the file attribute
  given by its name.

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5GetFileAttribInfoByName (
	const h5_file_t f,		///< [in]  file handle.
	const char* const name,     	///< [in]  name of attribute.
	h5_int64_t* type,               ///< [out] type of value..
	h5_size_t* nelems               ///< [out] number of elements.
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, "
		      "name=%s, "
		      "type=%p, nelems=%p",
                      (h5_file_p)f,
                      name,
                      type, nelems);
	H5_API_RETURN (h5_get_file_attrib_info_by_name (
			       f,
			       name,
			       type, nelems));
}

/**
  Gets the number of attributes in the file's root ("/").

  \return   Number of attributes
  \return   \c H5_FAILURE on error

  \see H5GetFileAttribInfo()
*/
static inline h5_int64_t
H5GetNumFileAttribs (
	const h5_file_t f              ///< [in]  file handle.
	) {
	H5_API_ENTER (h5_int64_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5_get_num_file_attribs (f));
}

/**
  Gets the name, type and number of elements of the file attribute
  given by its index.

  This function can be used to retrieve all attributes bound to the
  file \c f by looping from \c 0 to the number of attribute minus
  one.  The number of attributes bound to file \c f can be queried
  by calling \ref H5GetNumFileAttribs.

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5GetFileAttribInfo (
	const h5_file_t f,		///< [in]  file handle.
	const h5_size_t idx,    	///< [in]  index of attribute to query
	char* name,     		///< [out] name of attribute.
	const h5_size_t len_name,       ///< [in]  length of buffer \c name.
	h5_int64_t* type,               ///< [out] type of value..
	h5_size_t* nelems               ///< [out] number of elements.
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, "
		      "idx=%llu, name=%p, len_name=%llu, "
		      "type=%p, nelems=%p",
                      (h5_file_p)f,
                      (long long unsigned)idx,
                      name,
                      (long long unsigned)len_name,
                      type,
                      nelems);
	H5_API_RETURN (h5_get_file_attrib_info_by_idx (
			       f,
			       idx,
			       name, len_name,
			       type,
			       nelems));
}

/*
   __ _ _              _   _        _ _           _            
  / _(_) | ___    __ _| |_| |_ _ __(_) |__  _   _| |_ ___  ___ 
 | |_| | |/ _ \  / _` | __| __| '__| | '_ \| | | | __/ _ \/ __|
 |  _| | |  __/ | (_| | |_| |_| |  | | |_) | |_| | ||  __/\__ \
 |_| |_|_|\___|  \__,_|\__|\__|_|  |_|_.__/ \__,_|\__\___||___/

  _    __    
 (_)  / /__  
 | | / / _ \ 
 | |/ / (_) |
 |_/_/ \___/ 
*/

/**
  Write an attribute \c name with the string \c value to
  the file root ("/").

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5WriteFileAttribString (
	const h5_file_t f,             ///< [in]  file handle.
	const char *name,               ///< [in]  name of attribute to create.
	const char *value               ///< [in]  value of attribute. 
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', value='%s'",
                      (h5_file_p)f, name, value);
	H5_API_RETURN (h5_write_file_attrib (
			       f,
			       name,
			       H5T_NATIVE_CHAR,
			       value,
			       strlen(value) + 1 ));
}

/**
  Read a string into a \c buffer from an attribute \c name
  in the file root ("/").

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5ReadFileAttribString (
	const h5_file_t f,     	///< [in]  file handle.
	const char *name,       	///< [in]  name of attribute to create.
	char *buffer            	///< [out] value of attribute. 
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', value='%s'",
                      (h5_file_p)f, name, buffer);
	H5_API_RETURN (h5_read_file_attrib (
			       f,
			       name,
			       H5_STRING_T,
			       (void*)buffer));
}

/**
  Write an attribute \c name with float64 \c values to
  the file root ("/").

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5WriteFileAttribFloat64 (
	const h5_file_t f,		///< [in]  file handle.
	const char *name,		///< [in]  name of attribute to create.
	const h5_float64_t *values,	///< [in]  values of attribute.
	const h5_size_t nelems		///< [in]  number of values.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', values=%p, nelems=%llu",
		      (h5_file_p)f, name, values, (long long unsigned)nelems);
	H5_API_RETURN (h5_write_file_attrib (
			       f,
			       name,
			       H5T_NATIVE_DOUBLE,
			       values,
			       nelems));
}

/**
  Read float64 values into a \c buffer from an attribute \c name
  in the file root ("/").

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5ReadFileAttribFloat64 (
	const h5_file_t f,		///< [in]  file handle.
	const char *name,		///< [in]  name of attribute to create.
	h5_float64_t *buffer		///< [out] values of attribute.
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', buffer=%p",
		      (h5_file_p)f, name, buffer);
	H5_API_RETURN (h5_read_file_attrib (
			       f,
			       name,
			       H5_FLOAT64_T,
			       (void*)buffer));
}


/**
  Write an attribute \c name with float32 \c values to
  the file root ("/").

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5WriteFileAttribFloat32 (
	const h5_file_t f,		///< [in]  file handle.
	const char *name,		///< [in]  name of attribute to create.
	const h5_float32_t *values,	///< [in]  values of attribute.
	const h5_size_t nelems		///< [in]  number of values.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', values=%p, nelems=%llu",
		      (h5_file_p)f, name, values, (long long unsigned)nelems);
	H5_API_RETURN (h5_write_file_attrib (
			       f,
			       name,
			       H5T_NATIVE_FLOAT,
			       values,
			       nelems ));
}

/**
  Read float32 values into a \c buffer from an attribute \c name
  in the file root ("/").

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5ReadFileAttribFloat32 (
	const h5_file_t f,		///< [in]  file handle.
	const char *name,		///< [in]  name of attribute to create.
	h5_float32_t *buffer		///< [out] values of attribute.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', buffer=%p",
		      (h5_file_p)f, name, buffer);
	H5_API_RETURN (h5_read_file_attrib (
			       f,
			       name,
			       H5_FLOAT32_T,
			       (void*)buffer));
}

/**
  Write an attribute \c name with int64 \c values to
  the file root ("/").

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5WriteFileAttribInt64 (
	const h5_file_t f,		///< [in]  file handle.
	const char *name,		///< [in]  name of attribute to create.
	const h5_int64_t *values,	///< [in]  values of attribute.
	const h5_size_t nelems		///< [in]  number of values.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', values=%p, nelems=%llu",
		      (h5_file_p)f, name, values, (long long unsigned)nelems);
	H5_API_RETURN (h5_write_file_attrib (
			       f,
			       name,
			       H5T_NATIVE_INT64,
			       values,
			       nelems));
}

/**
  Read int64 values into a \c buffer from an attribute \c name
  in the file root ("/").

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5ReadFileAttribInt64 (
	const h5_file_t f,		///< [in]  file handle.
	const char *name,		///< [in]  name of attribute to create.
	h5_int64_t *buffer		///< [out] values of attribute.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', buffer=%p",
		      (h5_file_p)f, name, buffer);
	H5_API_RETURN (h5_read_file_attrib (
			       f,
			       name,
			       H5_INT64_T,
			       (void*)buffer));
}

/**
  Write an attribute \c name with int32 \c values to
  the file root ("/").

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5WriteFileAttribInt32 (
	const h5_file_t f,		///< [in]  file handle.
	const char *name,		///< [in]  name of attribute to create.
	const h5_int32_t *values,	///< [in]  values of attribute.
	const h5_size_t nelems		///< [in]  number of values.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', values=%p, nelems=%llu",
		      (h5_file_p)f, name, values, (long long unsigned)nelems);
	H5_API_RETURN (h5_write_file_attrib (
			       f,
			       name,
			       H5T_NATIVE_INT32,
			       values,
			       nelems));
}

/**
  Read int32 values into a \c buffer from an attribute \c name
  in the file root ("/").

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5ReadFileAttribInt32 (
	const h5_file_t f,		///< [in]  file handle.
	const char *name,		///< [in]  name of attribute to create.
	h5_int32_t *buffer		///< [out] values of attribute.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', buffer=%p",
                      (h5_file_p)f, name, buffer);
	H5_API_RETURN (h5_read_file_attrib (
			       f,
			       name,
			       H5_INT32_T,
			       (void*)buffer));
}

#ifdef __cplusplus
}
#endif

///< @}
#endif
