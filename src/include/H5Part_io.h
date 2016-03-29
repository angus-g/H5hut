/*
  Copyright (c) 2006-2016, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef H5PART_IO
#define H5PART_IO

#include "h5core/h5_types.h"
#include "h5core/h5.h"
#include "h5core/h5_debug.h"
#include "h5core/h5u_io.h"

/**
   \addtogroup h5part_io
   @{
*/

#ifdef __cplusplus
extern "C" {
#endif

/**
  Write array of 64 bit floating point data to file.

  After setting the number of elements with \c H5PartSetNumParticles() and
  the current timestep using \c H5SetStep(), you can start writing datasets
  into the file. Each dataset has a name associated with it (chosen by the
  user) in order to facilitate later retrieval. The name of the dataset is
  specified in the parameter \c name, which must be a null-terminated string.

  There are no restrictions on naming of datasets, but it is useful to arrive
  at some common naming convention when sharing data with other groups.

  The writing routines also implicitly store the datatype of the array so that
  the array can be reconstructed properly on other systems with incompatible
  type representations.

  All data that is written after setting the timestep is associated with that
  timestep. While the number of elements can change for each timestep, you
  cannot change the number of elements in the middle of a given timestep.

  The data is committed to disk before the routine returns.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
 */
static inline h5_err_t
H5PartWriteDataFloat64 (
	const h5_file_t f,              ///< [in]  file handle.
	const char* name,               ///< [in]  name to associate array with.
	const h5_float64_t* data	///< [in]  array to commit to disk.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', date=%p",
                      (h5_file_p)f, name, data);
	H5_API_RETURN (h5u_write_data (f, name, (void*)data, H5T_NATIVE_DOUBLE));
}

/**
  Write array of 32 bit floating point data to file.

  See \ref H5PartWriteDataFloat64() for more details.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
 */
static inline h5_err_t
H5PartWriteDataFloat32 (
	const h5_file_t f,              ///< [in]  file handle.
	const char* name,               ///< [in]  name to associate array with.
	const h5_float32_t* data	///< [in]  array to commit to disk.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', date=%p",
                      (h5_file_p)f, name, data);
	H5_API_RETURN (h5u_write_data(f, name, (void*)data, H5T_NATIVE_FLOAT));
}

/**
  Write array of 64 bit integer data to file.

  See \ref H5PartWriteDataFloat64() for more details.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
 */
static inline h5_err_t
H5PartWriteDataInt64 (
	const h5_file_t f,           	///< [in]  file handle.
	const char* name,       	///< [in]  name to associate array with.
	const h5_int64_t* data          ///< [in]  array to commit to disk.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', date=%p",
                      (h5_file_p)f, name, data);
	H5_API_RETURN (h5u_write_data (f, name, (void*)data, H5T_NATIVE_INT64));
}

/**
  Write array of 32 bit integer data to file.

  See \ref H5PartWriteDataFloat64() for more details.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
 */
static inline h5_err_t
H5PartWriteDataInt32 (
	const h5_file_t f,                   ///< [in]  file handle.
	const char* name,       	///< [in]  name to associate array with.
	const h5_int32_t* data  	///< [in]  array to commit to disk.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', date=%p",
                      (h5_file_p)f, name, data);
	H5_API_RETURN (h5u_write_data (f, name, (void*)data, H5T_NATIVE_INT32));
}

/**
  Read array of 64 bit floating point data from file.

  See \ref H5PartWriteDataFloat64() for more details.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5PartReadDataFloat64 (
	const h5_file_t f,              ///< [in]  file handle.
	const char* name,       	///< [in]  name to associate dataset with.
	h5_float64_t* data      	///< [out] array of data.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', date=%p",
                      (h5_file_p)f, name, data);
	H5_API_RETURN (h5u_read_data (f, name, data, H5T_NATIVE_DOUBLE));
}

/**
  Read array of 32 bit floating point data from file.

  See \ref H5PartWriteDataFloat64() for more details.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5PartReadDataFloat32 (
	const h5_file_t f,           	///< [in]  file handle.
	const char* name,       	///< [in]  name to associate dataset with.
	h5_float32_t* data      	///< [out] array of data.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', date=%p",
                      (h5_file_p)f, name, data);
	H5_API_RETURN (h5u_read_data (f, name, data, H5T_NATIVE_FLOAT));
}

/**
  Read array of 64 bit integer data from file.

  See \ref H5PartWriteDataFloat64() for more details.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5PartReadDataInt64 (
	const h5_file_t f,              ///< [in]  file handle.
	const char* name,       	///< [in]  name to associate dataset with.
	h5_int64_t* data        	///< [out] array of data.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', date=%p",
                      (h5_file_p)f, name, data);
	H5_API_RETURN (h5u_read_data (f, name, data, H5T_NATIVE_INT64));
}

/**
  Read array of 32 bit integer data from file.

  See \ref H5PartWriteDataFloat64() for more details.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5PartReadDataInt32 (
	const h5_file_t f,              ///< [in]  file handle.
	const char* name,               ///< [in]  name to associate dataset with.
	h5_int32_t* data                ///< [out] Array of data.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', date=%p",
                      (h5_file_p)f, name, data);
	H5_API_RETURN (h5u_read_data (f, name, data, H5T_NATIVE_INT32));
}

#ifdef __cplusplus
}
#endif

///< @}
#endif
