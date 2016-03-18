/*
  Copyright (c) 2006-2016, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5BLOCK_IO_H
#define __H5BLOCK_IO_H

#include "h5core/h5_types.h"
#include "h5core/h5.h"
#include "h5core/h5_debug.h"
#include "h5core/h5b_io.h"

/**
   \addtogroup h5block_io
   @{
*/

/**
  Write the 3-dimensional field \p name from the buffer pointed to by
  \p buffer to the current step using the previously defined field
  view.

  The data type is 64bit floating point (\c h5_float64_t). Ensure that
  the number of items in the buffer matches the view.  Use the FORTRAN
  indexing scheme to store data in the buffer.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dWriteScalarFieldFloat64 (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  name of dataset to write.
	const h5_float64_t* buffer	///< [in]  pointer to buffer containing data to write.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', buffer=%p",
                      (h5_file_p)f, name, buffer);
	H5_API_RETURN (h5b_write_scalar_data(f, name, (void*)buffer, H5T_NATIVE_DOUBLE ));
}

/**
  Read the 3-dimensional field \c name into the buffer pointed to by
  \p buffer from the current tep using the defined field layout.

  The data type is 64bit floating point (\c h5_float64_t). Ensure that
  the number of items in the buffer matches the view.  Use the FORTRAN
  indexing scheme to access data in the buffer.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dReadScalarFieldFloat64 (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  name of dataset to read.
	h5_float64_t* buffer		///< [out] pointer to read buffer.
	) {

	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', buffer=%p",
                      (h5_file_p)f, name, buffer);
	H5_API_RETURN (h5b_read_scalar_data(f, name, (void*)buffer, H5T_NATIVE_DOUBLE));
}

/**
  Write the 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_buf, \c y_buf and \c z_buf to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with floating points (64-bit) values.

  You must use the Fortran indexing scheme to access items in \c x_buf.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dWriteVector3dFieldFloat64 (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  name of dataset to write.
	const h5_float64_t* x_buf,	///< [in]  pointer to X axis buffer.
	const h5_float64_t* y_buf,	///< [in]  pointer to Y axis buffer.
	const h5_float64_t* z_buf	///< [in]  pointer to Z axis buffer.
	) {

	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p",
		      (h5_file_p)f, name, x_buf, y_buf, z_buf);
	H5_API_RETURN(h5b_write_vector3d_data(f, name,
		(void*)x_buf, (void*)y_buf, (void*)z_buf, H5T_NATIVE_DOUBLE));
}

/**
  Read a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_buf, \c y_buf and \c z_buf to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with floating points (64-bit) values.

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dReadVector3dFieldFloat64 (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  name of dataset to write.
	h5_float64_t* const x_buf,	///< [out] pointer to X axis buffer.
	h5_float64_t* const y_buf,	///< [out] pointer to Y axis buffer.
	h5_float64_t* const z_buf	///< [out] pointer to Z axis buffer.
	) {

	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p",
		      (h5_file_p)f, name, x_buf, y_buf, z_buf);
	H5_API_RETURN(h5b_read_vector3d_data(f, name,
		x_buf, y_buf, z_buf, H5T_NATIVE_DOUBLE));
}


/**
  Write a 3-dimensional field \c name from the buffer starting at \c data
  to the current time-step using the defined field layout. Values are
  floating points (32-bit).

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dWriteScalarFieldFloat32 (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  name of dataset to write.
	const h5_float32_t* buffer	///< [in]  pointer to write buffer.
	) {

	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', buffer=%p",
                      (h5_file_p)f, name, buffer);
	H5_API_RETURN (h5b_write_scalar_data(f, name, buffer, H5T_NATIVE_FLOAT ));
}

/**
  Read a 3-dimensional field \c name into the buffer starting at \c data from
  the current time-step using the defined field layout. Values are
  floating points (32-bit).

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dReadScalarFieldFloat32 (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  name of dataset to read.
	h5_float32_t* const buffer	///< [out] pointer to read buffer.
	) {

	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', buffer=%p",
                      (h5_file_p)f, name, buffer);
	H5_API_RETURN (h5b_read_scalar_data(f, name, buffer, H5T_NATIVE_FLOAT));
}

/**
  Write a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_buf, \c y_buf and \c z_buf to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with floating points (32-bit) values.

  You must use the Fortran indexing scheme to access items in \c x_buf.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dWriteVector3dFieldFloat32 (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  name of dataset to write.
	const h5_float32_t* x_buf,	///< [in]  pointer to X axis buffer.
	const h5_float32_t* y_buf,	///< [in]  pointer to Y axis buffer.
	const h5_float32_t* z_buf	///< [in]  pointer to Z axis buffer.
	) {

	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p",
		      (h5_file_p)f, name, x_buf, y_buf, z_buf);
	H5_API_RETURN(h5b_write_vector3d_data(f, name,
		x_buf, y_buf, z_buf, H5T_NATIVE_FLOAT));
}

/**
  Read a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_buf, \c y_buf and \c z_buf to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with floating points (32-bit) values.

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dReadVector3dFieldFloat32 (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  name of dataset to write.
	h5_float32_t* const x_buf,	///< [out] pointer to X axis buffer.
	h5_float32_t* const y_buf,	///< [out] pointer to Y axis buffer.
	h5_float32_t* const z_buf	///< [out] pointer to Z axis buffer.
	) {

	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p",
		      (h5_file_p)f, name, x_buf, y_buf, z_buf);
	H5_API_RETURN(h5b_read_vector3d_data(f, name,
		x_buf, y_buf, z_buf, H5T_NATIVE_FLOAT));
}


/**
  Write a 3-dimensional field \c name from the buffer starting at \c data
  to the current time-step using the defined field layout. Values are
  integers (64-bit).

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dWriteScalarFieldInt64 (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  name of dataset to write.
	const h5_int64_t* buffer	///< [in]  pointer to write buffer.
	) {

	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', buffer=%p",
                      (h5_file_p)f, name, buffer);
	H5_API_RETURN (h5b_write_scalar_data(f, name, buffer, H5T_NATIVE_INT64 ));
}

/**
  Read a 3-dimensional field \c name into the buffer starting at \c data from
  the current time-step using the defined field layout. Values are
  integers (64-bit).

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dReadScalarFieldInt64 (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  name of dataset to read.
	h5_int64_t* const buffer	///< [out] pointer to read buffer.
	) {

	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', buffer=%p",
                      (h5_file_p)f, name, buffer);
	H5_API_RETURN (h5b_read_scalar_data(f, name, buffer, H5T_NATIVE_INT64));
}


/**
  Write a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_buf, \c y_buf and \c z_buf to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with integers (64-bit) values.

  You must use the Fortran indexing scheme to access items in \c x_buf.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dWriteVector3dFieldInt64 (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  name of dataset to write.
	const h5_int64_t* x_buf,	///< [in]  pointer to X axis buffer.
	const h5_int64_t* y_buf,	///< [in]  pointer to Y axis buffer.
	const h5_int64_t* z_buf 	///< [in]  pointer to Z axis buffer.
	) {

	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p",
		      (h5_file_p)f, name, x_buf, y_buf, z_buf);
	H5_API_RETURN (h5b_write_vector3d_data(f, name,
					       x_buf, y_buf, z_buf, H5T_NATIVE_INT64));
}

/**
  Read a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_buf, \c y_buf and \c z_buf to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with integers (64-bit) values.

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dReadVector3dFieldInt64 (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  name of dataset to write.
	h5_int64_t* const x_buf,	///< [out] pointer to X axis buffer.
	h5_int64_t* const y_buf,	///< [out] pointer to Y axis buffer.
	h5_int64_t* const z_buf		///< [out] pointer to Z axis buffer.
	) {

	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p",
		      (h5_file_p)f, name, x_buf, y_buf, z_buf);
	H5_API_RETURN (h5b_read_vector3d_data(f, name,
					      x_buf, y_buf, z_buf, H5T_NATIVE_INT64));
}


/**
  Write a 3-dimensional field \c name from the buffer starting at \c data
  to the current time-step using the defined field layout. Values are
  integers (32-bit).

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dWriteScalarFieldInt32 (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  name of dataset to write.
	const h5_int32_t* buffer	///< [in]  pointer to write buffer.
	) {

	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', buffer=%p",
                      (h5_file_p)f, name, buffer);
	H5_API_RETURN (h5b_write_scalar_data(f, name, buffer, H5T_NATIVE_INT32 ));
}

/**
  Read a 3-dimensional field \c name into the buffer starting at \c data from
  the current time-step using the defined field layout. Values are
  integers (32-bit).

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dReadScalarFieldInt32 (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  name of dataset to read.
	h5_int32_t* const buffer	///< [out] pointer to read buffer.
	) {

	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', buffer=%p",
                      (h5_file_p)f, name, buffer);
	H5_API_RETURN (h5b_read_scalar_data(f, name, buffer, H5T_NATIVE_INT32));
}


/**
  Write a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_buf, \c y_buf and \c z_buf to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with integers (32-bit) values.

  You must use the Fortran indexing scheme to access items in \c x_buf.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dWriteVector3dFieldInt32 (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  name of dataset to write.
	const h5_int32_t* x_buf,	///< [in]  pointer to X axis buffer.
	const h5_int32_t* y_buf,	///< [in]  pointer to Y axis buffer.
	const h5_int32_t* z_buf         ///< [in]  pointer to Z axis buffer.
	) {

	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p",
		      (h5_file_p)f, name, x_buf, y_buf, z_buf);
	H5_API_RETURN(h5b_write_vector3d_data(f, name,
					      x_buf, y_buf, z_buf, H5T_NATIVE_INT32));
}

/**
  Read a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_buf, \c y_buf and \c z_buf to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with integers (32-bit) values.

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5Block3dReadVector3dFieldInt32 (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  name of dataset to write.
	h5_int32_t* const x_buf,	///< [out] pointer to X axis buffer.
	h5_int32_t* const y_buf,	///< [out] pointer to Y axis buffer.
	h5_int32_t* const z_buf		///< [out] pointer to Z axis buffer.
	) {

	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p",
		      (h5_file_p)f, name, x_buf, y_buf, z_buf);
	H5_API_RETURN(h5b_read_vector3d_data(f, name,
					     x_buf, y_buf, z_buf, H5T_NATIVE_INT32));
}

///< @}
#endif
