
#include "h5core/h5_core.h"
#include "H5Block_readwrite.h"

/*!
  \ingroup h5block_data

  Write a 3-dimensional field \c name from the buffer starting at \c data
  to the current time-step using the defined field layout. Values are
  floating points (64-bit).

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dWriteScalarFieldFloat64 (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to write */
	const h5_float64_t *buffer	/*!< IN: pointer to write buffer */
	) {

	H5_API_ENTER (h5_err_t, "f=%p, name='%s', buffer=%p", f, name, buffer);
	H5_API_RETURN (h5b_write_scalar_data(f, name, (void*)buffer, H5T_NATIVE_DOUBLE ));
}

/*!
  \ingroup h5block_data

  Read a 3-dimensional field \c name into the buffer starting at \c data from
  the current time-step using the defined field layout. Values are
  floating points (64-bit).

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dReadScalarFieldFloat64 (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to read */
	h5_float64_t *buffer		/*!< OUT: pointer to read buffer */
	) {

	H5_API_ENTER (h5_err_t, "f=%p, name='%s', buffer=%p", f, name, buffer);
	H5_API_RETURN (h5b_read_scalar_data(f, name, (void*)buffer, H5T_NATIVE_DOUBLE));
}

/*!
  \ingroup h5block_data
*/
/*!
  Write a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_buf, \c y_buf and \c z_buf to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with floating points (64-bit) values.

  You must use the Fortran indexing scheme to access items in \c x_buf.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dWriteVector3dFieldFloat64 (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to write */
	const h5_float64_t *x_buf,	/*!< IN: pointer to X axis buffer */
	const h5_float64_t *y_buf,	/*!< IN: pointer to Y axis buffer */
	const h5_float64_t *z_buf	/*!< IN: pointer to Z axis buffer */
	) {

	H5_API_ENTER (h5_err_t, "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p",
		      f, name, x_buf, y_buf, z_buf);
	H5_API_RETURN(h5b_write_vector3d_data(f, name,
		(void*)x_buf, (void*)y_buf, (void*)z_buf, H5T_NATIVE_DOUBLE));
}

/*!
  \ingroup h5block_data
*/
/*!
  Read a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_buf, \c y_buf and \c z_buf to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with floating points (64-bit) values.

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dReadVector3dFieldFloat64 (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to write */
	h5_float64_t *const x_buf,	/*!< OUT: pointer to X axis buffer */
	h5_float64_t *const y_buf,	/*!< OUT: pointer to Y axis buffer */
	h5_float64_t *const z_buf	/*!< OUT: pointer to Z axis buffer */
	) {

	H5_API_ENTER (h5_err_t, "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p",
		      f, name, x_buf, y_buf, z_buf);
	H5_API_RETURN(h5b_read_vector3d_data(f, name,
		x_buf, y_buf, z_buf, H5T_NATIVE_DOUBLE));
}

/*!
  \ingroup h5block_data

  Write a 3-dimensional field \c name from the buffer starting at \c data
  to the current time-step using the defined field layout. Values are
  floating points (32-bit).

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dWriteScalarFieldFloat32 (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to write */
	const h5_float32_t *buffer	/*!< IN: pointer to write buffer */
	) {

	H5_API_ENTER (h5_err_t, "f=%p, name='%s', buffer=%p", f, name, buffer);
	H5_API_RETURN (h5b_write_scalar_data(f, name, buffer, H5T_NATIVE_FLOAT ));
}

/*!
  \ingroup h5block_data

  Read a 3-dimensional field \c name into the buffer starting at \c data from
  the current time-step using the defined field layout. Values are
  floating points (32-bit).

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dReadScalarFieldFloat32 (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to read */
	h5_float32_t *const buffer		/*!< OUT: pointer to read buffer */
	) {

	H5_API_ENTER (h5_err_t, "f=%p, name='%s', buffer=%p", f, name, buffer);
	H5_API_RETURN (h5b_read_scalar_data(f, name, buffer, H5T_NATIVE_FLOAT));
}

/*!
  \ingroup h5block_data
*/
/*!
  Write a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_buf, \c y_buf and \c z_buf to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with floating points (32-bit) values.

  You must use the Fortran indexing scheme to access items in \c x_buf.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dWriteVector3dFieldFloat32 (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to write */
	const h5_float32_t *x_buf,	/*!< IN: pointer to X axis buffer */
	const h5_float32_t *y_buf,	/*!< IN: pointer to Y axis buffer */
	const h5_float32_t *z_buf	/*!< IN: pointer to Z axis buffer */
	) {

	H5_API_ENTER (h5_err_t, "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p",
		      f, name, x_buf, y_buf, z_buf);
	H5_API_RETURN(h5b_write_vector3d_data(f, name,
		x_buf, y_buf, z_buf, H5T_NATIVE_FLOAT));
}

/*!
  \ingroup h5block_data
*/
/*!
  Read a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_buf, \c y_buf and \c z_buf to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with floating points (32-bit) values.

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dReadVector3dFieldFloat32 (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to write */
	h5_float32_t *const x_buf,	/*!< OUT: pointer to X axis buffer */
	h5_float32_t *const y_buf,	/*!< OUT: pointer to Y axis buffer */
	h5_float32_t *const z_buf	/*!< OUT: pointer to Z axis buffer */
	) {

	H5_API_ENTER (h5_err_t, "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p",
		      f, name, x_buf, y_buf, z_buf);
	H5_API_RETURN(h5b_read_vector3d_data(f, name,
		x_buf, y_buf, z_buf, H5T_NATIVE_FLOAT));
}

/*!
  \ingroup h5block_data

  Write a 3-dimensional field \c name from the buffer starting at \c data
  to the current time-step using the defined field layout. Values are
  integers (64-bit).

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dWriteScalarFieldInt64 (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to write */
	const h5_int64_t *buffer	/*!< IN: pointer to write buffer */
	) {

	H5_API_ENTER (h5_err_t, "f=%p, name='%s', buffer=%p", f, name, buffer);
	H5_API_RETURN (h5b_write_scalar_data(f, name, buffer, H5T_NATIVE_INT64 ));
}

/*!
  \ingroup h5block_data

  Read a 3-dimensional field \c name into the buffer starting at \c data from
  the current time-step using the defined field layout. Values are
  integers (64-bit).

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dReadScalarFieldInt64 (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to read */
	h5_int64_t *const buffer	/*!< OUT: pointer to read buffer */
	) {

	H5_API_ENTER (h5_err_t, "f=%p, name='%s', buffer=%p", f, name, buffer);
	H5_API_RETURN (h5b_read_scalar_data(f, name, buffer, H5T_NATIVE_INT64));
}

/*!
  \ingroup h5block_data
*/
/*!
  Write a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_buf, \c y_buf and \c z_buf to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with integers (64-bit) values.

  You must use the Fortran indexing scheme to access items in \c x_buf.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dWriteVector3dFieldInt64 (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to write */
	const h5_int64_t *x_buf,	/*!< IN: pointer to X axis buffer */
	const h5_int64_t *y_buf,	/*!< IN: pointer to Y axis buffer */
	const h5_int64_t *z_buf	/*!< IN: pointer to Z axis buffer */
	) {

	H5_API_ENTER (h5_err_t, "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p",
		      f, name, x_buf, y_buf, z_buf);
	H5_API_RETURN (h5b_write_vector3d_data(f, name,
					       x_buf, y_buf, z_buf, H5T_NATIVE_INT64));
}

/*!
  \ingroup h5block_data
*/
/*!
  Read a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_buf, \c y_buf and \c z_buf to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with integers (64-bit) values.

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dReadVector3dFieldInt64 (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to write */
	h5_int64_t *const x_buf,	/*!< OUT: pointer to X axis buffer */
	h5_int64_t *const y_buf,	/*!< OUT: pointer to Y axis buffer */
	h5_int64_t *const z_buf		/*!< OUT: pointer to Z axis buffer */
	) {

	H5_API_ENTER (h5_err_t, "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p",
		      f, name, x_buf, y_buf, z_buf);
	H5_API_RETURN (h5b_read_vector3d_data(f, name,
					      x_buf, y_buf, z_buf, H5T_NATIVE_INT64));
}

/*!
  \ingroup h5block_data

  Write a 3-dimensional field \c name from the buffer starting at \c data
  to the current time-step using the defined field layout. Values are
  integers (32-bit).

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dWriteScalarFieldInt32 (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to write */
	const h5_int32_t *buffer	/*!< IN: pointer to write buffer */
	) {

	H5_API_ENTER (h5_err_t, "f=%p, name='%s', buffer=%p", f, name, buffer);
	H5_API_RETURN (h5b_write_scalar_data(f, name, buffer, H5T_NATIVE_INT32 ));
}

/*!
  \ingroup h5block_data

  Read a 3-dimensional field \c name into the buffer starting at \c data from
  the current time-step using the defined field layout. Values are
  integers (32-bit).

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dReadScalarFieldInt32 (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to read */
	h5_int32_t *const buffer	/*!< OUT: pointer to read buffer */
	) {

	H5_API_ENTER (h5_err_t, "f=%p, name='%s', buffer=%p", f, name, buffer);
	H5_API_RETURN (h5b_read_scalar_data(f, name, buffer, H5T_NATIVE_INT32));
}

/*!
  \ingroup h5block_data
*/
/*!
  Write a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_buf, \c y_buf and \c z_buf to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with integers (32-bit) values.

  You must use the Fortran indexing scheme to access items in \c x_buf.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dWriteVector3dFieldInt32 (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to write */
	const h5_int32_t *x_buf,	/*!< IN: pointer to X axis buffer */
	const h5_int32_t *y_buf,	/*!< IN: pointer to Y axis buffer */
	const h5_int32_t *z_buf	/*!< IN: pointer to Z axis buffer */
	) {

	H5_API_ENTER (h5_err_t, "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p",
		      f, name, x_buf, y_buf, z_buf);
	H5_API_RETURN(h5b_write_vector3d_data(f, name,
					      x_buf, y_buf, z_buf, H5T_NATIVE_INT32));
}

/*!
  \ingroup h5block_data
*/
/*!
  Read a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_buf, \c y_buf and \c z_buf to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with integers (32-bit) values.

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dReadVector3dFieldInt32 (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: name of dataset to write */
	h5_int32_t *const x_buf,	/*!< OUT: pointer to X axis buffer */
	h5_int32_t *const y_buf,	/*!< OUT: pointer to Y axis buffer */
	h5_int32_t *const z_buf		/*!< OUT: pointer to Z axis buffer */
	) {

	H5_API_ENTER (h5_err_t, "f=%p, name='%s', x_buf=%p, y_buf=%p, z_buf=%p",
		      f, name, x_buf, y_buf, z_buf);
	H5_API_RETURN(h5b_read_vector3d_data(f, name,
					     x_buf, y_buf, z_buf, H5T_NATIVE_INT32));
}

/*!
  \ingroup h5block_attrib

  Write float64 \c values as attribute \c attrib_name of field
  \c field_name.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockWriteFieldAttribFloat64 (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const h5_float64_t *buffer,		/*!< IN: attribute values */
	const h5_size_t nelems			/*!< IN: number of elements */
	) {

	H5_API_ENTER (h5_err_t,
		      "f=%p, field_name='%s', attrib_name='%s', "
		      "buffer=%p, nelems=%lld",
		      f, field_name, attrib_name, buffer, (long long)nelems);
	H5_API_RETURN (h5_write_field_attrib (
			       f,
			       field_name,
			       attrib_name,
			       H5T_NATIVE_DOUBLE,
			       buffer,
			       nelems ));
}

/*!
  \ingroup h5block_attrib

  Read float64 values from attribute \c attrib_name of field
  \c field_name into a \c buffer.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockReadFieldAttribFloat64 (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	h5_float64_t *buffer		        /*!< OUT: attribute values */
	) {

        H5_API_ENTER (h5_err_t, "f=%p, field_name='%s', attrib_name='%s', buffer=%p",
		      f, field_name, attrib_name, buffer);
	H5_API_RETURN (h5_read_field_attrib (
			       f,
			       field_name,
			       attrib_name,
			       H5T_NATIVE_DOUBLE,
			       (void*)buffer ));
}

/*!
  \ingroup h5block_attrib

  Write float32 \c values as attribute \c attrib_name of field
  \c field_name.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockWriteFieldAttribFloat32 (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const h5_float32_t *buffer,		/*!< IN: attribute values */
	const h5_size_t nelems			/*!< IN: number of elements */
	) {

	H5_API_ENTER (h5_err_t,
		      "f=%p, field_name='%s', attrib_name='%s', "
		      "buffer=%p, nelems=%lld",
		      f, field_name, attrib_name, buffer, (long long)nelems);
	H5_API_RETURN (h5_write_field_attrib (
			       f,
			       field_name,
			       attrib_name,
			       H5T_NATIVE_FLOAT,
			       buffer,
			       nelems ));
}

/*!
  \ingroup h5block_attrib

  Read float32 values from attribute \c attrib_name of field
  \c field_name into a \c buffer.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockReadFieldAttribFloat32 (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	h5_float32_t *const buffer		/*!< OUT: attribute values */
	) {

        H5_API_ENTER (h5_err_t, "f=%p, field_name='%s', attrib_name='%s', buffer=%p",
		      f, field_name, attrib_name, buffer);
	H5_API_RETURN (h5_read_field_attrib (
			       f,
			       field_name,
			       attrib_name,
			       H5T_NATIVE_FLOAT,
			       buffer ));
}

/*!
  \ingroup h5block_attrib

  Write int64 \c values as attribute \c attrib_name of field
  \c field_name.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockWriteFieldAttribInt64 (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const h5_int64_t *buffer,		/*!< IN: attribute values */
	const h5_size_t nelems			/*!< IN: number of elements */
	) {

	H5_API_ENTER (h5_err_t,
		      "f=%p, field_name='%s', attrib_name='%s', buffer=%p, nelems=%lld",
		      f, field_name, attrib_name, buffer, (long long)nelems);
	H5_API_RETURN (h5_write_field_attrib (
			       f,
			       field_name,
			       attrib_name,
			       H5T_NATIVE_INT64,
			       buffer,
			       nelems ));
}

/*!
  \ingroup h5block_attrib

  Read int64 values from attribute \c attrib_name of field
  \c field_name into a \c buffer.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockReadFieldAttribInt64 (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	h5_int64_t *const buffer	        /*!< OUT: attribute values */
	) {

        H5_API_ENTER (h5_err_t, "f=%p, field_name='%s', attrib_name='%s', buffer=%p",
		      f, field_name, attrib_name, buffer);
	H5_API_RETURN (h5_read_field_attrib (
			       f,
			       field_name,
			       attrib_name,
			       H5T_NATIVE_INT64,
			       buffer ));
}

/*!
  \ingroup h5block_attrib

  Write int32 \c values as attribute \c attrib_name of field
  \c field_name.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockWriteFieldAttribInt32 (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const h5_int32_t *buffer,		/*!< IN: attribute values */
	const h5_size_t nelems			/*!< IN: number of elements */
	) {

	H5_API_ENTER (h5_err_t,
		      "f=%p, field_name='%s', attrib_name='%s', "
		      "buffer=%p, nelems=%lld",
		      f, field_name, attrib_name, buffer, (long long)nelems);
	H5_API_RETURN (h5_write_field_attrib (
			       f,
			       field_name,
			       attrib_name,
			       H5T_NATIVE_INT32,
			       buffer,
			       nelems ));
}

/*!
  \ingroup h5block_attrib

  Read int32 values from attribute \c attrib_name of field
  \c field_name into a \c buffer.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockReadFieldAttribInt32 (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	h5_int32_t *buffer		        /*!< OUT: attribute values */
	) {
        H5_API_ENTER (h5_err_t, "f=%p, field_name='%s', attrib_name='%s', buffer=%p",
		      f, field_name, attrib_name, buffer);
	H5_API_RETURN (h5_read_field_attrib (
			       f,
			       field_name,
			       attrib_name,
			       H5T_NATIVE_INT32,
			       (void*)buffer ));
}
