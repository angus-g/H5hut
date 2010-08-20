
#include "H5hut.h"
#include "h5core/h5_core.h"

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
	h5_file_t *f,		  /*!< IN: file handle */
	const char *name,	       /*!< IN: name of dataset to write */
	const h5_float64_t *data      /*!< IN: scalar data to write */
	) {

	SET_FNAME( f, __func__ );

	return h5b_write_scalar_data(f, name, (void*)data, H5T_NATIVE_DOUBLE );
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
	h5_file_t *f,		  /*!< IN: file handle */
	const char *name,	       /*!< IN: name of dataset to read */
	h5_float64_t *data	  /*!< OUT: ptr to read buffer */
	) {

	SET_FNAME( f, __func__ );	

	return h5b_read_scalar_data(f, name, (void*)data, H5T_NATIVE_DOUBLE);
}

/*!
  \ingroup h5block_data
*/
/*!
  Write a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_data, \c y_data and \c z_data to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with floating points (64-bit) values.

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dWriteVector3dFieldFloat64 (
	h5_file_t *f,		  /*!< IN: file handle */
	const char *name,	       /*!< IN: name of dataset to write */
	const h5_float64_t *x_data, /*!< IN: X axis data */
	const h5_float64_t *y_data, /*!< IN: Y axis data */
	const h5_float64_t *z_data  /*!< IN: Z axis data */
	) {

	SET_FNAME( f, __func__ );

	return h5b_write_vector3d_data(f, name,
		(void*)x_data, (void*)y_data, (void*)z_data, H5T_NATIVE_DOUBLE);
}

/*!
  \ingroup h5block_data
*/
/*!
  Read a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_data, \c y_data and \c z_data to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with floating points (64-bit) values.

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dReadVector3dFieldFloat64 (
	h5_file_t *f,		  /*!< IN: file handle */
	const char *name,	       /*!< IN: name of dataset to write */
	h5_float64_t *x_data, /*!< OUT: X axis data */
	h5_float64_t *y_data, /*!< OUT: Y axis data */
	h5_float64_t *z_data  /*!< OUT: Z axis data */
	) {

	SET_FNAME( f, __func__ );
	
	return h5b_read_vector3d_data(f, name,
		(void*)x_data, (void*)y_data, (void*)z_data, H5T_NATIVE_DOUBLE);
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
	h5_file_t *f,		  /*!< IN: file handle */
	const char *name,	       /*!< IN: name of dataset to write */
	const h5_float32_t *data      /*!< IN: scalar data to write */
	) {

	SET_FNAME( f, __func__ );

	return h5b_write_scalar_data(f, name, (void*)data, H5T_NATIVE_FLOAT );
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
	h5_file_t *f,		  /*!< IN: file handle */
	const char *name,	       /*!< IN: name of dataset to read */
	h5_float32_t *data	  /*!< OUT: ptr to read buffer */
	) {

	SET_FNAME( f, __func__ );	

	return h5b_read_scalar_data(f, name, (void*)data, H5T_NATIVE_FLOAT);
}

/*!
  \ingroup h5block_data
*/
/*!
  Write a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_data, \c y_data and \c z_data to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with floating points (32-bit) values.

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dWriteVector3dFieldFloat32 (
	h5_file_t *f,		  /*!< IN: file handle */
	const char *name,	       /*!< IN: name of dataset to write */
	const h5_float32_t *x_data, /*!< IN: X axis data */
	const h5_float32_t *y_data, /*!< IN: Y axis data */
	const h5_float32_t *z_data  /*!< IN: Z axis data */
	) {

	SET_FNAME( f, __func__ );

	return h5b_write_vector3d_data(f, name,
		(void*)x_data, (void*)y_data, (void*)z_data, H5T_NATIVE_FLOAT);
}

/*!
  \ingroup h5block_data
*/
/*!
  Read a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_data, \c y_data and \c z_data to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with floating points (32-bit) values.

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dReadVector3dFieldFloat32 (
	h5_file_t *f,		  /*!< IN: file handle */
	const char *name,	       /*!< IN: name of dataset to write */
	h5_float32_t *x_data, /*!< OUT: X axis data */
	h5_float32_t *y_data, /*!< OUT: Y axis data */
	h5_float32_t *z_data  /*!< OUT: Z axis data */
	) {

	SET_FNAME( f, __func__ );
	
	return h5b_read_vector3d_data(f, name,
		(void*)x_data, (void*)y_data, (void*)z_data, H5T_NATIVE_FLOAT);
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
	h5_file_t *f,		  /*!< IN: file handle */
	const char *name,	       /*!< IN: name of dataset to write */
	const h5_int64_t *data      /*!< IN: scalar data to write */
	) {

	SET_FNAME( f, __func__ );

	return h5b_write_scalar_data(f, name, (void*)data, H5T_NATIVE_INT64 );
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
	h5_file_t *f,		  /*!< IN: file handle */
	const char *name,	       /*!< IN: name of dataset to read */
	h5_int64_t *data	  /*!< OUT: ptr to read buffer */
	) {

	SET_FNAME( f, __func__ );	

	return h5b_read_scalar_data(f, name, (void*)data, H5T_NATIVE_INT64);
}

/*!
  \ingroup h5block_data
*/
/*!
  Write a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_data, \c y_data and \c z_data to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with integers (64-bit) values.

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dWriteVector3dFieldInt64 (
	h5_file_t *f,		  /*!< IN: file handle */
	const char *name,	       /*!< IN: name of dataset to write */
	const h5_int64_t *x_data, /*!< IN: X axis data */
	const h5_int64_t *y_data, /*!< IN: Y axis data */
	const h5_int64_t *z_data  /*!< IN: Z axis data */
	) {

	SET_FNAME( f, __func__ );

	return h5b_write_vector3d_data(f, name,
		(void*)x_data, (void*)y_data, (void*)z_data, H5T_NATIVE_INT64);
}

/*!
  \ingroup h5block_data
*/
/*!
  Read a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_data, \c y_data and \c z_data to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with integers (64-bit) values.

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dReadVector3dFieldInt64 (
	h5_file_t *f,		  /*!< IN: file handle */
	const char *name,	       /*!< IN: name of dataset to write */
	h5_int64_t *x_data, /*!< OUT: X axis data */
	h5_int64_t *y_data, /*!< OUT: Y axis data */
	h5_int64_t *z_data  /*!< OUT: Z axis data */
	) {

	SET_FNAME( f, __func__ );
	
	return h5b_read_vector3d_data(f, name,
		(void*)x_data, (void*)y_data, (void*)z_data, H5T_NATIVE_INT64);
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
	h5_file_t *f,		  /*!< IN: file handle */
	const char *name,	       /*!< IN: name of dataset to write */
	const h5_int32_t *data      /*!< IN: scalar data to write */
	) {

	SET_FNAME( f, __func__ );

	return h5b_write_scalar_data(f, name, (void*)data, H5T_NATIVE_INT32 );
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
	h5_file_t *f,		  /*!< IN: file handle */
	const char *name,	       /*!< IN: name of dataset to read */
	h5_int32_t *data	  /*!< OUT: ptr to read buffer */
	) {

	SET_FNAME( f, __func__ );	

	return h5b_read_scalar_data(f, name, (void*)data, H5T_NATIVE_INT32);
}

/*!
  \ingroup h5block_data
*/
/*!
  Write a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_data, \c y_data and \c z_data to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with integers (32-bit) values.

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dWriteVector3dFieldInt32 (
	h5_file_t *f,		  /*!< IN: file handle */
	const char *name,	       /*!< IN: name of dataset to write */
	const h5_int32_t *x_data, /*!< IN: X axis data */
	const h5_int32_t *y_data, /*!< IN: Y axis data */
	const h5_int32_t *z_data  /*!< IN: Z axis data */
	) {

	SET_FNAME( f, __func__ );

	return h5b_write_vector3d_data(f, name,
		(void*)x_data, (void*)y_data, (void*)z_data, H5T_NATIVE_INT32);
}

/*!
  \ingroup h5block_data
*/
/*!
  Read a 3-dimensional field \c name with 3-dimensional vectors as values
  from the buffers starting at \c x_data, \c y_data and \c z_data to the
  current time-step using the defined field layout. Values are 3-dimensional
  vectors with integers (32-bit) values.

  You must use the Fortran indexing scheme to access items in \c data.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5Block3dReadVector3dFieldInt32 (
	h5_file_t *f,		  /*!< IN: file handle */
	const char *name,	       /*!< IN: name of dataset to write */
	h5_int32_t *x_data, /*!< OUT: X axis data */
	h5_int32_t *y_data, /*!< OUT: Y axis data */
	h5_int32_t *z_data  /*!< OUT: Z axis data */
	) {

	SET_FNAME( f, __func__ );
	
	return h5b_read_vector3d_data(f, name,
		(void*)x_data, (void*)y_data, (void*)z_data, H5T_NATIVE_INT32);
}

/*!
  \ingroup h5block_attrib

  Write float64 \c values as attribute \c attrib_name of field
  \c field_name.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockWriteFieldAttribFloat64 (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const h5_float64_t *values,		/*!< IN: attribute values */
	const h5_size_t nvalues			/*!< IN: number of elements */
	) {

	SET_FNAME( f, __func__ );

	return h5_write_field_attrib (
		f,
		field_name,
		attrib_name,
                H5T_NATIVE_DOUBLE,
                values,
		nvalues );
}

/*!
  \ingroup h5block_attrib

  Read float64 values from attribute \c attrib_name of field
  \c field_name into a \c buffer.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockReadFieldAttribFloat64 (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	h5_float64_t *buffer		        /*!< OUT: attribute values */
	) {

	SET_FNAME( f, __func__ );

	return h5_read_field_attrib (
		f,
		field_name,
		attrib_name,
                H5T_NATIVE_DOUBLE,
                (void*)buffer);
}

/*!
  \ingroup h5block_attrib

  Write float32 \c values as attribute \c attrib_name of field
  \c field_name.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockWriteFieldAttribFloat32 (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const h5_float32_t *values,		/*!< IN: attribute values */
	const h5_size_t nvalues			/*!< IN: number of elements */
	) {

	SET_FNAME( f, __func__ );

	return h5_write_field_attrib (
		f,
		field_name,
		attrib_name,
                H5T_NATIVE_FLOAT,
                values,
		nvalues );
}

/*!
  \ingroup h5block_attrib

  Read float32 values from attribute \c attrib_name of field
  \c field_name into a \c buffer.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockReadFieldAttribFloat32 (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	h5_float32_t *buffer		        /*!< OUT: attribute values */
	) {

	SET_FNAME( f, __func__ );

	return h5_read_field_attrib (
		f,
		field_name,
		attrib_name,
                H5T_NATIVE_FLOAT,
                (void*)buffer);
}

/*!
  \ingroup h5block_attrib

  Write int64 \c values as attribute \c attrib_name of field
  \c field_name.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockWriteFieldAttribInt64 (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const h5_int64_t *values,		/*!< IN: attribute values */
	const h5_size_t nvalues			/*!< IN: number of elements */
	) {

	SET_FNAME( f, __func__ );

	return h5_write_field_attrib (
		f,
		field_name,
		attrib_name,
                H5T_NATIVE_INT64,
                values,
		nvalues );
}

/*!
  \ingroup h5block_attrib

  Read int64 values from attribute \c attrib_name of field
  \c field_name into a \c buffer.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockReadFieldAttribInt64 (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	h5_int64_t *buffer		        /*!< OUT: attribute values */
	) {

	SET_FNAME( f, __func__ );

	return h5_read_field_attrib (
		f,
		field_name,
		attrib_name,
                H5T_NATIVE_INT64,
                (void*)buffer);
}

/*!
  \ingroup h5block_attrib

  Write int32 \c values as attribute \c attrib_name of field
  \c field_name.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockWriteFieldAttribInt32 (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const h5_int32_t *values,		/*!< IN: attribute values */
	const h5_size_t nvalues			/*!< IN: number of elements */
	) {

	SET_FNAME( f, __func__ );

	return h5_write_field_attrib (
		f,
		field_name,
		attrib_name,
                H5T_NATIVE_INT32,
                values,
		nvalues );
}

/*!
  \ingroup h5block_attrib

  Read int32 values from attribute \c attrib_name of field
  \c field_name into a \c buffer.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockReadFieldAttribInt32 (
	h5_file_t *f,				/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	h5_int32_t *buffer		        /*!< OUT: attribute values */
	) {

	SET_FNAME( f, __func__ );

	return h5_read_field_attrib (
		f,
		field_name,
		attrib_name,
                H5T_NATIVE_INT32,
                (void*)buffer);
}
