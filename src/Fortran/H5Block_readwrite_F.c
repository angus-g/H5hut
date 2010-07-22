
#include <stdlib.h>

#include "h5core/h5_core.h"
#include "Underscore.h"

#if defined(F77_SINGLE_UNDERSCORE)
#define F77NAME(a,b) a
#elif defined(F77_CRAY_UNDERSCORE)
#define F77NAME(a,b) b
#elif defined(F77_NO_UNDERSCORE)
#else
#error Error, no way to determine how to construct fortran bindings
#endif

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_3d_write_scalar_field_r8 F77NAME ( \
	h5bl_3d_write_scalar_field_r8_, \
	H5BL_3D_WRITE_SCALAR_FIELD_R8 )
#endif

h5_err_t
h5bl_3d_write_scalar_field_r8 (
	h5_int64_t *f,
	const char *field_name,
	const h5_float64_t *data,
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 =  h5_strdupfor2c ( field_name, l_field_name );
	h5_err_t herr = h5b_write_scalar_data (
		filehandle, field_name2, (void*)data, H5T_NATIVE_DOUBLE );
	free ( field_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_3d_read_scalar_field_r8 F77NAME ( \
	h5bl_3d_read_scalar_field_r8_, \
	H5BL_3D_READ_SCALAR_FIELD_R8 )
#endif

h5_err_t
h5bl_3d_read_scalar_field_r8 (
	h5_int64_t *f,
	const char *field_name,
	h5_float64_t *data,
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 =  h5_strdupfor2c ( field_name,  l_field_name );
	h5_err_t herr = h5b_read_scalar_data (
		filehandle, field_name2, data, H5T_NATIVE_DOUBLE );
	free ( field_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_3d_write_vector3d_field_r8 F77NAME ( \
	h5bl_3d_write_vector3d_field_r8_, \
	H5BL_3D_WRITE_VECTOR3D_FIELD_R8 )
#endif

h5_err_t
h5bl_3d_write_vector3d_field_r8 (
	h5_int64_t *f,	      /*!< file handle */
	const char *field_name,	 /*!< name of the data set */
	const h5_float64_t *xval,   /*!< array of x component data */
	const h5_float64_t *yval,   /*!< array of y component data */
	const h5_float64_t *zval,   /*!< array of z component data */
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 =  h5_strdupfor2c ( field_name,  l_field_name );
	h5_err_t herr = h5b_write_vector3d_data (
		filehandle, field_name2,
		(void*)xval, (void*)yval, (void*)zval, H5T_NATIVE_DOUBLE );
	free ( field_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_3d_read_vector3d_field_r8 F77NAME ( \
	h5bl_3d_read_vector3d_field_r8_, \
	H5BL_3D_READ_VECTOR3D_FIELD_R8 )
#endif

h5_err_t
h5bl_3d_read_vector3d_field_r8 (
	h5_int64_t *f,	      /*!< file handle */
	const char *field_name,	 /*!< name of the data set */
	h5_float64_t *xval,	 /*!< array of x component data */
	h5_float64_t *yval,	 /*!< array of y component data */
	h5_float64_t *zval,	 /*!< array of z component data */
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 =  h5_strdupfor2c ( field_name,  l_field_name );
	h5_err_t herr = h5b_read_vector3d_data (
		filehandle, field_name2,
		(void*)xval, (void*)yval, (void*)zval, H5T_NATIVE_DOUBLE );
	free ( field_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_3d_write_scalar_field_r4 F77NAME ( \
	h5bl_3d_write_scalar_field_r4_, \
	H5BL_3D_WRITE_SCALAR_FIELD_R4 )
#endif

h5_err_t
h5bl_3d_write_scalar_field_r4 (
	h5_int64_t *f,
	const char *field_name,
	const h5_float32_t *data,
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 =  h5_strdupfor2c ( field_name, l_field_name );
	h5_err_t herr = h5b_write_scalar_data (
		filehandle, field_name2, (void*)data, H5T_NATIVE_FLOAT );
	free ( field_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_3d_read_scalar_field_r4 F77NAME ( \
	h5bl_3d_read_scalar_field_r4_, \
	H5BL_3D_READ_SCALAR_FIELD_R4 )
#endif

h5_err_t
h5bl_3d_read_scalar_field_r4 (
	h5_int64_t *f,
	const char *field_name,
	h5_float32_t *data,
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 =  h5_strdupfor2c ( field_name,  l_field_name );
	h5_err_t herr = h5b_read_scalar_data (
		filehandle, field_name2, data, H5T_NATIVE_FLOAT );
	free ( field_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_3d_write_vector3d_field_r4 F77NAME ( \
	h5bl_3d_write_vector3d_field_r4_, \
	H5BL_3D_WRITE_VECTOR3D_FIELD_R4 )
#endif

h5_err_t
h5bl_3d_write_vector3d_field_r4 (
	h5_int64_t *f,	      /*!< file handle */
	const char *field_name,	 /*!< name of the data set */
	const h5_float32_t *xval,   /*!< array of x component data */
	const h5_float32_t *yval,   /*!< array of y component data */
	const h5_float32_t *zval,   /*!< array of z component data */
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 =  h5_strdupfor2c ( field_name,  l_field_name );
	h5_err_t herr = h5b_write_vector3d_data (
		filehandle, field_name2,
		(void*)xval, (void*)yval, (void*)zval, H5T_NATIVE_FLOAT );
	free ( field_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_3d_read_vector3d_field_r4 F77NAME ( \
	h5bl_3d_read_vector3d_field_r4_, \
	H5BL_3D_READ_VECTOR3D_FIELD_R4 )
#endif

h5_err_t
h5bl_3d_read_vector3d_field_r4 (
	h5_int64_t *f,	      /*!< file handle */
	const char *field_name,	 /*!< name of the data set */
	h5_float32_t *xval,	 /*!< array of x component data */
	h5_float32_t *yval,	 /*!< array of y component data */
	h5_float32_t *zval,	 /*!< array of z component data */
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 =  h5_strdupfor2c ( field_name,  l_field_name );
	h5_err_t herr = h5b_read_vector3d_data (
		filehandle, field_name2,
		(void*)xval, (void*)yval, (void*)zval, H5T_NATIVE_FLOAT );
	free ( field_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_3d_write_scalar_field_i8 F77NAME ( \
	h5bl_3d_write_scalar_field_i8_, \
	H5BL_3D_WRITE_SCALAR_FIELD_I8 )
#endif

h5_err_t
h5bl_3d_write_scalar_field_i8 (
	h5_int64_t *f,
	const char *field_name,
	const h5_int64_t *data,
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 =  h5_strdupfor2c ( field_name, l_field_name );
	h5_err_t herr = h5b_write_scalar_data (
		filehandle, field_name2, (void*)data, H5T_NATIVE_INT64 );
	free ( field_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_3d_read_scalar_field_i8 F77NAME ( \
	h5bl_3d_read_scalar_field_i8_, \
	H5BL_3D_READ_SCALAR_FIELD_I8 )
#endif

h5_err_t
h5bl_3d_read_scalar_field_i8 (
	h5_int64_t *f,
	const char *field_name,
	h5_int64_t *data,
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 =  h5_strdupfor2c ( field_name,  l_field_name );
	h5_err_t herr = h5b_read_scalar_data (
		filehandle, field_name2, data, H5T_NATIVE_INT64 );
	free ( field_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_3d_write_vector3d_field_i8 F77NAME ( \
	h5bl_3d_write_vector3d_field_i8_, \
	H5BL_3D_WRITE_VECTOR3D_FIELD_I8 )
#endif

h5_err_t
h5bl_3d_write_vector3d_field_i8 (
	h5_int64_t *f,	      /*!< file handle */
	const char *field_name,	 /*!< name of the data set */
	const h5_int64_t *xval,   /*!< array of x component data */
	const h5_int64_t *yval,   /*!< array of y component data */
	const h5_int64_t *zval,   /*!< array of z component data */
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 =  h5_strdupfor2c ( field_name,  l_field_name );
	h5_err_t herr = h5b_write_vector3d_data (
		filehandle, field_name2,
		(void*)xval, (void*)yval, (void*)zval, H5T_NATIVE_INT64 );
	free ( field_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_3d_read_vector3d_field_i8 F77NAME ( \
	h5bl_3d_read_vector3d_field_i8_, \
	H5BL_3D_READ_VECTOR3D_FIELD_I8 )
#endif

h5_err_t
h5bl_3d_read_vector3d_field_i8 (
	h5_int64_t *f,	      /*!< file handle */
	const char *field_name,	 /*!< name of the data set */
	h5_int64_t *xval,	 /*!< array of x component data */
	h5_int64_t *yval,	 /*!< array of y component data */
	h5_int64_t *zval,	 /*!< array of z component data */
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 =  h5_strdupfor2c ( field_name,  l_field_name );
	h5_err_t herr = h5b_read_vector3d_data (
		filehandle, field_name2,
		(void*)xval, (void*)yval, (void*)zval, H5T_NATIVE_INT64 );
	free ( field_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_3d_write_scalar_field_i4 F77NAME ( \
	h5bl_3d_write_scalar_field_i4_, \
	H5BL_3D_WRITE_SCALAR_FIELD_I4 )
#endif

h5_err_t
h5bl_3d_write_scalar_field_i4 (
	h5_int64_t *f,
	const char *field_name,
	const h5_int32_t *data,
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 =  h5_strdupfor2c ( field_name, l_field_name );
	h5_err_t herr = h5b_write_scalar_data (
		filehandle, field_name2, (void*)data, H5T_NATIVE_INT32 );
	free ( field_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_3d_read_scalar_field_i4 F77NAME ( \
	h5bl_3d_read_scalar_field_i4_, \
	H5BL_3D_READ_SCALAR_FIELD_I4 )
#endif

h5_err_t
h5bl_3d_read_scalar_field_i4 (
	h5_int64_t *f,
	const char *field_name,
	h5_int32_t *data,
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 =  h5_strdupfor2c ( field_name,  l_field_name );
	h5_err_t herr = h5b_read_scalar_data (
		filehandle, field_name2, data, H5T_NATIVE_INT32 );
	free ( field_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_3d_write_vector3d_field_i4 F77NAME ( \
	h5bl_3d_write_vector3d_field_i4_, \
	H5BL_3D_WRITE_VECTOR3D_FIELD_I4 )
#endif

h5_err_t
h5bl_3d_write_vector3d_field_i4 (
	h5_int64_t *f,	      /*!< file handle */
	const char *field_name,	 /*!< name of the data set */
	const h5_int32_t *xval,   /*!< array of x component data */
	const h5_int32_t *yval,   /*!< array of y component data */
	const h5_int32_t *zval,   /*!< array of z component data */
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 =  h5_strdupfor2c ( field_name,  l_field_name );
	h5_err_t herr = h5b_write_vector3d_data (
		filehandle, field_name2,
		(void*)xval, (void*)yval, (void*)zval, H5T_NATIVE_INT32 );
	free ( field_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_3d_read_vector3d_field_i4 F77NAME ( \
	h5bl_3d_read_vector3d_field_i4_, \
	H5BL_3D_READ_VECTOR3D_FIELD_I4 )
#endif

h5_err_t
h5bl_3d_read_vector3d_field_i4 (
	h5_int64_t *f,	      /*!< file handle */
	const char *field_name,	 /*!< name of the data set */
	h5_int32_t *xval,	 /*!< array of x component data */
	h5_int32_t *yval,	 /*!< array of y component data */
	h5_int32_t *zval,	 /*!< array of z component data */
	const int l_field_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 =  h5_strdupfor2c ( field_name,  l_field_name );
	h5_err_t herr = h5b_read_vector3d_data (
		filehandle, field_name2,
		(void*)xval, (void*)yval, (void*)zval, H5T_NATIVE_INT32 );
	free ( field_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_writefieldattrib_r8 F77NAME ( \
	h5bl_writefieldattrib_r8_, \
	H5BL_WRITEFIELDATTRIB_R8 )
#endif

h5_err_t
h5bl_writefieldattrib_r8 (
	h5_int64_t *f,
	const char *field_name,
	const char *attrib_name,
	const h5_float64_t *values,
	const h5_size_t *nvalues,
	const int l_field_name,
	const int l_attrib_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 = h5_strdupfor2c ( field_name,  l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	h5_err_t herr = h5_write_field_attrib (
		filehandle, field_name2, attrib_name2,
		H5T_NATIVE_DOUBLE, values, *nvalues );
	free ( field_name2 );
	free ( attrib_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_readfieldattrib_r8 F77NAME ( \
	h5bl_readfieldattrib_r8_, \
	H5BL_READFIELDATTRIB_R8 )
#endif

h5_err_t
h5bl_readfieldattrib_r8 (
	h5_int64_t *f,
	const char *field_name,
	const char *attrib_name,
	h5_float64_t *values,
	const int l_field_name,
	const int l_attrib_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 = h5_strdupfor2c ( field_name,  l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	h5_err_t herr = h5_read_field_attrib (
		filehandle, field_name2, attrib_name2, H5T_NATIVE_DOUBLE, values );
	free ( field_name2 );
	free ( attrib_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_writefieldattrib_r4 F77NAME ( \
	h5bl_writefieldattrib_r4_, \
	H5BL_WRITEFIELDATTRIB_R4 )
#endif

h5_err_t
h5bl_writefieldattrib_r4 (
	h5_int64_t *f,
	const char *field_name,
	const char *attrib_name,
	const h5_float32_t *values,
	const h5_size_t *nvalues,
	const int l_field_name,
	const int l_attrib_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 = h5_strdupfor2c ( field_name,  l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	h5_err_t herr = h5_write_field_attrib (
		filehandle, field_name2, attrib_name2,
		H5T_NATIVE_FLOAT, values, *nvalues );
	free ( field_name2 );
	free ( attrib_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_readfieldattrib_r4 F77NAME ( \
	h5bl_readfieldattrib_r4_, \
	H5BL_READFIELDATTRIB_R4 )
#endif

h5_err_t
h5bl_readfieldattrib_r4 (
	h5_int64_t *f,
	const char *field_name,
	const char *attrib_name,
	h5_float32_t *values,
	const int l_field_name,
	const int l_attrib_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 = h5_strdupfor2c ( field_name,  l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	h5_err_t herr = h5_read_field_attrib (
		filehandle, field_name2, attrib_name2, H5T_NATIVE_FLOAT, values );
	free ( field_name2 );
	free ( attrib_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_writefieldattrib_i8 F77NAME ( \
	h5bl_writefieldattrib_i8_, \
	H5BL_WRITEFIELDATTRIB_I8 )
#endif

h5_err_t
h5bl_writefieldattrib_i8 (
	h5_int64_t *f,
	const char *field_name,
	const char *attrib_name,
	const h5_int64_t *values,
	const h5_size_t *nvalues,
	const int l_field_name,
	const int l_attrib_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 = h5_strdupfor2c ( field_name,  l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	h5_err_t herr = h5_write_field_attrib (
		filehandle, field_name2, attrib_name2,
		H5T_NATIVE_INT64, values, *nvalues );
	free ( field_name2 );
	free ( attrib_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_readfieldattrib_i8 F77NAME ( \
	h5bl_readfieldattrib_i8_, \
	H5BL_READFIELDATTRIB_I8 )
#endif

h5_err_t
h5bl_readfieldattrib_i8 (
	h5_int64_t *f,
	const char *field_name,
	const char *attrib_name,
	h5_int64_t *values,
	const int l_field_name,
	const int l_attrib_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 = h5_strdupfor2c ( field_name,  l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	h5_err_t herr = h5_read_field_attrib (
		filehandle, field_name2, attrib_name2, H5T_NATIVE_INT64, values );
	free ( field_name2 );
	free ( attrib_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_writefieldattrib_i4 F77NAME ( \
	h5bl_writefieldattrib_i4_, \
	H5BL_WRITEFIELDATTRIB_I4 )
#endif

h5_err_t
h5bl_writefieldattrib_i4 (
	h5_int64_t *f,
	const char *field_name,
	const char *attrib_name,
	const h5_int32_t *values,
	const h5_size_t *nvalues,
	const int l_field_name,
	const int l_attrib_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 = h5_strdupfor2c ( field_name,  l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	h5_err_t herr = h5_write_field_attrib (
		filehandle, field_name2, attrib_name2,
		H5T_NATIVE_INT32, values, *nvalues );
	free ( field_name2 );
	free ( attrib_name2 );
	return herr;
}

#if ! defined(F77_NO_UNDERSCORE)
#define h5bl_readfieldattrib_i4 F77NAME ( \
	h5bl_readfieldattrib_i4_, \
	H5BL_READFIELDATTRIB_I4 )
#endif

h5_err_t
h5bl_readfieldattrib_i4 (
	h5_int64_t *f,
	const char *field_name,
	const char *attrib_name,
	h5_int32_t *values,
	const int l_field_name,
	const int l_attrib_name
	) {

	h5_file_t *filehandle = (h5_file_t*)(size_t)*f;
	h5_set_funcname( filehandle, __func__ );
	char *field_name2 = h5_strdupfor2c ( field_name,  l_field_name );
	char *attrib_name2 = h5_strdupfor2c ( attrib_name, l_attrib_name );
	h5_err_t herr = h5_read_field_attrib (
		filehandle, field_name2, attrib_name2, H5T_NATIVE_INT32, values );
	free ( field_name2 );
	free ( attrib_name2 );
	return herr;
}
