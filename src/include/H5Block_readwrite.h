
#ifndef __H5BLOCK_READWRITE_H
#define __H5BLOCK_READWRITE_H

#ifdef __cplusplus
extern "C" {
#endif

h5_err_t
H5Block3dWriteScalarFieldFloat64 (
	h5_file_t *f,
	const char *name,
	const h5_float64_t *data
	);

h5_err_t
H5Block3dReadScalarFieldFloat64 (
	h5_file_t *f,
	const char *name,
	h5_float64_t *data
	);

h5_err_t
H5Block3dWriteVector3dFieldFloat64 (
	h5_file_t *f,
	const char *name,
	const h5_float64_t *x_data,
	const h5_float64_t *y_data,
	const h5_float64_t *z_data
	);

h5_err_t
H5Block3dReadVector3dFieldFloat64 (
	h5_file_t *f,
	const char *name,
	h5_float64_t *x_data,
	h5_float64_t *y_data,
	h5_float64_t *z_data
	);

h5_err_t
H5Block3dWriteScalarFieldFloat32 (
	h5_file_t *f,
	const char *name,
	const h5_float32_t *data
	);

h5_err_t
H5Block3dReadScalarFieldFloat32 (
	h5_file_t *f,
	const char *name,
	h5_float32_t *data
	);

h5_err_t
H5Block3dWriteVector3dFieldFloat32 (
	h5_file_t *f,
	const char *name,
	const h5_float32_t *x_data,
	const h5_float32_t *y_data,
	const h5_float32_t *z_data
	);

h5_err_t
H5Block3dReadVector3dFieldFloat32 (
	h5_file_t *f,
	const char *name,
	h5_float32_t *x_data,
	h5_float32_t *y_data,
	h5_float32_t *z_data
	);

h5_err_t
H5Block3dWriteScalarFieldInt64 (
	h5_file_t *f,
	const char *name,
	const h5_int64_t *data
	);

h5_err_t
H5Block3dReadScalarFieldInt64 (
	h5_file_t *f,
	const char *name,
	h5_int64_t *data
	);

h5_err_t
H5Block3dWriteVector3dFieldInt64 (
	h5_file_t *f,
	const char *name,
	const h5_int64_t *x_data,
	const h5_int64_t *y_data,
	const h5_int64_t *z_data
	);

h5_err_t
H5Block3dReadVector3dFieldInt64 (
	h5_file_t *f,
	const char *name,
	h5_int64_t *x_data,
	h5_int64_t *y_data,
	h5_int64_t *z_data
	);

h5_err_t
H5Block3dWriteScalarFieldInt32 (
	h5_file_t *f,
	const char *name,
	const h5_int32_t *data
	);

h5_err_t
H5Block3dReadScalarFieldInt32 (
	h5_file_t *f,
	const char *name,
	h5_int32_t *data
	);

h5_err_t
H5Block3dWriteVector3dFieldInt32 (
	h5_file_t *f,
	const char *name,
	const h5_int32_t *x_data,
	const h5_int32_t *y_data,
	const h5_int32_t *z_data
	);

h5_err_t
H5Block3dReadVector3dFieldInt32 (
	h5_file_t *f,
	const char *name,
	h5_int32_t *x_data,
	h5_int32_t *y_data,
	h5_int32_t *z_data
	);

h5_err_t
H5BlockWriteFieldAttribFloat64 (
	h5_file_t *f,
	const char *field_name,
	const char *attrib_name,
	const h5_float64_t *values,
	const h5_size_t nvalues
	);

h5_err_t
H5BlockReadFieldAttribFloat64 (
	h5_file_t *f,
	const char *field_name,
	const char *attrib_name,
	h5_float64_t *buffer
	);

h5_err_t
H5BlockWriteFieldAttribFloat32 (
	h5_file_t *f,
	const char *field_name,
	const char *attrib_name,
	const h5_float32_t *values,
	const h5_size_t nvalues
	);

h5_err_t
H5BlockReadFieldAttribFloat32 (
	h5_file_t *f,
	const char *field_name,
	const char *attrib_name,
	h5_float32_t *buffer
	);

h5_err_t
H5BlockWriteFieldAttribInt64 (
	h5_file_t *f,
	const char *field_name,
	const char *attrib_name,
	const h5_int64_t *values,
	const h5_size_t nvalues
	);

h5_err_t
H5BlockReadFieldAttribInt64 (
	h5_file_t *f,
	const char *field_name,
	const char *attrib_name,
	h5_int64_t *buffer
	);

h5_err_t
H5BlockWriteFieldAttribInt32 (
	h5_file_t *f,
	const char *field_name,
	const char *attrib_name,
	const h5_int32_t *values,
	const h5_size_t nvalues
	);

h5_err_t
H5BlockReadFieldAttribInt32 (
	h5_file_t *f,
	const char *field_name,
	const char *attrib_name,
	h5_int32_t *buffer
	);

#ifdef __cplusplus
}
#endif

#endif
