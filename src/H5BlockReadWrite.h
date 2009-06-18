
#ifndef __H5BLOCKREADWRITE_H
#define __H5BLOCKREADWRITE_H

#ifdef __cplusplus
extern "C" {
#endif


h5part_int64_t
H5Block3dWriteScalarFieldFloat64 (
	H5PartFile *f,
	const char *name,
	const h5part_float64_t *data
	);

h5part_int64_t
H5Block3dReadScalarFieldFloat64 (
	H5PartFile *f,
	const char *name,
	h5part_float64_t *data
	);

h5part_int64_t
H5Block3dWrite3dVectorFieldFloat64 (
	H5PartFile *f,
	const char *name,
	const h5part_float64_t *xval,
	const h5part_float64_t *yval,
	const h5part_float64_t *zval
	);

h5part_int64_t
H5Block3dRead3dVectorFieldFloat64 (
	H5PartFile *f,
	const char *name,
	h5part_float64_t *xval,
	h5part_float64_t *yval,
	h5part_float64_t *zval
	);

h5part_int64_t
H5Block3dWriteScalarFieldFloat32 (
	H5PartFile *f,
	const char *name,
	const h5part_float32_t *data
	);

h5part_int64_t
H5Block3dReadScalarFieldFloat32 (
	H5PartFile *f,
	const char *name,
	h5part_float32_t *data
	);

h5part_int64_t
H5Block3dWrite3dVectorFieldFloat32 (
	H5PartFile *f,
	const char *name,
	const h5part_float32_t *xval,
	const h5part_float32_t *yval,
	const h5part_float32_t *zval
	);

h5part_int64_t
H5Block3dRead3dVectorFieldFloat32 (
	H5PartFile *f,
	const char *name,
	h5part_float32_t *xval,
	h5part_float32_t *yval,
	h5part_float32_t *zval
	);

h5part_int64_t
H5Block3dWriteScalarFieldInt64 (
	H5PartFile *f,
	const char *name,
	const h5part_int64_t *data
	);

h5part_int64_t
H5Block3dReadScalarFieldInt64 (
	H5PartFile *f,
	const char *name,
	h5part_int64_t *data
	);

h5part_int64_t
H5Block3dWrite3dVectorFieldInt64 (
	H5PartFile *f,
	const char *name,
	const h5part_int64_t *xval,
	const h5part_int64_t *yval,
	const h5part_int64_t *zval
	);

h5part_int64_t
H5Block3dRead3dVectorFieldInt64 (
	H5PartFile *f,
	const char *name,
	h5part_int64_t *xval,
	h5part_int64_t *yval,
	h5part_int64_t *zval
	);

h5part_int64_t
H5Block3dWriteScalarFieldInt32 (
	H5PartFile *f,
	const char *name,
	const h5part_int32_t *data
	);

h5part_int64_t
H5Block3dReadScalarFieldInt32 (
	H5PartFile *f,
	const char *name,
	h5part_int32_t *data
	);

h5part_int64_t
H5Block3dWrite3dVectorFieldInt32 (
	H5PartFile *f,
	const char *name,
	const h5part_int32_t *xval,
	const h5part_int32_t *yval,
	const h5part_int32_t *zval
	);

h5part_int64_t
H5Block3dRead3dVectorFieldInt32 (
	H5PartFile *f,
	const char *name,
	h5part_int32_t *xval,
	h5part_int32_t *yval,
	h5part_int32_t *zval
	);


#ifdef __cplusplus
}
#endif

#endif
