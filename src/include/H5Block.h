#ifndef __H5BLOCK_H
#define __H5BLOCK_H

#ifdef __cplusplus
extern "C" {
#endif

/*! 
  Interface for block structured field data

*/

h5_err_t
H5BlockDefine3DFieldLayout (
	h5_file_t *f,
	const h5_size_t i_start,
	const h5_size_t i_end,
	const h5_size_t j_start,
	const h5_size_t j_end,
	const h5_size_t k_start,
	const h5_size_t k_end
	);

h5_err_t
H5Block3dGetPartitionOfProc (
	h5_file_t *f,
	const h5_int64_t proc,
	h5_size_t *i_start,
	h5_size_t *i_end,
	h5_size_t *j_start,
	h5_size_t *j_end,
	h5_size_t *k_start,
	h5_size_t *k_end
	);

h5_err_t
H5Block3dGetReducedPartitionOfProc (
	h5_file_t *f,
	h5_id_t proc,
	h5_size_t *i_start, 
	h5_size_t *i_end,
	h5_size_t *j_start,
	h5_size_t *j_end,
	h5_size_t *k_start,
	h5_size_t *k_end
	);

h5_id_t
H5Block3dGetProcOf (
	h5_file_t *f,
	h5_size_t i,
	h5_size_t j,
	h5_size_t k
	);

h5_int64_t
H5Block3dWriteScalarField (
	h5_file_t *f,
	const char *name,
	const h5_float64_t *data
	);

h5_int64_t
H5Block3dReadScalarField (
	h5_file_t *f,
	const char *name,
	h5_float64_t *data
	);

h5_int64_t
H5BlockGetNumFields (
	h5_file_t *f
	);

h5_int64_t
H5BlockGetFieldInfo (
	h5_file_t *f,
	const h5_int64_t idx,
	char *name,
	const h5_int64_t len_name,
	h5_int64_t *grid_rank,
	h5_int64_t *grid_dims,
	h5_int64_t *field_dims
	);

h5_int64_t
H5BlockGetFieldInfoByName (
	h5_file_t *f,
	const char *field_name,
	h5_int64_t *grid_rank,
	h5_int64_t *grid_dims,
	h5_int64_t *field_dims
	);

h5_int64_t
H5Block3dGetFieldOrigin (
	h5_file_t *f,
	const char *field_name,
	h5_float64_t *x_origin,
	h5_float64_t *y_origin,
	h5_float64_t *z_origin
	);

h5_int64_t
H5Block3dSetFieldOrigin (
	h5_file_t *f,
	const char *field_name,
	const h5_float64_t x_origin,
	const h5_float64_t y_origin,
	const h5_float64_t z_origin
	);

h5_int64_t
H5Block3dGetFieldSpacing (
	h5_file_t *f,
	const char *field_name,
	h5_float64_t *x_spacing,
	h5_float64_t *y_spacing,
	h5_float64_t *z_spacing
	);

h5_int64_t
H5Block3dSetFieldSpacing (
	h5_file_t *f,
	const char *field_name,
	const h5_float64_t x_spacing,
	const h5_float64_t y_spacing,
	const h5_float64_t z_spacing
	);



h5_int64_t
H5Block3dWrite3dVectorField (
	h5_file_t *f,
	const char *name,
	const h5_float64_t *xval,
	const h5_float64_t *yval,
	const h5_float64_t *zval
	);	

h5_int64_t
H5Block3dRead3dVectorField (
	h5_file_t *f,
	const char *name,
	h5_float64_t *xval,
	h5_float64_t *yval,
	h5_float64_t *zval
	);

h5_int64_t
H5BlockWriteFieldAttrib (
	h5_file_t *f,
	const char *field_name,
	const char *attrib_name,
	const h5_int64_t attrib_type,
	const void *attrib_value,
	const h5_int64_t attrib_nelem
	);

h5_int64_t
H5BlockWriteFieldAttribString (
	h5_file_t *f,
	const char *field_name,
	const char *attrib_name,
	const char *attrib_value
	);

h5_int64_t
H5BlockGetNumFieldAttribs (
	h5_file_t *f,
	const char *field_name
	);

h5_int64_t
H5BlockGetFieldAttribInfo (
	h5_file_t *f,
	const char *field_name,
	const h5_int64_t attrib_idx,
	char *attrib_name,
	const h5_int64_t len_of_attrib_name,
	h5_int64_t *attrib_type,
	h5_int64_t *attrib_nelem
	);

h5_int64_t
H5BlockReadFieldAttrib (
	h5_file_t *f,
	const char *field_name,
	const char *attrib_name,
	void *attrib_value
	);

h5_int64_t
H5BlockHasFieldData (
	h5_file_t *f
	);


#ifdef __cplusplus
}
#endif

#endif
