#ifndef __H5BLOCK_H
#define __H5BLOCK_H

#ifdef __cplusplus
extern "C" {
#endif

h5_int64_t
H5Block3dHasView (
	h5_file_t *const f	/*!< IN: File handle */
	);

h5_err_t
H5Block3dSetView (
	h5_file_t *const f,		/*!< IN: File handle		*/
	const h5_int64_t i_start,	/*!< IN: start index of \c i	*/ 
	const h5_int64_t i_end,         /*!< IN: end index of \c i	*/  
	const h5_int64_t j_start,	/*!< IN: start index of \c j	*/ 
	const h5_int64_t j_end,	        /*!< IN: end index of \c j	*/ 
	const h5_int64_t k_start,	/*!< IN: start index of \c k	*/ 
	const h5_int64_t k_end	        /*!< IN: end index of \c k	*/
	);

h5_err_t
H5Block3dGetView (
	h5_file_t *const f,		/*!< IN: File handle */
	h5_size_t *const i_start,	/*!< OUT: start index of \c i	*/ 
	h5_size_t *const i_end,		/*!< OUT: end index of \c i	*/  
	h5_size_t *const j_start,	/*!< OUT: start index of \c j	*/ 
	h5_size_t *const j_end,		/*!< OUT: end index of \c j	*/ 
	h5_size_t *const k_start,	/*!< OUT: start index of \c k	*/ 
	h5_size_t *const k_end		/*!< OUT: end index of \c k	*/ 
	);

h5_err_t
H5Block3dGetReducedView (
	h5_file_t *const f,		/*!< IN: File handle */
	h5_size_t *const i_start,	/*!< OUT: start index of \c i */ 
	h5_size_t *const i_end,		/*!< OUT: end index of \c i */  
	h5_size_t *const j_start,	/*!< OUT: start index of \c j */ 
	h5_size_t *const j_end,		/*!< OUT: end index of \c j */ 
	h5_size_t *const k_start,	/*!< OUT: start index of \c j */ 
	h5_size_t *const k_end		/*!< OUT: end index of \c j */ 
	);

h5_err_t
H5Block3dSetChunk (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_size_t i,		/*!< IN: size of \c i */ 
	const h5_size_t j,		/*!< IN: size of \c j */  
	const h5_size_t k		/*!< IN: size of \c k */ 
	);

h5_err_t
H5Block3dGetChunk (
	h5_file_t *const f,		/*!< IN: File handle */
	const char *field_name, 	/*!< IN: name of dataset */
	h5_size_t *const i,		/*!< OUT: size of i */
	h5_size_t *const j,		/*!< OUT: size of j */
	h5_size_t *const k		/*!< OUT: size of k */
	);

#if defined(PARALLEL_IO)
h5_err_t
H5Block3dSetGrid (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_size_t i,		/*!< IN: dimension in \c i */ 
	const h5_size_t j,		/*!< IN: dimension in \c j */  
	const h5_size_t k		/*!< IN: dimension in \c k */ 
	);

h5_err_t
H5Block3dGetGridCoords (
	h5_file_t *const f,		/*!< IN: File handle */
	const int proc,			/*!< IN: MPI processor */
	h5_int64_t *const i,		/*!< OUT: index in \c i */ 
	h5_int64_t *const j,		/*!< OUT: index in \c j */  
	h5_int64_t *const k		/*!< OUT: index in \c k */ 
	);

h5_err_t
H5Block3dSetDims (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_size_t i,		/*!< IN: dimension in \c i */ 
	const h5_size_t j,		/*!< IN: dimension in \c j */  
	const h5_size_t k		/*!< IN: dimension in \c k */ 
	);
#endif

h5_err_t
H5Block3dSetHalo (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_size_t i,		/*!< IN: radius in \c i */ 
	const h5_size_t j,		/*!< IN: radius in \c j */  
	const h5_size_t k		/*!< IN: radius in \c k */ 
	);

h5_ssize_t
H5BlockGetNumFields (
	h5_file_t *const f			/*!< IN: file handle */
	);

h5_err_t
H5BlockGetFieldInfo (
	h5_file_t *const f,		/*!< IN: file handle */
	const h5_size_t idx,		/*!< IN: index of field */
	char *name,			/*!< OUT: field name */
	const h5_size_t len_name,	/*!< IN: buffer size */
	h5_size_t *const field_rank,	/*!< OUT: field rank */
	h5_size_t *const field_dims,	/*!< OUT: field dimensions */
	h5_size_t *const elem_rank,	/*!< OUT: element rank */
	h5_int64_t *const type		/*!< OUT: datatype */
	);

h5_err_t
H5BlockGetFieldInfoByName (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: field name */
	h5_size_t *const field_rank,	/*!< OUT: field rank */
	h5_size_t *const field_dims,	/*!< OUT: field dimensions */
	h5_size_t *const elem_rank,	/*!< OUT: element rank */
	h5_int64_t *const type		/*!< OUT: datatype */
	);

h5_err_t
H5BlockWriteFieldAttribString (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *const field_name,	/*!< IN: field name */
	const char *const attrib_name,	/*!< IN: attribute name */
	const char *const value		/*!< IN: attribute value */
	);

h5_err_t
H5BlockReadFieldAttribString (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *const field_name,	/*!< IN: field name */
	const char *const attrib_name,	/*!< IN: attribute name */
	char *const buffer		/*!< OUT: attribute value */
	);

h5_ssize_t
H5BlockGetNumFieldAttribs (
	h5_file_t *const f,		/*<! IN: file handle */
	const char *const field_name	/*<! IN: field name */
	);

h5_int64_t
H5BlockGetFieldAttribInfo (
	h5_file_t *const f,		/*<! IN: Handle to open file */
	const char *const field_name,	/*<! IN: field name */
	const h5_size_t attrib_idx,	/*<! IN: Index of attribute to
					           get infos about */
	char *const attrib_name,	/*<! OUT: Name of attribute */
	const h5_size_t len_of_attrib_name,
					/*<! IN: length of buffer \c name */
	h5_int64_t *const attrib_type,	/*<! OUT: Type of value. */
	h5_size_t *const attrib_nelem         /*<! OUT: Number of elements */
	);

h5_err_t
H5Block3dGetFieldOrigin (
	h5_file_t *f,
	const char *field_name,
	h5_float64_t *x_origin,
	h5_float64_t *y_origin,
	h5_float64_t *z_origin
	);

h5_err_t
H5Block3dSetFieldOrigin (
	h5_file_t *f,
	const char *field_name,
	const h5_float64_t x_origin,
	const h5_float64_t y_origin,
	const h5_float64_t z_origin
	);

h5_err_t
H5Block3dGetFieldSpacing (
	h5_file_t *f,
	const char *field_name,
	h5_float64_t *x_spacing,
	h5_float64_t *y_spacing,
	h5_float64_t *z_spacing
	);

h5_err_t
H5Block3dSetFieldSpacing (
	h5_file_t *f,
	const char *field_name,
	const h5_float64_t x_spacing,
	const h5_float64_t y_spacing,
	const h5_float64_t z_spacing
	);

#ifdef __cplusplus
}
#endif

#endif

