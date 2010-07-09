#ifndef __H5_ATTRIBS_H
#define __H5_ATTRIBS_H

h5_err_t
H5WriteFileAttribString (
	h5_file_t *const f,	/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name of attribute to create */
	const char *value	/*!< [in] Value of attribute */ 
	);

h5_err_t
H5WriteStepAttribString (
	h5_file_t *const f,	/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name of attribute to create */
	const char *value	/*!< [in] Value of attribute */ 
	);

h5_err_t
H5WriteFileAttribFloat32 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	const h5_float32_t *values,	/*!< [in] Values of attribute */
	const h5_size_t nelems		/*!< [in] Number of values */
	);

h5_err_t
H5WriteStepAttribFloat32 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	const h5_float32_t *values,	/*!< [in] Values of attribute */
	const h5_size_t nelems		/*!< [in] Number of values */
	);

h5_err_t
H5WriteFileAttribFloat64 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	const h5_float64_t *values,	/*!< [in] Values of attribute */
	const h5_size_t nelems		/*!< [in] Number of values */
	);

h5_err_t
H5WriteStepAttribFloat64 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	const h5_float64_t *values,	/*!< [in] Values of attribute */
	const h5_size_t nelems		/*!< [in] Number of values */
	);

h5_err_t
H5WriteFileAttribInt32 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	const h5_int32_t *values,	/*!< [in] Values of attribute */
	const h5_size_t nelems		/*!< [in] Number of values */
	);

h5_err_t
H5WriteStepAttribInt32 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	const h5_int32_t *values,	/*!< [in] Values of attribute */
	const h5_size_t nelems		/*!< [in] Number of values */
	);

h5_err_t
H5WriteFileAttribInt64 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	const h5_int64_t *values,	/*!< [in] Values of attribute */
	const h5_size_t nelems		/*!< [in] Number of values */
	);

h5_err_t
H5WriteStepAttribInt64 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	const h5_int64_t *values,	/*!< [in] Values of attribute */
	const h5_size_t nelems		/*!< [in] Number of values */
	);

h5_err_t
H5ReadFileAttribString (
	h5_file_t *const f,	/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name of attribute to create */
	char *buffer		/*!< [out] Value of attribute */ 
	);

h5_err_t
H5ReadStepAttribString (
	h5_file_t *const f,	/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name of attribute to create */
	char *buffer		/*!< [out] Value of attribute */ 
	);

h5_err_t
H5ReadFileAttribInt32 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	h5_int32_t *buffer		/*!< [out] Values of attribute */
	);

h5_err_t
H5ReadStepAttribInt32 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	h5_int32_t *buffer		/*!< [out] Values of attribute */
	);

h5_err_t
H5ReadFileAttribInt64 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	h5_int64_t *buffer		/*!< [out] Values of attribute */
	);

h5_err_t
H5ReadStepAttribInt64 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	h5_int64_t *buffer		/*!< [out] Values of attribute */
	);

h5_err_t
H5ReadFileAttribFloat32 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	h5_float32_t *buffer		/*!< [out] Values of attribute */
	);

h5_err_t
H5ReadStepAttribFloat32 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	h5_float32_t *buffer		/*!< [out] Values of attribute */
	);

h5_err_t
H5ReadFileAttribFloat64 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	h5_float64_t *buffer		/*!< [out] Values of attribute */
	);

h5_err_t
H5ReadStepAttribFloat64 (
	h5_file_t *const f,		/*!< [in] Handle to open file */
	const char *name,		/*!< [in] Name of attribute to create */
	h5_float64_t *buffer		/*!< [out] Values of attribute */
	);

h5_int64_t
H5GetNumFileAttribs (
	h5_file_t *const f	/*!< [in] Handle to open file */
	);

h5_int64_t
H5GetNumStepAttribs (
	h5_file_t *const f 	/*!< [in] Handle to open file */
	);

h5_int64_t
H5GetFileAttribInfo (
	h5_file_t *const f,		/*!< [in]  Handle to open file */
	const h5_size_t attrib_idx,	/*!< [in]  Index of attribute to get
					           infos about */
	char *attrib_name,		/*!< [out] Name of attribute */
	const h5_size_t len_of_attrib_name,
					/*!< [in]  length of buffer \c name */
	h5_int64_t *attrib_type,	/*!< [out] Type of value. */
	h5_size_t *attrib_nelem         /*!< [out] Number of elements */
	);

h5_int64_t
H5GetStepAttribInfo (
	h5_file_t *const f,		/*!< [in]  Handle to open file */
	const h5_size_t attrib_idx,	/*!< [in]  Index of attribute to
					           get infos about */
	char *attrib_name,		/*!< [out] Name of attribute */
	const h5_size_t len_of_attrib_name,
					/*!< [in]  length of buffer \c name */
	h5_int64_t *attrib_type,	/*!< [out] Type of value. */
	h5_size_t *attrib_nelem         /*!< [out] Number of elements */
	);


#endif
