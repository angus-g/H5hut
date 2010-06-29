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
	const h5_int64_t attrib_idx,	/*!< [in]  Index of attribute to get
					           infos about */
	char *attrib_name,		/*!< [out] Name of attribute */
	const h5_int64_t len_of_attrib_name,
					/*!< [in]  length of buffer \c name */
	h5_int64_t *attrib_type,	/*!< [out] Type of value. */
	h5_int64_t *attrib_nelem	/*!< [out] Number of elements */
	);

h5_int64_t
H5GetStepAttribInfo (
	h5_file_t *const f,		/*!< [in]  Handle to open file */
	const h5_int64_t attrib_idx,	/*!< [in]  Index of attribute to
					           get infos about */
	char *attrib_name,		/*!< [out] Name of attribute */
	const h5_int64_t len_of_attrib_name,
					/*!< [in]  length of buffer \c name */
	h5_int64_t *attrib_type,	/*!< [out] Type of value. */
	h5_int64_t *attrib_nelem	/*!< [out] Number of elements */
	);

