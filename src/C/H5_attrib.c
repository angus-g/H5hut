/********************** attribute API ****************************************/

/*!
  \ingroup h5part_c_api
  \defgroup h5part_c_api_attrib	Reading and Writing Attributes
*/

/*!
  \ingroup h5part_c_api_attrib

  Writes a string attribute bound to a file.

  This function creates a new attribute \c name with the string \c value as
  content. The attribute is bound to the file associated with the file handle 
  \c f.

  If the attribute already exists an error will be returned. There
  is currently no way to change the content of an existing attribute.

  \return	\c H5_SUCCESS or error code   
*/
h5_int64_t
H5PartWriteFileAttribString (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *attrib_name,/*!< [in] Name of attribute to create */
	const char *attrib_value/*!< [in] Value of attribute */ 
	) {

	SET_FNAME ( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_write_attrib (
		f,
		f->root_gid,
		attrib_name,
		H5T_NATIVE_CHAR,
		attrib_value,
		strlen ( attrib_value ) + 1 );
}

/*!
  \ingroup h5part_c_api_attrib

  Writes a string attribute bound to the current time-step.

  This function creates a new attribute \c name with the string \c value as
  content. The attribute is bound to the current time step in the file given
  by the file handle \c f.

  If the attribute already exists an error will be returned. There
  is currently no way to change the content of an existing attribute.

  \return	\c H5_SUCCESS or error code   
*/

h5_int64_t
H5PartWriteStepAttribString (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *attrib_name,/*!< [in] Name of attribute to create */
	const char *attrib_value/*!< [in] Value of attribute */ 
	) {

	SET_FNAME ( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_write_attrib (
		f, 
		f->step_gid,
		attrib_name,
		H5T_NATIVE_CHAR,
		attrib_value,
		strlen ( attrib_value ) + 1 );
}

/*!
  \ingroup h5part_c_api_attrib

  Writes a attribute bound to the current time-step.

  This function creates a new attribute \c name with the string \c value as
  content. The attribute is bound to the current time step in the file given
  by the file handle \c f.

  The value of the attribute is given the parameter \c type, which must be one
  of \c H5T_NATIVE_DOUBLE, \c H5T_NATIVE_INT64 of \c H5T_NATIVE_CHAR, the array
  \c value and the number of elements \c nelem in the array.

  If the attribute already exists an error will be returned. There
  is currently no way to change the content of an existing attribute.

  \return	\c H5_SUCCESS or error code   
*/

h5_int64_t
H5PartWriteStepAttrib (
	h5_file_t *f,			/*!< [in] Handle to open file */
	const char *attrib_name,	/*!< [in] Name of attribute */
	const h5_int64_t attrib_type,/*!< [in] Type of value. */
	const void *attrib_value,	/*!< [in] Value of attribute */ 
	const h5_int64_t attrib_nelem/*!< [in] Number of elements */
	){

	SET_FNAME ( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_write_attrib (
		f,
		f->step_gid,
		attrib_name,
		(hid_t)attrib_type,
		attrib_value,
		attrib_nelem );
}

/*!
  \ingroup h5part_c_api_attrib

  Writes a attribute bound to a file.

  This function creates a new attribute \c name with the string \c value as
  content. The attribute is bound to the file file given by the file handle
  \c f.

  The value of the attribute is given the parameter \c type, which must be one
  of H5T_NATIVE_DOUBLE, H5T_NATIVE_INT64 of H5T_NATIVE_CHAR, the array \c value
  and the number of elements \c nelem in the array.

  If the attribute already exists an error will be returned. There
  is currently no way to change the content of an existing attribute.

  \return	\c H5_SUCCESS or error code   
*/

h5_int64_t
H5PartWriteFileAttrib (
	h5_file_t *f,			/*!< [in] Handle to open file */
	const char *attrib_name,	/*!< [in] Name of attribute */
	const h5_int64_t attrib_type,/*!< [in] Type of value. */
	const void *attrib_value,	/*!< [in] Value of attribute */ 
	const h5_int64_t attrib_nelem/*!< [in] Number of elements */
	) {

	SET_FNAME ( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_write_attrib (
		f,
		f->root_gid,
		attrib_name,
		(hid_t)attrib_type,
		attrib_value,
		attrib_nelem );
}

/*!
  \ingroup h5part_c_api_attrib

  Gets the number of attributes bound to the current step.

  \return	Number of attributes bound to current time step or error code.
*/
h5_int64_t
H5PartGetNumStepAttribs (
	h5_file_t *f			/*!< [in] Handle to open file */
	) {

	SET_FNAME ( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_get_num_attribs ( f, f->step_gid );
}

/*!
  \ingroup h5part_c_api_attrib

  Gets the number of attributes bound to the file.

  \return	Number of attributes bound to file \c f or error code.
*/
h5_int64_t
H5PartGetNumFileAttribs (
	h5_file_t *f			/*!< [in] Handle to open file */
	) {

	SET_FNAME ( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_get_num_attribs ( f, f->root_gid );
}

/*!
  \ingroup h5part_c_api_attrib

  Gets the name, type and number of elements of the step attribute
  specified by its index.

  This function can be used to retrieve all attributes bound to the
  current time-step by looping from \c 0 to the number of attribute
  minus one.  The number of attributes bound to the current
  time-step can be queried by calling the function
  \c H5PartGetNumStepAttribs().

  \return	\c H5_SUCCESS or error code 
*/
h5_int64_t
H5PartGetStepAttribInfo (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	const h5_int64_t attrib_idx,/*!< [in]  Index of attribute to
					           get infos about */
	char *attrib_name,		/*!< [out] Name of attribute */
	const h5_int64_t len_of_attrib_name,
					/*!< [in]  length of buffer \c name */
	h5_int64_t *attrib_type,	/*!< [out] Type of value. */
	h5_int64_t *attrib_nelem	/*!< [out] Number of elements */
	) {
	
	SET_FNAME ( f, __func__ );

   	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno( f );

	return h5_get_attrib_info (
		f,
		f->step_gid,
		attrib_idx,
		attrib_name,
		len_of_attrib_name,
		attrib_type,
		attrib_nelem );
}

/*!
  \ingroup h5part_c_api_attrib

  Gets the name, type and number of elements of the file attribute
  specified by its index.

  This function can be used to retrieve all attributes bound to the
  file \c f by looping from \c 0 to the number of attribute minus
  one.  The number of attributes bound to file \c f can be queried
  by calling the function \c H5PartGetNumFileAttribs().

  \return	\c H5_SUCCESS or error code 
*/

h5_int64_t
H5PartGetFileAttribInfo (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	const h5_int64_t attrib_idx,/*!< [in]  Index of attribute to get
					           infos about */
	char *attrib_name,		/*!< [out] Name of attribute */
	const h5_int64_t len_of_attrib_name,
					/*!< [in]  length of buffer \c name */
	h5_int64_t *attrib_type,	/*!< [out] Type of value. */
	h5_int64_t *attrib_nelem	/*!< [out] Number of elements */
	) {

	SET_FNAME ( f, __func__ );

	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno ( f );

	return h5_get_attrib_info (
		f,
		f->root_gid,
		attrib_idx,
		attrib_name,
		len_of_attrib_name,
		attrib_type,
		attrib_nelem );
}

/*!
  \ingroup h5part_c_api_attrib

  Reads an attribute bound to current time-step.

  \return \c H5_SUCCESS or error code 
*/
h5_int64_t
H5PartReadStepAttrib (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	const char *attrib_name,	/*!< [in] Name of attribute to read */
	void *attrib_value		/*!< [out] Value of attribute */
	) {

	SET_FNAME ( f, __func__ );

	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno ( f );

	return h5_read_attrib ( f, f->step_gid, attrib_name, attrib_value );
}

/*!
  \ingroup h5part_c_api_attrib

  Reads an attribute bound to file \c f.

  \return \c H5_SUCCESS or error code 
*/
h5_int64_t
H5PartReadFileAttrib ( 
	h5_file_t *f,
	const char *attrib_name,
	void *attrib_value
	) {

	SET_FNAME ( f, __func__ );

	if ( h5_check_filehandle ( f ) != H5_SUCCESS )
		return h5_get_errno ( f );

	return h5_read_attrib ( f, f->root_gid, attrib_name, attrib_value );
}


