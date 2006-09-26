#ifndef __H5BLOCK_H
#define __H5BLOCK_H

#ifdef __cplusplus
extern "C" {
#endif

/*! 
  Interface for block structured field data

*/

h5part_int64_t
H5BlockDefine3DFieldLayout (
	H5PartFile *f,			/*!< file handle */
	const h5part_int64_t i_start,	/*!< start index of i */
	const h5part_int64_t i_end,	/*!< end index of i */
	const h5part_int64_t j_start,	/*!< start index of j */
	const h5part_int64_t j_end,	/*!< end index of j */
	const h5part_int64_t k_start,	/*!< start index of k */
	const h5part_int64_t k_end	/*!< end index of k */
	);

h5part_int64_t
H5Block3dGetPartitionOfProc (
	H5PartFile *f,			/*!< file handle */
	const h5part_int64_t proc,
	h5part_int64_t *i_start,	/*!< start index of i */
	h5part_int64_t *i_end,		/*!< end index of i */
	h5part_int64_t *j_start,	/*!< start index of j */
	h5part_int64_t *j_end,		/*!< end index of j */
	h5part_int64_t *k_start,	/*!< start index of k */
	h5part_int64_t *k_end		/*!< end index of k */
	);

h5part_int64_t
H5Block3dGetReducedPartitionOfProc (
	H5PartFile *f,
	h5part_int64_t proc,
	h5part_int64_t *i_start, 
	h5part_int64_t *i_end,
	h5part_int64_t *j_start,
	h5part_int64_t *j_end,
	h5part_int64_t *k_start,
	h5part_int64_t *k_end
	);

h5part_int64_t
H5Block3dGetProcOf (
	H5PartFile *f,
	h5part_int64_t i,
	h5part_int64_t j,
	h5part_int64_t k
	);

h5part_int64_t
H5Block3dWriteScalarField (
	H5PartFile *f,
	const char *name,
	const h5part_float64_t *data
	);

h5part_int64_t
H5Block3dReadScalarField (
	H5PartFile *f,
	const char *name,
	h5part_float64_t *data
	);

h5part_int64_t
H5BlockGetNumFields (
	H5PartFile *f
	);

h5part_int64_t
H5BlockGetFieldInfo (
	H5PartFile *f,
	const h5part_int64_t idx,
	char *name,
	const h5part_int64_t len_name,
	h5part_int64_t *grid_rank,
	h5part_int64_t *grid_dims,
	h5part_int64_t *field_dims
	);

h5part_int64_t
H5BlockGetFieldInfoByName (
	H5PartFile *f,
	const char *field_name,
	h5part_int64_t *grid_rank,
	h5part_int64_t *grid_dims,
	h5part_int64_t *field_dims
	);

h5part_int64_t
H5Block3dGetFieldOrigin (
	H5PartFile *f,
	const char *field_name,
	h5part_float64_t *x_origin,
	h5part_float64_t *y_origin,
	h5part_float64_t *z_origin
	);

h5part_int64_t
H5Block3dSetFieldOrigin (
	H5PartFile *f,
	const char *field_name,
	const h5part_float64_t x_origin,
	const h5part_float64_t y_origin,
	const h5part_float64_t z_origin
	);

h5part_int64_t
H5Block3dGetFieldSpacing (
	H5PartFile *f,
	const char *field_name,
	h5part_float64_t *x_spacing,
	h5part_float64_t *y_spacing,
	h5part_float64_t *z_spacing
	);

h5part_int64_t
H5Block3dSetFieldSpacing (
	H5PartFile *f,
	const char *field_name,
	const h5part_float64_t x_spacing,
	const h5part_float64_t y_spacing,
	const h5part_float64_t z_spacing
	);



h5part_int64_t
H5Block3dWrite3dVectorField (
	H5PartFile *f,			/*!< file handle */
	const char *name,		/*!< name of the data set */
	const h5part_float64_t *xval,	/*!< array of x component data */
	const h5part_float64_t *yval,	/*!< array of y component data */
	const h5part_float64_t *zval	/*!< array of z component data */
	);	

h5part_int64_t
H5Block3dRead3dVectorField (
	H5PartFile *f,			/*!< file handle */
	const char *name,		/*!< name of the data set */
	h5part_float64_t *xval,		/*!< array of x component data */
	h5part_float64_t *yval,		/*!< array of y component data */
	h5part_float64_t *zval		/*!< array of z component data */
	);

h5part_int64_t
H5BlockWriteFieldAttrib (
	H5PartFile *f,
	const char *field_name,
	const char *attrib_name,
	const h5part_int64_t attrib_type,
	const void *attrib_value,
	const h5part_int64_t attrib_nelem
	);

h5part_int64_t
H5BlockWriteFieldAttribString (
	H5PartFile *f,
	const char *field_name,
	const char *attrib_name,
	const char *attrib_value
	);

h5part_int64_t
H5BlockGetNumFieldAttribs (
	H5PartFile *f,
	const char *field_name
	);

h5part_int64_t
H5BlockGetFieldAttribInfo (
	H5PartFile *f,
	const char *field_name,
	const h5part_int64_t attrib_idx,
	char *attrib_name,
	const h5part_int64_t len_of_attrib_name,
	h5part_int64_t *attrib_type,
	h5part_int64_t *attrib_nelem
	);

h5part_int64_t
H5BlockReadFieldAttrib (
	H5PartFile *f,
	const char *field_name,
	const char *attrib_name,
	void *attrib_value
	);

h5part_int64_t
H5BlockHasFieldData (
	H5PartFile *f
	);

/*!
  Checks if the block of the actual time step is refined i.e. if we
  have a patch

  \return \c 1 if we have a patch, otherwise \c 0
*/
h5part_int64_t
H5BlockIsPatch (
	H5PartFile *f,		/*!< file handle */
	const char *name	/*!< name of the data set */
	);

/*!
  return the maximum level refinement of refinement at the current
  time step

  \return number or error code
*/
h5part_int64_t
H5BlockGetMaxRefinementLevel (
	H5PartFile *f,		/*!< file handle */
	const char *name	/*!< name of the data set */
	);

/*!
  Write the mesh spacing for the active (current) block 

  \return \c H5PART_SUCCESS or error code
*/
h5part_int64_t
H5BlockSetMeshSpacing (
	H5PartFile *f,		/*!< file handle */
	char *name,		/*!< name of the data set */
	h5part_float64_t dx,	/*!< mesh spacing in x */
	h5part_float64_t dy,	/*!< mesh spacing in y */
	h5part_float64_t dz	/*!< mesh spacing in z */
	);

/*==========================================================================*/
/*
  The following reflects the fact that we have fields which are decomposed
  into modes.

  Augment the field name with the mode number and use this name to store
  the field.  For the mode freqency use the same procedure.
*/

/*!	
  Read the number of stored modes at actual timestep 

  \return \c H5PART_SUCCESS or error code
*/
h5part_int64_t
H5BlockGetNumberOfModes (
	H5PartFile *f,		/*!< file handle */
	char *name		/*!< name of the data set */
	); 

/*!
  Set the number of stored modes at actual timestep 

  \return \c H5PART_SUCCESS or error code
*/
h5part_int64_t
H5BlockSetNumberOfModes (
	H5PartFile *f,		/*!< file handle */
	char *name,		/*!< name of the data set */
	int modes		/*!< the number of modes */
	); 

/*!
  Write the quality factor for a given mode

  \return \c H5PART_SUCCESS or error code
*/
h5part_int64_t
H5BlockSetQFactor (
	H5PartFile *f,		/*!< file handle */
	char *name,		/*!< name of the data set */
	int mode,		/*!< the mode number of the field */
	h5part_float64_t Q	/*!< the quality factor */
	);

/*!
  Write the S-parameters for a given mode

  \return \c H5PART_SUCCESS or error code
*/	
h5part_int64_t
H5BlockSetSParams (
	H5PartFile *f,		/*!< file handle */
	char *name,		/*!< name of the data set */
	int mode,		/*!< the mode number of the field */
	h5part_float64_t **s	/*!< S-parameters */
	);

/*!
  Read the quality factor for a given mode

  \return \c H5PART_SUCCESS or error code
*/
h5part_int64_t
H5BlockGetQFactor (
	H5PartFile *f,		/*!< file handle */
	char *name,		/*!< name of the data set */
	int mode,		/*!< the mode number of the field */
	h5part_float64_t *Q	/*!< the quality factor */
	);

/*!
  Read the S-parameters for a given mode

  \return \c H5PART_SUCCESS or error code
*/	
h5part_int64_t
H5BlockGetSParams ( 
	H5PartFile *f,		/*!< file handle */
	char *name,		/*!< name of the data set */
	int mode,		/*!< the mode number of the field */
	h5part_float64_t **s	/*!< S-parameters */
	);


#ifdef __cplusplus
}
#endif

#endif
