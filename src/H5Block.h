#ifndef __H5BLOCK_H
#define __H5BLOCK_H

/*! 
  Interface for block structured field data

  This  is used to store and retrieve scalar or vector valued field data.
  With \f$  \Omega\ \f$  .... 

  Basic items are blocks (\f$ \Omaga_i \f$ ), cartesian subdomains of
  \f$ \Omega \f$ . If a  \f$ \Omega_i \f$ is refined we view this
  \f$ \Omaga_i \f$  as a patch.
   
*/



/*!
  Define the field layout (FL) given the dense index space at the actual
  time step.

  \return \c H5PART_SUCCESS or error code

  \note
  This defines  \f$ \Omega \f$ and the view for HDF5	
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

/*!
  Write a 3D real valued vector field using the defined FL for this block

  \return \c H5PART_SUCCESS or error code

  \note we have to make a 1D and 2D version

  \note Q: what about a dimension independent version?
	  
*/
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
  Write a 3D real valued vector field using the defined FL for this block

  \return \c H5PART_SUCCESS or error code

  \note we have to make a 1D and 2D version

  \note Q: what about a dimension independent version?

  \note We need another name for this function!
*/
h5part_int64_t
H5BlockWrite3DVField_ (
	H5PartFile *f,		/*!< file handle */
	char *name,		/*!< name of the data set */
	int modeNumber,		/*!< the mode number of the field */
	h5part_float64_t omega,	/*!< the eigenmode of the field */
	h5part_float64_t *xval,	/*!< array of x component data */
	h5part_float64_t *yval,	/*!< array of y component data */
	h5part_float64_t *zval	/*!< array of z component data */
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

#endif // H5BLOCK_H
