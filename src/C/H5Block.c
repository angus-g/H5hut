/*!
  \defgroup h5block_c_api H5Block C API
*/

/*!
  \internal

  \defgroup h5block_kernel H5Block Kernel
*/

/*!
  \internal

  \defgroup h5block_private H5Block Private
*/

/*!
  \ingroup h5block_c_api
  \defgroup h5block_model	Setting up the Data Model
*/  
/*!
  \ingroup h5block_c_api
  \defgroup h5block_data	Reading and Writing Datasets
*/  
/*!
  \ingroup h5block_c_api
  \defgroup h5block_attrib	Reading and Writing Attributes
*/

/*!
  \note
  Different field sizes are allowed in the same time-step.

  \note
  The same layout can be used, if the size of the field matches the
  size of the layout.  If the size of the layout doesn't match the
  size of the field, an error will be indicated. 
 
  \note
  In write mode partitions are shrinked to make them non-overlaping. This 
  process may shrink the partitions more than required.

  \note
  In read-mode partitions may not cross boundaries. This means, if the grid
  size is (X, Y, Z), all partitions must fit into this grid.


  \todo
  check whether layout is reasonable

  API function names
*/


#include <stdlib.h>
#include <string.h>

#include "H5hut.h"
#include "h5core/h5_core.h"

/********************** defining the layout **********************************/

/*!
  \ingroup h5block_model

  Tests whether a view has been set, either directly with
  \ref H5Block3dSetView or indirectly with \ref H5Block3dSetGrid.

  \return 0 on false, 1 on true
*/
h5_int64_t
H5Block3dHasView (
	h5_file_t *const f	/*!< IN: File handle */
	) {
	H5_API_ENTER;
	h5_int64_t retval = h5b_3d_has_view(f);
	H5_API_RETURN (retval);
}

/*!
  \ingroup h5block_model

  Defines the partition of the field that this processor owns, using
  Fortran ordering: the fastest moving index is \c i.

  This routine uses an MPI_Allgather, so at large concurrency it should
  be called as infrequently as possible. For instance, if several timesteps
  use the same field dimensions, set the layout only once before the
  first timestep.

  \return \c H5_SUCCESS on success
*/
h5_err_t
H5Block3dSetView (
	h5_file_t *const f,		/*!< IN: File handle		*/
	const h5_int64_t i_start,	/*!< IN: start index of \c i	*/ 
	const h5_int64_t i_end,         /*!< IN: end index of \c i	*/  
	const h5_int64_t j_start,	/*!< IN: start index of \c j	*/ 
	const h5_int64_t j_end,	        /*!< IN: end index of \c j	*/ 
	const h5_int64_t k_start,	/*!< IN: start index of \c k	*/ 
	const h5_int64_t k_end	        /*!< IN: end index of \c k	*/
	) {
	H5_API_ENTER;
	h5_err_t h5err = h5b_3d_set_view(f, i_start, i_end, j_start, j_end, k_start, k_end);
	H5_API_RETURN (h5err);
}

/*!
  \ingroup h5block_model

  Return the view of this processor.

  \return \c H5_SUCCESS on success
*/
h5_err_t
H5Block3dGetView (
	h5_file_t *const f,	/*!< IN: File handle */
	h5_size_t *i_start,	/*!< OUT: start index of \c i	*/ 
	h5_size_t *i_end,	/*!< OUT: end index of \c i	*/  
	h5_size_t *j_start,	/*!< OUT: start index of \c j	*/ 
	h5_size_t *j_end,	/*!< OUT: end index of \c j	*/ 
	h5_size_t *k_start,	/*!< OUT: start index of \c k	*/ 
	h5_size_t *k_end	/*!< OUT: end index of \c k	*/ 
	) {
	H5_API_ENTER;
	h5_err_t h5err = h5b_3d_get_view(f, i_start, i_end, j_start, j_end, k_start, k_end);
	H5_API_RETURN (h5err);
}

/*!
  \ingroup h5block_model

  Return the reduced (ghost-zone free) view of this processor.

  \return \c H5_SUCCESS on success
*/
h5_err_t
H5Block3dGetReducedView (
	h5_file_t *const f,		/*!< IN: File handle */
	h5_size_t *const i_start,	/*!< OUT: start index of \c i */ 
	h5_size_t *const i_end,		/*!< OUT: end index of \c i */  
	h5_size_t *const j_start,	/*!< OUT: start index of \c j */ 
	h5_size_t *const j_end,		/*!< OUT: end index of \c j */ 
	h5_size_t *const k_start,	/*!< OUT: start index of \c j */ 
	h5_size_t *const k_end		/*!< OUT: end index of \c j */ 
	) {
	H5_API_ENTER;
	h5_err_t h5err = h5b_3d_get_reduced_view(f, i_start, i_end, j_start, j_end, k_start, k_end);
	H5_API_RETURN (h5err);
}

/*!
  \ingroup h5block_model

  Define the chunk dimensions and enable chunking in the underlying
  HDF5 dataset.

  \return \c H5_SUCCESS on success
*/
h5_err_t
H5Block3dSetChunk (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_size_t i,		/*!< IN: size of \c i */ 
	const h5_size_t j,		/*!< IN: size of \c j */  
	const h5_size_t k		/*!< IN: size of \c k */ 
	) {
	H5_API_ENTER;
	h5_err_t h5err = h5b_3d_set_chunk(f, i, j, k);
	H5_API_RETURN (h5err);
}

/*!
  \ingroup h5block_model

  Lookup the chunk dimensions of the underlying HDF5 dataset.

  \return \c H5_SUCCESS on success
*/
h5_err_t
H5Block3dGetChunk (
	h5_file_t *const f,		/*!< IN: File handle */
	const char *field_name, 	/*!< IN: name of dataset */
	h5_size_t *const i,		/*!< OUT: size of \c i */ 
	h5_size_t *const j,		/*!< OUT: size of \c j */  
	h5_size_t *const k		/*!< OUT: size of \c k */ 
	) {
	H5_API_ENTER;
	h5_err_t h5err = h5b_3d_get_chunk(f, field_name, i, j, k);
	H5_API_RETURN (h5err);
}

#ifdef PARALLEL_IO
/*!
  \ingroup h5block_model

  Define an underlying 3D Cartesian grid on the processors with dimensions
  (\c i,\c j,\c k). You can look up a processor's index into the grid
  using \ref H5Block3dGetGridCoords.

  This function can be used in conjunction with \ref H5Block3dSetDims
  to setup the view for a regular grid.

  The product of the dimensions must equal the size of the MPI communicator.

  \return \c H5_SUCCESS on success
*/
h5_err_t
H5Block3dSetGrid (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_size_t i,		/*!< IN: dimension in \c i */ 
	const h5_size_t j,		/*!< IN: dimension in \c j */  
	const h5_size_t k		/*!< IN: dimension in \c k */ 
	) {
	H5_API_ENTER;
	h5_err_t h5err = h5b_3d_set_grid(f, i, j, k);
	H5_API_RETURN (h5err);
}

/*!
  \ingroup h5block_model

  Look up the index (\c i, \c j, \c k) in the grid belonging to MPI processor
  \c proc.

  \return \c H5_SUCCESS on success
*/
h5_err_t
H5Block3dGetGridCoords (
	h5_file_t *const f,		/*!< IN: File handle */
	const int proc,			/*!< IN: MPI processor */
	h5_int64_t *i,			/*!< OUT: index in \c i */ 
	h5_int64_t *j,			/*!< OUT: index in \c j */  
	h5_int64_t *k			/*!< OUT: index in \c k */ 
	) {
	H5_API_ENTER;
	h5_err_t h5err = h5b_3d_get_grid_coords(f, proc, i, j, k);
	H5_API_RETURN (h5err);
}

/*!
  \ingroup h5block_model

  Set the dimensions of each processor's block when the field is a regular
  grid.
  
  A grid must be already set with \ref H5Block3dSetGrid, and all processors
  must specify the same dimensions.

  \return \c H5_SUCCESS on success
*/
h5_err_t
H5Block3dSetDims (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_size_t i,		/*!< IN: dimension in \c i */ 
	const h5_size_t j,		/*!< IN: dimension in \c j */  
	const h5_size_t k		/*!< IN: dimension in \c k */ 
	) {
	H5_API_ENTER;
	h5_err_t h5err = h5b_3d_set_dims(f, i, j, k);
	H5_API_RETURN (h5err);
}
#endif

/*!
  \ingroup h5block_model

  Sets the additional cells (\c i, \c j, \c k) in each direction to use as
  the `halo` region (or `ghost zone`) that overlaps between neighboring
  processors on the grid.

  A grid with dimensions must already be set with \ref H5Block3dSetGrid and
  \ref H5Block3dSetDims, and all processors must specify the same halo radii.

  \return \c H5_SUCCESS on success
*/
h5_err_t
H5Block3dSetHalo (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_size_t i,		/*!< IN: radius in \c i */ 
	const h5_size_t j,		/*!< IN: radius in \c j */  
	const h5_size_t k		/*!< IN: radius in \c k */ 
	) {
	H5_API_ENTER;
	h5_err_t h5err = h5b_3d_set_halo(f, i, j, k);
	H5_API_RETURN (h5err);
}

/*!
  \ingroup h5block_model

  Query number of fields in current time step.

  \return \c H5_SUCCESS or error code
*/
h5_size_t
H5BlockGetNumFields (
	h5_file_t *const f			/*!< IN: file handle */
	) {
	H5_API_ENTER;
	h5_err_t h5err = h5b_get_num_fields(f);
	H5_API_RETURN (h5err);
}


/*!
  \ingroup h5block_model

  Get the name, rank and dimensions of the field specified by the
  index \c idx.

  \c elem_rank reports the rank of the elements in the field
  (e.g. scalar or vector).

  This function can be used to retrieve all fields bound to the
  current time-step by looping from \c 0 to the number of fields
  minus one.  The number of fields bound to the current time-step
  can be queried by calling the function \ref H5BlockGetNumFields.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockGetFieldInfo (
	h5_file_t *const f,			/*!< IN: file handle */
	const h5_size_t idx,			/*!< IN: index of field */
	char *name,				/*!< OUT: field name */
	const h5_size_t len_name,		/*!< IN: buffer size */
	h5_size_t *field_rank,			/*!< OUT: field rank */
	h5_size_t *field_dims,			/*!< OUT: field dimensions */
	h5_size_t *elem_rank,			/*!< OUT: element rank */
	h5_int64_t *type			/*!< OUT: datatype */
	) {
	H5_API_ENTER;
	h5_err_t h5err = h5b_get_field_info (f,
		idx, name, len_name, field_rank, field_dims, elem_rank, type);
	H5_API_RETURN (h5err);
}

/*!
  \ingroup h5block_model

  Get the rank and dimensions of the field specified by its name.
  See \ref H5BlockGetFieldInfo.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockGetFieldInfoByName (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: field name */
	h5_size_t *field_rank,			/*!< OUT: field rank */
	h5_size_t *field_dims,			/*!< OUT: field dimensions */
	h5_size_t *elem_rank,			/*!< OUT: element rank */
	h5_int64_t *type		/*!< OUT: datatype */
	) {
	H5_API_ENTER;
	h5_err_t h5err = h5b_get_field_info_by_name (f,
		name, field_rank, field_dims, elem_rank, type );
	H5_API_RETURN (h5err);
}

/********************** reading and writing attribute ************************/

/*!
  \ingroup h5block_attrib

  Write the string \c value as attribute \c attrib_name of field
  \c field_name.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockWriteFieldAttribString (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const char *value			/*!< IN: attribute value */
	) {
	H5_API_ENTER;
	h5_err_t h5err = h5_write_field_attrib (
		f,
		field_name,
		attrib_name,
		H5T_NATIVE_CHAR,
		value,
		strlen(value) + 1 );
	H5_API_RETURN (h5err);
}

/*!
  \ingroup h5block_attrib

  Read the string value from attribute \c attrib_name of field
  \c field_name into a \c buffer.

  \return \c H5_SUCCESS or error code
*/
h5_err_t
H5BlockReadFieldAttribString (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	char *buffer			        /*!< OUT: attribute value */
	) {
	H5_API_ENTER;
	h5_err_t h5err = h5_read_field_attrib (
		f,
		field_name,
		attrib_name,
		H5_STRING_T,
		(void*)buffer);
	H5_API_RETURN (h5err);
}

/*!
  \ingroup h5block_attrib

  Query the number of attributes of field \c field_name.

  \return number of attributes or error code
*/
h5_ssize_t
H5BlockGetNumFieldAttribs (
	h5_file_t *const f,			/*<! IN: file handle */
	const char *field_name			/*<! IN: field name */
	) {
	H5_API_ENTER;
	h5_ssize_t nattribs = h5b_get_num_field_attribs(f, field_name);
	H5_API_RETURN (nattribs);
}

/*!
  \ingroup h5block_attrib

  Gets the name, type and number of elements of the field attribute
  specified by its index.

  This function can be used to retrieve all attributes bound to the
  specified field by looping from \c 0 to the number of attribute
  minus one.  The number of attributes bound to the
  field can be queried by calling \ref H5BlockGetNumFieldAttribs.

  \return	\c H5_SUCCESS or error code 
*/
h5_int64_t
H5BlockGetFieldAttribInfo (
	h5_file_t *const f,		/*<! IN: Handle to open file */
	const char *field_name,		/*<! IN: field name */
	const h5_size_t attrib_idx,	/*<! IN: Index of attribute to
					           get infos about */
	char *attrib_name,		/*<! OUT: Name of attribute */
	const h5_size_t len_of_attrib_name,
					/*<! IN: length of buffer \c name */
	h5_int64_t *attrib_type,	/*<! OUT: Type of value. */
	h5_size_t *attrib_nelem         /*<! OUT: Number of elements */
	) {
	H5_API_ENTER;
	h5_err_t h5err = h5b_get_field_attrib_info (
		f,
		field_name,
		attrib_idx,
		attrib_name,
		len_of_attrib_name,
		attrib_type,
		attrib_nelem );
	H5_API_RETURN (h5err);
}

