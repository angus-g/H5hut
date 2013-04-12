/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5BLOCK_MODEL
#define __H5BLOCK_MODEL

#include "h5core/h5_types.h"
#include "h5core/h5.h"
#include "h5core/h5_debug.h"
#include "h5core/h5b_model.h"

#ifdef __cplusplus
extern "C" {
#endif

/********************** defining the layout **********************************/

/*!
  \ingroup h5block_model
  \anchor H5BlockHasFieldData

  Checks whether the current time-step has field data or not.

  \return \c H5_SUCCESS if field data is available otherwise \c
  H5_NOK.
*/
static inline h5_err_t
H5BlockHasFieldData (
	const h5_file_t f	///< [in] file handle
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, ", 
                      (h5_file_p)f);
        H5_API_RETURN (h5b_has_field_data (f));
}

/*!
  \ingroup h5block_model
  \anchor H5Block3dHasView

  Tests whether a view has been set, either directly with
  \ref H5Block3dSetView or indirectly with \ref H5Block3dSetGrid.

  \return \c 1 on true.
  \return \c 0 on false.
*/
static inline h5_int64_t
H5Block3dHasView (
	const h5_file_t f      	///< [in]  File handle */
	) {
	H5_API_ENTER (h5_int64_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5b_3d_has_view (f));
}

/*!
  \ingroup h5block_model
  \anchor H5Block3dSetView

  Defines the partition of the field that this processor owns, using
  Fortran ordering: the fastest moving index is \c i.

  This routine uses an MPI_Allgather, so at large concurrency it should
  be called as infrequently as possible. For instance, if several timesteps
  use the same field dimensions, set the layout only once before the
  first timestep.

  \return \c H5_SUCCESS or \c H5_FAILURE
*/
static inline h5_err_t
H5Block3dSetView (
	const h5_file_t f,		///< [in]  File handle.
	const h5_int64_t i_start,	///< [in]  start index of \c i
	const h5_int64_t i_end,         ///< [in]  end index of \c i
	const h5_int64_t j_start,	///< [in]  start index of \c j
	const h5_int64_t j_end,	        ///< [in]  end index of \c j
	const h5_int64_t k_start,	///< [in]  start index of \c k
	const h5_int64_t k_end	        ///< [in]  end index of \c k
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, "
		      "i_start=%lld, i_end=%lld, "
		      "j_start=%lld, j_end=%lld, "
		      "k_start=%lld, k_end=%lld",
		      (h5_file_p)f,
		      (long long)i_start, (long long)i_end,
		      (long long)j_start, (long long)j_end,
		      (long long)k_start, (long long)k_end);
	H5_API_RETURN (h5b_3d_set_view (f,
                                        i_start, i_end,
                                        j_start, j_end,
                                        k_start, k_end));
}

/*!
  \ingroup h5block_model
  \anchor H5Block3dGetView
  Return the view of this processor.

  \return \c H5_SUCCESS or \c H5_FAILURE
*/
static inline h5_err_t
H5Block3dGetView (
	const h5_file_t f,             ///< [in]  File handle.
	h5_size_t* i_start,     	///< [out] start index of \c i
	h5_size_t* i_end,       	///< [out] end index of \c i
	h5_size_t* j_start,     	///< [out] start index of \c j
	h5_size_t* j_end,       	///< [out] end index of \c j
	h5_size_t* k_start,     	///< [out] start index of \c k
	h5_size_t* k_end        	///< [out] end index of \c k
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, "
		      "i_start=%p, i_end=%p, "
		      "j_start=%p, j_end=%p, "
		      "k_start=%p, k_end=%p",
		      (h5_file_p)f,
		      i_start, i_end,
		      j_start, j_end,
		      k_start, k_end);
	H5_API_RETURN (h5b_3d_get_view (f, i_start, i_end, j_start, j_end, k_start, k_end));
}

/*!
  \ingroup h5block_model
  \anchor H5Block3dGetReducedView

  Return the reduced (ghost-zone free) view of this processor.

  \return \c H5_SUCCESS or \c H5_FAILURE
*/
static inline h5_err_t
H5Block3dGetReducedView (
	const h5_file_t f,		///< [in]  file handle.
	h5_size_t* const i_start,	///< [out] start index of \c i
	h5_size_t* const i_end,		///< [out] end index of \c i  
	h5_size_t* const j_start,	///< [out] start index of \c j
	h5_size_t* const j_end,		///< [out] end index of \c j
	h5_size_t* const k_start,	///< [out] start index of \c j
	h5_size_t* const k_end		///< [out] end index of \c j
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, "
		      "i_start=%p, i_end=%p, "
		      "j_start=%p, j_end=%p, "
		      "k_start=%p, k_end=%p",
		      (h5_file_p)f,
		      i_start, i_end,
		      j_start, j_end,
		      k_start, k_end);
	H5_API_RETURN (h5b_3d_get_reduced_view(f, i_start, i_end, j_start, j_end, k_start, k_end));
}

/*!
  \ingroup h5block_model
  \anchor H5Block3dSetChunkSize

  Define the chunk dimensions and enable chunking in the underlying
  HDF5 dataset.

  \return \c H5_SUCCESS or \c H5_FAILURE
*/
static inline h5_err_t
H5Block3dSetChunkSize (
	const h5_file_t f,		///< [in]  file handle.
	const h5_size_t i,		///< [in]  size of \c i
	const h5_size_t j,		///< [in]  size of \c j
	const h5_size_t k		///< [in]  size of \c k
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, i=%llu, j=%llu, k=%llu",
		      (h5_file_p)f,
		      (long long unsigned)i,
		      (long long unsigned)j,
		      (long long unsigned)k);
	H5_API_RETURN (h5b_3d_set_chunk(f, i, j, k));
}

/*!
  \ingroup h5block_model
  \anchor H5Block3dGetChunkSize

  Lookup the chunk dimensions of the underlying HDF5 dataset.

  \return \c H5_SUCCESS or \c H5_FAILURE
*/
static inline h5_err_t
H5Block3dGetChunkSize (
	const h5_file_t f,		///< [in]  file handle.
	const char* field_name, 	///< [in]  name of dataset
	h5_size_t* const i,		///< [out] size of \c i 
	h5_size_t* const j,		///< [out] size of \c j
	h5_size_t* const k		///< [out] size of \c k
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, i=%p, j=%p, k=%p",
		      (h5_file_p)f, i, j, k);
	H5_API_RETURN (h5b_3d_get_chunk(f, field_name, i, j, k));
}

#ifdef PARALLEL_IO
/*!
  \ingroup h5block_model
  \anchor H5Block3dSetGrid

  Define an underlying 3D Cartesian grid on the processors with dimensions
  (\c i,\c j,\c k). You can look up a processor's index into the grid
  using \ref H5Block3dGetGridCoords.

  This function can be used in conjunction with \ref H5Block3dSetDims
  to setup the view for a regular grid.

  The product of the dimensions must equal the size of the MPI communicator.

  \return \c H5_SUCCESS or \c H5_FAILURE
*/
static inline h5_err_t
H5Block3dSetGrid (
	const h5_file_t f,		///< [in]  file handle.
	const h5_size_t i,		///< [in]  dimension in \c i
	const h5_size_t j,		///< [in]  dimension in \c j
	const h5_size_t k		///< [in]  dimension in \c k
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, i=%llu, j=%llu, k=%llu",
		      (h5_file_p)f,
		      (long long unsigned)i,
		      (long long unsigned)j,
		      (long long unsigned)k);
	H5_API_RETURN (h5b_3d_set_grid(f, i, j, k));
}

/*!
  \ingroup h5block_model
  \anchor H5Block3dGetGridCoords

  Look up the index (\c i, \c j, \c k) in the grid belonging to MPI processor
  \c proc.

  \return \c H5_SUCCESS or \c H5_FAILURE
*/
static inline h5_err_t
H5Block3dGetGridCoords (
	const h5_file_t f,		///< [in]  file handle.
	const int proc,			///< [in]  MPI processor
	h5_int64_t* i,			///< [out] index in \c i
	h5_int64_t* j,			///< [out] index in \c j
	h5_int64_t* k			///< [out] index in \c k
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, proc=%d, i=%p, j=%p, k=%p",
		      (h5_file_p)f, proc, i, j, k);
	H5_API_RETURN (h5b_3d_get_grid_coords(f, proc, i, j, k));
}

/*!
  \ingroup h5block_model
  \anchor H5Block3dSetDims

  Set the dimensions of each processor's block when the field is a regular
  grid.
  
  A grid must be already set with \ref H5Block3dSetGrid, and all processors
  must specify the same dimensions.

  \return \c H5_SUCCESS or \c H5_FAILURE
*/
static inline h5_err_t
H5Block3dSetDims (
	const h5_file_t f,		///< [in]  file handle.
	const h5_size_t i,		///< [in]  dimension in \c i
	const h5_size_t j,		///< [in]  dimension in \c j
	const h5_size_t k		///< [in]  dimension in \c k
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, i=%llu, j=%llu, k=%llu",
		      (h5_file_p)f,
		      (long long unsigned)i,
		      (long long unsigned)j,
		      (long long unsigned)k);
	H5_API_RETURN (h5b_3d_set_dims(f, i, j, k));
}
#endif

/*!
  \ingroup h5block_model
  \anchor H5Block3dSetHalo

  Sets the additional cells (\c i, \c j, \c k) in each direction to use as
  the `halo` region (or `ghost zone`) that overlaps between neighboring
  processors on the grid.

  A grid with dimensions must already be set with \ref H5Block3dSetGrid and
  \ref H5Block3dSetDims, and all processors must specify the same halo radii.

  \return \c H5_SUCCESS or \c H5_FAILURE
*/
static inline h5_err_t
H5Block3dSetHalo (
	const h5_file_t f,		///< [in]  file handle.
	const h5_size_t i,		///< [in]  radius in \c i
	const h5_size_t j,		///< [in]  radius in \c j
	const h5_size_t k		///< [in]  radius in \c k
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, i=%llu, j=%llu, k=%llu",
		      (h5_file_p)f,
		      (long long unsigned)i,
		      (long long unsigned)j,
		      (long long unsigned)k);
	H5_API_RETURN (h5b_3d_set_halo(f, i, j, k));
}

/*!
  \ingroup h5block_model
  \anchor H5BlockGetNumFields

  Query number of fields in current time step.

  \return \c number of fields
  \return H5_FAILURE on error
*/
static inline h5_ssize_t
H5BlockGetNumFields (
	const h5_file_t f		///< [in]  file handle.
	) {
	H5_API_ENTER (h5_ssize_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5b_get_num_fields(f));
}


/*!
  \ingroup h5block_model
  \anchor H5BlockGetFieldInfo

  Get the name, rank and dimensions of the field specified by the
  index \c idx.

  \c elem_rank reports the rank of the elements in the field
  (e.g. scalar or vector).

  This function can be used to retrieve all fields bound to the
  current time-step by looping from \c 0 to the number of fields
  minus one.  The number of fields bound to the current time-step
  can be queried by calling the function \ref H5BlockGetNumFields.

  \return \c H5_SUCCESS or \c H5_FAILURE
*/
static inline h5_err_t
H5BlockGetFieldInfo (
	const h5_file_t f,		///< [in]  file handle.
	const h5_size_t idx,		///< [in]  index of field.
	char* name,			///< [out] field name.
	const h5_size_t len_name,	///< [in]  buffer size.
	h5_size_t* field_rank,		///< [out] field rank.
	h5_size_t* field_dims,		///< [out] field dimensions.
	h5_size_t* elem_rank,		///< [out] element rank.
	h5_int64_t* type		///< [out] datatype.
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, idx=%llu, "
		      "name=%p, len_name=%llu, "
		      "field_rank=%p, field_dims=%p, elem_rank=%p, type=%p",
		      (h5_file_p)f, (long long unsigned)idx,
		      name, (long long unsigned)len_name, 
		      field_rank, field_dims, elem_rank,
		      type);
	H5_API_RETURN (
		h5b_get_field_info (
			f,
			idx,
			name,
			len_name,
			field_rank,
			field_dims,
			elem_rank,
			type));
}

/*!
  \ingroup h5block_model
  \anchor H5BlockGetFieldInfoByName

  Get the rank and dimensions of the field specified by its name.
  See \ref H5BlockGetFieldInfo.

  \return \c H5_SUCCESS or \c H5_FAILURE
*/
static inline h5_err_t
H5BlockGetFieldInfoByName (
	const h5_file_t f,		///< [in]  file handle.
	const char* name,		///< [in]  field name.
	h5_size_t* field_rank,		///< [out] field rank.
	h5_size_t* field_dims,		///< [out] field dimensions.
	h5_size_t* elem_rank,		///< [out] element rank.
	h5_int64_t* type		///< [out] datatype.
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', "
		      "field_rank=%p, field_dims=%p, elem_rank=%p, type=%p",
		      (h5_file_p)f, name, field_rank, field_dims, elem_rank, type);
	H5_API_RETURN (
		h5b_get_field_info_by_name (
			f,
			name,
			field_rank,
			field_dims,
			elem_rank,
			type));
}

#ifdef __cplusplus
}
#endif

#endif
