/*
  Copyright (c) 2006-2016, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef H5PART_IO
#define H5PART_IO

#include "h5core/h5_types.h"
#include "h5core/h5.h"
#include "h5core/h5_debug.h"
#include "h5core/h5u_io.h"
#include "h5core/h5u_model.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
   !   _                   _          
   !  (_)_ __   __ _ _   _(_)_ __ ___ 
   !  | | '_ \ / _` | | | | | '__/ _ \
   !  | | | | | (_| | |_| | | | |  __/
   !  |_|_| |_|\__, |\__,_|_|_|  \___|
   !              |_|
   !
*/

/**
   \addtogroup h5part_io
   @{
*/
/**
  Get the number of datasets that are stored at the current step.

  \return   number of datasets in current timestep
  \return   \c H5_FAILURE on error
*/
static inline h5_ssize_t
H5PartGetNumDatasets (
	const h5_file_t f		///< [in]  file handle
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5u_get_num_datasets(f));
}

/**
  Query the name of a dataset given by it's index in the current step.

  If the number of datasets is \c n, the range of \c _index is \c 0 to \c n-1.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error
*/
static inline h5_err_t
H5PartGetDatasetName (
	const h5_file_t f,           	///< [in]  file handle
	const h5_id_t idx,      	///< [in]  index of the dataset
	char* name,             	///< [out] name of dataset
	const h5_size_t len     	///< [in]  size of buffer \c name
	) {
	H5_API_ENTER (h5_err_t, 
		       "f=%p, "
		       "idx=%lld, "
		       "name='%p', len=%llu, ",
                      (h5_file_p)f,
		       (long long)idx,
		       name, (unsigned long long)len);
	H5_API_RETURN (h5u_get_dataset_info(f, idx, name, len, NULL, NULL));
}

/**
  Gets the name, type and number of elements of a dataset based on its
  index in the current timestep.

  Type is one of the following values:

  - \c H5_FLOAT64_T (for \c h5_float64_t)
  - \c H5_FLOAT32_T (for \c h5_float32_t)
  - \c H5_INT64_T (for \c h5_int64_t)
  - \c H5_INT32_T (for \c h5_int32_t)

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error

  \see H5PartGetNumDatasets()
  \see H5PartGetDatasetInfoByName()
*/
static inline h5_err_t
H5PartGetDatasetInfo (
	const h5_file_t f,           	///< [in]  file handle
	const h5_id_t idx,      	///< [in]  index of the dataset
	char* name,             	///< [out] name of dataset
	const h5_size_t len_name,       ///< [in]  size of buffer \c name
	h5_int64_t* type,       	///< [out] type of data in dataset
	h5_size_t* nelems        	///< [out] number of elements
	) {
	H5_API_ENTER (h5_int64_t, 
		      "f=%p, "
		      "idx=%lld, "
		      "name='%p', len_name=%llu, "
		      "type=%p, nelems=%p",
		      (h5_file_p)f,
		      (long long)idx,
		      name, (long long unsigned)len_name,
		      type, nelems);
	H5_API_RETURN (h5u_get_dataset_info (
			       f, idx, name, len_name, type, nelems));
}
/**
  Determines whether a dataset with given name exists in current step.

  \return      true (value \c >0) if step exists
  \return      false (\c 0) if step does not exist
  \return      \c H5_FAILURE on error
*/
static inline h5_err_t
H5PartHasDataset (
	const h5_file_t f,           	///< [in]  file handle
	const char* const name         	///< [in]  name of dataset
	) {
	H5_API_ENTER (h5_int64_t, 
		      "f=%p, name='%s'",
		      (h5_file_p)f, name);
	H5_API_RETURN (h5u_has_dataset (f, name));
}

/**
  Gets the type and number of elements of a dataset based on its
  name in the current timestep.

  Type is one of the following values:

  - \c H5_FLOAT64_T (for \c h5_float64_t)
  - \c H5_FLOAT32_T (for \c h5_float32_t)
  - \c H5_INT64_T (for \c h5_int64_t)
  - \c H5_INT32_T (for \c h5_int32_t)

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error

  \see H5PartHasDataset()
  \see H5PartGetDatasetInfo()
*/
static inline h5_err_t
H5PartGetDatasetInfoByName (
	const h5_file_t f,           	///< [in]  file handle
	const char* const name,         ///< [in]  name of dataset
	h5_int64_t* type,       	///< [out] type of data in dataset
	h5_size_t* nelems        	///< [out] number of elements
	) {
	H5_API_ENTER (h5_int64_t, 
		      "f=%p, "
		      "name='%s', "
		      "type=%p, nelems=%p",
		      (h5_file_p)f,
		      name,
		      type, nelems);
	H5_API_RETURN (h5u_get_dataset_info_by_name (
			       f, name, type, nelems));
}

/**
  This function returns the number of particles in this processor's view,
  if a view has been set.

  If not, it returns the total number of particles across all processors
  from the last \ref H5PartSetNumParticles() call.

  If you have neither set the number of particles
  nor set a view, then this returns the total number of
  particles in the first data set of the current time step.
  Note that H5Part assumes that all data sets within a given time step
  have the same number of particles (although the number particles can
  vary across time steps).
  
  If none of these conditions are met, an error is thrown.

  \return	number of elements in datasets in current step.
  \return       \c H5_FAILURE on error.
 */
	static inline h5_ssize_t
H5PartGetNumPoints (
	const h5_file_t f		///< [in]  file handle.
	) {
	H5_API_ENTER (h5_ssize_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5u_get_num_points (f));
}

/**
  \see H5PartGetNumPoints()
*/
static inline h5_ssize_t
H5PartGetNumParticles (
	const h5_file_t f		///< [in]  file handle.
	) {
	H5_API_ENTER (h5_ssize_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5u_get_num_points (f));
}

/*
  !                 _ _       
  !  __      ___ __(_) |_ ___ 
  !  \ \ /\ / / '__| | __/ _ \
  !   \ V  V /| |  | | ||  __/
  !    \_/\_/ |_|  |_|\__\___|
*/

/**
   \fn h5_err_t H5PartWriteDataFloat64 (
	const h5_file_t f,
	const char* name,
	const h5_float64_t* data
	)

   \fn h5_err_t H5PartWriteDataFloat32 (
	const h5_file_t f,
	const char* name,
	const h5_float32_t* data
	)

   \fn h5_err_t H5PartWriteDataInt64 (
	const h5_file_t f,
	const char* name,
	const h5_int64_t* data
	)

   \fn h5_err_t H5PartWriteDataInt32 (
	const h5_file_t f,
	const char* name,
	const h5_int32_t* data
	)

  Write a dataset to the current step.

  After the current (time-)step and view, you can start writing datasets
  into the file. Each dataset has a name associated with it (chosen by the
  user) in order to facilitate later retrieval. The name of the dataset is
  specified in the parameter \c name, which must be a null-terminated string.

  There are no restrictions on naming of datasets, but it is useful to arrive
  at some common naming convention when sharing data with other groups.

  The writing routines also implicitly store the datatype of the array so that
  the array can be reconstructed properly on other systems with incompatible
  type representations.

  All data that is written after setting the timestep is associated with that
  timestep. While the number of elements can change for each timestep, you
  cannot change the number of elements in the middle of a given timestep.

  The data is committed to disk before the routine returns.
  \param f	[in]  file handle.
  \param name   [in]  name to associate array with
  \param data	[in]  array to commit to disk.

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error

  \see H5PartReadDataFloat64()
  \see H5PartReadDataFloat32()
  \see H5PartReadDataInt64()
  \see H5PartReadDataInt32()
 */
static inline h5_err_t
H5PartWriteDataFloat64 (
	const h5_file_t f,
	const char* name,
	const h5_float64_t* data
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', date=%p",
                      (h5_file_p)f, name, data);
	H5_API_RETURN (h5u_write_data (f, name, (void*)data, H5T_NATIVE_DOUBLE));
}

static inline h5_err_t
H5PartWriteDataFloat32 (
	const h5_file_t f,
	const char* name,
	const h5_float32_t* data
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', date=%p",
                      (h5_file_p)f, name, data);
	H5_API_RETURN (h5u_write_data(f, name, (void*)data, H5T_NATIVE_FLOAT));
}

static inline h5_err_t
H5PartWriteDataInt64 (
	const h5_file_t f,
	const char* name,
	const h5_int64_t* data
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', date=%p",
                      (h5_file_p)f, name, data);
	H5_API_RETURN (h5u_write_data (f, name, (void*)data, H5T_NATIVE_INT64));
}

static inline h5_err_t
H5PartWriteDataInt32 (
	const h5_file_t f,
	const char* name,
	const h5_int32_t* data
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', date=%p",
                      (h5_file_p)f, name, data);
	H5_API_RETURN (h5u_write_data (f, name, (void*)data, H5T_NATIVE_INT32));
}

/**
   \fn h5_err_t H5PartReadDataFloat64 (
	const h5_file_t f,
	const char* name,
	h5_float64_t* data
	)

   \fn h5_err_t H5PartReadDataFloat32 (
	const h5_file_t f,
	const char* name,
	h5_float32_t* data
	)

   \fn h5_err_t H5PartReadDataInt64 (
	const h5_file_t f,
	const char* name,
	h5_int64_t* data
	)

   \fn h5_err_t H5PartReadDataInt32 (
	const h5_file_t f,
	const char* name,
	h5_int32_t* data
	)

  Read dataset from file.

  See \ref H5PartWriteDataFloat64() etc. for more details.

  \param f	[in]  file handle
  \param name   [in]  name of dataset to be read
  \param data	[out] buffer for data to be read

  \return \c H5_SUCCESS on success
  \return \c H5_FAILURE on error

  \see H5PartWriteDataFloat64()
  \see H5PartWriteDataFloat32()
  \see H5PartWriteDataInt64()
  \see H5PartWriteDataInt32()
*/
static inline h5_err_t
H5PartReadDataFloat64 (
	const h5_file_t f,
	const char* name,
	h5_float64_t* data
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', date=%p",
                      (h5_file_p)f, name, data);
	H5_API_RETURN (h5u_read_data (f, name, data, H5T_NATIVE_DOUBLE));
}

static inline h5_err_t
H5PartReadDataFloat32 (
	const h5_file_t f,
	const char* name,
	h5_float32_t* data
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', date=%p",
                      (h5_file_p)f, name, data);
	H5_API_RETURN (h5u_read_data (f, name, data, H5T_NATIVE_FLOAT));
}

static inline h5_err_t
H5PartReadDataInt64 (
	const h5_file_t f,
	const char* name,
	h5_int64_t* data
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', date=%p",
                      (h5_file_p)f, name, data);
	H5_API_RETURN (h5u_read_data (f, name, data, H5T_NATIVE_INT64));
}

static inline h5_err_t
H5PartReadDataInt32 (
	const h5_file_t f,
	const char* name,
	h5_int32_t* data
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, name='%s', date=%p",
                      (h5_file_p)f, name, data);
	H5_API_RETURN (h5u_read_data (f, name, data, H5T_NATIVE_INT32));
}

#ifdef __cplusplus
}
#endif

///< @}
#endif
