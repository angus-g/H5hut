#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>	/* va_arg - System dependent ?! */
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>

#ifndef WIN32
#include <unistd.h>
#else /* WIN32 */
#include <io.h>
#define open  _open
#define close _close
#endif /* WIN32 */

#include "h5core/h5_core.h"
#include "H5Part.h"

/*!
  \ingroup h5part_model

  Set the number of particles for the current time-step.
  After you call this subroutine, all subsequent 
  operations will assume this number of particles will be written.

  For the parallel library, the \c nparticles value is the number of
  particles that the \e individual task will write. You can use
  a different value on different tasks.
  This function uses an \c MPI_Allgather
  call to aggregate each tasks number of particles and determine
  the appropiate offsets. Because of the use of this MPI collective,
  it is advisable to call this function as
  few times as possible when running at large concurrency.

  This function assumes that your particles' data fields are in stored in
  contiguous 1D arrays.
  For instance, the fields \e x and \e y for your particles are stored
  in separate arrays \c x[] and \c y[].
  
  If instead you store your particles as tuples, so that the values
  are arranged \f$ x_1,y_1,x_2,y_2\f$... than you need to setup striding
  (in this case with value 2) using \ref H5PartSetNumParticlesStrided.

  \return	\c H5_SUCCESS or error code
 */
h5_err_t
H5PartSetNumParticles (
	h5_file_t *f,		/*!< [in] Handle to open file */
	h5_size_t nparticles	/*!< [in] Number of particles */
	) {
	H5_API_ENTER2 (h5_err_t,
		      "f=0x%p, nparticles=%llu",
		      f, (long long unsigned)nparticles);
	h5_size_t stride = 1;
	H5_API_RETURN (h5u_set_num_particles(f, nparticles, stride));
}

/*!
  \ingroup h5part_model

  Set the number of particles for the current time-step.
  After you call this subroutine, all subsequent 
  operations will assume this number of particles will be written.

  For the parallel library, the \c nparticles value is the number of
  particles that the \e individual task will write. You can use
  a different value on different tasks.
  This function uses an \c MPI_Allgather
  call to aggregate each tasks number of particles and determine
  the appropiate offsets. Because of the use of this MPI collective,
  it is advisable to call this function as
  few times as possible when running at large concurrency.

  This function assumes that your particles' data fields are
  stored tuples. For instance, the fields \e x and \e y of your
  particles are arranged \f$x_1,y_1,x_2,y_2\f$... in a single data
  array. In this example, the stride value would be 2.
  
  If you instead have a separate array for each fields,
  such as \c x[] and \c y[],
  use \ref H5PartSetNumParticles.

  \return	\c H5_SUCCESS or error code
*/
h5_err_t
H5PartSetNumParticlesStrided (
	h5_file_t *f,		/*!< [in] Handle to open file */
	h5_size_t nparticles,	/*!< [in] Number of particles */
	h5_size_t stride	/*!< [in] Stride value (e.g. number of fields in the particle array) */
	) {
	H5_API_ENTER3 (h5_err_t,
		       "f=0x%p, nparticles=%llu, stride=%llu",
		       f, (long long unsigned)nparticles,
		       (long long unsigned)stride);
	H5_API_RETURN (h5u_set_num_particles (f, nparticles, stride));
}

/*!
  \ingroup h5part_model

  Define the chunk \c size and enables chunking in the underlying
  HDF5 layer.

  Note that this policy wastes disk space, but can improve write
  bandwidth on parallel filesystems that are sensitive to write alignment
  (e.g. lustre). It is only recommended when using the MPI-POSIX or MPI-IO
  independent VFDs (see \ref H5OpenFile).

  \return	\c H5_SUCCESS or error code
*/
h5_err_t
H5PartSetChunk (
	h5_file_t *f,
	h5_size_t size
	) {
	H5_API_ENTER2 (h5_err_t,
		      "f=0x%p, size=%llu",
		      f, (long long unsigned)size);
	H5_API_RETURN (h5u_set_chunk (f, size));
}

/*!
  \ingroup h5part_data

  Write array of 64 bit floating point data to file.

  After setting the number of particles with \c H5PartSetNumParticles() and
  the current timestep using \c H5SetStep(), you can start writing datasets
  into the file. Each dataset has a name associated with it (chosen by the
  user) in order to facilitate later retrieval. The name of the dataset is
  specified in the parameter \c name, which must be a null-terminated string.

  There are no restrictions on naming of datasets, but it is useful to arrive
  at some common naming convention when sharing data with other groups.

  The writing routines also implicitly store the datatype of the array so that
  the array can be reconstructed properly on other systems with incompatible
  type representations.

  All data that is written after setting the timestep is associated with that
  timestep. While the number of particles can change for each timestep, you
  cannot change the number of particles in the middle of a given timestep.

  The data is committed to disk before the routine returns.

  \return	\c H5_SUCCESS or error code
 */
h5_err_t
H5PartWriteDataFloat64 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate array with */
	const h5_float64_t *data	/*!< [in] Array to commit to disk */
	) {
	H5_API_ENTER3 (h5_err_t, "f=0x%p, name=\"%s\", date=0x%p", f,name,data);
	H5_API_RETURN (h5u_write_data (f, name, (void*)data, H5T_NATIVE_DOUBLE));
}

/*!
  \ingroup h5part_data

  Write array of 32 bit floating point data to file.

  After setting the number of particles with \c H5PartSetNumParticles() and
  the current timestep using \c H5SetStep(), you can start writing datasets
  into the file. Each dataset has a name associated with it (chosen by the
  user) in order to facilitate later retrieval. The name of the dataset is
  specified in the parameter \c name, which must be a null-terminated string.

  There are no restrictions on naming of datasets, but it is useful to arrive
  at some common naming convention when sharing data with other groups.

  The writing routines also implicitly store the datatype of the array so that
  the array can be reconstructed properly on other systems with incompatible
  type representations.

  All data that is written after setting the timestep is associated with that
  timestep. While the number of particles can change for each timestep, you
  cannot change the number of particles in the middle of a given timestep.

  The data is committed to disk before the routine returns.

  \return	\c H5_SUCCESS or error code
 */
h5_err_t
H5PartWriteDataFloat32 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate array with */
	const h5_float32_t *data	/*!< [in] Array to commit to disk */
	) {
	H5_API_ENTER3 (h5_err_t, "f=0x%p, name=\"%s\", date=0x%p", f,name,data);
	h5_err_t h5err = h5u_write_data( f, name, (void*)data, H5T_NATIVE_FLOAT );
	H5_API_RETURN (h5err);
}

/*!
  \ingroup h5part_data

  Write array of 64 bit integer data to file.

  After setting the number of particles with \c H5PartSetNumParticles() and
  the current timestep using \c H5SetStep(), you can start writing datasets
  into the file. Each dataset has a name associated with it (chosen by the
  user) in order to facilitate later retrieval. The name of the dataset is
  specified in the parameter \c name, which must be a null-terminated string.

  There are no restrictions on naming of datasets, but it is useful to arrive
  at some common naming convention when sharing data with other groups.

  The writing routines also implicitly store the datatype of the array so that
  the array can be reconstructed properly on other systems with incompatible
  type representations.

  All data that is written after setting the timestep is associated with that
  timestep. While the number of particles can change for each timestep, you
  cannot change the number of particles in the middle of a given timestep.

  The data is committed to disk before the routine returns.

  \return	\c H5_SUCCESS or error code
 */
h5_err_t
H5PartWriteDataInt64 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate array with */
	const h5_int64_t *data	/*!< [in] Array to commit to disk */
	) {
	H5_API_ENTER3 (h5_err_t, "f=0x%p, name=\"%s\", date=0x%p", f,name,data);
	H5_API_RETURN (h5u_write_data (f, name, (void*)data, H5T_NATIVE_INT64));
}

/*!
  \ingroup h5part_data

  Write array of 32 bit integer data to file.

  After setting the number of particles with \c H5PartSetNumParticles() and
  the current timestep using \c H5SetStep(), you can start writing datasets
  into the file. Each dataset has a name associated with it (chosen by the
  user) in order to facilitate later retrieval. The name of the dataset is
  specified in the parameter \c name, which must be a null-terminated string.

  There are no restrictions on naming of datasets, but it is useful to arrive
  at some common naming convention when sharing data with other groups.

  The writing routines also implicitly store the datatype of the array so that
  the array can be reconstructed properly on other systems with incompatible
  type representations.

  All data that is written after setting the timestep is associated with that
  timestep. While the number of particles can change for each timestep, you
  cannot change the number of particles in the middle of a given timestep.

  The data is committed to disk before the routine returns.

  \return	\c H5_SUCCESS or error code
 */
h5_err_t
H5PartWriteDataInt32 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate array with */
	const h5_int32_t *data	/*!< [in] Array to commit to disk */
	) {
	H5_API_ENTER3 (h5_err_t, "f=0x%p, name=\"%s\", date=0x%p", f,name,data);
	H5_API_RETURN (h5u_write_data (f, name, (void*)data, H5T_NATIVE_INT32));
}

/*!
  \ingroup h5part_data

  Read array of 64 bit floating point data from file.

  When retrieving datasets from disk, you ask for them
  by name. There are no restrictions on naming of arrays,
  but it is useful to arrive at some common naming
  convention when sharing data with other groups.

  \return	\c H5_SUCCESS or error code
*/
h5_err_t
H5PartReadDataFloat64 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate dataset with */
	h5_float64_t *data	/*!< [out] Array of data */
	) {
	H5_API_ENTER3 (h5_err_t, "f=0x%p, name=\"%s\", date=0x%p", f,name,data);
	H5_API_RETURN (h5u_read_data (f, name, data, H5T_NATIVE_DOUBLE));
}

/*!
  \ingroup h5part_data

  Read array of 32 bit floating point data from file.

  When retrieving datasets from disk, you ask for them
  by name. There are no restrictions on naming of arrays,
  but it is useful to arrive at some common naming
  convention when sharing data with other groups.

  \return	\c H5_SUCCESS or error code
*/
h5_err_t
H5PartReadDataFloat32 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate dataset with */
	h5_float32_t *data	/*!< [out] Array of data */
	) {
	H5_API_ENTER3 (h5_err_t, "f=0x%p, name=\"%s\", date=0x%p", f,name,data);
	H5_API_RETURN (h5u_read_data (f, name, data, H5T_NATIVE_FLOAT));
}

/*!
  \ingroup h5part_data

  Read array of 64 bit integer data from file.

  When retrieving datasets from disk, you ask for them
  by name. There are no restrictions on naming of arrays,
  but it is useful to arrive at some common naming
  convention when sharing data with other groups.

  \return	\c H5_SUCCESS or error code
*/
h5_err_t
H5PartReadDataInt64 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate dataset with */
	h5_int64_t *data	/*!< [out] Array of data */
	) {
	H5_API_ENTER3 (h5_err_t, "f=0x%p, name=\"%s\", date=0x%p", f,name,data);
	H5_API_RETURN (h5u_read_data (f, name, data, H5T_NATIVE_INT64));
}

/*!
  \ingroup h5part_data

  Read array of 32 bit integer data from file.

  When retrieving datasets from disk, you ask for them
  by name. There are no restrictions on naming of arrays,
  but it is useful to arrive at some common naming
  convention when sharing data with other groups.

  \return	\c H5_SUCCESS or error code
*/
h5_err_t
H5PartReadDataInt32 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate dataset with */
	h5_int32_t *data	/*!< [out] Array of data */
	) {
	H5_API_ENTER3 (h5_err_t, "f=0x%p, name=\"%s\", date=0x%p", f,name,data);
	H5_API_RETURN (h5u_read_data (f, name, data, H5T_NATIVE_INT32));
}

/*!
  \ingroup h5part_model

  Get the number of datasets that are stored at the current time-step.

  \return	number of datasets in current timestep or error code
*/

h5_ssize_t
H5PartGetNumDatasets (
	h5_file_t *f			/*!< [in]  Handle to open file */
	) {
	H5_API_ENTER1 (h5_err_t, "f=0x%p", f);
	H5_API_RETURN (h5u_get_num_datasets(f));
}

/*!
  \ingroup h5part_model

  This reads the name of a dataset specified by it's index in the current
  time-step.

  If the number of datasets is \c n, the range of \c _index is \c 0 to \c n-1.

  \result	\c H5_SUCCESS
*/
h5_err_t
H5PartGetDatasetName (
	h5_file_t *f,		/*!< [in]  Handle to open file */
	const h5_id_t idx,	/*!< [in]  Index of the dataset */
	char *name,		/*!< [out] Name of dataset */
	const h5_size_t len	/*!< [in]  Size of buffer \c name */
	) {
	H5_API_ENTER4 (h5_err_t, 
			    "f=0x%p, "
			    "idx=%llu, "
			    "name=\"%s\", len=%llu, ",
			    f,
			    idx,
			    name, len);
	H5_API_RETURN (h5u_get_dataset_info(f, idx, name, len, NULL, NULL));
}

/*!
  \ingroup h5part_model

  Gets the name, type and number of elements of a dataset based on its
  index in the current timestep.

  Type is one of the following values:

  - \c H5_FLOAT64_T (for \c h5_float64_t)
  - \c H5_FLOAT32_T (for \c h5_float32_t)
  - \c H5_INT64_T (for \c h5_int64_t)
  - \c H5_INT32_T (for \c h5_int32_t)

  \return	\c H5_SUCCESS
*/
h5_err_t
H5PartGetDatasetInfo (
	h5_file_t *f,		/*!< [in]  Handle to open file */
	const h5_id_t idx,	/*!< [in]  Index of the dataset */
	char *dataset_name,	/*!< [out] Name of dataset */
	const h5_size_t len_dataset_name,
				/*!< [in]  Size of buffer \c dataset_name */
	h5_int64_t *type,	/*!< [out] Type of data in dataset */
	h5_size_t *nelem	/*!< [out] Number of elements. */
	) {
	H5_API_ENTER6 (h5_int64_t, 
			    "f=0x%p, "
			    "idx=%llu, "
			    "dataset_name=\"%s\", len_dataset_name=%llu, "
			    "type=0x%p, nelem=0x%p",
			    f,
			    idx,
			    dataset_name, len_dataset_name,
			    type, nelem);
	H5_API_RETURN (h5u_get_dataset_info(f, idx, dataset_name, len_dataset_name, type, nelem));
}

/*!
  \ingroup h5part_model

  This function returns the number of particles in this processor's view,
  if a view has been set.

  If not, it returns the total number of particles across all processors
  from the last \ref H5PartSetNumParticles call.

  If you have neither set the number of particles
  nor set a view, then this returns the total number of
  particles in the first data set of the current time step.
  Note that H5Part assumes that all data sets within a given time step
  have the same number of particles (although the number particles can
  vary across time steps).
  
  If none of these conditions are met, an error is thrown.

  \return	number of particles in current timestep or an error
		code.
 */
h5_ssize_t
H5PartGetNumParticles (
	h5_file_t *f			/*!< [in]  Handle to open file */
	) {
	H5_API_ENTER1 (h5_ssize_t, "f=0x%p", f);
	CHECK_FILEHANDLE( f );
	H5_API_RETURN (h5u_get_num_particles (f));
}

/*!
  \ingroup h5part_model

  Reset the view.

  \return	\c H5_SUCCESS
*/
h5_err_t
H5PartResetView (
 	h5_file_t *f			/*!< [in]  Handle to open file */
	) {
	H5_API_ENTER1 (h5_ssize_t, "f=0x%p", f);
	H5_API_RETURN (h5u_reset_view (f));
}

/*!
  \ingroup h5part_model

  Check whether a view has been set, either automatically with
  \ref H5PartSetNumParticles or manually with \ref H5PartSetView
  or \ref H5PartSetViewIndices.

  \return 0 for false or 1 for true
*/
h5_err_t
H5PartHasView (
 	h5_file_t *f			/*!< [in]  Handle to open file */
	) {
	H5_API_ENTER1 (h5_ssize_t, "f=0x%p", f);
	H5_API_RETURN (h5u_has_view (f));
}

/*!
  \ingroup h5part_model

  For parallel I/O or for subsetting operations on the datafile,
  this function allows you to define a subset of the total
  particle dataset to operate on.
  The concept of "view" works for both serial
  and for parallel I/O.  The "view" will remain in effect until a new view
  is set, or the number of particles in a dataset changes, or the view is
  "unset" by calling \c H5PartSetView(file,-1,-1);

  Before you set a view, \ref H5PartGetNumParticles will return the
  total number of particles in the current time-step (even for the parallel
  reads).  However, after you set a view, it will return the number of
  particles contained in the view.

  The range is \e inclusive: the end value is the last index of the
  data.

  \return	\c H5_SUCCESS or error code
*/
h5_err_t
H5PartSetView (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	h5_int64_t start,	/*!< [in]  Start particle */
	h5_int64_t end	/*!< [in]  End particle */
	) {
	H5_API_ENTER3 (h5_err_t,
		       "f=0x%p, start=%lld, end=%lld",
		       f, start, end);
	H5_API_RETURN (h5u_set_view (f, start, end));
}

/*!
  \ingroup h5part_model

  For parallel I/O or for subsetting operations on the datafile,
  this function allows you to define a subset of the total
  dataset to operate on by specifying a list of indices.
  The concept of "view" works for both serial
  and for parallel I/O.  The "view" will remain in effect until a new view
  is set, or the number of particles in a dataset changes, or the view is
  "unset" by calling \c H5PartSetViewIndices(NULL,0);

  When you perform a read or write on a view consisting of indices, it
  is assumed that your buffer is \b unpacked, meaning that there is room
  for all the intermediate values (which will not be touched by the read
  or write).

  Before you set a view, the \c H5PartGetNumParticles() will return the
  total number of particles in the current time-step (even for the parallel
  reads).  However, after you set a view, it will return the number of
  particles contained in the view.

  \return	\c H5_SUCCESS or error code
*/
h5_err_t
H5PartSetViewIndices (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	const h5_size_t *indices,	/*!< [in]  List of indices */
	h5_size_t nelems		/*!< [in]  Size of list */
	) {
	H5_API_ENTER3 (h5_err_t,
		       "f=0x%p, indices=0x%p, nelems=%llu",
		       f, indices, nelems);
	H5_API_RETURN (h5u_set_view_indices (f, indices, nelems));
}

/*!
  \ingroup h5part_model

   Allows you to query the current view. Start and End
   will be \c -1 if there is no current view established.
   Use \c H5PartHasView() to see if the view is smaller than the
   total dataset.

   \return	number of elements in the view or error code
*/
h5_err_t
H5PartGetView (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	h5_int64_t *start,		/*!< [out]  Start particle */
	h5_int64_t *end			/*!< [out]  End particle */
	) {
	H5_API_ENTER3 (h5_err_t,
		       "f=0x%p, start=0x%p, end=0x%p",
		       f, start, end);
	H5_API_RETURN (h5u_get_view (f, start, end));
}

/*!
  \ingroup h5part_model

  If it is too tedious to manually set the start and end coordinates
  for a view, the \c H5SetCanonicalView() will automatically select an
  appropriate domain decomposition of the data arrays for the degree
  of parallelism and set the "view" accordingly.

  \return		H5_SUCCESS or error code
*/
h5_err_t
H5PartSetCanonicalView (
	h5_file_t *f			/*!< [in]  Handle to open file */
	) {
	H5_API_ENTER1 (h5_err_t, "f=0x%p", f);
	H5_API_RETURN (h5u_set_canonical_view (f));
}

