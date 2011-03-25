#ifndef __H5PART_H
#define __H5PART_H

#ifdef __cplusplus
extern "C" {
#endif

h5_err_t
H5PartSetNumParticles (
	h5_file_t *f,		/*!< [in] Handle to open file */
	h5_size_t nparticles	/*!< [in] Number of particles */
	);

h5_err_t
H5PartSetNumParticlesStrided (
	h5_file_t *f,		/*!< [in] Handle to open file */
	h5_size_t nparticles,	/*!< [in] Number of particles */
	h5_size_t stride	/*!< [in] Stride value (e.g. number of fields in the particle array) */
        );

h5_err_t
H5PartSetChunk (
	h5_file_t *f,
	h5_size_t size
	);

h5_err_t
H5PartWriteDataFloat64 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate array with */
	const h5_float64_t *data	/*!< [in] Array to commit to disk */
	);

h5_err_t
H5PartWriteDataFloat32 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate array with */
	const h5_float32_t *data	/*!< [in] Array to commit to disk */
	);

h5_err_t
H5PartWriteDataInt64 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate array with */
	const h5_int64_t *data	/*!< [in] Array to commit to disk */
	);

h5_err_t
H5PartWriteDataInt32 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate array with */
	const h5_int32_t *data	/*!< [in] Array to commit to disk */
	);

h5_err_t
H5PartReadDataFloat64 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate dataset with */
	h5_float64_t *data	/*!< [out] Array of data */
	);

h5_err_t
H5PartReadDataFloat32 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate dataset with */
	h5_float32_t *data	/*!< [out] Array of data */
	);

h5_err_t
H5PartReadDataInt64 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate dataset with */
	h5_int64_t *data	/*!< [out] Array of data */
	);

h5_err_t
H5PartReadDataInt32 (
	h5_file_t *f,		/*!< [in] Handle to open file */
	const char *name,	/*!< [in] Name to associate dataset with */
	h5_int32_t *data	/*!< [out] Array of data */
	);

h5_ssize_t
H5PartGetNumDatasets (
	h5_file_t *f			/*!< [in]  Handle to open file */
	);

h5_err_t
H5PartGetDatasetName (
	h5_file_t *f,		/*!< [in]  Handle to open file */
	const h5_id_t idx,	/*!< [in]  Index of the dataset */
	char *name,		/*!< [out] Name of dataset */
	const h5_size_t len	/*!< [in]  Size of buffer \c name */
	);

h5_err_t
H5PartGetDatasetInfo (
	h5_file_t *f,		/*!< [in]  Handle to open file */
	const h5_id_t idx,	/*!< [in]  Index of the dataset */
	char *dataset_name,	/*!< [out] Name of dataset */
	const h5_size_t len_dataset_name,
				/*!< [in]  Size of buffer \c dataset_name */
	h5_int64_t *type,	/*!< [out] Type of data in dataset */
	h5_size_t *nelem	/*!< [out] Number of elements. */
	);

h5_ssize_t
H5PartGetNumParticles (
	h5_file_t *f			/*!< [in]  Handle to open file */
	);

h5_err_t
H5PartResetView (
 	h5_file_t *f			/*!< [in]  Handle to open file */
	);

h5_err_t
H5PartHasView (
 	h5_file_t *f			/*!< [in]  Handle to open file */
	);

h5_err_t
H5PartSetView (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	h5_int64_t start,	/*!< [in]  Start particle */
	h5_int64_t end	/*!< [in]  End particle */
	);

h5_err_t
H5PartSetViewIndices (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	const h5_id_t *indices,		/*!< [in]  List of indices */
	h5_size_t nelems		/*!< [in]  Size of list */
	);

h5_err_t
H5PartGetView (
	h5_file_t *f,			/*!< [in]  Handle to open file */
	h5_int64_t *start,		/*!< [out]  Start particle */
	h5_int64_t *end			/*!< [out]  End particle */
	);

h5_err_t
H5PartSetCanonicalView (
	h5_file_t *f			/*!< [in]  Handle to open file */
	);

#ifdef __cplusplus
}
#endif

#endif
