#ifndef __H5B_MODEL_H
#define __H5B_MODEL_H

h5_err_t
h5b_3d_set_view (
	h5_file_t *const f,		/*!< IN: File handle		*/
	const h5_size_t i_start,	/*!< IN: start index of \c i	*/ 
	const h5_size_t i_end,		/*!< IN: end index of \c i	*/  
	const h5_size_t j_start,	/*!< IN: start index of \c j	*/ 
	const h5_size_t j_end,	        /*!< IN: end index of \c j	*/ 
	const h5_size_t k_start,	/*!< IN: start index of \c k	*/ 
	const h5_size_t k_end	        /*!< IN: end index of \c k	*/
	);

h5_err_t
h5b_3d_set_chunk (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_size_t i,		/*!< IN: size of \c i */ 
	const h5_size_t j,		/*!< IN: size of \c j */  
	const h5_size_t k		/*!< IN: size of \c k */ 
	);

h5_err_t
h5b_3d_get_chunk (
	h5_file_t *const f,		/*!< IN: File handle */
	const char *field_name, 	/*!< IN: name of dataset */
	h5_size_t *dims    	        /*!< OUT: array containing the chunk dimensions */
	);

h5_err_t
h5b_3d_get_view (
	h5_file_t *const f,	/*!< IN: File handle */
	const int proc,		/*!< IN: Processor to get partition from */
	h5_size_t *i_start,	/*!< OUT: start index of \c i	*/ 
	h5_size_t *i_end,	/*!< OUT: end index of \c i	*/  
	h5_size_t *j_start,	/*!< OUT: start index of \c j	*/ 
	h5_size_t *j_end,	/*!< OUT: end index of \c j	*/ 
	h5_size_t *k_start,	/*!< OUT: start index of \c k	*/ 
	h5_size_t *k_end	/*!< OUT: end index of \c k	*/ 
	);

h5_err_t
h5b_3d_get_reduced_view (
	h5_file_t *const f,	/*!< IN: File handle */
	const int proc,		/*!< IN: Processor to get partition from */
	h5_size_t *i_start,	/*!< OUT: start index of \c i	*/ 
	h5_size_t *i_end,	/*!< OUT: end index of \c i	*/  
	h5_size_t *j_start,	/*!< OUT: start index of \c j	*/ 
	h5_size_t *j_end,	/*!< OUT: end index of \c j	*/ 
	h5_size_t *k_start,	/*!< OUT: start index of \c k	*/ 
	h5_size_t *k_end	/*!< OUT: end index of \c k	*/ 
	);

int
h5b_3d_get_proc (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_int64_t i,		/*!< IN: \c i coordinate */
	const h5_int64_t j,		/*!< IN: \c j coordinate */
	const h5_int64_t k		/*!< IN: \c k coordinate */
	);

h5_ssize_t
h5b_get_num_fields (
	h5_file_t *const f		/*!< IN: File handle */
        );

h5_err_t
h5b_get_field_info_by_name (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *name,			/*!< OUT: field name */
	h5_size_t *field_rank,			/*!< OUT: field rank */
	h5_size_t *field_dims,			/*!< OUT: field dimensions */
	h5_size_t *elem_rank,			/*!< OUT: element rank */
	h5_int64_t *type			/*!< OUT: datatype */
	);

h5_err_t
h5b_get_field_info (
	h5_file_t *const f,			/*!< IN: file handle */
	const h5_size_t idx,			/*!< IN: index of field */
	char *name,				/*!< OUT: field name */
	const h5_size_t len_name,		/*!< IN: buffer size */
	h5_size_t *field_rank,			/*!< OUT: field rank */
	h5_size_t *field_dims,			/*!< OUT: field dimensions */
	h5_size_t *elem_rank,			/*!< OUT: element rank */
	h5_int64_t *type			/*!< OUT: datatype */
	);

#endif

