#ifndef __H5B_MODEL_H
#define __H5B_MODEL_H

h5_err_t
h5bpriv_release_hyperslab (
	h5_file_t *const f			/*!< IN: file handle */
	);

h5_err_t
h5bpriv_open_block_group (
	h5_file_t *const f		/*!< IN: file handle */
	);

h5_err_t
h5bpriv_have_field_group (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *name
	);

h5_err_t
h5bpriv_open_field_group (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *name
	);

h5_err_t
h5bpriv_create_field_group (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name		/*!< IN: name of field group to create */
	);

h5_int64_t
h5b_3d_has_view (
	h5_file_t *const f		/*!< IN: File handle		*/
	);

h5_err_t
h5b_3d_set_view (
	h5_file_t *const f,		/*!< IN: File handle		*/
	const h5_size_t i_start,	/*!< IN: start index of \c i	*/ 
	const h5_size_t i_end,		/*!< IN: end index of \c i	*/  
	const h5_size_t j_start,	/*!< IN: start index of \c j	*/ 
	const h5_size_t j_end,		/*!< IN: end index of \c j	*/ 
	const h5_size_t k_start,	/*!< IN: start index of \c k	*/ 
	const h5_size_t k_end		/*!< IN: end index of \c k	*/
	);

h5_err_t
h5b_3d_get_view (
	h5_file_t *const f,		/*!< IN: File handle */
	h5_size_t *const i_start,	/*!< OUT: start index of \c i	*/ 
	h5_size_t *const i_end,		/*!< OUT: end index of \c i	*/  
	h5_size_t *const j_start,	/*!< OUT: start index of \c j	*/ 
	h5_size_t *const j_end,		/*!< OUT: end index of \c j	*/ 
	h5_size_t *const k_start,	/*!< OUT: start index of \c k	*/ 
	h5_size_t *const k_end		/*!< OUT: end index of \c k	*/ 
	);

h5_err_t
h5b_3d_get_reduced_view (
	h5_file_t *const f,		/*!< IN: File handle */
	h5_size_t *const i_start,	/*!< OUT: start index of \c i	*/ 
	h5_size_t *const i_end,		/*!< OUT: end index of \c i	*/  
	h5_size_t *const j_start,	/*!< OUT: start index of \c j	*/ 
	h5_size_t *const j_end,		/*!< OUT: end index of \c j	*/ 
	h5_size_t *const k_start,	/*!< OUT: start index of \c k	*/ 
	h5_size_t *const k_end		/*!< OUT: end index of \c k	*/ 
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
	h5_size_t *const i,		/*!< OUT: size of \c i */ 
	h5_size_t *const j,		/*!< OUT: size of \c j */  
	h5_size_t *const k		/*!< OUT: size of \c k */ 
	);

#if defined(PARALLEL_IO)
h5_err_t
h5b_3d_set_grid (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_size_t i,		/*!< IN: dimension in \c i */ 
	const h5_size_t j,		/*!< IN: dimension in \c j */  
	const h5_size_t k		/*!< IN: dimension in \c k */ 
	);

h5_err_t
h5b_3d_get_grid_coords (
	h5_file_t *const f,		/*!< IN: File handle */
	const int proc,			/*!< IN: MPI processor */
	h5_size_t *const i,		/*!< OUT: index in \c i */ 
	h5_size_t *const j,		/*!< OUT: index in \c j */  
	h5_size_t *const k		/*!< OUT: index in \c k */ 
	);

h5_err_t
h5b_3d_set_dims (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_size_t i,		/*!< IN: dimension in \c i */ 
	const h5_size_t j,		/*!< IN: dimension in \c j */  
	const h5_size_t k		/*!< IN: dimension in \c k */ 
	);
#endif

h5_err_t
h5b_3d_set_halo (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_size_t i,		/*!< IN: radius in \c i */ 
	const h5_size_t j,		/*!< IN: radius in \c j */  
	const h5_size_t k		/*!< IN: radius in \c k */ 
	);

h5_ssize_t
h5b_get_num_fields (
	h5_file_t *const f		/*!< IN: File handle */
	);

h5_err_t
h5b_get_field_info_by_name (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *name,			/*!< OUT: field name */
	h5_size_t *const field_rank,		/*!< OUT: field rank */
	h5_size_t *const field_dims,		/*!< OUT: field dimensions */
	h5_size_t *const elem_rank,		/*!< OUT: element rank */
	h5_int64_t *const type			/*!< OUT: datatype */
	);

h5_err_t
h5b_get_field_info (
	h5_file_t *const f,			/*!< IN: file handle */
	const h5_size_t idx,			/*!< IN: index of field */
	char *name,				/*!< OUT: field name */
	const h5_size_t len_name,		/*!< IN: buffer size */
	h5_size_t *const field_rank,		/*!< OUT: field rank */
	h5_size_t *const field_dims,		/*!< OUT: field dimensions */
	h5_size_t *const elem_rank,		/*!< OUT: element rank */
	h5_int64_t *const type			/*!< OUT: datatype */
	);


#endif
