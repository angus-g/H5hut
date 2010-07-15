#ifndef __H5BLOCK_H
#define __H5BLOCK_H

#include "H5.h"
#include "H5Block_readwrite.h"

h5_err_t
H5Block3dSetView (
	h5_file_t *const f,		/*!< IN: File handle		*/
	const h5_int64_t i_start,	/*!< IN: start index of \c i	*/ 
	const h5_int64_t i_end,         /*!< IN: end index of \c i	*/  
	const h5_int64_t j_start,	/*!< IN: start index of \c j	*/ 
	const h5_int64_t j_end,	        /*!< IN: end index of \c j	*/ 
	const h5_int64_t k_start,	/*!< IN: start index of \c k	*/ 
	const h5_int64_t k_end	        /*!< IN: end index of \c k	*/
	);

h5_err_t
H5Block3dGetView (
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
H5Block3dSetChunk (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_int64_t i,		/*!< IN: size of \c i */ 
	const h5_int64_t j,		/*!< IN: size of \c j */  
	const h5_int64_t k		/*!< IN: size of \c k */ 
	);

h5_err_t
H5Block3dGetChunk (
	h5_file_t *const f,		/*!< IN: File handle */
	const char *field_name, 	/*!< IN: name of dataset */
	h5_size_t *dims			/*!< OUT: array containing the chunk dimensions */
	);

h5_err_t
H5Block3dGetReducedView (
	h5_file_t *const f,	/*!< IN: File handle */
	const int proc,		/*!< IN: Processor to get partition from */
	h5_size_t *i_start,	/*!< OUT: start index of \c i */ 
	h5_size_t *i_end,	/*!< OUT: end index of \c i */  
	h5_size_t *j_start,	/*!< OUT: start index of \c j */ 
	h5_size_t *j_end,	/*!< OUT: end index of \c j */ 
	h5_size_t *k_start,	/*!< OUT: start index of \c j */ 
	h5_size_t *k_end	/*!< OUT: end index of \c j */ 
	);

int
H5Block3dGetProc (
	h5_file_t *const f,		/*!< IN: File handle */
	const h5_int64_t i,		/*!< IN: \c i coordinate */
	const h5_int64_t j,		/*!< IN: \c j coordinate */
	const h5_int64_t k		/*!< IN: \c k coordinate */
	);

h5_size_t
H5BlockGetNumFields (
	h5_file_t *const f			/*!< IN: file handle */
	);

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
	);

h5_err_t
H5BlockGetFieldInfoByName (
	h5_file_t *const f,		/*!< IN: file handle */
	const char *name,		/*!< IN: field name */
	h5_size_t *field_rank,			/*!< OUT: field rank */
	h5_size_t *field_dims,			/*!< OUT: field dimensions */
	h5_size_t *elem_rank,			/*!< OUT: element rank */
	h5_int64_t *type		/*!< OUT: datatype */
	);

h5_err_t
H5BlockWriteFieldAttribString (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	const char *value			/*!< IN: attribute value */
	);

h5_err_t
H5BlockReadFieldAttribString (
	h5_file_t *const f,			/*!< IN: file handle */
	const char *field_name,			/*!< IN: field name */
	const char *attrib_name,		/*!< IN: attribute name */
	char *buffer			        /*!< OUT: attribute value */
	);

h5_ssize_t
H5BlockGetNumFieldAttribs (
	h5_file_t *const f,			/*<! IN: file handle */
	const char *field_name			/*<! IN: field name */
	);

h5_int64_t
H5BlockGetFieldAttribInfo (
	h5_file_t *const f,		/*!< [in]  Handle to open file */
	const char *field_name,		/*<! IN: field name */
	const h5_size_t attrib_idx,	/*!< [in]  Index of attribute to
					           get infos about */
	char *attrib_name,		/*!< [out] Name of attribute */
	const h5_size_t len_of_attrib_name,
					/*!< [in]  length of buffer \c name */
	h5_int64_t *attrib_type,	/*!< [out] Type of value. */
	h5_size_t *attrib_nelem         /*!< [out] Number of elements */
	);


#endif
