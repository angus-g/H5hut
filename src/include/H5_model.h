/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5_MODEL_H
#define __H5_MODEL_H

#include "h5core/h5_types.h"
#include "h5core/h5.h"
#include "h5core/h5_debug.h"
#include "h5core/h5_model.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
  \ingroup h5hut_model

  Define format of the step names.

  Example: ==H5SetStepNameFormat( f, "Step", 6 )== defines step names 
  like ==Step#000042==.

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5SetStepNameFormat (
	const h5_file_t f,	///< [in] file handle
	const char* name,	///< [in] prefix, defaults to \c Step
	const h5_int64_t width	///< [in] width of step number
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, name='%s', width=%lld",
		      (h5_file_p)f, name, (long long) width);
	H5_API_RETURN (h5_set_stepname_fmt (f, name, width));
}

/**
  \ingroup h5hut_model

  Get format of the step names.

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5GetStepNameFormat (
	const h5_file_t f,	///< [in]  file handle
	char* name,		///< [out] prefix
	const h5_size_t l_name,	///< [in]  length of buffer name
	int* width		///< [out] width of step number
	) {
	H5_API_ENTER (h5_err_t,
		      "f=%p, name=%p, l_name=%llu, width=%p",
		      (h5_file_p)f, name, (unsigned long long)l_name, width);
	H5_API_RETURN (h5_get_stepname_fmt (f, name, l_name, width));
}

/**
  \ingroup h5hut_model

  Set the current step.

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5SetStep (
	const h5_file_t f,	///< [in]  file handle.
	const h5_id_t step	///< [in]  step to set.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p, step=%lld",
                      (h5_file_p)f, (long long)step);
	H5_API_RETURN (h5_set_step (f, step));
}

/**
  \ingroup h5hut_model

  Get current step.

  \return   Step number
  \return   \c H5_FAILURE on error
*/
static inline h5_id_t
H5GetStep (
	const h5_file_t f	///< [in] file handle.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5_get_step (f));
}

/**
  \ingroup h5hut_model

  Get the number of processors.

  \return   Number of processors.
  \return   \c H5_FAILURE on error.
 */
static inline int
H5GetNumProcs (
	const h5_file_t f	///< [in] file handle.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5_get_num_procs(f));
}

/**
  \ingroup h5hut_model

  Get the number of time-steps that are currently stored in the file
  \c f.

  It works for both reading and writing of files, but is probably
  only typically used when you are reading.

  \return	Number of time-steps
  \return       \c H5_FAILURE on error.
*/
static inline h5_ssize_t
H5GetNumSteps (
	const h5_file_t f	///< [in] file handle.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5_get_num_steps(f));
}

/**
  \ingroup h5hut_model

  Query whether a particular step already exists in the file.

  \return      \c H5_OK if step exists
  \return      \c H5NOK if step does not exist
  \return      \c H5_FAILURE on error
*/
static inline h5_err_t
H5HasStep (
	const h5_file_t f,	///< [in] file handle.
	h5_id_t stepno          ///< [in] step number to query for existence.
	) {
	H5_API_ENTER (h5_err_t, 
		      "f=%p, stepno=%lld",
		      (h5_file_p)f, (long long)stepno);
	H5_API_RETURN (h5_has_step (f, stepno));
}

/**
  \ingroup h5hut_model

  Start traversing steps.

  \note This function is not yet implemented!

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5StartTraverseSteps (
	const h5_file_t f	///< [in] file handle.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5_start_traverse_steps (f));
}

/**
  \ingroup h5hut_model

  Go to next step.

  \note This function is not yet implemented!

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5TraverseSteps (
	const h5_file_t f	///< [in] file handle.
	) {
	H5_API_ENTER (h5_err_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5_traverse_steps (f));
}

/**
  \ingroup h5hut_model

  Return number of attached files.

  \return   number of attachments.
  \return   \c H5_FAILURE on error.
*/
static inline h5_ssize_t
H5GetNumAttachments (
	const h5_file_t f               ///< [in]  file handle.
	) {
	H5_API_ENTER (h5_ssize_t,
                      "f=%p",
                      (h5_file_p)f);
	H5_API_RETURN (h5_get_num_attachments (f));
}

/**
  \ingroup h5hut_model

  Get name and size of attachment given by index \c idx. Return the file name
  in \c fname and the original size in \c fsize.

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5GetAttachmentInfoByIdx (      	
	const h5_file_t f,              ///< [in]  file handle.
	const h5_size_t idx,		///< [in]  index of attachment to be queried.
	char* const fname,		///< [out] original file name.
	h5_size_t len_fname,		///< [in]  max length of file name.
	h5_size_t* const fsize		///< [out] size of original file.
	) {
	H5_API_ENTER (h5_err_t,
		      "idx=%llu, fname=%p, len_fname=%llu, fsize=%p",
		      (long long unsigned)idx,
		      fname, (long long unsigned)len_fname,
		      fsize);
	H5_API_RETURN (h5_get_attachment_info_by_idx (
			       f, idx, fname, len_fname, fsize));
}

/**
  \ingroup h5hut_model

  Get size of attached file with name \c fname.

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5GetAttachmentInfoByName (
	const h5_file_t f,              ///< [in]  file handle.
	char* const fname,		///< [in]  original file name.
	h5_size_t* const fsize		///< [out] size of original file.
	) {
	H5_API_ENTER (h5_err_t, "fname='%s', fsize=%p", fname, fsize);
	H5_API_RETURN (h5_get_attachment_info_by_name (
			       f, fname, fsize));
}

/**
  \ingroup h5hut_model

  Attach file \c fname.

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5AddAttachment (
	const h5_file_t f,		///< [in]  file handle.
	const char* fname		///< [in]  name of file to attach.
	) {
	H5_API_ENTER (h5_err_t, "fname='%s'", fname);
	H5_API_RETURN (h5_add_attachment (f, fname));
}

/**
  \ingroup h5hut_model

  Get attachment \c fname from H5hut file and write it to disk in
  the current working directory.

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5GetAttachment (
	const h5_file_t f,              ///< [in]  file handle.
	const char* fname               ///< [in]  name of attachment.
	) {
	H5_API_ENTER (h5_err_t, "fname='%s'", fname);
	H5_API_RETURN (h5_get_attachment (f, fname));
}

/**
  \ingroup h5hut_model

  Delete attachment \c fname.

  \return   \c H5_SUCCESS on success
  \return   \c H5_FAILURE on error
*/
static inline h5_err_t
H5DeleteAttachment (
	const h5_file_t f,              ///< [in]  file handle.
	const char* const fname         ///< [in]  name of attachment.
	) {
	H5_API_ENTER (h5_err_t, "fname='%s'", fname);
	H5_API_RETURN (h5_delete_attachment (f, fname));
}

#ifdef __cplusplus
}
#endif

#endif


