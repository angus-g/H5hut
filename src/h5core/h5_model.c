/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include <string.h>

#include "h5core/h5.h"
#include "h5_types_private.h"
#include "h5_hdf5_private.h"
#include "h5_model_private.h"
#include "h5_mpi_private.h"
#include "h5_readwrite_private.h"
#include "h5_va_macros.h"

h5_err_t
h5priv_close_step (
	const h5_file_p f
	) {
	H5_PRIV_API_ENTER (h5_err_t, "f=%p", f);
	if (f->step_gid <= 0)
		H5_PRIV_API_LEAVE (H5_SUCCESS);
	TRY (hdf5_close_group (f->step_gid));

	f->step_gid = -1;

	H5_PRIV_API_RETURN (H5_SUCCESS);
}

h5_err_t
h5_set_step (
	const h5_file_t f_,		/*!< [in]  Handle to open file */
	const h5_id_t step_idx		/*!< [in]  Step to set. */
	) {
        h5_file_p f = (h5_file_p)f_;
	H5_CORE_API_ENTER (h5_err_t,
                           "f=%p, step_idx=%lld",
                           f, (long long)step_idx);
	TRY (h5priv_close_step (f));
	f->step_idx = step_idx;

	sprintf (
		f->step_name,
		"%s#%0*lld",
		f->props->prefix_step_name, f->props->width_step_idx,
                (long long) f->step_idx);
	h5_info (
		"Open step #%lld for file %lld",
		(long long)f->step_idx,
		(long long)(size_t) f);
	
	TRY (f->step_gid = h5priv_open_group (is_writable(f),
                                              f->file,
                                              f->step_name));
	H5_CORE_API_RETURN (H5_SUCCESS);
}

/*!
   Normalize HDF5 type
*/
h5_int64_t
h5priv_normalize_h5_type (
	hid_t type
	) {
	H5_CORE_API_ENTER (h5_int64_t, "type=%d", type);
	H5T_class_t tclass;
	int size;
	TRY (tclass = H5Tget_class (type));
	TRY (size = H5Tget_size (type));

	switch (tclass){
	case H5T_INTEGER:
		if (size==8) {
			H5_CORE_API_LEAVE (H5_INT64_T);
		} else if (size==4) {
		        H5_CORE_API_LEAVE (H5_INT32_T);
		} else if (size==2) {
		        H5_CORE_API_LEAVE (H5_INT16_T);
		}
		break;
	case H5T_FLOAT:
		if ( size==8 ) {
			H5_CORE_API_LEAVE (H5_FLOAT64_T);
		}
		else if ( size==4 ) {
			H5_CORE_API_LEAVE (H5_FLOAT32_T);
		}
		break;
	case H5T_STRING:
		H5_CORE_API_LEAVE (H5_STRING_T);
	default:
		; /* NOP */
	}
	H5_CORE_API_RETURN (h5_warn ("Unknown type %d", (int)type));
}

h5_int64_t
h5priv_get_dataset_type(
	const hid_t group_id,
	const char* dset_name
	) {
	H5_CORE_API_ENTER (h5_int64_t,
			   "group_id=%d, dset_name='%s'",
			   group_id, dset_name);
	hid_t dset_id;
	hid_t hdf5_type;
	h5_int64_t type;
	TRY (dset_id = hdf5_open_dataset (group_id, dset_name));
	TRY (hdf5_type = hdf5_get_dataset_type (dset_id));
	TRY (type = h5priv_normalize_h5_type (hdf5_type));
	TRY (hdf5_close_type (hdf5_type));
	TRY (hdf5_close_dataset (dset_id));

	H5_CORE_API_RETURN (type);
}

/*
  returns:
  TRUE (value > 0): if step exists
  FALSE (i.e. 0): if step does not exist
  H5_FAILURE: on error
 */
h5_err_t
h5_has_step (
	const h5_file_t f_,		/*!< [in]  Handle to open file */
	const h5_id_t step_idx		/*!< [in]  Step number to query */
	) {
        h5_file_p f = (h5_file_p)f_;
	H5_CORE_API_ENTER (h5_err_t, "f=%p, step_idx=%lld", f, (long long)step_idx);
	char name[2*H5_STEPNAME_LEN];
	sprintf (name,
		"%s#%0*lld",
		f->props->prefix_step_name, f->props->width_step_idx,
		 (long long)step_idx);
        h5_err_t exists;
        TRY (exists = hdf5_link_exists (f->file, name));
	H5_CORE_API_RETURN (exists);
}

h5_err_t
h5priv_normalize_dataset_name (
	const char *name,
	char *name2
	) {
	H5_CORE_API_ENTER (h5_err_t, "name='%s', name2='%p'", name, name2);
	if ( strlen(name) > H5_DATANAME_LEN-1 ) {
		strncpy ( name2, name, H5_DATANAME_LEN-1 );
		name2[H5_DATANAME_LEN-1] = '\0';
		h5_warn ("Truncated name '%s' to '%s'.", name, name2);
	} else {
		strcpy ( name2, name );
	}

	if ( strcmp( name2, H5BLOCK_GROUPNAME_BLOCK ) == 0 ) {
		H5_CORE_API_LEAVE (
			h5_error (
				H5_ERR_INVAL,
				"Can't create dataset or field with name '%s'"
				" because it is reserved by H5Block.",
				H5BLOCK_GROUPNAME_BLOCK));
	}
	H5_CORE_API_RETURN (H5_SUCCESS);
}

