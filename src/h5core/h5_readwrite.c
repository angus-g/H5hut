/*
  Copyright (c) 2006-2012, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "h5core/h5.h"
#include "h5_types_private.h"

#include "h5_errorhandling_private.h"
#include "h5_hdf5_private.h"
#include "h5_model_private.h"
#include "h5_readwrite_private.h"

/*!
   Write data to dataset.

   - Open/Create dataset
   - set hyperslabs for disk and memory via callback functions
   - Write data
   - Close dataset
 */
h5_err_t
h5priv_write_dataset_by_name (
		h5t_mesh_t* const m,
		const h5_file_p f,
        const hid_t loc_id,
        h5_dsinfo_t* dsinfo,
        hid_t (*set_memspace)(h5t_mesh_t*,hid_t),
        hid_t (*set_diskspace)(h5t_mesh_t*,hid_t),
        const void* const data
        ) {
	H5_PRIV_API_ENTER (h5_err_t,
	                   "m=%p, f=%p, loc_id=%d (%s), dsinfo=%p, set_memspace=%p, "
	                   "set_diskspace=%p, data=%p",
	                   m, f, loc_id, hdf5_get_objname(loc_id),
	                   dsinfo,
	                   set_memspace, set_diskspace, data);
	h5_info ("Writing dataset %s/%s.",
	         hdf5_get_objname (loc_id), dsinfo->name);

	h5_err_t exists;
	TRY (exists = hdf5_link_exists (loc_id, dsinfo->name));
	if ((exists > 0) && ((f->props->mode==H5_O_WRONLY) || (f->props->mode==H5_O_APPEND))) {
		h5_warn ("Dataset %s/%s already exist.",
		         hdf5_get_objname (loc_id), dsinfo->name);
		H5_PRIV_API_LEAVE (h5priv_handle_file_mode_error(f->props->mode));
	}

	/*
	   open/create dataset
	 */

	hid_t dset_id;
	hid_t dataspace_id;
	hid_t diskspace_id;
	hid_t memspace_id;

	if (exists) {
		/* overwrite dataset */
		TRY (dset_id = hdf5_open_dataset (loc_id, dsinfo->name));
		TRY (dataspace_id = hdf5_get_dataset_space (dset_id));
		TRY (hdf5_set_dataset_extent (dset_id, dsinfo->dims));
		/* exten dataset? */
	} else {
		/* create dataset */
		TRY (dataspace_id = hdf5_create_dataspace (
		             dsinfo->rank,
		             dsinfo->dims,
		             dsinfo->max_dims));
		TRY (dset_id = hdf5_create_dataset (
		             loc_id,
		             dsinfo->name,
		             dsinfo->type_id,
		             dataspace_id,
		             dsinfo->create_prop));
	}
	TRY (memspace_id = (*set_memspace)(m, 0));
	TRY (diskspace_id = (*set_diskspace)(m, dataspace_id));
#ifdef PARALLEL_IO
	TRY (h5priv_start_throttle (f));
#endif
	TRY (hdf5_write_dataset (
	             dset_id,
	             dsinfo->type_id,
	             memspace_id,
	             diskspace_id,
	             f->props->xfer_prop,
	             data));
#ifdef PARALLEL_IO
	TRY (h5priv_end_throttle (f));
#endif
	TRY (hdf5_close_dataspace (diskspace_id));
	TRY (hdf5_close_dataspace (memspace_id));
	TRY (hdf5_close_dataset (dset_id));
	f->empty = 0;

	H5_PRIV_API_RETURN (H5_SUCCESS);
}



/*!
   Write data to dataset.

   - Check existance dataset
   - Write data
	is needed if dset, mspace, dspace can't be set by the callback functions above
 */
h5_err_t
h5priv_write_dataset_by_name_id (
	 	const h5_file_p f,
        const hid_t loc_id,
        h5_dsinfo_t* dsinfo,
        hid_t dset_id,
        hid_t memspace_id,
        hid_t diskspace_id,
        const void* const data
        ) {
	H5_PRIV_API_ENTER (h5_err_t,
	                   "f=%p, loc_id=%d (%s), dsinfo=%p, dset_id=%d, memspace_id=%d, "
	                   "diskspace_id=%d, data=%p",
	                   f, loc_id, hdf5_get_objname(loc_id),
	                   dsinfo,
	                   dset_id,
	                   memspace_id, diskspace_id, data);
	h5_info ("Writing dataset %s/%s.",
	         hdf5_get_objname (loc_id), dsinfo->name);

	h5_err_t exists;
	TRY (exists = hdf5_link_exists (loc_id, dsinfo->name));
	if ((exists > 0) && ((f->props->mode==H5_O_WRONLY) || (f->props->mode==H5_O_APPEND))) {
		h5_warn ("Dataset %s/%s already exist.",
		         hdf5_get_objname (loc_id), dsinfo->name);
		H5_PRIV_API_LEAVE (h5priv_handle_file_mode_error(f->props->mode));
	}

#ifdef PARALLEL_IO
	TRY (h5priv_start_throttle (f));
#endif
	TRY (hdf5_write_dataset (
	             dset_id,
	             dsinfo->type_id,
	             memspace_id,
	             diskspace_id,
	             f->props->xfer_prop,
	             data));
#ifdef PARALLEL_IO
	TRY (h5priv_end_throttle (f));
#endif


	H5_PRIV_API_RETURN (H5_SUCCESS);
}
