/*
  Copyright (c) 2006-2016, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __PRIVATE_H5_ATTRIBS_H
#define __PRIVATE_H5_ATTRIBS_H

#include "private/h5_types.h"
#include "private/h5_model.h"
#include "private/h5_hdf5.h"

h5_err_t
h5priv_read_attrib (
	const hid_t id,
	const char* attrib_name,
	const hid_t attrib_type,
	void* const attrib_value
	);

h5_err_t
h5priv_write_attrib (
	const hid_t id,
	const char* attrib_name,
	const hid_t attrib_type,
	const void* attrib_value,
	const hsize_t attrib_nelem,
	const int overwrite
	);

static inline h5_err_t
get_attrib_info (
	hid_t attrib_id,
	h5_int64_t* attrib_type,	/*!< OUT: H5 type of attribute */
	h5_size_t* attrib_nelem		/*!< OUT: number of elements */
	) {
        H5_INLINE_FUNC_ENTER (h5_err_t);
	hid_t mytype;
        TRY (mytype = hdf5_get_attribute_type (attrib_id));

        H5T_class_t type_class;
        TRY (type_class = hdf5_get_class_type (mytype));

	if (attrib_nelem) {
                if (type_class == H5T_STRING) {
                        *attrib_nelem = H5Tget_size(mytype);
                } else {
                        hid_t space_id;
                        TRY (space_id = hdf5_get_attribute_dataspace (attrib_id));
                        TRY (*attrib_nelem = hdf5_get_npoints_of_dataspace (space_id));
                        TRY (hdf5_close_dataspace (space_id));
                }
	}
	if (attrib_type) {
		TRY (*attrib_type = h5priv_normalize_h5_type (mytype));
	}
        TRY (hdf5_close_type (mytype));
	TRY (hdf5_close_attribute (attrib_id));
	H5_INLINE_FUNC_RETURN (H5_SUCCESS);
}

static inline h5_err_t
h5priv_get_attrib_info_by_name (
	const hid_t id,			/*!< IN: HDF5 object ID */
	const char* const attrib_name,	/*!< IN: name of attribute */
	h5_int64_t* attrib_type,	/*!< OUT: H5 type of attribute */
	h5_size_t* attrib_nelem		/*!< OUT: number of elements */
	) {
	H5_PRIV_API_ENTER (h5_err_t,
			   "id=%lld, "
			   "attrib_name=%s,"
			   "attrib_type=%p, attrib_nelem=%p",
			   (long long int)id,
			   attrib_name,
			   attrib_type,
			   attrib_nelem);
	hid_t attrib_id;
        TRY (attrib_id = hdf5_open_attribute (id, attrib_name));
        H5_PRIV_API_RETURN (get_attrib_info (attrib_id, attrib_type, attrib_nelem));
}

h5_err_t
h5priv_get_attrib_info_by_idx (
	const hid_t id,
	const h5_size_t attrib_idx,
	char* attrib_name,
	const h5_size_t len_attrib_name,
	h5_int64_t* attrib_type,
	h5_size_t* attrib_nelem
	);
#endif
