/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5_ATTRIBS_PRIVATE_H
#define __H5_ATTRIBS_PRIVATE_H

#include "h5_types_private.h"

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

h5_err_t
h5priv_get_attrib_info (
	const hid_t id,
	const h5_size_t attrib_idx,
	char* attrib_name,
	const h5_size_t len_attrib_name,
	h5_int64_t* attrib_type,
	h5_size_t* attrib_nelem
	);
#endif
