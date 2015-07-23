/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5CORE_H5_ATTRIBS_H
#define __H5CORE_H5_ATTRIBS_H

#include "h5core/h5_types.h"

#ifdef __cplusplus
extern "C" {
#endif

h5_err_t
h5_read_file_attrib (
	const h5_file_t, const char*, const hid_t, void*);

h5_err_t
h5_read_step_attrib (
	const h5_file_t, const char*, const hid_t, void*);

h5_err_t
h5_write_file_attrib (
	const h5_file_t, const char*, const hid_t, const void*, const hsize_t);

h5_err_t
h5_write_step_attrib (
	const h5_file_t, const char*, const hid_t, const void*, const hsize_t);

h5_err_t
h5_get_file_attrib_info_by_name (
	const h5_file_t, const char* const, h5_int64_t* const, h5_size_t*);

h5_err_t
h5_get_file_attrib_info_by_idx (
	const h5_file_t, const h5_size_t, char*, const h5_size_t, h5_int64_t* const,
	h5_size_t*);

h5_err_t
h5_get_step_attrib_info_by_name (
	const h5_file_t, const char* const, h5_int64_t*, h5_size_t*);

h5_err_t
h5_get_step_attrib_info_by_idx (
	const h5_file_t, const h5_size_t, char*, const h5_size_t, h5_int64_t*,
	h5_size_t*);

h5_ssize_t
h5_get_num_file_attribs (
	const h5_file_t);

h5_ssize_t
h5_get_num_step_attribs (
	const h5_file_t);

#ifdef __cplusplus
}
#endif

#endif
