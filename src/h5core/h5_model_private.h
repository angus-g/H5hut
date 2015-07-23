/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5_MODEL_PRIVATE_H
#define __H5_MODEL_PRIVATE_H


/* WARNING! Changing these values will alter the data model and introduce
 * file incompatibilities with previous versions. */

#define H5_DATANAME_LEN		H5_MAX_NAME_LEN
#define H5_STEPNAME_LEN		H5_MAX_NAME_LEN
#define H5_STEPNAME		"Step"
#define H5_STEPWIDTH		1
#define H5BLOCK_GROUPNAME_BLOCK	"Block"
#define H5_BLOCKNAME_X		"0"
#define H5_BLOCKNAME_Y		"1"
#define H5_BLOCKNAME_Z		"2"
#define H5_ATTACHMENT		"Attachment"

#include "h5core/h5_types.h"
#include "h5core/h5_model.h"


h5_err_t
h5priv_start_throttle (
	const h5_file_p);

h5_err_t
h5priv_end_throttle (
	const h5_file_p);

h5_err_t
h5priv_close_step (
	const h5_file_p f
	);

#endif
