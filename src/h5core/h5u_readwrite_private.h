/*
  Copyright (c) 2006-2012, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5U_READWRITE_PRIVATE_H
#define __H5U_READWRITE_PRIVATE_H

#include "h5core/h5_types.h"

h5_err_t
h5upriv_open_file (
	const h5_file_p f
	);

h5_err_t
h5upriv_close_file (
	const h5_file_p f
	);
#endif
