/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#include "h5_private.h"

#include "h5core/h5.h"
#include "h5core/h5_debug.h"
#include "h5core/h5_model.h"

/* H5hut data model */

#define h5_hasstep F77_NAME(                  \
                h5_hasstep,                   \
                h5_hasstep_,                  \
                H5_HASSTEP)
int
h5_hasstep (
	const h5_int64_t* f,
	const h5_int64_t* stepno
	) {

	h5_file_t fh = h5_filehandlefor2c(f);
	H5_API_ENTER (
                int,
                "f=%p, stepno=%lld",
                (h5_file_p)fh, (long long int)stepno);
	H5_API_RETURN (h5_has_step (fh, *stepno));
}

#define h5_setstep F77_NAME(                    \
                h5_setstep,                     \
                h5_setstep_,                    \
                H5_SETSTEP)
h5_int64_t
h5_setstep (
	const h5_int64_t *f,
	h5_int64_t *step ) {

	h5_file_t fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_int64_t, "f=%p, step=%lld", (h5_file_p)fh, (long long)*step);
	H5_API_RETURN (h5_set_step (fh, *step));
}

#define h5_getstep F77_NAME(                    \
                h5_getstep,                     \
                h5_getstep_,                    \
                H5_GETSTEP)
h5_int64_t
h5_getstep (
	const h5_int64_t *f
	) {

	h5_file_t fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_int64_t, "f=%p", (h5_file_p)fh);
	H5_API_RETURN (h5_get_step (fh) + 1);
}

#define h5_getnsteps F77_NAME(                  \
                h5_getnsteps,                   \
                h5_getnsteps_,                  \
                H5_GETNSTEPS)
h5_int64_t
h5_getnsteps (
	const h5_int64_t *f
	) {

	h5_file_t fh = h5_filehandlefor2c(f);
	H5_API_ENTER (h5_int64_t, "f=%p", (h5_file_p)fh);
	H5_API_RETURN (h5_get_num_steps (fh));
}
