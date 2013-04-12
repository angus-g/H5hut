/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5_QSORT_PRIVATE_H
#define __H5_QSORT_PRIVATE_H

#include "h5core/h5_types.h"
void
h5priv_qsort_r (
        void *a,
        size_t n,
        size_t es,
        void *thunk,
        int (*compar)(void *, const void *, const void *)
        );

void
h5priv_qsort (
        void *a,
        size_t n,
        size_t es,
        int (*compar)(const void *, const void *)
        );

#endif
