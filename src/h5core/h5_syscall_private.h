/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5_SYSCALL_PRIVATE_H
#define __H5_SYSCALL_PRIVATE_H

#include "h5core/h5_types.h"

void*
h5priv_tsearch (
        const void* key,
        void** rootp,
        int (*compar)(const void* key1, const void* key2)
        );

void*
h5priv_tfind (
        const void* key,
        void *const* rootp,
        int (*compar)(const void* key1, const void* key2)
        );
#endif
