/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5BLOCK_H
#define __H5BLOCK_H

/**
  \ingroup h5block_model

  \note
  Different field sizes are allowed in the same time-step.

  \note
  The same layout can be used, if the size of the field matches the
  size of the layout.  If the size of the layout doesn't match the
  size of the field, an error will be indicated. 
 
  \note In write mode views might be reduced to make them
  non-overlaping, i.e. ghost-zones are eliminated. This may shrink
  views in an unexpected way.

  \todo
  check whether layout is reasonable
*/

#include "H5Block_attribs.h"
#include "H5Block_model.h"
#include "H5Block_io.h"

#endif

