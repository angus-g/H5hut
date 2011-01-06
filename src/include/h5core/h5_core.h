#ifndef __H5_CORE_H
#define __H5_CORE_H

#define UNUSED_ARGUMENT(x) (void)x

#define TRY( func )						\
	if ((int64_t)(ptrdiff_t)(func) <= (int64_t)H5_ERR)	\
		return H5_ERR;					\

#define TRY2( func )						\
	if ((int64_t)(ptrdiff_t)(func) <= (int64_t)H5_ERR)	\
		goto done;					\


#include "h5_types.h"
#include "h5_errno.h"

#include "h5_attribs.h"
#include "h5_hdf5.h"
#include "h5_maps.h"
#include "h5_openclose.h"
#include "h5_readwrite.h"
#include "h5_syscall.h"

#include "h5u_readwrite.h"
#include "h5u_model.h"

#include "h5b_readwrite.h"
#include "h5b_model.h"
#include "h5b_attribs.h"

#include "h5t_core.h"

#include "h5_errorhandling.h"

#endif
