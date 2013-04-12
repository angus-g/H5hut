#ifndef __H5_PRIVATE_H
#define __H5_PRIVATE_H

#include "h5_types_private.h"
#include "h5_errorhandling_private.h"

static inline h5_err_t
check_filehandle (
	const h5_file_p f
	) {
	if (f == NULL || f->file < 0 || f->u == NULL || f->b == NULL) {
		return h5_error (
			H5_ERR_BADF,
			"Called with bad filehandle.");
	}
	return H5_SUCCESS;
}

#define CHECK_FILEHANDLE(f)   \
        TRY (check_filehandle (f));

#endif
