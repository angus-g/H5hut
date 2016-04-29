#ifndef __PRIVATE_H5_H
#define __PRIVATE_H5_H

#include "private/h5_types.h"
#include "private/h5_debug.h"
#include "private/h5_errorhandling.h"

#define UNUSED_ARGUMENT(x) (void)x

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

#define is_writable(f) (f->props->flags & (H5_O_RDWR | H5_O_WRONLY | H5_O_APPENDONLY))
#define is_readable(f) (f->props->flags & (H5_O_RDWR | H5_O_RDONLY))
#define is_readonly(f) (f->props->flags & H5_O_RDONLY)
#define is_appendonly(f) (f->props->flags & H5_O_APPENDONLY)

#define CHECK_WRITABLE_MODE(f)                                          \
	TRY (is_writable (f) ? H5_SUCCESS : h5_error (                  \
                     H5_ERR_INVAL,                                      \
                     "Attempting to write to read-only file"));

#define CHECK_TIMEGROUP(f)                                             \
	TRY ((f->step_gid > 0) ? H5_SUCCESS : h5_error (               \
                     H5_ERR_INVAL,                                     \
                     "Time step is invalid! Have you set the time step?"));



#endif
