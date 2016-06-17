#ifndef __PRIVATE_H5_FILE_H
#define __PRIVATE_H5_FILE_H

#include "private/h5_types.h"
#include "private/h5_log.h"
#include "private/h5_err.h"
#include "h5core/h5_file.h"

#define H5_VFD_MPIO_POSIX       0x00000010
#define H5_VFD_MPIO_INDEPENDENT 0x00000020
#define H5_VFD_MPIO_COLLECTIVE  0x00000040
#define H5_VFD_CORE		0x00000080

#define H5_FLUSH_FILE		0x00001000
#define H5_FLUSH_STEP		0x00002000
#define H5_FLUSH_DATASET	0x00004000

#define H5_FS_LUSTRE		0x00010000

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
