#ifndef __H5_OPENCLOSE_H
#define __H5_OPENCLOSE_H

h5_file_t *
h5_open_file (
	const char *filename,
	unsigned flags,
	MPI_Comm comm
	);

h5_int64_t
h5_check_filehandle (
	const h5_file_t *f
	);

h5_int64_t
h5_close_file (
	h5_file_t *f
	);

h5_int64_t
h5_define_stepname_fmt (
	h5_file_t *f,
	const char *name,
	const h5_int64_t width
	);

h5_err_t
h5_get_stepname_fmt (
	h5_file_t *f,
	char *name,
	const h5_size_t l_name,
	h5_size_t *width
	);

h5_err_t
_h5_close_step (
	h5_file_t *f
	);

h5_int64_t
h5_has_step (
	h5_file_t * f,
	h5_int64_t step
	);

h5_int64_t
h5_get_step (
	h5_file_t *f
	);

#endif
