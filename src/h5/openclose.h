#ifndef __OPENCLOSE_H
#define __OPENCLOSE_H

h5_file*
H5_open_file (
	const char *filename,
	unsigned flags,
	MPI_Comm comm
	);

h5part_int64_t
H5_check_filehandle (
	const h5_file *f
	);

h5part_int64_t
H5_close_file (
	h5_file *f
	);

h5part_int64_t
H5_define_stepname (
	h5_file *f,
	const char *name,
	const h5part_int64_t width
	);

h5_err_t
_h5_close_step (
	h5_file *f
	);
#endif
