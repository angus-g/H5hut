#ifndef __OPENCLOSE_H
#define __OPENCLOSE_H

H5PartFile*
H5_open_file (
	const char *filename,
	unsigned flags,
	MPI_Comm comm,
	int f_parallel
	);

h5part_int64_t
H5_check_filehandle (
	const H5PartFile *f
	);

H5PartFile*
H5_open_file (
	const char *filename,
	unsigned flags,
	MPI_Comm comm,
	int f_parallel
	);

h5part_int64_t
H5_close_file (
	H5PartFile *f
	);

h5part_int64_t
H5_define_stepname (
	H5PartFile *f,
	const char *name,
	const h5part_int64_t width
	);

#endif
