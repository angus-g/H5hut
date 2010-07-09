#ifndef __H5_OPENCLOSE_H
#define __H5_OPENCLOSE_H

h5_file_t *
h5_open_file (
	const char *filename,
	h5_int32_t flags,
	MPI_Comm comm,
	const char *funcname
	);

h5_int64_t
h5_check_filehandle (
	h5_file_t * const f
	);

h5_int64_t
h5_close_file (
	h5_file_t * const f
	);

h5_int64_t
h5_set_stepname_fmt (
	h5_file_t * const f,
	const char *name,
	int width
	);

h5_err_t
h5_get_stepname_fmt (
	h5_file_t * const f,
	char *name,
	int l_name,
	int *width
	);

h5_err_t
h5priv_close_step (
	h5_file_t * const f
	);

int
h5_get_num_procs (
	h5_file_t* const f
	);

hid_t
h5_get_hdf5_file(
	h5_file_t* const f
	);

h5_size_t
h5_get_num_steps (
	h5_file_t* const f
	);

h5_int64_t
h5_has_step (
	h5_file_t * const f,
	h5_int64_t step
	);

h5_int64_t
h5_get_step (
	h5_file_t * const f
	);

h5_err_t
h5_start_traverse_steps (
	h5_file_t * f			/*!< file handle		*/
	);

h5_err_t
h5_traverse_steps (
	h5_file_t * f			/*!< file handle		*/
	);

#endif
