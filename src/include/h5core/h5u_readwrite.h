#ifndef __H5U_READWRITE_H
#define __H5U_READWRITE_H

#ifdef __cplusplus
extern "C" {
#endif

h5_int64_t
h5u_read_data (
	h5_file_t *f,
	const char *name,
	void *array,
	const hid_t type
	);

h5_int64_t
h5u_write_data (
	h5_file_t *f,
	const char *name,
	const void *array,
	const hid_t type
	);

#ifdef __cplusplus
}
#endif

#endif

