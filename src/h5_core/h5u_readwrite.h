#ifndef __U_READWRITE_H
#define __U_READWRITE_H

h5_int64_t
H5U_get_num_elems (
	h5_file *f
	);

h5_int64_t
H5U_read_elems (
	h5_file *f,
	const char *name,
	void *array,
	const hid_t type
	);

h5_int64_t
H5U_set_num_elements (
	h5_file *f,
	h5_int64_t nparticles
	);

h5_int64_t
H5U_write_data (
	h5_file *f,
	const char *name,
	const void *array,
	const hid_t type
	);

h5_int64_t
h5u_has_view (
	h5_file *f
	);

h5_int64_t
H5U_reset_view (
	h5_file *f
	);

h5_int64_t
H5U_set_view (
	h5_file *f,
	h5_int64_t start,
	h5_int64_t end
	);

h5_int64_t 
H5U_get_view (
	h5_file *f,
	h5_int64_t *start,
	h5_int64_t *end
	);

h5_int64_t
H5U_set_canonical_view (
	h5_file *f
	);

h5_int64_t
H5U_get_dataset_info (
	h5_file *f,
	const h5_int64_t idx,
	char *dataset_name,
	const h5_int64_t len_dataset_name,
	h5_int64_t *type,
	h5_int64_t *nelem
	);
#endif
