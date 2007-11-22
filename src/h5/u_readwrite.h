#ifndef __U_READWRITE_H
#define __U_READWRITE_H

h5part_int64_t
H5U_get_num_elems (
	H5PartFile *f
	);

h5part_int64_t
H5U_read_elems (
	H5PartFile *f,
	const char *name,
	void *array,
	const hid_t type
	);

h5part_int64_t
H5U_set_num_elements (
	H5PartFile *f,
	h5part_int64_t nparticles
	);

h5part_int64_t
H5U_write_data (
	H5PartFile *f,
	const char *name,
	const void *array,
	const hid_t type
	);

h5part_int64_t
H5U_reset_view (
	H5PartFile *f
	);

h5part_int64_t
H5U_set_view (
	H5PartFile *f,
	h5part_int64_t start,
	h5part_int64_t end
	);

h5part_int64_t 
H5U_get_view (
	H5PartFile *f,
	h5part_int64_t *start,
	h5part_int64_t *end
	);

h5part_int64_t
H5U_set_canonical_view (
	H5PartFile *f
	);

h5part_int64_t
H5U_get_dataset_info (
	H5PartFile *f,
	const h5part_int64_t idx,
	char *dataset_name,
	const h5part_int64_t len_dataset_name,
	h5part_int64_t *type,
	h5part_int64_t *nelem
	);
#endif
