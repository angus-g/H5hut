#ifndef __H5U_MODEL_H
#define __H5U_MODEL_H

h5_int64_t
h5u_get_num_particles (
	h5_file_t *f
	);

h5_int64_t
h5u_set_num_particles (
	h5_file_t *f,
	h5_int64_t nparticles,
	h5_int64_t stride
	);

h5_int64_t
h5u_has_view (
	h5_file_t *f
	);

h5_int64_t
h5u_reset_view (
	h5_file_t *f
	);

h5_int64_t
h5u_set_view (
	h5_file_t *f,
	h5_int64_t start,
	h5_int64_t end
	);

h5_int64_t
h5u_set_view_indices (
	h5_file_t *f,
	const h5_int64_t *indices,
	h5_int64_t nelems
	);

h5_int64_t 
h5u_get_view (
	h5_file_t *f,
	h5_int64_t *start,
	h5_int64_t *end
	);

h5_int64_t
h5u_set_canonical_view (
	h5_file_t *f
	);

h5_int64_t
h5u_get_num_datasets (
	h5_file_t *f
	);

h5_int64_t
h5u_get_dataset_info (
	h5_file_t *f,
	const h5_int64_t idx,
	char *dataset_name,
	const h5_int64_t len_dataset_name,
	h5_int64_t *type,
	h5_int64_t *nelem
	);

h5_err_t
h5u_set_chunk (
        h5_file_t *f,
        h5_size_t size
        );

#endif

