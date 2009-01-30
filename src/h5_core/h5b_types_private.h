#ifndef __H5B_TYPES_PRIVATE_H
#define __H5B_TYPES_PRIVATE_H

struct h5b_partition {
	h5_int64_t	i_start;
	h5_int64_t	i_end;
	h5_int64_t	j_start;
	h5_int64_t	j_end;
	h5_int64_t	k_start;
	h5_int64_t	k_end;
};

struct h5b_fdata {
	h5_id_t step_idx;
	h5_size_t i_max;
	h5_size_t j_max;
	h5_size_t k_max;
	struct h5b_partition *user_layout;
	struct h5b_partition *write_layout;
	int have_layout;
	h5_size_t chunk[3];

	hid_t shape;
	hid_t memshape;
	hid_t diskshape;
	hid_t blockgroup;
	hid_t field_group_id;
};
typedef struct h5b_fdata h5b_fdata_t;
#endif
