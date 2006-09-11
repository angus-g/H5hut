#ifndef __H5BLOCKTYPES_H
#define __H5BLOCKTYPES_H

struct H5BlockPartition {
	h5part_int64_t	i_start;
	h5part_int64_t	i_end;
	h5part_int64_t	j_start;
	h5part_int64_t	j_end;
	h5part_int64_t	k_start;
	h5part_int64_t	k_end;
};

struct H5BlockStruct {
	h5part_int64_t timestep;
	h5part_int64_t i_max;
	h5part_int64_t j_max;
	h5part_int64_t k_max;
	struct H5BlockPartition *user_layout;
	struct H5BlockPartition *write_layout;

	hid_t shape;
	hid_t memshape;
	hid_t diskshape;
	hid_t blockgroup;
	hid_t field_group_id;
};

#define H5PART_ERR_LAYOUT	-100
#define H5PART_ERR_NOENT	-101

#endif
