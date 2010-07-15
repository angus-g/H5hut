#ifndef __H5B_TYPES_PRIVATE_H
#define __H5B_TYPES_PRIVATE_H

struct h5b_partition {
	h5_size_t	i_start;
	h5_size_t	i_end;
	h5_size_t	j_start;
	h5_size_t	j_end;
	h5_size_t	k_start;
	h5_size_t	k_end;
};

struct h5b_fdata {
	h5_id_t step_idx;
	h5_size_t i_max;
	h5_size_t j_max;
	h5_size_t k_max;
	struct h5b_partition user_layout[1];
	struct h5b_partition write_layout[1];
	int have_layout;

	hid_t shape;
	hid_t memshape;
	hid_t diskshape;
	hid_t block_gid;
	hid_t field_gid;
	hid_t dcreate_prop;

	MPI_Datatype partition_mpi_t;
};
typedef struct h5b_fdata h5b_fdata_t;
typedef struct h5b_partition h5b_partition_t;
#endif
