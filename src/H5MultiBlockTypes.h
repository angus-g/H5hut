#ifndef __H5MULTIBLOCKTYPES_H
#define __H5MULTIBLOCKTYPES_H

struct H5MultiBlockStruct {
	h5part_int64_t halo_radii[3];
	h5part_int64_t block_dims[3];
	h5part_int64_t field_dims[3];
	h5part_int64_t decomp[3];
	h5part_int64_t offsets[3];
	char field_edges;
	int read;
	int halo;
	int have_decomp;
};

#define H5PART_ERR_DECOMP	-102

#define H5MB_EDGE_X0 0x01
#define H5MB_EDGE_X1 0x02
#define H5MB_EDGE_Y0 0x04
#define H5MB_EDGE_Y1 0x08
#define H5MB_EDGE_Z0 0x10
#define H5MB_EDGE_Z1 0x20

#endif
