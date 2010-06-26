#ifndef __H5U_TYPES_PRIVATE_H
#define __H5U_TYPES_PRIVATE_H

struct h5u_fdata {
	hsize_t nparticles;		/* -> u.nparticles */
	
	h5_int64_t viewstart; /* -1 if no view is available: A "view" looks */
	h5_int64_t viewend;   /* at a subset of the data. */
	char viewindexed; /* flag whether this view is a list of indices */
  
	hid_t shape;
	hid_t diskshape;
	hid_t memshape;

	hid_t dcreate_prop;
};
typedef struct h5u_fdata h5u_fdata_t;
#endif
