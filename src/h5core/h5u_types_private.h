#ifndef __H5U_TYPES_PRIVATE_H
#define __H5U_TYPES_PRIVATE_H

struct h5u_fdata {
	hsize_t nparticles;		/* -> u.nparticles */
	
	h5_int64_t viewstart; /* -1 if no view is available: A "view" looks */
	h5_int64_t viewend;   /* at a subset of the data. */
  
	/**
	   the number of particles in each processor.
	   With respect to the "VIEW", these numbers
	   can be regarded as non-overlapping subsections
	   of the particle array stored in the file.
	   So they can be used to compute the offset of
	   the view for each processor
	*/
	h5_int64_t *pnparticles;

	hid_t shape;
	hid_t diskshape;
	hid_t memshape;
};
typedef struct h5u_fdata h5u_fdata_t;
#endif
