
/*
  System dependend definitions
*/

#ifndef _H5PARTTYPES_H_
#define _H5PARTTYPES_H_

#include "h5/h5_types.h"

struct h5b_fdata;
struct h5t_fdata;

/**
   \struct h5_file

   This is an essentially opaque datastructure that
   acts as the filehandle for all practical purposes.
   It is created by H5PartOpenFile<xx>() and destroyed by
   H5PartCloseFile().  
*/
struct h5_file {
	hsize_t nparticles;
	
	h5part_int64_t viewstart; /* -1 if no view is available: A "view" looks */
	h5part_int64_t viewend;   /* at a subset of the data. */
  
	/**
	   the number of particles in each processor.
	   With respect to the "VIEW", these numbers
	   can be regarded as non-overlapping subsections
	   of the particle array stored in the file.
	   So they can be used to compute the offset of
	   the view for each processor
	*/
	h5part_int64_t *pnparticles;

	hid_t shape;
	hid_t diskshape;
	hid_t memshape;

	/*
	 */

	hid_t	file;
	unsigned mode;
	hid_t xfer_prop;
	hid_t create_prop;
	hid_t access_prop;

	hid_t	root_id;		/* id of group "/" */
	char	*groupname_step;
	int	stepno_width;
	int	empty;

	char	index_name[128];
       
	h5_int64_t timestep;
	hid_t timegroup;


	/**
	   Number of processors
	*/
	int nprocs;
	
	/**
	   The index of the processor this process is running on.
	*/
	int myproc;

	/**
	   MPI comnunicator
	*/
	MPI_Comm comm;

	struct h5b_fdata *block;
	h5_int64_t (*close_block)(struct h5_file *f);

	struct h5t_fdata *topo;
	h5_int64_t (*close_topo)(struct h5_file *f);
};

typedef struct h5_file H5PartFile;
typedef struct h5_file h5_file;

#ifdef IPL_XT3
# define SEEK_END 2 
#endif

#endif
