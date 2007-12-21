#ifndef __H5_TYPES_H
#define __H5_TYPES_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>


#ifdef   WIN32
typedef __int64			int64_t;
#endif /* WIN32 */

typedef int64_t			h5_int64_t;
typedef	h5_int64_t		h5_int_t;
typedef	h5_int64_t		h5part_int64_t;
typedef int32_t			h5_id_t;
typedef int32_t			h5_size_t;	/* size in number of elements */

typedef double			h5_float64_t;
typedef	h5_float64_t		h5_float_t;
typedef	h5_float64_t		h5part_float64_t;

struct h5_complex {
	h5_float_t		r,i;
};
typedef struct h5_complex	h5_complex;

typedef h5_int64_t (*h5_error_handler)( const char*, const h5_int64_t, const char*,...)
#ifdef __GNUC__
__attribute__ ((format (printf, 3, 4)))
#endif
 ;

typedef h5_error_handler	h5part_error_handler;

#ifndef PARALLEL_IO
typedef unsigned long		MPI_Comm;
#endif

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

typedef struct h5_file h5_file;

#endif
