#ifndef __H5_TYPES_H
#define __H5_TYPES_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>

#define H5_O_RDWR		0
#define H5_O_RDONLY		1
#define H5_O_WRONLY		2
#define H5_O_APPEND		3

extern const char * const H5_O_MODES[];

#ifdef   WIN32
typedef __int64			int64_t;
#endif /* WIN32 */

typedef int64_t			h5_int64_t;
typedef int32_t			h5_int32_t;
typedef int64_t			h5_id_t;
typedef int64_t			h5_size_t;	/* size in number of elements */
typedef int64_t			h5_ssize_t;	/* size in number of elements */
typedef int64_t			h5_err_t;

typedef double			h5_float64_t;
typedef float			h5_float32_t;

struct h5_complex {
	h5_float64_t		r,i;
};
typedef struct h5_complex	h5_complex_t;

struct h5_file;
typedef h5_err_t (*h5_errorhandler_t)(
	struct h5_file * const,
	const char*,
	va_list ap );

#ifndef PARALLEL_IO
typedef unsigned long		MPI_Comm;
#endif

struct smap;
typedef struct smap smap_t;
struct idmap;

/**
   \struct h5_file

   This is an essentially opaque datastructure that
   acts as the filehandle for all practical purposes.
   It is created by H5PartOpenFile<xx>() and destroyed by
   H5PartCloseFile().  
*/
struct h5_file {
	hid_t	file;			/* file id -> fid		*/
	unsigned mode;			/* file access mode		*/
	int	empty;

	h5_err_t	__errno;	/* error number			*/
	const char *	__funcname;	/* H5Block/Fed/Part API function*/

	/* MPI */

	MPI_Comm comm;			/* MPI communicator		*/
	int	nprocs;			/* number of processors		*/
	int	myproc;			/* The index of the processor	
					   this process is running on.	*/

	/* HDF5 */
	hid_t	xfer_prop;		/* file transfer properties	*/
	hid_t	create_prop;		/* file create properties	*/
	hid_t	access_prop;		/* file access properties	*/


	hid_t	root_gid;		/* id of root group		*/
	hid_t	step_gid;		/* id of current step		*/

	/* step internal data						*/
	char	*prefix_step_name;	/* Prefix of step name		*/
	int	width_step_idx;		/* pad step index with 0 up to this */
	char	step_name[128];		/* full step name		*/
	h5_int64_t step_idx;		/* step index			*/
	int	is_new_step;

	/*
	  BEGIN unstructured stuff,
	  should be moved to struct h5u_fdata
	*/

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

	/* END unstructured */
	
	struct h5u_fdata *u;
	struct h5b_fdata *b;
	struct h5t_fdata *t;
};

typedef struct h5_file h5_file_t;

enum h5_oid {		/* enum with number of vertices(!) */
	H5_OID_VERTEX = 1,
	H5_OID_EDGE = 2,
	H5_OID_TRIANGLE = 3,
	H5_OID_TETRAHEDRON = 4
};
typedef enum h5_oid h5_oid_t;

#define H5_MAX_VERTICES_PER_ENTITY H5_OID_TETRAHEDRON

#endif
