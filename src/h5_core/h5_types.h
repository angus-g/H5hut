#ifndef __H5_TYPES_H
#define __H5_TYPES_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>

#define H5_SUCCESS		0
#define H5_ERR			-1
#define H5_ERR_BADF		-9
#define H5_ERR_NOMEM		-12
#define H5_ERR_INVAL		-22
#define H5_ERR_BADFD		-77

#define H5_ERR_LAYOUT		-100
#define H5_ERR_NOENT		-101

#define H5_ERR_INIT		-200
#define H5_ERR_NOENTRY		-201

#define H5_ERR_MPI		-201
#define H5_ERR_HDF5		-202
#define H5_ERR_INTERNAL		-253
#define H5_ERR_NOT_IMPLEMENTED	-254

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
typedef int32_t			h5_id_t;
typedef int32_t			h5_size_t;	/* size in number of elements */
typedef int32_t			h5_err_t;

typedef double			h5_float64_t;

struct h5_complex {
	h5_float64_t		r,i;
};
typedef struct h5_complex	h5_complex_t;


typedef h5_err_t (*h5_error_handler)( const char*, const h5_err_t, const char*,...)
#ifdef __GNUC__
	__attribute__ ((format (printf, 3, 4)))
#endif
	;

typedef h5_err_t (*h5_verror_handler)(
	const char*,
	const h5_err_t,
	const char*,
	va_list ap );

#ifndef PARALLEL_IO
typedef unsigned long		MPI_Comm;
#endif

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

struct H5BlockPartition {
	h5_int64_t	i_start;
	h5_int64_t	i_end;
	h5_int64_t	j_start;
	h5_int64_t	j_end;
	h5_int64_t	k_start;
	h5_int64_t	k_end;
};

struct h5b_fdata {
	h5_int64_t step_idx;
	h5_int64_t i_max;
	h5_int64_t j_max;
	h5_int64_t k_max;
	struct H5BlockPartition *user_layout;
	struct H5BlockPartition *write_layout;
	int have_layout;

	hid_t shape;
	hid_t memshape;
	hid_t diskshape;
	hid_t blockgroup;
	hid_t field_group_id;
};


/**
   \struct h5_file

   This is an essentially opaque datastructure that
   acts as the filehandle for all practical purposes.
   It is created by H5PartOpenFile<xx>() and destroyed by
   H5PartCloseFile().  
*/
struct h5_file {
	hid_t	file;			/* file id -> fid		*/
	int	empty;

	unsigned mode;			/* file access mode		*/
	hid_t	xfer_prop;
	hid_t	create_prop;
	hid_t	access_prop;

	hid_t	root_gid;		/* id of group "/"		*/

	int	nprocs;			/* number of processors		*/
	int	myproc;			/* The index of the processor	
					   this process is running on.	*/
	MPI_Comm comm;			/* MPI communicator		*/

	/* step internal data						*/
	char	*prefix_step_name;	/* Prefix of step name		*/
	int	width_step_idx;		/* pad step index with 0 up to this */

	char	step_name[128];		/* full step name		*/
       
	h5_int64_t step_idx;		/* step index			*/
	hid_t	step_gid;		/* HDF5 grp id of current step	*/

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
	struct h5b_fdata *block;
	struct h5t_fdata *t;
};

typedef struct h5_file h5_file_t;

struct smap {
	h5_size_t	size;		/* allocated space in number of items */
	h5_size_t	num_items;	/* stored items	*/
	h5_id_t		*items;
};

struct idmap {
	h5_size_t	size;		/* allocated space in number of items */
	h5_size_t	num_items;	/* stored items	*/
	struct {
		h5_id_t	global_id;
		h5_id_t	local_id;
	}		*items;
};

enum h5_oid {		/* enum with number of vertices(!) */
	H5_OID_VERTEX = 1,
	H5_OID_EDGE = 2,
	H5_OID_TRIANGLE = 3,
	H5_OID_TETRAHEDRON = 4
};
typedef enum h5_oid h5_oid_t;

#define H5_MAX_VERTICES_PER_ENTITY H5_OID_TETRAHEDRON

#endif
