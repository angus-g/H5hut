
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
typedef int32_t			h5_err_t;

typedef double			h5_float64_t;
typedef	h5_float64_t		h5_float_t;
typedef	h5_float64_t		h5part_float64_t;

struct h5_complex {
	h5_float64_t		r,i;
};
typedef struct h5_complex	h5_complex;

struct h5_vertex {  /* 32Byte */
	h5_id_t		id;
	h5_id_t		unused;	/* for right alignment */
	h5_float64_t	P[3];
};

struct h5_edge { /* 16Bytes */
	h5_id_t		id;
	h5_id_t		parent_id;
	h5_id_t		vertex_ids[2];
};

struct h5_triangle { /*24Bytes*/
	h5_id_t		id;
	h5_id_t		parent_id;
	h5_id_t		vertex_ids[3];
	h5_id_t		unused;	/* for alignment */
};

struct h5_tetrahedron { /* 24Bytes */
	h5_id_t		id;
	h5_id_t		parent_id;
	h5_id_t		vertex_ids[4];
};

typedef struct h5_vertex	h5_vertex;
typedef struct h5_edge		h5_edg;
typedef struct h5_triangle	h5_triangle;
typedef struct h5_tetrahedron	h5_tetrahedron;

typedef h5_int64_t (*h5_error_handler)( const char*, const h5_int64_t, const char*,...)
#ifdef __GNUC__
__attribute__ ((format (printf, 3, 4)))
#endif
 ;

typedef h5_error_handler	h5part_error_handler;

#ifndef PARALLEL_IO
typedef unsigned long		MPI_Comm;
#endif

struct h5u_fdata;			/* unstructured data */ 
struct h5b_fdata;			/* block structured data */
struct h5t_fdata;			/* topology data */

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
	int	empty_step;
       
	h5_int64_t step_idx;		/* step index			*/
	hid_t	step_gid;		/* HDF5 grp id of current step	*/


	/*
	  BEGIN unstructured stuff,
	  should be moved to struct h5u_fdata
	*/

	hsize_t nparticles;		/* -> u.nparticles */
	
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

	/* END unstructured */
	
	struct h5u_fdata u;
	struct h5b_fdata *block;
	struct h5t_fdata t;
};

typedef struct h5_file h5_file;

struct h5u_fdata {
	hsize_t nparticles;		/* -> u.nparticles */
	
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
}

struct h5b_fdata {
	h5part_int64_t step_idx;
	h5part_int64_t i_max;
	h5part_int64_t j_max;
	h5part_int64_t k_max;
	struct H5BlockPartition *user_layout;
	struct H5BlockPartition *write_layout;
	int have_layout;

	hid_t shape;
	hid_t memshape;
	hid_t diskshape;
	hid_t blockgroup;
	hid_t field_group_id;
};

struct h5t_fdata_level {
	int		new_level;
	h5_id_t		last_stored_vertex; 
	h5_size_t	num_vertices;
	h5_vertex	* vertices;
	h5_id_t		last_stored_tet;
	h5_size_t	num_tets;
	h5_tetrahedron	* tets;
};

struct h5t_fdata {
	h5_id_t		cur_level;
	h5_size_t	num_levels;
	hid_t		mesh_gid;
	struct h5t_fdata_level levels[];
};
#endif
