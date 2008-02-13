
#ifndef __H5_TYPES_H
#define __H5_TYPES_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>

#define H5_SUCCESS		0
#define H5_ERR_NOMEM		-12
#define H5_ERR_INVAL		-22
#define H5_ERR_BADFD		-77

#define H5_ERR_LAYOUT		-100
#define H5_ERR_NOENT		-101

#define H5_ERR_INIT		-200
#define H5_ERR_NOENTRY		-201

#define H5_ERR_MPI		-201
#define H5_ERR_HDF5		-202

#define H5_O_RDONLY		0x01
#define H5_O_WRONLY		0x02
#define H5_O_APPEND		0x03
#define H5_O_RDWR		0x04

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

typedef h5_err_t		h5_int32_t;

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
	h5_id_t		refined_on_evel; /* = 0 if not refined*/
	h5_id_t		unused;	/* for right alignment */
	h5_id_t		vertex_ids[2];
};

struct h5_triangle { /*24Bytes*/
	h5_id_t		id;
	h5_id_t		parent_id;
	h5_id_t		vertex_ids[3];
	h5_id_t		refined_on_level;
};

struct h5_tetrahedron { /* 24Bytes */
	h5_id_t		id;
	h5_id_t		parent_id;
	h5_id_t		refined_on_level;
	h5_id_t		unused;	/* for right alignment */
	h5_id_t		vertex_ids[4];
};

typedef struct h5_vertex	h5_vertex;
typedef struct h5_edge		h5_edge;
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
};

struct H5BlockPartition {
	h5part_int64_t	i_start;
	h5part_int64_t	i_end;
	h5part_int64_t	j_start;
	h5part_int64_t	j_end;
	h5part_int64_t	k_start;
	h5part_int64_t	k_end;
};

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
};

struct h5t_fdata {
	char		mesh_name[16];
	h5_id_t		cur_mesh;
	h5_id_t		new_mesh;	/* idx of the first new mesh or -1 */

	h5_id_t		new_level;	/* idx of the first new level or -1 */
	h5_id_t		cur_level;
	h5_size_t	num_levels;

	h5_size_t	*num_vertices;
	h5_size_t	*num_tets;
	h5_size_t	*map_tets_g2l;

	h5_id_t		last_stored_vertex_id; 
	h5_vertex	* vertices;

	h5_id_t		last_stored_tet_id;
	h5_tetrahedron	* tets;

	/* HDF5 objects */

	hid_t		topo_gid;	/* grp id of mesh in current level */
	hid_t		mesh_gid;
	hid_t		coord_gid;
	hid_t		vmesh_gid;

	/* type ids' for compound types */
	hid_t		float64_3tuple_tid;
	hid_t		int32_2tuple_tid;
	hid_t		int32_3tuple_tid;
	hid_t		int32_4tuple_tid;
	hid_t		vertex_tid;
	hid_t		tet_tid;
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

#endif
