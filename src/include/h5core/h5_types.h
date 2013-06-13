#ifndef __H5_TYPES_H
#define __H5_TYPES_H

#include <hdf5.h>
#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
  file modes:
  H5_O_RDONLY: only reading allowed
  H5_O_WRONLY: create new file, dataset must not exist
  H5_O_APPEND: allows to append a new datasets to an existing file
  H5_O_RDWR:   dataset may exist
*/

#define H5_O_RDWR		0x00000001
#define H5_O_RDONLY		0x00000002
#define H5_O_WRONLY		0x00000004
#define H5_O_APPEND		0x00000008

#define H5_VFD_MPIPOSIX         0x00000010
#define H5_VFD_MPIIO_IND        0x00000020
#define H5_VFD_INDEPENDENT      H5_VFD_MPIIO_IND // obsolete(?)
#define H5_VFD_CORE		0x00000040

#define H5_FLUSH_FILE		0x00001000
#define H5_FLUSH_STEP		0x00002000
#define H5_FLUSH_DATASET	0x00004000

#define H5_FS_LUSTRE		0x00010000

#define H5_ID_T			H5T_NATIVE_INT64
#define H5_FLOAT64_T		H5T_NATIVE_DOUBLE
#define H5_FLOAT32_T		H5T_NATIVE_FLOAT
#define H5_INT64_T		H5T_NATIVE_INT64
#define H5_INT32_T		H5T_NATIVE_INT32
#define H5_INT16_T		H5T_NATIVE_INT16
#define H5_STRING_T		H5T_NATIVE_CHAR
#define H5_COMPOUND_T		H5T_COMPOUND

#define H5_VER_STRING		"2.0.0"

extern const char * const H5_O_MODES[];

#ifdef   WIN32
typedef __int64			int64_t;
#endif /* WIN32 */

typedef int64_t			h5_int64_t;
typedef int32_t			h5_int32_t;
typedef int64_t			h5_id_t;
typedef int16_t			h5t_lvl_idx_t;
typedef h5_int32_t		h5t_elem_flags_t;
typedef int64_t			h5_glb_idx_t;	// type for a global index

#if defined(USE_LARGE_INDICES)
typedef int64_t                 h5_loc_idx_t;   // type for a local index
typedef int64_t                 h5_loc_id_t;    // type for a local ID
#else
typedef int32_t                 h5_loc_idx_t;   // type for a local index
typedef int32_t                 h5_loc_id_t;    // type for a local ID
#endif

typedef int64_t			h5_glb_id_t;	// type for a global ID
typedef uint64_t		h5_size_t;	/* size in number of elements */
typedef int64_t			h5_ssize_t;	/* size in number of elements */
typedef int64_t			h5_err_t;

typedef char*			char_p;
typedef void*			void_p;
typedef double			h5_float64_t;
typedef float			h5_float32_t;

typedef struct h5_complex {
	h5_float64_t		r,i;
} h5_complex_t;

typedef h5_float64_t h5_coord3d_t[3];

struct h5_file;
typedef struct h5_file h5_file_t;
typedef h5_file_t* h5_file_p;

struct h5t_mesh;
typedef struct h5t_mesh h5t_mesh_t;
typedef h5t_mesh_t* h5t_mesh_p;

typedef h5_err_t (*h5_errorhandler_t)(
	const char*,
	va_list ap );

#ifndef PARALLEL_IO
typedef int	MPI_Comm;
typedef int	MPI_Datatype;
#endif

typedef struct h5_loc_idlist {
	int32_t		size;		/* allocated space in number of items */
	int32_t		num_items;	/* stored items	*/
	h5_loc_id_t	items[1];
} h5_loc_idlist_t;

struct h5_idxmap;
typedef struct h5_idxmap h5_idxmap_t;


enum h5_oid {
	H5_OID_VERTEX = 1,
	H5_OID_EDGE = 2,
	H5_OID_TRIANGLE = 3,
	H5_OID_TETRAHEDRON = 4,
	H5_OID_MAX = 5	
};
typedef enum h5_oid h5_oid_t;

#define H5_MAX_VERTICES_PER_ELEM H5_OID_TETRAHEDRON

#define H5_TRIANGLE_MESH	 (H5_OID_TRIANGLE)
#define H5_TETRAHEDRAL_MESH	 (H5_OID_TETRAHEDRON)

#ifdef __cplusplus
}
#endif

#endif
