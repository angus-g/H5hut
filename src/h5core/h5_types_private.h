#ifndef __H5_TYPES_PRIVATE_H
#define __H5_TYPES_PRIVATE_H

/**
   \struct h5_file

   This is an essentially opaque datastructure that
   acts as a filehandle and is defined as type \c h5_file_t.
   It is created by \ref H5OpenFile and destroyed by
   \ref H5CloseFile.
*/
struct h5_file {
	hid_t		file;		/* file id -> fid		*/
	h5_int32_t	mode;		/* file access mode		*/
	char		empty;

	/* MPI */

	MPI_Comm comm;			/* MPI communicator		*/
	int	nprocs;			/* number of processors		*/
	int	myproc;			/* The index of the processor	
					   this process is running on.	*/
        int	throttle;

	/* HDF5 */
	hid_t	xfer_prop;		/* dataset transfer properties	*/
	hid_t	access_prop;		/* file access properties	*/
	hid_t	create_prop;		/* file create properties	*/
	hid_t	root_gid;		/* id of root   		*/
	hid_t	step_gid;		/* id of current step		*/

	/* step internal data						*/
	char	prefix_step_name[H5_STEPNAME_LEN];	/* Prefix of step name		*/
	int	width_step_idx;		/* pad step index with 0 up to this */
	char	step_name[2*H5_STEPNAME_LEN];		/* full step name		*/
	h5_int64_t step_idx;		/* step index			*/
	int	is_new_step;

	struct h5u_fdata *u;
	struct h5b_fdata *b;
};

struct h5_idxmap_el {
	h5_glb_idx_t	glb_idx;
	h5_loc_idx_t	loc_idx;
};
typedef struct h5_idxmap_el h5_idxmap_el_t;

struct h5_idxmap {
	h5_size_t	size;		/* allocated space in number of items */
	h5_size_t	num_items;	/* stored items	*/
	h5_idxmap_el_t*  items;
};
#endif
