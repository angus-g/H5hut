/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5_TYPES_PRIVATE_H
#define __H5_TYPES_PRIVATE_H

#include <hdf5.h>
#include "h5core/h5_types.h"

struct h5_prop {                        // generic property class
        h5_int64_t class;               // property class
        char pad[248];                  // sizeof (struct h5_prop) == 256
};

struct h5_prop_file {                        // file property
        h5_int64_t class;               // property class == H5_PROP_FILE
        h5_int64_t flags;               // file access mode (read-write, readonly ...
        h5_int64_t align;               // HDF5 alignment
        h5_int64_t throttle;
        MPI_Comm comm;
	hid_t	xfer_prop;		// dataset transfer properties
	hid_t	access_prop;		// file access properties
	hid_t	create_prop;		// file create properties
	char*	prefix_step_name;	// Prefix of step name
	int	width_step_idx;		// pad step index with 0 up to this
};
typedef struct h5_prop_file h5_prop_file_t;
typedef h5_prop_file_t* h5_prop_file_p;

/**
   \struct h5_file

   This is an essentially opaque datastructure that
   acts as a filehandle and is defined as type \c h5_file_t.
   It is created by \ref H5OpenFile and destroyed by
   \ref H5CloseFile.
*/
struct h5_file {
	hid_t		file;		// HDF5 file id
        h5_prop_file_p  props;           // file properties
	char		empty;          // flag (should be int?!)

	/* MPI */
	int             nprocs;		// number of processors
	int             myproc;		// index of my processor

	/* HDF5 */
	hid_t           root_gid;	// HDF5 group id of root
	hid_t           step_gid;	// HDF5 group id of current step

	/* step internal data						*/
	char*           step_name;      // full current step name
	h5_int64_t      step_idx;	// current step index
	int             is_new_step;    // :FIXME: ?

	struct h5u_fdata *u;            // pointer to unstructured data
	struct h5b_fdata *b;            // pointer to block data
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
