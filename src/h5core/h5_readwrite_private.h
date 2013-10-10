/*
  Copyright (c) 2006-2013, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5_READWRITE_PRIVATE_H
#define __H5_READWRITE_PRIVATE_H

#include "h5core/h5_types.h"


#define is_writable(f) (f->props->flags & (H5_O_RDWR | H5_O_WRONLY | H5_O_APPENDONLY))
#define is_readable(f) (f->props->flags & (H5_O_RDWR | H5_O_RDONLY))
#define is_readonly(f) (f->props->flags & H5_O_RDONLY)
#define is_appendonly(f) (f->props->flags & H5_O_APPENDONLY)

#define CHECK_WRITABLE_MODE(f)                                          \
	TRY (is_writable (f) ? H5_SUCCESS : h5_error (                  \
                     H5_ERR_INVAL,                                      \
                     "Attempting to write to read-only file"));

#define CHECK_TIMEGROUP(f)                                             \
	TRY ((f->step_gid > 0) ? H5_SUCCESS : h5_error (               \
                     H5_ERR_INVAL,                                     \
                     "Time step is invalid! Have you set the time step?"));


/*
   information about HDF5 dataset
 */
typedef struct h5_dataset_info {
	char name[256];
	int rank;
	hsize_t dims[4];
	hsize_t max_dims[4];
	hsize_t chunk_dims[4];
	hid_t type_id;
	hid_t create_prop;
	hid_t access_prop;
} h5_dsinfo_t;

h5_err_t
h5priv_write_dataset_by_name (
		h5t_mesh_t* const m,
	 	const h5_file_p f,
        hid_t loc_id,
        h5_dsinfo_t* ds_info,
        hid_t (*set_memspace)(h5t_mesh_t*,hid_t),
        hid_t (*set_diskspace)(h5t_mesh_t*,hid_t),
        const void* const data
        );

h5_err_t
h5priv_write_dataset_by_name_id (
	 	const h5_file_p f,
        const hid_t loc_id,
        h5_dsinfo_t* dsinfo,
        hid_t dset_id,
        hid_t memspace_id,
        hid_t diskspace_id,
        const void* const data
        );

h5_err_t
h5priv_read_dataset (
	const h5_file_p,
	const hid_t, h5_dsinfo_t*,
        hid_t (*)(h5t_mesh_t* const, const hid_t),
        hid_t (*)(h5t_mesh_t* const, const hid_t),
	void* const);

h5_int64_t
h5priv_normalize_h5_type (
	hid_t type
	);

h5_int64_t
h5priv_get_dataset_type(
	const hid_t group_id,
	const char *dataset_name
	);

h5_err_t
h5priv_normalize_dataset_name (
	const char *name,
	char *name2
	);

#endif
