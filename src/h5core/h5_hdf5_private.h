/*
  Copyright (c) 2006-2015, The Regents of the University of California,
  through Lawrence Berkeley National Laboratory (subject to receipt of any
  required approvals from the U.S. Dept. of Energy) and the Paul Scherrer
  Institut (Switzerland).  All rights reserved.

  License: see file COPYING in top level of source distribution.
*/

#ifndef __H5_HDF5_PRIVATE_H
#define __H5_HDF5_PRIVATE_H

#include <hdf5.h>

#include "h5core/h5_types.h"
#include "h5core/h5_errorhandling.h"
#include "h5_debug_private.h"

ssize_t
hdf5_get_num_groups (
	const hid_t loc_id
	);

ssize_t
hdf5_get_num_groups_matching_prefix (
	const hid_t loc_id,
	char* prefix
	);

h5_err_t
hdf5_get_name_of_group_by_idx (
	hid_t loc_id,
	hsize_t idx,
	char *name,
	size_t len
	);

ssize_t
hdf5_get_num_datasets (
	const hid_t loc_id
	);

h5_err_t
hdf5_get_name_of_dataset_by_idx (
	hid_t loc_id,
	hsize_t idx,
	char *name,
	size_t len
	);

const char *
hdf5_get_objname (
	hid_t id
	);


/****** L i n k **************************************************************/

/*
   Determine whether a link with the specified name exists in a group.

   Result:
   1	if link exists
   0	if link doesn't exist
   error else
*/
static inline h5_err_t
hdf5_link_exists (
        const hid_t loc_id,
        const char* name
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "loc_id=%d (%s), name='%s'",
	                    loc_id, hdf5_get_objname (loc_id), name);
	/* Save old error handler */
	H5E_auto2_t old_func;
	void *old_client_data;

	H5Eget_auto2(H5E_DEFAULT, &old_func, &old_client_data);

	/* Turn off error handling */
	H5Eset_auto(H5E_DEFAULT, NULL, NULL);

	/* Probe. Likely to fail, but that’s okay */
	htri_t exists = H5Lexists ( loc_id, name, H5P_DEFAULT );

	/* Restore previous error handler */
	H5Eset_auto(H5E_DEFAULT, old_func, old_client_data);

	if (exists < 0 )
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot query link %s/%s.",
		                hdf5_get_objname (loc_id), name));
	HDF5_WRAPPER_RETURN (exists);
}

static inline h5_err_t
hdf5_delete_link (
        hid_t loc_id,
        const char* name,
        hid_t lapl_id
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "loc_id=%d (%s), name='%s', lapl_id=%d",
	                    loc_id, hdf5_get_objname (loc_id), name, lapl_id);
	if (H5Ldelete (loc_id, name, lapl_id)  < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot delete link %s/%s.",
		                hdf5_get_objname (loc_id), name));

	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

/****** G r o u p ************************************************************/

static inline hid_t
hdf5_open_group (
        const hid_t loc_id,
        const char* const group_name
        ) {
	HDF5_WRAPPER_ENTER (hid_t,
	                    "loc_id=%d (%s), group_name='%s'",
	                    loc_id,
	                    hdf5_get_objname (loc_id),
	                    group_name);
	hid_t group_id = H5Gopen (loc_id, group_name, H5P_DEFAULT);
	if (group_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot open group '%s/%s'.",
		                hdf5_get_objname (loc_id),
		                group_name));
	HDF5_WRAPPER_RETURN (group_id);
}

static inline hid_t
hdf5_create_group (
        const hid_t loc_id,
        const char* const group_name
        ) {
	HDF5_WRAPPER_ENTER (hid_t,
	                    "loc_id=%d (%s), group_name='%s'",
	                    loc_id,
	                    hdf5_get_objname (loc_id),
	                    group_name);
	hid_t group_id = H5Gcreate (
	        loc_id, group_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	if (group_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot create group '%s/%s'.",
		                hdf5_get_objname (loc_id),
		                group_name));
	HDF5_WRAPPER_RETURN (group_id);
}

h5_err_t
h5priv_open_group_ (int, hid_t, const char const*[], size_t);

#define h5priv_open_group(create_intermediate, loc_id, ...)             \
        (h5priv_open_group_ (create_intermediate,                       \
                             loc_id,                                     \
                             (const char const*[]) {__VA_ARGS__},                        \
                             PP_NARG(__VA_ARGS__)))

h5_err_t
h5priv_link_exists_ (
        const hid_t loc_id,
        const char const* path[],
        size_t size
        );
#define h5priv_link_exists(loc_id, ...)         \
        (h5priv_link_exists_ (loc_id, (const char const*[]){__VA_ARGS__}, PP_NARG(__VA_ARGS__)))


/*!
   Close group.

   \param[in]	f		file handle
   \param[in]	group_id        id of group to close
 */
static inline h5_err_t
hdf5_close_group (
        const hid_t group_id
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "group_id=%d (%s)",
	                    group_id,
	                    hdf5_get_objname (group_id));

	if (group_id == 0 || group_id == -1)
		HDF5_WRAPPER_LEAVE (H5_SUCCESS);
	if (H5Gclose (group_id) < 0 ) {
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot terminate access to group '%s').",
		                hdf5_get_objname (group_id)));
	}
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_ssize_t
hdf5_get_num_objs_in_group (
        const hid_t group_id
        ) {
	HDF5_WRAPPER_ENTER (h5_ssize_t,
	                    "group_id=%d (%s)",
	                    group_id,
	                    hdf5_get_objname (group_id));
	H5G_info_t group_info;
	if (H5Gget_info (group_id, &group_info) < 0) {
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot get number of objects in group '%s'.",
		                hdf5_get_objname(group_id)));
	}
	HDF5_WRAPPER_RETURN ((h5_ssize_t)group_info.nlinks);
}


/*
   Get name of object given by index \c idx in group \c loc_id. If name is \c NULL,
   return size of name.
 */
static inline h5_ssize_t
hdf5_get_objname_by_idx (
        hid_t loc_id,
        hsize_t idx,
        char *name,
        size_t size
        ) {
	HDF5_WRAPPER_ENTER (h5_ssize_t,
	                    "loc_id=%d (%s), idx=%lld",
	                    loc_id,
	                    hdf5_get_objname (loc_id),
	                    (long long)idx);

	if (name == NULL) {
		size = 0;
	}
	ssize_t len = H5Lget_name_by_idx (loc_id, ".",
	                                  H5_INDEX_NAME, H5_ITER_INC,
	                                  idx,
	                                  name, size,
	                                  H5P_DEFAULT);
	if (len < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot get name of object %llu in group '%s'.",
		                (unsigned long long)idx,
		                hdf5_get_objname (loc_id)));
	HDF5_WRAPPER_RETURN (len);
}

/****** D a t a s p a c e ****************************************************/
/*!
   Create dataspace for dataset. H5Screate_simple wrapper.

   \param[in]	rank		rank of dataspace
   \param[in]	dims		dimensions of dataspace
   \param[in]	maxdims		maximum dimensions of dataspace

 */
static inline hid_t
hdf5_create_dataspace (
        const int rank,
        const hsize_t* dims,
        const hsize_t* maxdims
        ) {
	HDF5_WRAPPER_ENTER (hid_t,
	                    "rank=%d",
	                    rank);
	hid_t dataspace_id = H5Screate_simple (rank, dims, maxdims);
	if (dataspace_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot create dataspace with rank %d.",
		                rank));
	HDF5_WRAPPER_RETURN (dataspace_id);
}

static inline hid_t
hdf5_create_dataspace_scalar (
        void
        ) {
	HDF5_WRAPPER_ENTER (hid_t, "%s", "void");
	hid_t dataspace_id = H5Screate (H5S_SCALAR);
	if (dataspace_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot create scalar dataspace."));
	HDF5_WRAPPER_RETURN (dataspace_id);
}

static inline h5_err_t
hdf5_select_hyperslab_of_dataspace (
        hid_t space_id,
        H5S_seloper_t op,
        const hsize_t* start,
        const hsize_t* stride,
        const hsize_t* count,
        const hsize_t* block
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t, "%d", space_id);
	herr_t herr = H5Sselect_hyperslab (
	        space_id,
	        op,
	        start,
	        stride,
	        count,
	        block);
	if (herr < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot set select hyperslap region or add the "
		                "specified region"));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_err_t
hdf5_select_elements_of_dataspace (
        hid_t space_id,
        H5S_seloper_t op,
        hsize_t nelems,
        const hsize_t* indices
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t, "%d", space_id);
	herr_t herr;
	if ( nelems > 0 ) {
		herr = H5Sselect_elements (
		        space_id,
		        op,
		        (size_t)nelems,
		        indices);
	} else {
		herr = H5Sselect_none ( space_id );
	}
	if (herr < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot set select hyperslap region or add the "
		                "specified region"));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_err_t
hdf5_select_none (
        hid_t space_id
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "%d",
	                    space_id);
	herr_t herr = H5Sselect_none (space_id);
	if (herr < 0) {
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Selection for writing zero-length data failed"));
	}
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_ssize_t
hdf5_get_selected_npoints_of_dataspace (
        hid_t space_id
        ) {
	HDF5_WRAPPER_ENTER (h5_ssize_t, "%d", space_id);
	hssize_t size = H5Sget_select_npoints (space_id);
	if (size < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error(
		                H5_ERR_HDF5,
		                "Cannot determine number of "
		                "selected elements in dataspace."));
	HDF5_WRAPPER_RETURN (size);
}

static inline h5_ssize_t
hdf5_get_npoints_of_dataspace (
        hid_t space_id
        ) {
	HDF5_WRAPPER_ENTER (h5_ssize_t, "%d", space_id);
	hssize_t size = H5Sget_simple_extent_npoints (space_id);
	if (size < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error(
		                H5_ERR_HDF5,
		                "Cannot determine number of"
		                "elements in dataspace."));
	HDF5_WRAPPER_RETURN (size);
}

static inline int
hdf5_get_dims_of_dataspace (
        hid_t space_id,
        hsize_t* dims,
        hsize_t* maxdims
        ) {
	HDF5_WRAPPER_ENTER (int, "%d", space_id);
	int rank = H5Sget_simple_extent_dims (space_id, dims, maxdims);
	if (rank < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot determine rank of dataspace."));
	HDF5_WRAPPER_RETURN (rank);
}


/*!
   Close space.

   \param[in]	f		file handle
   \param[in]	dataspace_id	id of space to close
 */
static inline h5_err_t
hdf5_close_dataspace (
        const hid_t dataspace_id
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t, "dataspace=%d", dataspace_id);
	if (dataspace_id <= 0 || dataspace_id == H5S_ALL)
		HDF5_WRAPPER_LEAVE (H5_SUCCESS);

	herr_t herr = H5Sclose (dataspace_id);
	if (herr < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error(
		                H5_ERR_HDF5,
		                "Cannot terminate access to dataspace!"));

	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

/****** D a t a s e t ********************************************************/
/*!
   Open dataset. H5Dopen wrapper.

   \param[in]	f		file handle
   \param[in]	loc_id		location id
   \param[in]	dataset_name	name of dataset to open
 */
static inline hid_t
hdf5_open_dataset (
        const hid_t loc_id,
        const char* const dataset_name
        ) {
	HDF5_WRAPPER_ENTER (hid_t,
	                    "loc_id=%d (%s), dataset_name='%s'",
	                    loc_id,
	                    hdf5_get_objname (loc_id),
	                    dataset_name);
	hid_t dataset_id = H5Dopen (
	        loc_id,
	        dataset_name,
	        H5P_DEFAULT);
	if (dataset_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot open dataset '%s/%s'.",
		                hdf5_get_objname (loc_id),
		                dataset_name));
	HDF5_WRAPPER_RETURN (dataset_id);
}

/*!
   Create new dataset

   \param[in]	f		file handle
   \param[in]	loc_id		id of group or file
   \param[in]	dataset_name	name of dataset
   \param[in]	type_id		type used in dataset
   \param[in]	dataspace_id	dataspace of dataset
   \param[in]	create_prop	property list for dataset creation

 */
static inline hid_t
hdf5_create_dataset (
        hid_t loc_id,
        const char* dataset_name,
        const hid_t type_id,
        const hid_t dataspace_id,
        const hid_t create_proplist
        ) {
	HDF5_WRAPPER_ENTER (hid_t,
	                    "loc_id=%d (%s), dataset_name='%s'",
	                    loc_id,
	                    hdf5_get_objname (loc_id),
	                    dataset_name);
	hid_t dataset_id = H5Dcreate (
	        loc_id,
	        dataset_name,
	        type_id,
	        dataspace_id,
	        H5P_DEFAULT,
	        create_proplist,
	        H5P_DEFAULT);
	if (dataset_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error(
		                H5_ERR_HDF5,
		                "Cannot create dataset '%s/%s'",
		                hdf5_get_objname (loc_id),
		                dataset_name));
	HDF5_WRAPPER_RETURN (dataset_id);
}

/*!
   Close dataset.

   \param[in]	f		file handle
   \param[in]	dataset_id	id of dataset to close
 */
static inline h5_err_t
hdf5_close_dataset (
        const hid_t dataset_id
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "dataset_id=%d (%s)",
	                    dataset_id,
	                    hdf5_get_objname (dataset_id));
	if (dataset_id < 0)
		HDF5_WRAPPER_LEAVE (H5_SUCCESS);

	if (H5Dclose (dataset_id) < 0) {
		HDF5_WRAPPER_LEAVE (
		        h5_error(
		                H5_ERR_HDF5,
		                "Close of dataset '%s' failed.",
		                hdf5_get_objname (dataset_id)));
	}
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

/*!
   Get dataspace of existing dataset

   \param[in]	f		file handle
   \param[in]	dataset_id	id of dataset

 */
static inline hid_t
hdf5_get_dataset_space (
        const hid_t dataset_id
        ) {
	HDF5_WRAPPER_ENTER (hid_t,
	                    "dataset_id=%d (%s)",
	                    dataset_id,
	                    hdf5_get_objname(dataset_id));
	hid_t dataspace_id = H5Dget_space (dataset_id);
	if (dataspace_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot get dataspace for dataset '%s'.",
		                hdf5_get_objname (dataset_id)));
	HDF5_WRAPPER_RETURN (dataspace_id);
}

/*!
   Wrapper for H5Dwrite.

   \param[in]	f		file handle
   \param[in]	dataset_id	id of dataset
   \param[in]	type_id		type used in dataset
   \param[in]	memspace_id	id of memory space
   \param[in]	diskspace_id	id of disk space
   \param[in]	xfer_prop	transfer property list
   \param[in]	buf		buffer with date to write

 */
static inline h5_err_t
hdf5_write_dataset (
        const hid_t dataset_id,
        const hid_t type_id,
        const hid_t memspace_id,
        const hid_t diskspace_id,
        const hid_t xfer_prop,
        const void* buf
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "dataset_id=%d (%s) type_id=%d",
	                    dataset_id,
	                    hdf5_get_objname(dataset_id),
	                    type_id);

	herr_t herr = H5Dwrite (
	        dataset_id,
	        type_id,
	        memspace_id,
	        diskspace_id,
	        xfer_prop,
	        buf);
	if (herr < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Write to dataset '%s' failed.",        \
		                hdf5_get_objname (dataset_id)));

	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

/*
   Wrapper for H5Dread
 */
static inline h5_err_t
hdf5_read_dataset (
        const hid_t dataset_id,
        const hid_t type_id,
        const hid_t memspace_id,
        const hid_t diskspace_id,
        const hid_t xfer_prop,
        void* const buf ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "dataset_id=%d (%s) type_id=%d",
	                    dataset_id,
	                    hdf5_get_objname(dataset_id),
	                    type_id);
	herr_t herr = H5Dread (
	        dataset_id,
	        type_id,
	        memspace_id,
	        diskspace_id,
	        xfer_prop,
	        buf);
	if (herr < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error(
		                H5_ERR_HDF5,
		                "Error reading dataset '%s'.",
		                hdf5_get_objname (dataset_id)));

	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline hid_t
hdf5_get_dataset_type (
        const hid_t dataset_id
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "dataset_id=%d (%s)",
	                    dataset_id,
	                    hdf5_get_objname(dataset_id));
	hid_t datatype_id = H5Dget_type (dataset_id);
	if (datatype_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error(
		                H5_ERR_HDF5,
		                "Cannot determine dataset type."));

	HDF5_WRAPPER_RETURN (datatype_id);
}

static inline h5_err_t
hdf5_set_dataset_extent (
        hid_t dataset_id,
        const hsize_t* size
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "dataset_id=%d (%s), size=%llu",
	                    dataset_id,
	                    hdf5_get_objname(dataset_id),
	                    *size);
	if (H5Dset_extent(dataset_id, size) < 0) {
		HDF5_WRAPPER_LEAVE (
		        h5_error(
		                H5_ERR_HDF5,
		                "Changing size of dataset '%s' dimensions failed.",
		                hdf5_get_objname (dataset_id)));
	}
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_ssize_t
hdf5_get_npoints_of_dataset (
        hid_t dataset_id
        ) {
	HDF5_WRAPPER_ENTER (h5_ssize_t,
	                    "dataset_id=%d (%s)",
	                    dataset_id,
	                    hdf5_get_objname(dataset_id));
	hid_t dspace_id;
	hsize_t size;
	TRY (dspace_id = hdf5_get_dataset_space (dataset_id));
	TRY (size = hdf5_get_npoints_of_dataspace (dspace_id));
	TRY (hdf5_close_dataspace (dspace_id));
	HDF5_WRAPPER_RETURN (size);
}

static inline h5_ssize_t
hdf5_get_npoints_of_dataset_by_name (
        const hid_t loc_id,
        const char* const name
        ) {
	HDF5_WRAPPER_ENTER (h5_ssize_t,
	                    "loc_id=%d (%s), name='%s'",
	                    loc_id,
	                    hdf5_get_objname(loc_id),
	                    name);
	hid_t dset_id;
	hsize_t size;
	TRY (dset_id = hdf5_open_dataset (loc_id, name));
	TRY (size = hdf5_get_npoints_of_dataset (dset_id));
	TRY (hdf5_close_dataset (dset_id));
	HDF5_WRAPPER_RETURN (size);
}

/****** D a t a t y p e ******************************************************/
/*!
   Create array type. Wrapper for "H5Tarray_create".

   \param[in]	f		file handle
   \param[in]	base_type_id	base type
   \param[in]	rank		rank of array
   \param[in]	dims		dimensions
 */
static inline char_p
hdf5_get_type_name (
        hid_t type_id
        ) {
	if (type_id == H5_INT32_T)
		return "H5_INT32_T";
	if (type_id == H5_INT64_T)
		return "H5_INT64_T";
	if (type_id == H5_FLOAT32_T)
		return "H5_FLOAT32_T";
	if (type_id == H5_FLOAT64_T)
		return "H5_FLOAT64_T";
	if (type_id == H5_STRING_T)
		return "H5_STRING_T";

	h5_warn ("Unknown type id %d", type_id);
	return "[unknown]";
}

static inline const char*
get_class_type_name (
        const hid_t class_id
        ) {
	const char* const map[] = {
		[H5T_INTEGER]   "H5T_INTEGER",
		[H5T_FLOAT]     "H5T_FLOAT",
		[H5T_TIME]      "H5T_TIME",
		[H5T_STRING]    "H5T_STRING",
		[H5T_BITFIELD]  "H5T_BITFIELD",
		[H5T_OPAQUE]    "H5T_OPAQUE",
		[H5T_COMPOUND]  "H5T_COMPOUND",
		[H5T_REFERENCE] "H5T_REFERENCE",
		[H5T_ENUM]      "H5T_ENUM",
		[H5T_VLEN]      "H5T_VLEN",
		[H5T_ARRAY]     "H5T_ARRAY"
	};
	if (class_id < 0 || class_id >= H5T_NCLASSES) {
		return ("[unknown]");
	}
	return map[class_id];
}

static inline hid_t
hdf5_create_array_type (
        const hid_t base_type_id,
        const int rank,
        const hsize_t* dims
        ) {
	HDF5_WRAPPER_ENTER (hid_t,
	                    "base_type_id=%d (%s), rank=%d",
	                    base_type_id,
	                    hdf5_get_type_name (base_type_id),
	                    rank);
	hid_t type_id = H5Tarray_create (base_type_id, rank, dims);
	if (type_id < 0) {
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Can't create array datatype object with base "
		                "type %s and rank %d",
		                hdf5_get_type_name (base_type_id),
		                rank));
	}
	HDF5_WRAPPER_RETURN (type_id);
}

static inline hid_t
hdf5_create_type (
        H5T_class_t class,
        const size_t size
        ) {
	HDF5_WRAPPER_ENTER (hid_t,
	                    "class=%d (%s)",
	                    class,
	                    get_class_type_name (class));
	hid_t type_id = H5Tcreate (class, size);
	if (type_id < 0) {
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Can't create datatype object of class %s.",
		                get_class_type_name (class)));
	}
	HDF5_WRAPPER_RETURN (type_id);
}

static inline hid_t
hdf5_create_string_type(
        const hsize_t len
        ) {
	HDF5_WRAPPER_ENTER (hid_t, "len = %llu", len);
	hid_t type_id = H5Tcopy ( H5T_C_S1 );
	if (type_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error(
		                H5_ERR_HDF5,
		                "Can't duplicate C string type."));

	herr_t herr = H5Tset_size ( type_id, len );
	if (herr < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error(
		                H5_ERR_HDF5,
		                "Can't set length of C string type."));
	HDF5_WRAPPER_RETURN (type_id);
}

static inline h5_err_t
hdf5_insert_type (
        hid_t type_id,
        const char* name,
        size_t offset,
        hid_t field_id
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t, "type_id=%d, name='%s'", type_id, name);
	herr_t herr = H5Tinsert (type_id, name, offset, field_id);
	if (herr < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error(
		                H5_ERR_HDF5,
		                "Can't insert field %s to compound datatype.",
		                name));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline H5T_class_t
hdf5_get_class_type (
        hid_t dtype_id
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t, "dtype_id=%d", dtype_id);
        H5T_class_t class = H5Tget_class (dtype_id);
	if (class < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error(
		                H5_ERR_HDF5,
		                "Can't determine class of type %d.",
		                dtype_id));
	HDF5_WRAPPER_RETURN (class);
}

static inline h5_ssize_t
hdf5_get_sizeof_type (
        hid_t dtype_id
        ) {
	HDF5_WRAPPER_ENTER (h5_ssize_t, "dtype_id=%d", dtype_id);
        h5_ssize_t size = H5Tget_size (dtype_id);
        if (size == 0) {
		HDF5_WRAPPER_LEAVE (
		        h5_error(
		                H5_ERR_HDF5,
		                "Can't determine size of type %d.",
		                dtype_id));
        }
	HDF5_WRAPPER_RETURN (size);
}


static inline h5_err_t
hdf5_close_type (
        hid_t dtype_id
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t, "dtype_id=%d", dtype_id);
	herr_t herr = H5Tclose (dtype_id);
	if (herr < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot release datatype."));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

/****** P r o p e r t y ******************************************************/

static inline hid_t
hdf5_create_property (
        hid_t cls_id
        ) {
	HDF5_WRAPPER_ENTER (hid_t, "cls_id=%d", cls_id);
	hid_t prop_id = H5Pcreate (cls_id);
	if (prop_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot create property list."));
	HDF5_WRAPPER_RETURN (prop_id);
}

/*!
   Get create properties of existing dataset

   \param[in]	f		file handle
   \param[in]	dataset_id	id of dataset

 */
static inline hid_t
hdf5_get_dataset_create_plist (
        const hid_t dataset_id
        ) {
	HDF5_WRAPPER_ENTER (hid_t, "dataset_id=%d (%s)",
	                    dataset_id,
	                    hdf5_get_objname (dataset_id));
	hid_t plist_id = H5Dget_create_plist (dataset_id);
	if (plist_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot get create properties for dataset '%s'.",
		                hdf5_get_objname (dataset_id)));
	HDF5_WRAPPER_RETURN (plist_id);
}

static inline h5_err_t
hdf5_set_chunk_property (
        hid_t plist,
        int rank,
        hsize_t* dims
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "plist=%d, rank=%d, dims[0]=%llu ...",
	                    plist, rank, dims[0]);
	if (H5Pset_chunk (plist, rank, dims) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot add chunking property to list."));

	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_err_t
hdf5_get_chunk_property (
        hid_t plist,
        int rank,
        hsize_t* dims
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t, "plist=%d, rank=%d", plist, rank);
	if (H5Pget_chunk (plist, rank, dims) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot get chunking property from list."));

	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_err_t
hdf5_set_layout_property (
        hid_t plist,
        H5D_layout_t layout
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t, "plist=%d", plist);
	if (H5Pset_layout (plist, layout) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot add layout property to list."));

	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

#ifdef PARALLEL_IO
static inline h5_err_t
hdf5_set_fapl_mpio_property (
        hid_t fapl_id,
        MPI_Comm comm,
        MPI_Info info
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "fapl_id=%d, comm=..., info=...",
	                    fapl_id);
	if (H5Pset_fapl_mpio (fapl_id, comm, info) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot store IO communicator information to the "
		                "file access property list."));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

#if H5_VERSION_LE(1,8,12)
static inline h5_err_t
hdf5_set_fapl_mpiposix_property (
        hid_t fapl_id,
        MPI_Comm comm,
        hbool_t use_gpfs
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "fapl_id=%d, comm=..., use_gpfs=%d",
	                    fapl_id, (int)use_gpfs);
	if ( H5Pset_fapl_mpiposix (fapl_id, comm, use_gpfs) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot store IO communicator information to"
		                " the file access property list."));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}
#endif

static inline h5_err_t
hdf5_set_dxpl_mpio_property (
        hid_t dxpl_id,
        H5FD_mpio_xfer_t mode
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t, "dxpl_id=%d, mode=%d",dxpl_id,(int)mode);
	if (H5Pset_dxpl_mpio (dxpl_id, mode) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot store IO communicator information to"
		                " the dataset transfer property list."));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}
#endif

static inline h5_err_t
hdf5_set_mdc_property (
        hid_t fapl_id,
        H5AC_cache_config_t *config
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t, "fapl_id=%d, config=%p",fapl_id,config);
	if (H5Pset_mdc_config (fapl_id, config) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot store metadata cache configuration in"
		                " the file access property list."));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_err_t
hdf5_get_mdc_property (
        hid_t fapl_id,
        H5AC_cache_config_t *config
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t, "fapl_id=%d, config=%p",fapl_id,config);
	if (H5Pget_mdc_config (fapl_id, config) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot get metadata cache configuration in"
		                " the file access property list."));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_err_t
hdf5_set_btree_ik_property (
        const hid_t fcpl_id,
        const hsize_t btree_ik
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "fapl_id=%d, btree_ik=%llu",
	                    fcpl_id, btree_ik);
	if (H5Pset_istore_k (fcpl_id, btree_ik) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot set btree size in the "
		                "file access property list."));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_err_t
hdf5_set_alignment_property (
        hid_t plist,
        hsize_t threshold,
        hsize_t alignment
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "plist=%d, threshold=%llu, alignment=%llu",
	                    plist, threshold, alignment);
	if (H5Pset_alignment (plist, threshold, alignment) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot set alignment property to %llu and threshold %llu",
		                alignment, threshold));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_err_t
hdf5_set_meta_block_size (
        hid_t fapl_id,
        hsize_t size
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "fapl_id=%d, size=%llu",
	                    fapl_id, size);
	if (H5Pset_meta_block_size (fapl_id, size) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot set meta block size property to %llu",
		                size));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_err_t
hdf5_set_fapl_core (
        hid_t fapl_id,
        size_t increment,
        hbool_t backing_store
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
			    "fapl_id=%d, size=%zu, backing_store=%d",
			    fapl_id, increment, backing_store);
        if (H5Pset_fapl_core (fapl_id, increment, backing_store))
		HDF5_WRAPPER_LEAVE (
			h5_error (
				H5_ERR_HDF5,
				"Cannot set property to use the H5FD_CORE driver."));
        HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_err_t
hdf5_close_property (
        hid_t prop
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t, "prop=%d", prop);
	if (H5Pclose (prop) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot close property."));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

/****** F i l e **************************************************************/

static inline h5_err_t
hdf5_close_file (
        hid_t file_id
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t, "file_id=%d (%s)",
	                    file_id, hdf5_get_objname (file_id));
	if (H5Fclose (file_id) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot close file '%s'.",
		                hdf5_get_objname (file_id)));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_err_t
hdf5_close (
	void
	) {
	HDF5_WRAPPER_ENTER (h5_err_t, "%s", "void");
	if (H5close () < 0)
		HDF5_WRAPPER_LEAVE (
			h5_error (
				H5_ERR_HDF5,
				"Cannot close HDF5 library."));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_err_t
hdf5_flush (
        hid_t obj_id,
        H5F_scope_t scope
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "obj_id=%d (%s)",
	                    obj_id,
	                    hdf5_get_objname (obj_id));
	if (H5Fflush (obj_id, scope) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot flush data \"%s\".",
		                hdf5_get_objname (obj_id)));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

/****** E r r o r h a n d l i n g ********************************************/

static inline h5_err_t
hdf5_set_errorhandler (
        hid_t estack_id,
        H5E_auto_t func,
        void* client_data
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "estack_id=%d, func=%p, client_data=%p",
	                    estack_id, func, client_data);
	if (H5Eset_auto (estack_id, func, client_data) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot initialize H5."));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

/****** A t t r i b u t e ****************************************************/
static inline hid_t
hdf5_attribute_exists (
        hid_t loc_id,
        const char* attrib_name
        ) {
	HDF5_WRAPPER_ENTER (hid_t,
	                    "loc_id=%d (%s), attr_name='%s'",
	                    loc_id, hdf5_get_objname (loc_id), attrib_name);
	htri_t exists = H5Aexists (loc_id, attrib_name);
	if (exists < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot query attribute '%s' of '%s'.",
		                attrib_name,
		                hdf5_get_objname (loc_id)));
	HDF5_WRAPPER_RETURN (exists);
}

static inline hid_t
hdf5_open_attribute (
        hid_t loc_id,
        const char* attrib_name
        ) {
	HDF5_WRAPPER_ENTER (hid_t,
	                    "loc_id=%d (%s), attr_name='%s'",
	                    loc_id, hdf5_get_objname (loc_id), attrib_name);
	hid_t attrib_id = H5Aopen (loc_id, attrib_name, H5P_DEFAULT);
	if (attrib_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot open attribute '%s' of '%s'.",
		                attrib_name,
		                hdf5_get_objname (loc_id)));
	HDF5_WRAPPER_RETURN (attrib_id);
}

static inline hid_t
hdf5_open_attribute_idx (
        hid_t loc_id,
        unsigned int idx
        ) {
	HDF5_WRAPPER_ENTER (hid_t,
	                    "loc_id=%d (%s), idx=%u",
	                    loc_id, hdf5_get_objname (loc_id), idx);
	hid_t attr_id = H5Aopen_idx (loc_id, idx);
	if (attr_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot open attribute '%u' of '%s'.",
		                idx,
		                hdf5_get_objname (loc_id)));
	HDF5_WRAPPER_RETURN (attr_id);
}

static inline hid_t
hdf5_open_attribute_by_name (
        hid_t loc_id,
        const char* obj_name,
        const char* attr_name
        ) {
	HDF5_WRAPPER_ENTER (hid_t,
	                    "loc_id=%d (%s), obj_name='%s', attr_name='%s'",
	                    loc_id, hdf5_get_objname (loc_id),
	                    obj_name, attr_name);
	hid_t attr_id = H5Aopen_by_name (
	        loc_id,
	        obj_name,
	        attr_name,
	        H5P_DEFAULT,
	        H5P_DEFAULT);
	if (attr_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot open attribute '%s' of '%s'.",
		                attr_name,
		                obj_name));
	HDF5_WRAPPER_RETURN (attr_id);
}

static inline hid_t
hdf5_create_attribute (
        hid_t loc_id,
        const char* attr_name,
        hid_t type_id,
        hid_t space_id,
        hid_t acpl_id,
        hid_t aapl_id
        ) {
	HDF5_WRAPPER_ENTER (hid_t,
	                    "loc_id=%d (%s), attr_name='%s', type_id=%d",
	                    loc_id, hdf5_get_objname (loc_id),
	                    attr_name, type_id);
	hid_t attr_id = H5Acreate (
	        loc_id,
	        attr_name,
	        type_id,
	        space_id,
	        acpl_id,
	        aapl_id);
	if (attr_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot create attribute '%s' for '%s'.",
		                attr_name,
		                hdf5_get_objname (loc_id)));
	HDF5_WRAPPER_RETURN (attr_id);
}

static inline h5_err_t
hdf5_read_attribute (
        hid_t attr_id,
        hid_t mem_type_id,
        void* buf
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "attr_id=%d (%s), mem_type_id=%d, buf=%p",
	                    attr_id, hdf5_get_objname (attr_id),
	                    mem_type_id, buf);
	if (H5Aread (attr_id, mem_type_id, buf) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot read attribute '%s'.",
		                hdf5_get_objname (attr_id)));
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

/*
   Wrapper for H5Awrite.
 */
static inline h5_err_t
hdf5_write_attribute (
        hid_t attr_id,
        hid_t mem_type_id,
        const void* buf
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "attr_id=%d (%s), mem_type_id=%d, buf=%p",
	                    attr_id, hdf5_get_objname (attr_id),
	                    mem_type_id, buf);
	if (H5Awrite (attr_id, mem_type_id, buf) < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot write attribute '%s'.",
		                hdf5_get_objname (attr_id)));

	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

static inline h5_ssize_t
hdf5_get_attribute_name (
        hid_t attr_id,
        size_t buf_size,
        char *buf
        ) {
	HDF5_WRAPPER_ENTER (h5_ssize_t,
	                    "attr_id=%d (%s), buf_size=%llu, buf=%p",
	                    attr_id, hdf5_get_objname (attr_id),
	                    (unsigned long long)buf_size, buf);
	ssize_t size = H5Aget_name ( attr_id, buf_size, buf );
	if (size < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot get attribute name." ));
	HDF5_WRAPPER_RETURN ((h5_size_t)size);
}

static inline hid_t
hdf5_get_attribute_type (
        hid_t attr_id
        ) {
	HDF5_WRAPPER_ENTER (hid_t,
	                    "attr_id=%d (%s)",
	                    attr_id, hdf5_get_objname (attr_id));
	hid_t datatype_id = H5Aget_type (attr_id);
	if (datatype_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot get type of attribute '%s'.",
		                hdf5_get_objname (attr_id)));
	HDF5_WRAPPER_RETURN (datatype_id);
}

static inline hid_t
hdf5_get_attribute_dataspace (
        hid_t attr_id
        ) {
	HDF5_WRAPPER_ENTER (hid_t,
	                    "attr_id=%d (%s)",
	                    attr_id, hdf5_get_objname (attr_id));
	hid_t space_id = H5Aget_space (attr_id);
	if (space_id < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot get dataspace of attribute '%s'.",
		                hdf5_get_objname (attr_id)));
	HDF5_WRAPPER_RETURN (space_id);
}

static inline int
hdf5_get_num_attribute (
        hid_t loc_id
        ) {
	HDF5_WRAPPER_ENTER (int,
	                    "loc_id=%d (%s)",
	                    loc_id, hdf5_get_objname (loc_id));
	int num = H5Aget_num_attrs (loc_id);
	if (num < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot get number of attributes of '%s'.",
		                hdf5_get_objname (loc_id)));
	HDF5_WRAPPER_RETURN (num);
}

static inline herr_t
hdf5_delete_attribute (
        hid_t loc_id,
        const char* attrib_name
        ) {
	HDF5_WRAPPER_ENTER (herr_t,
	                    "loc_id=%d (%s), attr_name='%s'",
	                    loc_id, hdf5_get_objname (loc_id), attrib_name);
	herr_t herr = H5Adelete (loc_id, attrib_name);
	if (herr < 0)
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot delete attribute '%s' of '%s'.",
		                attrib_name,
		                hdf5_get_objname (loc_id)));
	HDF5_WRAPPER_RETURN (herr);
}

static inline h5_err_t
hdf5_close_attribute (
        hid_t attr_id
        ) {
	HDF5_WRAPPER_ENTER (h5_err_t,
	                    "attr_id=%d (%s)",
	                    attr_id, hdf5_get_objname (attr_id));
	if (H5Aclose (attr_id))
		HDF5_WRAPPER_LEAVE (
		        h5_error (
		                H5_ERR_HDF5,
		                "Cannot close attribute '%s'.",
		                hdf5_get_objname (attr_id)));

	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

#endif
