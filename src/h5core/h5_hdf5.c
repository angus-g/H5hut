#include <stdlib.h>
#include <string.h>
#include <hdf5.h>

#include "h5core/h5_core.h"
#include "h5_core_private.h"


typedef struct op_data {
	int queried_idx;
	int cnt;
	H5O_type_t type;
	char *name;
	size_t len;
	char *prefix;
} op_data_t;

static H5O_type_t
iter_op_get_obj_type (
	const hid_t g_id,
	const char* name,
	const H5L_info_t* info
	) {
	herr_t herr;
	H5O_info_t objinfo;

	if (info->type == H5L_TYPE_EXTERNAL) {
		char* buf = h5_calloc (1, info->u.val_size);
		if ((ptrdiff_t)buf == (ptrdiff_t)H5_ERR) {
			return H5O_TYPE_UNKNOWN;
		}
		herr = H5Lget_val(g_id, name, buf,
					info->u.val_size, H5P_DEFAULT);
		if (herr < 0) {
			h5_error (
				H5_ERR_HDF5,
				"Can't get external link for object '%s'!",
				name);
			return H5O_TYPE_UNKNOWN;
		}
		const char *filename;
		const char *objname;
		herr = H5Lunpack_elink_val(buf, info->u.val_size, 0,
					   &filename, &objname);
		if (herr < 0) {
			h5_error(
				H5_ERR_HDF5,
				"Can't unpack external link for object '%s'!",
				name);
			return H5O_TYPE_UNKNOWN;
		}
		h5_debug(
			"Followed external link to file '%s' / object '%s'.",
			filename, objname);

		h5_free (buf);

		hid_t obj_id = H5Oopen(g_id, name, H5P_DEFAULT);
		if (obj_id < 0) {
			h5_error(
				H5_ERR_HDF5,
				"Can't open external link for object '%s'!",
				name);
			return H5O_TYPE_UNKNOWN;
		}
		herr = H5Oget_info(obj_id, &objinfo);	
	}
	else { // H5L_TYPE_HARD
		herr = H5Oget_info_by_name(g_id, name, &objinfo, H5P_DEFAULT);
	}
	
	if (herr < 0) {
		h5_error(
			H5_ERR_HDF5,
			"Can't query object with name '%s'!", name);
		return H5O_TYPE_UNKNOWN;
	}
	return objinfo.type;
}

static herr_t
iter_op_count (
	hid_t g_id,
	const char* name,
	const H5L_info_t* info,
	void* _op_data
	) {
	op_data_t* op_data = (op_data_t*)_op_data;
	H5O_type_t type = iter_op_get_obj_type (g_id, name, info);
	if (type == H5O_TYPE_UNKNOWN)
		return -1;
	if (type == op_data->type)
		op_data->cnt++;
	return 0;
}

static herr_t
iter_op_idx (
	hid_t g_id,
	const char* name,
	const H5L_info_t* info,
	void* _op_data
	) {
	op_data_t* op_data = (op_data_t*)_op_data;
	H5O_type_t type = iter_op_get_obj_type (g_id, name, info);
	if (type == H5O_TYPE_UNKNOWN)
		return -1;
	if (type != op_data->type)
		return 0;	// ignore on wrong type
	op_data->cnt++;
	/* stop iterating if index is equal cnt */
	if (op_data->queried_idx == op_data->cnt) {
		memset (op_data->name, 0, op_data->len);
		strncpy (op_data->name, name, op_data->len-1);
		return 1;
	}
	return 0;
}

static herr_t
iter_op_count_match (
	hid_t g_id,
	const char* name,
	const H5L_info_t* info,
	void* _op_data
	) {
	H5_PRIV_FUNC_ENTER4 (herr_t,
			     "g_id=%d, name=\"%s\", info=0x%p, _op_data=0x%p",
			     g_id, name, info, _op_data);
	op_data_t* op_data = (op_data_t*)_op_data;
	H5O_type_t type;
	TRY (type = iter_op_get_obj_type (g_id, name, info));
	if (type != op_data->type)
		H5_PRIV_FUNC_LEAVE (0);
	/* count if prefix matches */
	if (strncmp (name, op_data->prefix, strlen(op_data->prefix)) == 0) {
		op_data->cnt++;
	}
	H5_PRIV_FUNC_RETURN (0);
}

ssize_t
hdf5_get_num_groups (
	const hid_t loc_id
	) {
	HDF5_WRAPPER_ENTER2 (ssize_t,
			     "loc_id=%d (%s)", loc_id, hdf5_get_objname (loc_id));
	op_data_t op_data;
	memset (&op_data, 0, sizeof (op_data));
	op_data.type = H5O_TYPE_GROUP;
	hsize_t start_idx = 0;
	herr_t herr = H5Literate (loc_id, H5_INDEX_NAME, H5_ITER_INC,
				  &start_idx,
				  iter_op_count, &op_data);
	if (herr < 0) {
		HDF5_WRAPPER_LEAVE (
			h5_error (
				H5_ERR_HDF5,
				"Cannot get number of groups in \"%s\".",
				hdf5_get_objname (loc_id)));
	}
	HDF5_WRAPPER_RETURN (op_data.cnt);
}

ssize_t
hdf5_get_num_groups_matching_prefix (
	const hid_t loc_id,
	char* prefix
	) {
	HDF5_WRAPPER_ENTER3 (ssize_t,
			     "loc_id=%d (%s), prefix=\"%s\"",
			     loc_id, hdf5_get_objname (loc_id), prefix);
	op_data_t op_data;
	memset (&op_data, 0, sizeof (op_data));
	op_data.type = H5O_TYPE_GROUP;
	op_data.prefix = prefix;
	hsize_t start_idx = 0;
	herr_t herr = H5Literate (loc_id, H5_INDEX_NAME, H5_ITER_INC,
				  &start_idx,
				  iter_op_count_match, &op_data);
	if (herr < 0) {
		HDF5_WRAPPER_LEAVE (
			h5_error (
				H5_ERR_HDF5,
				"Cannot get number of groups with prefix"
				" \"%s\" in \"%s\".",
				prefix, hdf5_get_objname (loc_id)));
	}
	HDF5_WRAPPER_RETURN (op_data.cnt);
}

h5_err_t
hdf5_get_name_of_group_by_idx (
	hid_t loc_id,
	hsize_t idx,
	char *name,
	size_t len
	) {
	HDF5_WRAPPER_ENTER5 (h5_err_t,
			     "loc_id=%d (%s), idx=%llu, name=0x%p, len=%llu",
			     loc_id, hdf5_get_objname (loc_id),
			     idx, name, (unsigned long long)len);
	op_data_t op_data;
	memset (&op_data, 0, sizeof (op_data));
	op_data.type = H5O_TYPE_GROUP;
	op_data.cnt = -1;
	op_data.queried_idx = idx;
        op_data.name = name;
        op_data.len = len;
	hsize_t start_idx = 0;
	herr_t herr = H5Literate (loc_id, H5_INDEX_NAME, H5_ITER_INC,
				  &start_idx,
				  iter_op_idx, &op_data);
	if (herr < 0) {
		HDF5_WRAPPER_LEAVE (
			h5_error (
				H5_ERR_HDF5,
				"Cannot get name of group with index"
				" \"%lu\" in \"%s\".",
				(long unsigned int)idx,
				hdf5_get_objname (loc_id)));
	}
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

ssize_t
hdf5_get_num_datasets (
	const hid_t loc_id
	) {
	HDF5_WRAPPER_ENTER2 (ssize_t,
			     "loc_id=%d (%s)", loc_id, hdf5_get_objname (loc_id));
	op_data_t op_data;
	memset (&op_data, 0, sizeof (op_data));
	op_data.type = H5O_TYPE_DATASET;
	hsize_t start_idx = 0;
	herr_t herr = H5Literate (loc_id, H5_INDEX_NAME, H5_ITER_INC,
				  &start_idx,
				  iter_op_count, &op_data);
	if (herr < 0) {
		HDF5_WRAPPER_LEAVE (
			h5_error (
				H5_ERR_HDF5,
				"Cannot get number of datasets in \"%s\".",
				hdf5_get_objname (loc_id)));
	}
	HDF5_WRAPPER_RETURN (op_data.cnt);
}

/*
  Get name of the \c idx th dataset in group \c loc_id.
 */
h5_err_t
hdf5_get_name_of_dataset_by_idx (
	hid_t loc_id,
	hsize_t idx,
	char *name,
	size_t len
	) {
	HDF5_WRAPPER_ENTER5 (h5_err_t,
			     "loc_id=%d (%s), idx=%llu, name=0x%p, len=%llu",
			     loc_id, hdf5_get_objname (loc_id),
			     idx, name, (unsigned long long)len);
	op_data_t op_data;
	memset (&op_data, 0, sizeof (op_data));
	op_data.type = H5O_TYPE_DATASET;
	op_data.cnt = -1;
	op_data.queried_idx = idx;
        op_data.name = name;
        op_data.len = len;
	hsize_t start_idx = 0;
	herr_t herr = H5Literate (loc_id, H5_INDEX_NAME, H5_ITER_INC,
				  &start_idx,
				  iter_op_idx, &op_data);
	if (herr < 0) {
		HDF5_WRAPPER_LEAVE (
			h5_error (
				H5_ERR_HDF5,
				"Cannot get name of dataset with index"
				" \"%lu\" in \"%s\".",
				(long unsigned int)idx,
				hdf5_get_objname (loc_id)));
	}
	HDF5_WRAPPER_RETURN (H5_SUCCESS);
}

/****** I d e n t i f i e r **************************************************/

const char *
hdf5_get_objname (
	hid_t id
	) {
	static char objname[256];

	// memset ( objname, 0, sizeof(objname) );
	if (id == -1) {
		strcpy ( objname, "[none]" );
	} else {
		ssize_t size = H5Iget_name ( id, objname, sizeof(objname) );
		if ( size < 0 ) {
			strcpy ( objname, "[error getting object name]" );
		} else if ( size == 0 ) {
			strcpy ( objname, "[no name associated with identifier]" );
		}
	}
	return objname;
}
