#include <string.h>
#include <hdf5.h>
#include "h5_core.h"
#include "h5_core_private.h"

struct _iter_op_data {
	int stop_idx;
	int count;
	int type;
	char *name;
	size_t len;
	char *pattern;
};

/*!
  \ingroup h5part_kernel

  Iterator for \c H5Giterate().
*/
static herr_t
_iteration_operator (
	hid_t group_id,		/*!< [in]  group id */
	const char *member_name,/*!< [in]  group name */
	void *operator_data	/*!< [in,out] data passed to the iterator */
	) {

	struct _iter_op_data *data = (struct _iter_op_data*)operator_data;
	herr_t herr;
	H5G_stat_t objinfo;

	if ( data->type != H5G_UNKNOWN ) {
		herr = H5Gget_objinfo ( group_id, member_name, 1, &objinfo );
		if ( herr < 0 ) 
			return -1;

		if ( objinfo.type != data->type )
			return 0;/* don't count, continue iteration */
	}

	if ( data->name && (data->stop_idx == data->count) ) {
		memset ( data->name, 0, data->len );
		strncpy ( data->name, member_name, data->len-1 );
		
		return 1;	/* stop iteration */
	}
	/*
	  count only if pattern is NULL or member name matches
	*/
	if ( !data->pattern ||
	     (strncmp (member_name, data->pattern, strlen(data->pattern)) == 0)
	      ) {
		data->count++;
	}
	return 0;		/* continue iteration */
}

/*!
  \ingroup h5part_kernel

  Get number of objects of type \c type matching ^pattern.

  If pattern is NULL, count all objects of given type.
*/
h5_int64_t
hdf5_get_num_objects_matching_pattern (
	hid_t group_id,
	const char *group_name,
	const hid_t type,
	char * const pattern
	) {

	int idx = 0;
	struct _iter_op_data data;

	memset ( &data, 0, sizeof ( data ) );
	data.type = type;
	data.pattern = pattern;

	TRY( H5Giterate ( group_id, group_name, &idx,
			  _iteration_operator, &data ) );
	
	return data.count;
}



/*!
  \ingroup h5part_kernel

  Get number of object of type \c type in HDF5 group \c group_id.
*/
h5_int64_t
hdf5_get_num_objects (
	hid_t group_id,
	const char *group_name,
	const hid_t type
	) {

	return hdf5_get_num_objects_matching_pattern (
		group_id,
		group_name,
		type,
		NULL );
}

const char *
h5_get_objname (
	hid_t id
	) {
	static char objname[256];

	memset ( objname, 0, sizeof(objname) );
	ssize_t size = H5Iget_name ( id, objname, sizeof(objname) );
	if ( size < 0 ) {
		strcpy ( objname, "[error getting object name]" );
	} else if ( size == 0 ) {
		strcpy ( objname, "[no name associated with identifier]" );
	}

	return objname;
}

/*!
  \ingroup h5part_kernel

  Iterator for \c H5Giterate().
  @@@ to be fixed: idx not found error handling
*/
h5_int64_t
hdf5_get_object_name (
	hid_t group_id,
	const char *group_name,
	const hid_t type,
	const h5_int64_t idx,
	char *obj_name,
	const h5_int64_t len_obj_name
	) {

	herr_t herr;
	struct _iter_op_data data;
	int iterator_idx = 0;

	memset ( &data, 0, sizeof ( data ) );
	data.stop_idx = (hid_t)idx;
	data.type = type;
	data.name = obj_name;
	data.len = (size_t)len_obj_name;

	herr = H5Giterate ( group_id, group_name, &iterator_idx,
			    _iteration_operator,
			    &data );
	if ( herr < 0 ) return (h5_int64_t)herr;

	return H5_SUCCESS;
}

