#include <hdf5.h>

#include "h5_core/h5_core.h"
#include "h5_core/h5_core_private.h"

hid_t
_h5_open_group (
	h5_file_t * f,
	const hid_t parent_gid,
	const char * const grpname
	) {
	hid_t gid;
	herr_t herr = H5Gget_objinfo(
		parent_gid, grpname, 1, NULL );

	if ( herr >= 0 ) {
		h5_info (
			"Opening group %s/%s.",
			h5_get_objname(parent_gid),
			grpname );
		gid = H5Gopen ( parent_gid, grpname, H5P_DEFAULT );
	} else {
		h5_info (
			"Creating group %s/%s.",
			h5_get_objname(parent_gid),
			grpname );
		gid = H5Gcreate ( parent_gid, grpname, 0,
				  H5P_DEFAULT, H5P_DEFAULT );
	}
	if ( gid < 0 )
		return HANDLE_H5G_OPEN_ERR (
			h5_get_objname(parent_gid),
			grpname );

	return gid;
}

h5_err_t
_h5_close_group (
	hid_t group_id
	) {
	const char *group_name = h5_get_objname( group_id );
	herr_t herr = H5Gclose ( group_id );
	if ( herr < 0 ) {
		return (*h5_get_errorhandler()) (
			h5_get_funcname(),
			H5_ERR_HDF5,
			"Cannot terminate access to group \"%s\".",
			group_name );
	}
	return H5_SUCCESS;
}
